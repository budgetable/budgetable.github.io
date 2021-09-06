{-# LANGUAGE
    OverloadedLabels
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  #-}

module ScheduledTransferView where

import Finance (ScheduledTransfer (..), RepeatingTransfer (..), DayOfWeek (Sun), prettyPrintDayOfWeek, isRepeating)
import DayView (dayEdit)

import Prelude hiding (max, min)
import Shpadoinkle (Html, text, RawNode (..), listenRaw)
import Shpadoinkle.Html (option, select, onOption, input', type', value, placeholder, step,
                         onInput, max, min, br'_, onCheckM, checked, selected)
import Shpadoinkle.Lens (onSum)
import Shpadoinkle.Continuation (done, pur)

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Generics.Labels ()
import qualified Data.Text as T
import Data.Time.Calendar (Day, toGregorian, fromGregorian, gregorianMonthLength)
import Data.Time.Clock (getCurrentTime, utctDay)
import Text.Read (readMaybe)
import Language.Javascript.JSaddle (makeObject, unsafeGetProp, fromJSValUnchecked)


data RepeatingTransferPicker
  = PickerRepeatingDaily
  | PickerRepeatingWeekly
  | PickerRepeatingMonthly
  | PickerRepeatingYearly
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
repeatingToPicker :: RepeatingTransfer -> RepeatingTransferPicker
repeatingToPicker r = case r of
  RepeatingDaily -> PickerRepeatingDaily
  RepeatingWeekly _ -> PickerRepeatingWeekly
  RepeatingMonthly _ -> PickerRepeatingMonthly
  RepeatingYearly _ -> PickerRepeatingYearly
isRepeatingPickedDifferent :: RepeatingTransfer -> RepeatingTransferPicker -> Bool
isRepeatingPickedDifferent r p = case (r,p) of
  (RepeatingDaily, PickerRepeatingDaily) -> False
  (RepeatingWeekly _, PickerRepeatingWeekly) -> False
  (RepeatingMonthly _, PickerRepeatingMonthly) -> False
  (RepeatingYearly _, PickerRepeatingYearly) -> False
  _ -> True

scheduledTransferEdit :: forall m. MonadIO m => ScheduledTransfer -> [Html m ScheduledTransfer]
scheduledTransferEdit s =
  [ "Repeating: "
  , input' [type' "checkbox", checked (isRepeating s), onCheckM checkedRepeating]
  , br'_
  ] <> schedule
  where
    checkedRepeating :: Bool -> m (ScheduledTransfer -> ScheduledTransfer)
    checkedRepeating r = case r of
      True -> pure . const $ RepeatingTransfer RepeatingDaily
      False -> do
        today <- utctDay <$> liftIO getCurrentTime
        pure . const $ DateTransfer today
    schedule = case s of
      DateTransfer day -> map (onSum #_DateTransfer) (dayEdit day)
      RepeatingTransfer r ->
        let selectRepeat =
              let mkPicker :: RepeatingTransferPicker -> Html m ScheduledTransfer
                  mkPicker p = option [value . T.pack $ show p]
                    [ case p of
                        PickerRepeatingDaily -> "Repeating Daily"
                        PickerRepeatingWeekly -> "Repeating Weekly"
                        PickerRepeatingMonthly -> "Repeating Monthly"
                        PickerRepeatingYearly -> "Repeating Yearly"
                    ]
                  changePicker t oldT = case oldT of
                    DateTransfer _ -> oldT
                    RepeatingTransfer r ->
                      let p = read $ T.unpack t
                      in  if isRepeatingPickedDifferent r p
                      then RepeatingTransfer $ case p of
                        PickerRepeatingDaily -> RepeatingDaily
                        PickerRepeatingWeekly -> RepeatingWeekly Sun
                        PickerRepeatingMonthly -> RepeatingMonthly 1
                        PickerRepeatingYearly -> RepeatingYearly 1
                      else oldT
              in  select
                    [ value . T.pack . show $ repeatingToPicker r
                    , onOption changePicker
                    ] (mkPicker <$> [minBound .. maxBound])
        in  [ selectRepeat
            , case r of
                RepeatingDaily -> ""
                RepeatingWeekly w ->
                  let changeWeek t _ =
                        let w' = read $ T.unpack t
                        in  RepeatingTransfer $ RepeatingWeekly w'
                      mkWeekday :: DayOfWeek -> Html m ScheduledTransfer
                      mkWeekday w' =
                        let shownW = T.pack $ show w'
                        in  option [value shownW] [text shownW]
                  in  select [value . T.pack $ show w, onOption changeWeek]
                        (mkWeekday <$> [minBound .. maxBound])
                RepeatingMonthly m ->
                  input'
                    [ type' "number"
                    , min "1"
                    , max "31"
                    , step "1"
                    , value . T.pack $ show m
                    , listenRaw "blur" $ \(RawNode n) _ -> do
                        o <- makeObject n
                        v <- unsafeGetProp "value" o
                        t <- fromJSValUnchecked v
                        case readMaybe t of
                          Nothing -> pure done
                          Just m -> pure . pur $ const . RepeatingTransfer $ RepeatingMonthly m
                    , onInput $ \t old -> case readMaybe $ T.unpack t of
                        Nothing -> old
                        Just new -> new
                    ]
                RepeatingYearly m ->
                  input'
                    [ type' "number"
                    , min "1"
                    , max "366"
                    , step "1"
                    , value . T.pack $ show m
                    , listenRaw "blur" $ \(RawNode n) _ -> do
                        o <- makeObject n
                        v <- unsafeGetProp "value" o
                        t <- fromJSValUnchecked v
                        case readMaybe t of
                          Nothing -> pure done
                          Just y -> pure . pur $ const . RepeatingTransfer $ RepeatingMonthly y
                    , onInput $ \t old -> case readMaybe $ T.unpack t of
                        Nothing -> old
                        Just new -> new
                    ]
            ]

scheduledTransferView :: ScheduledTransfer -> Html m a
scheduledTransferView s = case s of
  RepeatingTransfer r -> case r of
    RepeatingDaily -> "Repeating daily"
    RepeatingWeekly w -> text $ "Repeating every " <> prettyPrintDayOfWeek w
    RepeatingMonthly m -> text $ "Repeating every " <> T.pack (show m) <> daySuffix m <> " day of the month"
    RepeatingYearly d -> text $ "Repeating every " <> T.pack (show d) <> daySuffix d <> " day of the year"
  DateTransfer day -> text $ "Occurring once on " <> T.pack (show day)
  where
    daySuffix x
      | deca < 20 && deca > 10 = "th"
      | otherwise = notTeen
      where
        deca = x `mod` 100
        notTeen
          | centa == 1 = "st"
          | centa == 2 = "nd"
          | centa == 3 = "rd"
          | otherwise = "th"
          where
            centa = x `mod` 10
