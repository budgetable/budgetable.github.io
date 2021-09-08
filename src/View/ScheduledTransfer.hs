{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module View.ScheduledTransfer where

import           View.Day                    (dayEdit)
import           Finance                     (DayOfWeek (Sun),
                                              RepeatingTransfer (..),
                                              ScheduledTransfer (..),
                                              isRepeating, prettyPrintDayOfWeek)

import           Prelude                     hiding (div, max, min)
import           Shpadoinkle                 (Html, RawNode (..), listenRaw,
                                              text)
import           Shpadoinkle.Continuation    (done, pur)
import           Shpadoinkle.Html            (br'_, checked, className, div,
                                              input', label, label_, max, min,
                                              onCheckM, onInput, onOption,
                                              option, placeholder, select,
                                              selected, step, type', value)
import           Shpadoinkle.Lens            (onSum)

import           Control.Monad.IO.Class      (MonadIO (liftIO))
import           Data.Generics.Labels        ()
import           Data.Maybe                  (fromMaybe)
import qualified Data.Text                   as T
import           Data.Time.Calendar          (Day, fromGregorian,
                                              gregorianMonthLength, toGregorian)
import           Data.Time.Clock             (getCurrentTime, utctDay)
import           Language.Javascript.JSaddle (fromJSValUnchecked, makeObject,
                                              unsafeGetProp)
import           Text.Read                   (readMaybe)


data RepeatingTransferPicker
  = PickerRepeatingDaily
  | PickerRepeatingWeekly
  | PickerRepeatingMonthly
  | PickerRepeatingYearly
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
repeatingToPicker :: RepeatingTransfer -> RepeatingTransferPicker
repeatingToPicker r = case r of
  RepeatingDaily     -> PickerRepeatingDaily
  RepeatingWeekly _  -> PickerRepeatingWeekly
  RepeatingMonthly _ -> PickerRepeatingMonthly
  RepeatingYearly _  -> PickerRepeatingYearly
isRepeatingPickedDifferent :: RepeatingTransfer -> RepeatingTransferPicker -> Bool
isRepeatingPickedDifferent r p = case (r,p) of
  (RepeatingDaily, PickerRepeatingDaily)       -> False
  (RepeatingWeekly _, PickerRepeatingWeekly)   -> False
  (RepeatingMonthly _, PickerRepeatingMonthly) -> False
  (RepeatingYearly _, PickerRepeatingYearly)   -> False
  _                                            -> True

scheduledTransferEdit :: forall m. MonadIO m => ScheduledTransfer -> [Html m ScheduledTransfer]
scheduledTransferEdit s =
  [ div [className "col-md-2"] . (: []) $ div [className "form-check"]
    [ input' [type' "checkbox", checked (isRepeating s), onCheckM checkedRepeating, className "form-check-input"]
    , label [className "form-check-label"] ["Is Repeating?"]
    ]
  ] <> schedule
  where
    checkedRepeating :: Bool -> m (ScheduledTransfer -> ScheduledTransfer)
    checkedRepeating r
      | r = pure . const $ RepeatingTransfer RepeatingDaily
      | otherwise = do
          today <- utctDay <$> liftIO getCurrentTime
          pure . const $ DateTransfer today
    schedule = case s of
      DateTransfer day ->
        [div [className "col"] . (: []) $ onSum #_DateTransfer (dayEdit day)]
      RepeatingTransfer r ->
        let selectRepeat =
              let mkPicker :: RepeatingTransferPicker -> Html m ScheduledTransfer
                  mkPicker p = option [value . T.pack $ show p, selected (p == repeatingToPicker r)]
                    [ case p of
                        PickerRepeatingDaily   -> "Repeating Daily"
                        PickerRepeatingWeekly  -> "Repeating Weekly"
                        PickerRepeatingMonthly -> "Repeating Monthly"
                        PickerRepeatingYearly  -> "Repeating Yearly"
                    ]
                  changePicker t oldT = case oldT of
                    DateTransfer _ -> oldT
                    RepeatingTransfer r ->
                      let p = read $ T.unpack t
                      in  if isRepeatingPickedDifferent r p
                      then RepeatingTransfer $ case p of
                        PickerRepeatingDaily   -> RepeatingDaily
                        PickerRepeatingWeekly  -> RepeatingWeekly Sun
                        PickerRepeatingMonthly -> RepeatingMonthly 1
                        PickerRepeatingYearly  -> RepeatingYearly 1
                      else oldT
              in  select
                    [ value . T.pack . show $ repeatingToPicker r
                    , onOption changePicker
                    , className "form-select"
                    ] (mkPicker <$> [minBound .. maxBound])
        in  [div [className "col"] [selectRepeat]]
            <> ( case r of
                    RepeatingDaily -> []
                    RepeatingWeekly w ->
                      let changeWeek t _ =
                            let w' = read $ T.unpack t
                            in  RepeatingTransfer $ RepeatingWeekly w'
                          mkWeekday :: DayOfWeek -> Html m ScheduledTransfer
                          mkWeekday w' =
                            let shownW = T.pack $ show w'
                            in  option [value shownW, selected (w' == w)] [text shownW]
                      in  [ div [className "col"] . (: []) $ select
                              [ value . T.pack $ show w
                              , onOption changeWeek
                              , className "form-select"
                              ]
                              (mkWeekday <$> [minBound .. maxBound])
                          ]
                    RepeatingMonthly m ->
                      [ div [className "col"] . (: []) $ input'
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
                        , onInput $ \t old -> fromMaybe old . readMaybe $ T.unpack t
                        , className "form-control"
                        ]
                      ]
                    RepeatingYearly m ->
                      [ div [className "col"] . (: []) $ input'
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
                        , onInput $ \t old -> fromMaybe old . readMaybe $ T.unpack t
                        , className "form-control"
                        ]
                      ]
               )

scheduledTransferView :: ScheduledTransfer -> Html m a
scheduledTransferView s = case s of
  RepeatingTransfer r -> case r of
    RepeatingDaily -> "everyday"
    RepeatingWeekly w -> text $ "every " <> prettyPrintDayOfWeek w
    RepeatingMonthly m -> text $ "every " <> T.pack (show m) <> daySuffix m <> " per month"
    RepeatingYearly d -> text $ "every " <> T.pack (show d) <> daySuffix d <> " day per year"
  DateTransfer day -> text $ "once on " <> T.pack (show day)
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
