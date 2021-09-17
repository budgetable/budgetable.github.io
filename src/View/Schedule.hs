{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module View.Schedule where

import           Bootstrap.Popover           (popoverDismissable)
import           Finance.DayOf               (DayOfWeek (Sun),
                                              prettyPrintDayOfWeek)
import           Finance.Schedule            (Repeating (..),
                                              RepeatingInterval (..),
                                              Schedule (..), isRepeating)
import           View.Day                    (dayEdit)

import           Prelude                     hiding (div, max, min)
import           Shpadoinkle                 (Html, RawNode (..), listenRaw,
                                              text)
import           Shpadoinkle.Continuation    (done, pur)
import           Shpadoinkle.Html            (checked, className, div, div_,
                                              input', label, label_, max, min,
                                              onCheckM, onInput, onOption,
                                              option, placeholder, select,
                                              selected, step, type', value)
import           Shpadoinkle.Lens            (onRecord, onSum)

import           Control.Lens                ((.~), (?~))
import           Control.Lens.Prism          (_Just)
import           Control.Lens.Tuple          (_1, _2)
import           Control.Monad.IO.Class      (MonadIO (liftIO))
import           Data.Default                (def)
import           Data.Generics.Labels        ()
import           Data.Maybe                  (fromMaybe, isJust, isNothing)
import qualified Data.Text                   as T
import           Data.Time.Calendar          (Day)
import           Data.Time.Clock             (getCurrentTime, utctDay)
import           Language.Javascript.JSaddle (fromJSValUnchecked, makeObject,
                                              unsafeGetProp)
import           Text.Read                   (readMaybe)


data RepeatingIntervalPicker
  = PickerRepeatingDaily
  | PickerRepeatingWeekly
  | PickerRepeatingMonthly
  | PickerRepeatingYearly
  deriving (Eq, Ord, Show, Read, Enum, Bounded)
repeatingToPicker :: RepeatingInterval -> RepeatingIntervalPicker
repeatingToPicker r = case r of
  RepeatingDaily     -> PickerRepeatingDaily
  RepeatingWeekly _  -> PickerRepeatingWeekly
  RepeatingMonthly _ -> PickerRepeatingMonthly
  RepeatingYearly _  -> PickerRepeatingYearly
isRepeatingPickedDifferent :: RepeatingInterval -> RepeatingIntervalPicker -> Bool
isRepeatingPickedDifferent r p = case (r,p) of
  (RepeatingDaily, PickerRepeatingDaily)       -> False
  (RepeatingWeekly _, PickerRepeatingWeekly)   -> False
  (RepeatingMonthly _, PickerRepeatingMonthly) -> False
  (RepeatingYearly _, PickerRepeatingYearly)   -> False
  _                                            -> True

scheduleEdit :: forall m. MonadIO m => Schedule -> [Html m Schedule]
scheduleEdit s =
  [ div [className "col-md-2"] . (: []) $ div [className "form-check form-switch"]
    [ input'
      [ type' "checkbox"
      , checked $ isRepeating s
      , onCheckM checkedRepeating
      , className "form-check-input"
      ]
    , label [className "form-check-label"] ["Is Repeating?"]
    ]
  ] <> schedule
  where
    checkedRepeating :: Bool -> m (Schedule -> Schedule)
    checkedRepeating r
      | r = pure . const $ RepeatingSchedule def
      | otherwise = do
          today <- utctDay <$> liftIO getCurrentTime
          pure . const $ DateSchedule today
    schedule = case s of
      DateSchedule day ->
        [div [className "col-md-10"] . (: []) $ onSum #_DateSchedule (dayEdit day)]
      RepeatingSchedule r ->
        map (onSum #_RepeatingSchedule) (repeatingEdit r)

scheduleView :: Schedule -> Html m a
scheduleView s = case s of
  RepeatingSchedule r -> repeatingView r
  DateSchedule day    -> div_ . (: []) . text $ "once on " <> T.pack (show day)

repeatingEdit :: forall m. MonadIO m => Repeating -> [Html m Repeating]
repeatingEdit Repeating{..} =
  [ div [className "col-md-10"] . (: []) . div [className "row"] $
      onRecord #repeatingInterval <$> repeatingIntervalEdit repeatingInterval
  , div [className "col-12"] $
    [ div [className "form-check form-switch"]
      [ let checkedSkipping s
              | s = do
                  today <- utctDay <$> liftIO getCurrentTime
                  pure $ #repeatingSkipping ?~ (today, 0)
              | otherwise = pure $ #repeatingSkipping .~ Nothing
        in  input'
              [ type' "checkbox"
              , checked $ isJust repeatingSkipping
              , onCheckM checkedSkipping
              , className "form-check-input"
              ]
      , label [className "form-check-label"]
        [ "Skips Intervals? "
        , popoverDismissable
          "Skipping"
          "If you'd like to say \"I get paid every two weeks\", or \"I buy gas every 4 days\", then you'll need to <em>skip</em> some of the intervals you're applying on your finance plan."
          [className "badge rounded-pill bg-light text-dark"]
          ["?"]
        ]
      ]
    ] <> case repeatingSkipping of
        Nothing -> []
        Just (sDay,sTimes) ->
          [ label_
            [ "Reference Date: "
            , popoverDismissable
              "Date of Reference for Skipped Intervals"
              "It's easy to say something like \"get paid every other week\", but in <em>reference</em> to what week? This is the date we're concerned with &mdash; if we get paid \"every other week\", what is the first week payment actually occurs?"
              [className "badge rounded-pill bg-light text-dark"]
              ["?"]
            ]
          , onSum (#repeatingSkipping . _Just . _1) (dayEdit sDay) -- FIXME add validation / labels
          , label_ ["Number of Skips per Interval:"]
          , let changedSkipping t = case readMaybe (T.unpack t) of
                  Nothing -> id
                  Just x  -> #repeatingSkipping . _Just . _2 .~ x
            in  input'
                  [ type' "number"
                  , min "0"
                  , step "1"
                  , placeholder "Number of Skips"
                  , value . T.pack $ show sTimes
                  , onInput changedSkipping
                  , className "form-control"
                  ]
          ]
  , div [className "col-12"]
    [ div [className "form-check form-switch"]
      [ let checkedBegin :: Bool -> m (Repeating -> Repeating)
            checkedBegin r
              | r = do
                  today <- utctDay <$> liftIO getCurrentTime
                  pure $ #repeatingBegin ?~ today
              | otherwise = pure $ #repeatingBegin .~ Nothing
        in  input'
              [ type' "checkbox"
              , checked $ isJust repeatingBegin
              , onCheckM checkedBegin
              , className "form-check-input"
              ]
      , label [className "form-check-label"]
        [ "Has Limiting Start Date? "
        , popoverDismissable
          "Start Date for Finance Plan"
          "This gives us a method for saying something like \"I'll receive income starting at a specific date\" &mdash; that date can be supplied here to <em>delimit</em> finance plans."
          [className "badge rounded-pill bg-light text-dark"]
          ["?"]
        ]
      ]
    , case repeatingBegin of
        Nothing -> ""
        Just b  -> onSum (#repeatingBegin . _Just) (dayEdit b)
    ]
  , div [className "col-12"]
    [ div [className "form-check form-switch"]
      [ let checkedEnd :: Bool -> m (Repeating -> Repeating)
            checkedEnd r
              | r = do
                  today <- utctDay <$> liftIO getCurrentTime
                  pure $ #repeatingEnd ?~ today
              | otherwise = pure $ #repeatingEnd .~ Nothing
        in  input'
              [ type' "checkbox"
              , checked $ isJust repeatingEnd
              , onCheckM checkedEnd
              , className "form-check-input"
              ]
      , label [className "form-check-label"]
        [ "Has Limiting End Date? "
        , popoverDismissable
          "End Date for Finance Plan"
          "This gives us a method for saying something like \"I'm making payments up until some date\" &mdash; that date can be supplied here to <em>delimit</em> finance plans."
          [className "badge rounded-pill bg-light text-dark"]
          ["?"]
        ]
      ]
    , case repeatingEnd of
        Nothing -> ""
        Just b  -> onSum (#repeatingEnd . _Just) (dayEdit b)
    ]
  ]

repeatingView :: Repeating -> Html m a
repeatingView Repeating{..} = div_
  [ repeatingIntervalView repeatingSkipping repeatingInterval
  , case repeatingBegin of
      Nothing -> ""
      Just b  -> text $ ", beginning on " <> T.pack (show b)
  , case repeatingEnd of
      Nothing -> ""
      Just e  ->
        let mAnd = case repeatingBegin of
              Nothing -> ""
              Just _  -> "and "
        in  text $ ", " <> mAnd <> "ending on " <> T.pack (show e)
  ]

repeatingIntervalEdit :: RepeatingInterval -> [Html m RepeatingInterval]
repeatingIntervalEdit r =
  let selectRepeat =
        let mkPicker :: RepeatingIntervalPicker -> Html m RepeatingInterval
            mkPicker p = option [value . T.pack $ show p, selected (p == repeatingToPicker r)]
              [ case p of
                  PickerRepeatingDaily   -> "Repeating Daily"
                  PickerRepeatingWeekly  -> "Repeating Weekly"
                  PickerRepeatingMonthly -> "Repeating Monthly"
                  PickerRepeatingYearly  -> "Repeating Yearly"
              ]
            changePicker t rOld =
              let p = read $ T.unpack t
              in  if isRepeatingPickedDifferent rOld p
              then case p of
                PickerRepeatingDaily   -> RepeatingDaily
                PickerRepeatingWeekly  -> RepeatingWeekly Sun
                PickerRepeatingMonthly -> RepeatingMonthly 1
                PickerRepeatingYearly  -> RepeatingYearly 1
              else rOld
        in  select
              [ value . T.pack . show $ repeatingToPicker r
              , onOption changePicker
              , className "form-select"
              ] (mkPicker <$> [minBound .. maxBound])
  in  [ div [className "col"]
        [ div [className "form-group"]
          [ label_ ["Interval:"]
          , selectRepeat
          ]
        ]
      ] <> viewRepeat
  where
    viewRepeat = case r of
      RepeatingDaily -> []
      RepeatingWeekly w ->
        let mkWeekday :: DayOfWeek -> Html m RepeatingInterval
            mkWeekday w' =
              let shownW = T.pack $ show w'
              in  option [value shownW, selected (w' == w)] [text shownW]
        in  [ div [className "col"] . (: []) $ div [className "form-group"]
              [ label_ ["Weekday:"]
              , select
                [ value . T.pack $ show w
                , onOption $ const . RepeatingWeekly . read . T.unpack
                , className "form-select"
                ]
                (mkWeekday <$> [minBound .. maxBound])
              ]
            ]
      RepeatingMonthly m ->
        [ div [className "col"] . (: []) $ div [className "form-group"]
          [ label_ ["Day:"]
          , input'
            [ type' "number"
            , min "1"
            , max "31"
            , step "1"
            , value . T.pack $ show m
            , listenRaw "blur" $ \(RawNode n) _ -> do
                o <- makeObject n
                v <- unsafeGetProp "value" o
                t <- fromJSValUnchecked v
                pure $ case readMaybe t of
                  Nothing   -> done
                  Just mNew -> pur . const $ RepeatingMonthly mNew
            , onInput $ \t old -> fromMaybe old . readMaybe $ T.unpack t
            , className "form-control"
            ]
          ]
        ]
      RepeatingYearly m ->
        [ div [className "col"] . (: []) $ div [className "form-group"]
          [ label_ ["Day of Year:"]
          , input'
            [ type' "number"
            , min "1"
            , max "366"
            , step "1"
            , value . T.pack $ show m
            , listenRaw "blur" $ \(RawNode n) _ -> do
                o <- makeObject n
                v <- unsafeGetProp "value" o
                t <- fromJSValUnchecked v
                pure $ case readMaybe t of
                  Nothing -> done
                  Just y  -> pur . const $ RepeatingMonthly y
            , onInput $ \t old -> fromMaybe old . readMaybe $ T.unpack t
            , className "form-control"
            ]
          ]
        ]

repeatingIntervalView :: Maybe (Day, Integer) -> RepeatingInterval -> Html m a
repeatingIntervalView mSkipping r = text $ case r of
  RepeatingDaily
    | isNothing mSkipping -> "everyday"
    | otherwise -> "every " <> skipping <> "day" <> skippingStarting
  RepeatingWeekly w -> "every " <> skipping <> prettyPrintDayOfWeek w <> skippingStarting
  RepeatingMonthly m
    | isNothing mSkipping -> "every " <> intWithSuffix m <> " per month"
    | otherwise -> "every " <> skipping <> "month on the " <> intWithSuffix m <> skippingStarting
  RepeatingYearly d
    | isNothing mSkipping -> "every " <> intWithSuffix d <> " day per year"
    | otherwise -> "every " <> skipping <> "year on the " <> intWithSuffix d <> " day per year" <> skippingStarting
  where
    intWithSuffix i = T.pack (show i) <> daySuffix i
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
    skipping = case mSkipping of
      Nothing -> ""
      Just (_,i)
        | i == 1 -> "other "
        | otherwise -> intWithSuffix (i + 1) <> " "
    skippingStarting = case mSkipping of
      Nothing    -> ""
      Just (d,_) -> ", starting on " <> T.pack (show d)
