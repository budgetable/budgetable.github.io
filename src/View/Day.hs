{-# LANGUAGE OverloadedStrings #-}

module View.Day where

import           Prelude            hiding (div)
import           Shpadoinkle        (Html, text)
import           Shpadoinkle.Html   (className, div, input', label_, onInput,
                                     onOption, option, placeholder, select,
                                     selected, step, type', value)

import qualified Data.Text          as T
import           Data.Time.Calendar (Day, fromGregorian, gregorianMonthLength,
                                     toGregorian)
import           Text.Read          (readMaybe)


data MonthPicker
  = Jan
  | Feb
  | Mar
  | Apr
  | May
  | Jun
  | Jul
  | Aug
  | Sep
  | Oct
  | Nov
  | Dec
  deriving (Show, Read, Eq, Ord, Enum, Bounded)
monthPickerValue :: MonthPicker -> Int
monthPickerValue x = case x of
  Jan -> 1
  Feb -> 2
  Mar -> 3
  Apr -> 4
  May -> 5
  Jun -> 6
  Jul -> 7
  Aug -> 8
  Sep -> 9
  Oct -> 10
  Nov -> 11
  Dec -> 12
unsafeValueToMonthPicker :: Int -> MonthPicker
unsafeValueToMonthPicker x = case x of
  1  -> Jan
  2  -> Feb
  3  -> Mar
  4  -> Apr
  5  -> May
  6  -> Jun
  7  -> Jul
  8  -> Aug
  9  -> Sep
  10 -> Oct
  11 -> Nov
  12 -> Dec
  _  -> error $ "Value is not proper range (1-12): " <> show x

dayEdit :: Day -> Html m Day
dayEdit day = div [className "row"]
  [ div [className "col"] . (: []) $ div [className "form-group"]
    [ label_ ["Year:"]
    , let changedYear t oldDay =
            let (_,mOld,dOld) = toGregorian oldDay
            in  case readMaybe $ T.unpack t of
              Nothing -> oldDay -- FIXME warn year parsing
              Just y' -> fromGregorian y' mOld dOld
      in  input'
            [ type' "number"
            , step "1"
            , placeholder "year"
            , value . T.pack $ show y
            , onInput changedYear
            , className "form-control"
            ]
    ]
  , div [className "col"] . (: []) $ div [className "form-group"]
    [ label_ ["Month:"]
    , let mkMonth :: MonthPicker -> Html m Day
          mkMonth m' =
            let shownM = T.pack $ show m'
            in  option [value shownM, selected (monthPickerValue m' == m)] [text shownM]
          changedMonth t oldDay =
            let (yOld,_,dOld) = toGregorian oldDay
            in  fromGregorian yOld (monthPickerValue . read $ T.unpack t) dOld
      in  select
            [ value . T.pack . show $ unsafeValueToMonthPicker m
            , onOption changedMonth
            , className "form-select"
            ] (mkMonth <$> [minBound .. maxBound])
    ]
  , div [className "col"] . (: []) $ div [className "form-group"]
    [ label_ ["Day:"]
    , let mkDay d' =
            let shownD = T.pack $ show d'
            in  option [value shownD, selected (d' == d)] [text shownD]
          changedDay t oldDay =
            let (yOld,mOld,_) = toGregorian oldDay
            in  fromGregorian yOld mOld (read $ T.unpack t)
      in  select
            [ value . T.pack $ show d
            , onOption changedDay
            , className "form-select"
            ] (mkDay <$> [1 .. gregorianMonthLength y m])
    ]
  ]
  where
    (y,m,d) = toGregorian day
