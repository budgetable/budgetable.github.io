{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Finance.DayOf where

import           Utils.ChartChange (CausesChartChange (..))

import           Control.DeepSeq   (NFData)
import           Data.Binary       (Binary)
import           Data.Text         (Text)
import           GHC.Generics      (Generic)


data DayOfWeek = Sun | Mon | Tue | Wed | Thu | Fri | Sat
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
instance NFData DayOfWeek
instance Binary DayOfWeek
instance CausesChartChange DayOfWeek where
  chartChangeEq x y = x == y

dayOfWeekNum :: DayOfWeek -> Int
dayOfWeekNum x = case x of
  Sun -> 7
  Mon -> 1
  Tue -> 2
  Wed -> 3
  Thu -> 4
  Fri -> 5
  Sat -> 6
prettyPrintDayOfWeek :: DayOfWeek -> Text
prettyPrintDayOfWeek x = case x of
  Sun -> "Sunday"
  Mon -> "Monday"
  Tue -> "Tuesday"
  Wed -> "Wednesday"
  Thu -> "Thursday"
  Fri -> "Friday"
  Sat -> "Saturday"
type DayOfMonth = Int
type DayOfYear = Int
