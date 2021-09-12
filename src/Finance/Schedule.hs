{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Finance.Schedule where

import           Finance.DayOf               (DayOfMonth, DayOfWeek, DayOfYear,
                                              dayOfWeekNum)

import           Control.DeepSeq             (NFData)
import           Data.Time.Calendar          (Day (..), gregorianMonthLength,
                                              isLeapYear, toGregorian)
import           Data.Time.Calendar.MonthDay (monthAndDayToDayOfYear)
import           Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Binary (Binary)
import           GHC.Generics                (Generic)


deriving instance Generic Day
instance Binary Day

class Schedulable a where
  isApplicableOn :: a -> Day -> Bool

-- | Repeating transfers happen over some period of time (assuming forever)
data RepeatingInterval
  = RepeatingDaily -- ^ everyday
  | RepeatingWeekly DayOfWeek -- ^ once a week on a specific weekday
  | RepeatingMonthly DayOfMonth -- ^ once a month
  | RepeatingYearly DayOfYear -- ^ on some day of the year
  deriving (Show, Read, Eq, Ord, Generic)
instance NFData RepeatingInterval
instance Binary RepeatingInterval
instance Schedulable RepeatingInterval where
  isApplicableOn r day = case r of
    RepeatingDaily -> True
    RepeatingWeekly w ->
      let (_,_,weekDay) = toWeekDate day
      in  weekDay == dayOfWeekNum w
    RepeatingMonthly dayInMonth ->
      let (y,m,d) = toGregorian day
          mLen = gregorianMonthLength y m
      in  d == dayInMonth || (mLen < dayInMonth && d == mLen)
    RepeatingYearly dayInYear ->
      let (y,m,d) = toGregorian day
      in  dayInYear == monthAndDayToDayOfYear (isLeapYear y) m d

-- | The schedule of a transfer
data Schedule
  = RepeatingInterval RepeatingInterval -- ^ on some repeating schedule
  | DateSchedule Day -- ^ on some specific day (should be in the future)
  deriving (Show, Read, Eq, Ord, Generic)
instance NFData Schedule
instance Binary Schedule
instance Schedulable Schedule where
  isApplicableOn t day = case t of
    DateSchedule day'   -> day == day'
    RepeatingInterval r -> isApplicableOn r day
isRepeating :: Schedule -> Bool
isRepeating (RepeatingInterval _) = True
isRepeating _                     = False

