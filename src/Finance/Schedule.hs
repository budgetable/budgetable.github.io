{-# LANGUAGE DeriveGeneric #-}

module Finance.Schedule where

import           Finance.DayOf               (DayOfMonth, DayOfWeek, DayOfYear,
                                              dayOfWeekNum)

import           Control.DeepSeq             (NFData)
import           Data.Time.Calendar          (Day, addDays,
                                              gregorianMonthLength, isLeapYear,
                                              toGregorian)
import           Data.Time.Calendar.MonthDay (monthAndDayToDayOfYear)
import           Data.Time.Calendar.WeekDate (toWeekDate)
import           GHC.Generics                (Generic)


class Schedulable a where
  isApplicableOn :: a -> Day -> Bool

-- | Repeating transfers happen over some period of time (assuming forever)
data RepeatingTransfer
  = RepeatingDaily -- ^ everyday
  | RepeatingWeekly DayOfWeek -- ^ once a week on a specific weekday
  | RepeatingMonthly DayOfMonth -- ^ once a month
  | RepeatingYearly DayOfYear -- ^ on some day of the year
  deriving (Show, Read, Eq, Ord, Generic)
instance NFData RepeatingTransfer
instance Schedulable RepeatingTransfer where
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
data ScheduledTransfer
  = RepeatingTransfer RepeatingTransfer -- ^ on some repeating schedule
  | DateTransfer Day -- ^ on some specific day (should be in the future)
  deriving (Show, Read, Eq, Ord, Generic)
instance NFData ScheduledTransfer
instance Schedulable ScheduledTransfer where
  isApplicableOn t day = case t of
    DateTransfer day'   -> day == day'
    RepeatingTransfer r -> isApplicableOn r day
isRepeating :: ScheduledTransfer -> Bool
isRepeating (RepeatingTransfer _) = True
isRepeating _                     = False

