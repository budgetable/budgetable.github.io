{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Finance.Schedule where

import           Finance.DayOf               (DayOfMonth, DayOfWeek, DayOfYear,
                                              dayOfWeekNum)

import           Control.DeepSeq             (NFData)
import           Data.Binary                 (Binary)
import           Data.Time.Calendar          (Day (..), gregorianMonthLength,
                                              isLeapYear, toGregorian)
import           Data.Time.Calendar.MonthDay (monthAndDayToDayOfYear)
import           Data.Time.Calendar.WeekDate (toWeekDate)
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

data Repeating = Repeating
  { repeatingInterval :: RepeatingInterval
  , repeatingBegin    :: Maybe Day
  , repeatingEnd      :: Maybe Day
  } deriving (Show, Read, Eq, Ord, Generic)
instance NFData Repeating
instance Binary Repeating
instance Schedulable Repeating where
  isApplicableOn (Repeating i mB mE) day = case mB of
    Nothing -> hasEnd
    Just b
      | day < b -> False
      | otherwise -> hasEnd
    where
      hasEnd = case mE of
        Nothing -> continue
        Just e
          | day > e -> False
          | otherwise -> continue
        where
          continue = isApplicableOn i day

-- | The schedule of a transfer
data Schedule
  = RepeatingSchedule Repeating -- ^ on some repeating schedule
  | DateSchedule Day -- ^ on some specific day (should be in the future)
  deriving (Show, Read, Eq, Ord, Generic)
instance NFData Schedule
instance Binary Schedule
instance Schedulable Schedule where
  isApplicableOn t day = case t of
    DateSchedule day'   -> day == day'
    RepeatingSchedule r -> isApplicableOn r day
isRepeating :: Schedule -> Bool
isRepeating (RepeatingSchedule _) = True
isRepeating _                     = False

