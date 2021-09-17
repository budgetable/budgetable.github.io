{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Finance.Schedule where

import           Finance.DayOf               (DayOfMonth, DayOfWeek, DayOfYear,
                                              dayOfWeekNum)

import           Control.DeepSeq             (NFData)
import           Data.Binary                 (Binary)
import           Data.Default                (Default (..))
import           Data.Time.Calendar          (Day (..), diffDays,
                                              gregorianMonthLength, isLeapYear,
                                              toGregorian)
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
instance Default RepeatingInterval where
  def = RepeatingDaily

data Repeating = Repeating
  { repeatingInterval :: RepeatingInterval
  , repeatingSkipping :: Maybe (Day, Integer)
    -- ^ Reference date of applicability based on interval, and number of applicable times to skip.
  , repeatingBegin    :: Maybe Day
  , repeatingEnd      :: Maybe Day
  } deriving (Show, Read, Eq, Ord, Generic)
instance NFData Repeating
instance Binary Repeating
instance Schedulable Repeating where
  isApplicableOn Repeating{..} day = case repeatingBegin of
    Nothing -> hasEnd
    Just b
      | day < b -> False
      | otherwise -> hasEnd
    where
      hasEnd = case repeatingEnd of
        Nothing -> continue
        Just e
          | day > e -> False
          | otherwise -> continue
        where
          continue = case repeatingSkipping of
            Nothing -> onInterval
            Just (refDate, timesToSkip)
              | timesToSkip < 1 -> onInterval -- not skipping
              | otherwise ->
                let daysBetween = diffDays day refDate
                    timesSince = timesToSkip + 1 -- skipping = between, since = end - start
                    monthsBetween =
                      let (dayY,dayM,_) = toGregorian day
                          (refY,refM,_) = toGregorian refDate
                      in  12 * (dayY - refY) + fromIntegral (dayM - refM)
                    yearsBetween =
                      let (dayY,_,_) = toGregorian day
                          (refY,_,_) = toGregorian refDate
                      in  dayY - refY
                in  case repeatingInterval of
                  RepeatingDaily
                    | daysBetween `mod` timesSince == 0 -> onInterval
                  RepeatingWeekly _
                    | (daysBetween `div` 7) `mod` timesSince == 0 -> onInterval
                  RepeatingMonthly _
                    | monthsBetween `mod` timesSince == 0 -> onInterval
                  RepeatingYearly _
                    | yearsBetween `mod` timesSince == 0 -> onInterval
                  _ -> False
            where
              onInterval = isApplicableOn repeatingInterval day
instance Default Repeating where
  def = Repeating
    { repeatingInterval = def
    , repeatingSkipping = Nothing
    , repeatingBegin = Nothing
    , repeatingEnd = Nothing
    }

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

defSchedule :: Day -> Schedule
defSchedule = DateSchedule
