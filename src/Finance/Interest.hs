{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Finance.Interest where

import           Finance.Dollar              (Dollar)
import           Finance.Schedule            (RepeatingInterval (..),
                                              Schedulable (..))

import           Control.DeepSeq             (NFData)
import           Data.Binary                 (Binary)
import           Data.Default                (Default (..))
import           Data.Time.Calendar          (Day, fromGregorian, isLeapYear,
                                              toGregorian)
import           Data.Time.Calendar.WeekDate (toWeekDate)
import           GHC.Generics                (Generic)
import           Text.Printf                 (PrintfArg)


newtype Interest = Interest {getInterest :: Double}
  deriving (Eq, Ord, Show, Read, Num, NFData, Real, RealFrac, Fractional,
            Generic, RealFloat, Floating, PrintfArg, Binary)
instance Default Interest where
  def = Interest 0

data CompoundingInterest = CompoundingInterest
  { compoundingInterestAnnualRate :: Interest -- ^ APR / APY of the interest rate
  , compoundingInterestInterval   :: RepeatingInterval -- ^ What interval does the interest apply on?
  } deriving (Eq, Ord, Show, Read, Generic)
instance NFData CompoundingInterest
instance Binary CompoundingInterest
instance Schedulable CompoundingInterest where
  isApplicableOn (CompoundingInterest _ interval) day = isApplicableOn interval day
instance Default CompoundingInterest where
  def = CompoundingInterest def (RepeatingMonthly 1)

compoundingRateWhenApplied :: CompoundingInterest -> Day -> Interest
compoundingRateWhenApplied (CompoundingInterest rate' interval) day = case interval of
  RepeatingDaily
    | isLeapYear y -> rate / 366
    | otherwise -> rate / 365
  RepeatingWeekly _
    | (weekdayOfJan1 == 4 && not (isLeapYear y))
      || (weekdayOfJan1 == 3 && isLeapYear y) -> rate / 53
    | otherwise -> rate / 52
  RepeatingMonthly _ -> rate / 12
  RepeatingYearly _ -> rate
  where
    rate = rate' / 100
    (y,_,weekdayOfJan1) =
      let (y',_,_) = toGregorian day
      in  toWeekDate (fromGregorian y' 1 1)

makeInterestIfApplicable :: CompoundingInterest -> Dollar -> Day -> Dollar
makeInterestIfApplicable interest value day
  | isApplicableOn interest day =
    let valueToAdd = compoundingRateWhenApplied interest day * fromIntegral value
    in  round valueToAdd
  | otherwise = 0
