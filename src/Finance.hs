{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Finance where

import           Finance.Account            (Balances)
import           Finance.Plan                (FinancePlan, applyTransaction)
import           Finance.Schedule            (isApplicableOn)

import           Control.Applicative         ((<|>))
import           Control.DeepSeq             (NFData)
import           Data.Attoparsec.Text        (Parser, char, digit, many1,
                                              parseOnly)
import           Data.Foldable               (foldl')
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Maybe                  (maybe)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time.Calendar          (Day, addDays,
                                              gregorianMonthLength, isLeapYear,
                                              toGregorian)
import           Data.Time.Calendar.MonthDay (monthAndDayToDayOfYear)
import           Data.Time.Calendar.WeekDate (toWeekDate)
import           GHC.Generics                (Generic)
import           Text.Read                   (Read (readsPrec))


-- | Creates an infinite list of balances indexed by the day
balancesOverTime :: Day -- ^ Today
                 -> Balances -- ^ Initial state
                 -> [FinancePlan] -- ^ Things that happen
                 -> [(Day, Balances)]
balancesOverTime today accounts financePlans =
  (today, newAccounts) : balancesOverTime (addDays 1 today) newAccounts financePlans
  where
    newAccounts =
      let go acc f
            | f `isApplicableOn` today = applyTransaction acc f
            | otherwise = acc
      in  foldl' go accounts financePlans

everyWeek :: [(Day, Balances)] -> [(Day, Balances)]
everyWeek [] = []
everyWeek (x:xs) = x : filter (\(d,_) -> let (_,_,w) = toWeekDate d in w == 7) xs

everyMonth :: [(Day, Balances)] -> [(Day, Balances)]
everyMonth [] = []
everyMonth (x:xs) = x : filter (\(d,_) -> let (_,_,d') = toGregorian d in d' == 1) xs

everyYear :: [(Day, Balances)] -> [(Day, Balances)]
everyYear [] = []
everyYear (x:xs) = x : filter (\(d,_) -> let (_,m,d') = toGregorian d in d' == 1 && m == 1) xs
