{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Finance where

import           Finance.Account             (AccountAux (AccountAux, accountAuxBalance, accountAuxDisabled),
                                              Accounts, Balances, applyInterest)
import           Finance.Plan                (FinancePlan, applyTransaction)
import           Finance.Schedule            (isApplicableOn)

import           Data.Foldable               (foldl')
import qualified Data.Map                    as Map
import           Data.Time.Calendar          (Day, addDays, toGregorian)
import           Data.Time.Calendar.WeekDate (toWeekDate)


-- | Creates an infinite list of balances indexed by the day. Note, interest is applied /before/
-- finance plans are applied, as its assumed that interest takes precedence to customer action.
balancesOverTime :: Day -- ^ Today
                 -> Accounts -- ^ Initial state
                 -> [FinancePlan] -- ^ Things that happen
                 -> [(Day, Balances)]
balancesOverTime today accounts financePlans =
  ( today
  , accountAuxBalance
    <$> Map.filter (\AccountAux{accountAuxDisabled} -> not accountAuxDisabled) newAccounts
  ) : balancesOverTime (addDays 1 today) newAccounts financePlans
  where
    newAccounts :: Accounts
    newAccounts =
      let applyFinancePlan acc f
            | f `isApplicableOn` today = applyTransaction acc f
            | otherwise = acc
      in  foldl' applyFinancePlan (applyInterest today <$> accounts) financePlans

everyWeek :: [(Day, Balances)] -> [(Day, Balances)]
everyWeek [] = []
everyWeek (x:xs) = x : filter (\(d,_) -> let (_,_,w) = toWeekDate d in w == 7) xs

everyMonth :: [(Day, Balances)] -> [(Day, Balances)]
everyMonth [] = []
everyMonth (x:xs) = x : filter (\(d,_) -> let (_,_,d') = toGregorian d in d' == 1) xs

everyYear :: [(Day, Balances)] -> [(Day, Balances)]
everyYear [] = []
everyYear (x:xs) = x : filter (\(d,_) -> let (_,m,d') = toGregorian d in d' == 1 && m == 1) xs
