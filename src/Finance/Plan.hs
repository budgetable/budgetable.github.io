{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Finance.Plan where

import           Finance.Account  (Account (..), AccountLimit (..),
                                   accountLimit)
import           Finance.Balances (Balances)
import           Finance.Dollar   (Dollar)
import           Finance.Schedule (Schedulable (..), ScheduledTransfer)

import           Control.DeepSeq  (NFData)
import qualified Data.Map         as Map
import           Data.Text        (Text)
import           GHC.Generics     (Generic)


class ApplyTransaction a where
  applyTransaction :: Balances -> a -> Balances

-- | A transfer with its schedule, accounts, and value
data Transfer = Transfer
  { transferFromAccount :: Account -- ^ the account transferring value from
  , transferToAccount   :: Account -- ^ the account receiving the value
  } deriving (Show, Read, Eq, Ord, Generic)
instance NFData Transfer

-- | Genesis of value for accounts
newtype Income = Income
  { incomeAccount :: Account -- ^ the account receiving the value
  } deriving (Show, Read, Eq, Ord, Generic, NFData)

-- | Burning of value for accounts
newtype Cost = Cost
  { costAccount :: Account -- ^ the account losing the value
  } deriving (Show, Read, Eq, Ord, Generic, NFData)

data FinancePlanType
  = FinancePlanTypeTransfer Transfer
  | FinancePlanTypeIncome   Income
  | FinancePlanTypeCost     Cost
  deriving (Show, Read, Eq, Ord, Generic)
instance NFData FinancePlanType

data FinancePlan = FinancePlan
  { financePlanType     :: FinancePlanType -- ^ type of the financial plan
  , financePlanSchedule :: ScheduledTransfer -- ^ when the finance plan is scheduled
  , financePlanValue    :: Dollar -- ^ the value of the finance plan
  , financePlanNote     :: Text -- ^ optional note for reference
  } deriving (Show, Read, Eq, Ord, Generic)
instance NFData FinancePlan
instance Schedulable FinancePlan where
  isApplicableOn (FinancePlan _ s _ _) d = isApplicableOn s d
instance ApplyTransaction FinancePlan where
  applyTransaction balances FinancePlan{financePlanType, financePlanValue = x} = case financePlanType of
    FinancePlanTypeTransfer (Transfer f t) -> case (Map.lookup f balances, Map.lookup t balances) of
      (Just fromBal, Just toBal)
        | accountLimit f == OnlyPositive && x > fromBal ->
          Map.insert f 0 . Map.insert t (toBal + fromBal) $ balances -- cannibalizes `from`
        | accountLimit t == OnlyNegative && x > negate toBal ->
          Map.insert f (fromBal - negate toBal) . Map.insert t 0 $ balances -- pays off `to`
        | otherwise ->
          Map.insert f (fromBal - x) . Map.insert t (toBal + x) $ balances
      _ -> balances -- fail when non-existent
    FinancePlanTypeIncome (Income a) -> case Map.lookup a balances of
      Just ys
        | accountLimit a == OnlyNegative && x > negate ys ->
          Map.insert (Account "__unclaimed_income" OnlyPositive "") (x - negate ys) . Map.insert a 0 $ balances
        | otherwise -> Map.insert a (ys + x) balances
      Nothing -> balances
    FinancePlanTypeCost (Cost a) -> case Map.lookup a balances of
      Just ys
        | accountLimit a == OnlyPositive && x > ys ->
          Map.insert (Account "__unpaid_cost" OnlyNegative "") (x - ys) . Map.insert a 0 $ balances
        | otherwise -> Map.insert a (ys - x) balances
      Nothing -> balances
