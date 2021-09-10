{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module Finance.Plan where

import           Finance.Account  (AccountAux (accountAuxLimit), AccountId,
                                   AccountLimit (OnlyNegative, OnlyPositive),
                                   Balances)
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
  { transferFromAccount    :: AccountId -- ^ the account transferring value from
  , transferFromAccountAux :: AccountAux
  , transferToAccount      :: AccountId -- ^ the account receiving the value
  , transferToAccountAux   :: AccountAux
  } deriving (Show, Read, Eq, Ord, Generic)
instance NFData Transfer

-- | Genesis of value for accounts
data Income = Income
  { incomeAccount    :: AccountId -- ^ the account receiving the value
  , incomeAccountAux :: AccountAux
  } deriving (Show, Read, Eq, Ord, Generic)
instance NFData Income

-- | Burning of value for accounts
data Cost = Cost
  { costAccount    :: AccountId -- ^ the account losing the value
  , costAccountAux :: AccountAux
  } deriving (Show, Read, Eq, Ord, Generic)
instance NFData Cost

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
    FinancePlanTypeTransfer (Transfer f faux t taux) -> case (Map.lookup f balances, Map.lookup t balances) of
      (Just fromBal, Just toBal)
        | accountAuxLimit faux == OnlyPositive && x > fromBal ->
          Map.insert f 0 . Map.insert t (toBal + fromBal) $ balances -- cannibalizes `from`
        | accountAuxLimit taux == OnlyNegative && x > negate toBal ->
          Map.insert f (fromBal - negate toBal) . Map.insert t 0 $ balances -- pays off `to`
        | otherwise ->
          Map.insert f (fromBal - x) . Map.insert t (toBal + x) $ balances
      _ -> balances -- fail when non-existent
    FinancePlanTypeIncome (Income a aux) -> case Map.lookup a balances of
      Just ys
        | accountAuxLimit aux == OnlyNegative && x > negate ys ->
          Map.insert "__unclaimed_income" (x - negate ys) . Map.insert a 0 $ balances
        | otherwise -> Map.insert a (ys + x) balances
      Nothing -> balances
    FinancePlanTypeCost (Cost a aux) -> case Map.lookup a balances of
      Just ys
        | accountAuxLimit aux == OnlyPositive && x > ys ->
          Map.insert "__unpaid_cost" (x - ys) . Map.insert a 0 $ balances -- FIXME make discrete?
        | otherwise -> Map.insert a (ys - x) balances
      Nothing -> balances
