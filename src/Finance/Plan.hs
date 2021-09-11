{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Finance.Plan where

import           Finance.Account      (AccountAux (..), AccountId,
                                       AccountLimit (OnlyNegative, OnlyPositive),
                                       Accounts)
import           Finance.Dollar       (Dollar)
import           Finance.Schedule     (Schedulable (..), Schedule)

import           Control.DeepSeq      (NFData)
import           Control.Lens         ((%~), (.~))
import           Data.Generics.Labels ()
import qualified Data.Map             as Map
import           Data.Text            (Text)
import           GHC.Generics         (Generic)


class ApplyTransaction a where
  applyTransaction :: Accounts -> a -> Accounts

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
  , financePlanSchedule :: Schedule -- ^ when the finance plan is scheduled
  , financePlanValue    :: Dollar -- ^ the value of the finance plan
  , financePlanNote     :: Text -- ^ optional note for reference
  } deriving (Show, Read, Eq, Ord, Generic)
instance NFData FinancePlan
instance Schedulable FinancePlan where
  isApplicableOn (FinancePlan _ s _ _) d = isApplicableOn s d
instance ApplyTransaction FinancePlan where
  applyTransaction balances FinancePlan{financePlanType, financePlanValue = x} = case financePlanType of
    FinancePlanTypeTransfer (Transfer f faux t taux) -> case (Map.lookup f balances, Map.lookup t balances) of
      (Just AccountAux{accountAuxBalance = fromBal}, Just AccountAux{accountAuxBalance = toBal})
        | accountAuxLimit faux == OnlyPositive && x > fromBal ->
          Map.adjust (#accountAuxBalance .~ 0) f
          . Map.adjust (#accountAuxBalance .~ (toBal + fromBal)) t
          $ balances -- cannibalizes `from`
        | accountAuxLimit taux == OnlyNegative && x > negate toBal ->
          Map.adjust (#accountAuxBalance .~ (fromBal + toBal)) f
          . Map.adjust (#accountAuxBalance .~ 0) t
          $ balances -- pays off `to`
        | otherwise ->
          Map.adjust (#accountAuxBalance %~ (\y -> y - x)) f
          . Map.adjust (#accountAuxBalance %~ (+ x)) t
          $ balances
      _ -> balances -- fail when non-existent
    -- NOTE the following assumes neither Income or Cost are applied to OnlyNegative or OnlyPositive, respectively
    FinancePlanTypeIncome Income{incomeAccount} ->
      Map.adjust (#accountAuxBalance %~ (+ x)) incomeAccount balances
    FinancePlanTypeCost Cost{costAccount} ->
      Map.adjust (#accountAuxBalance %~ (\y -> y - x)) costAccount balances
