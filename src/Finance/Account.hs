{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}

module Finance.Account where

import           Finance.Dollar     (Dollar, dollarPrinter)
import           Finance.Interest   (CompoundingInterest (..),
                                     makeInterestIfApplicable)

import           Control.DeepSeq    (NFData)
import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Binary        (Binary)
import           Data.Foldable      (foldlM)
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Maybe         (isJust)
import           Data.String        (IsString)
import           Data.Text          (Text)
import qualified Data.Text          as T
import           Data.Time.Calendar (Day)
import           GHC.Generics       (Generic)


type Accounts = Map AccountId AccountAux
type Balances = Map AccountId Dollar

getBalances :: Accounts -> Balances
getBalances = fmap accountAuxBalance

data AccountAux = AccountAux
  { accountAuxLimit    :: AccountLimit
  , accountAuxColor    :: Text
  , accountAuxBalance  :: Dollar
  , accountAuxInterest :: Maybe CompoundingInterest
  } deriving (Show, Read, Eq, Ord, Generic)
instance NFData AccountAux
instance Binary AccountAux

-- makeInterestFinancePlan :: Day -> AccountId -> AccountAux -> Maybe FinancePlan
-- makeInterestFinancePlan today accountId accountAux@AccountAux{accountAuxBalance,accountAuxInterest} = case accountAuxInterest of
--   Nothing -> Nothing
--   Just i@CompoundingInterest{compoundingInterestInterval} -> Just $ FinancePlan
--     { financePlanType = FinancePlanIncome $ Income accountId accountAux
--     , financePlanSchedule = RepeatingSchedule compoundingInterestInterval
--     , financePlanValue = makeInterestIfApplicable i accountAuxBalance today
--     , financePlanNote = "Account " <> T.pack (show accountId) <> " applied interest"
--     }

applyInterest :: Day -> AccountAux -> AccountAux
applyInterest day a@AccountAux{accountAuxBalance,accountAuxInterest,accountAuxLimit} = case accountAuxInterest of
  Nothing -> a
  Just i ->
    a { accountAuxBalance =
        let newBalance = accountAuxBalance + makeInterestIfApplicable i accountAuxBalance day
        in  case accountAuxLimit of
          NoRestriction -> newBalance
          OnlyPositive
            | newBalance < 0 -> 0
            | otherwise -> newBalance
          OnlyNegative
            | newBalance > 0 -> 0
            | otherwise -> newBalance
      }

data AccountLimit
  = NoRestriction
  | OnlyPositive
  | OnlyNegative
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
instance NFData AccountLimit
instance Binary AccountLimit

-- | An account with a unique identifier
newtype AccountId = AccountId {getAccountId :: Text}
  deriving (Show, Read, Eq, Ord, Generic, NFData, IsString, ToJSON, FromJSON, Binary)

blankAccount :: (AccountId, AccountAux)
blankAccount =
  ( ""
  , AccountAux
    { accountAuxLimit = NoRestriction
    , accountAuxColor = ""
    , accountAuxBalance = 0
    , accountAuxInterest = Nothing
    }
  )

outOfLimitError :: AccountId -> AccountLimit -> Dollar -> Maybe Text
outOfLimitError name limit v = case limit of
  NoRestriction -> Nothing
  OnlyPositive
    | v < 0 ->
      Just $ "Account " <> T.pack (show name) <> " is Only Positive but has value of $" <> dollarPrinter v
    | otherwise -> Nothing
  OnlyNegative
    | v > 0 ->
      Just $ "Account " <> T.pack (show name) <> " is Only Negative but has value of $" <> dollarPrinter v
    | otherwise -> Nothing

validate :: AccountId -> AccountLimit -> Dollar -> Maybe Text
validate name limit v
  | name == "" = Just "Account name can't be left blank"
  | otherwise = outOfLimitError name limit v

addAccount :: Accounts -> AccountId -> AccountAux -> Maybe Accounts
addAccount acc name aux@AccountAux{accountAuxLimit,accountAuxBalance}
  | isJust (validate name accountAuxLimit accountAuxBalance) = Nothing
  | Map.null (Map.filterWithKey (\a' _ -> a' == name) acc) =
      pure $ Map.insert name aux acc
  | otherwise = Nothing

mkAccounts :: [(AccountId, AccountAux)] -> Maybe Accounts
mkAccounts = foldlM (uncurry . addAccount) Map.empty
-- FIXME make it create as much as it can instead
