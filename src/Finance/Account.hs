{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Finance.Account where

import           Finance.Dollar  (Dollar, dollarPrinter)

import           Control.DeepSeq (NFData)
import Data.Foldable (foldlM)
import           Data.Text       (Text)
import qualified Data.Text       as T
import Data.Maybe (isJust)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.String (IsString)
import           GHC.Generics    (Generic)


type Accounts = Map AccountId AccountAux

data AccountAux = AccountAux
  { accountAuxLimit :: AccountLimit
  , accountAuxColor :: Text
  , accountAuxBalance :: Dollar
  } deriving (Show, Read, Eq, Ord, Generic)
instance NFData AccountAux

data AccountLimit
  = NoRestriction
  | OnlyPositive
  | OnlyNegative
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
instance NFData AccountLimit

-- | An account with a unique identifier
newtype AccountId = AccountId {getAccountId :: Text}
  deriving (Show, Read, Eq, Ord, Generic, NFData, IsString)

blankAccount :: (AccountId, AccountAux)
blankAccount = ("", AccountAux NoRestriction "" 0)

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
addAccount acc name aux@(AccountAux limit _ v)
  | isJust (validate name limit v) = Nothing
  | Map.null (Map.filterWithKey (\a' _ -> a' == name) acc) =
      pure $ Map.insert name aux acc
  | otherwise = Nothing

mkAccounts :: [(AccountId, AccountAux)] -> Maybe Accounts
mkAccounts = foldlM (uncurry . addAccount) Map.empty
