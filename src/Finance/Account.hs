{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Finance.Account where

import           Finance.Dollar  (Dollar, dollarPrinter)

import           Control.DeepSeq (NFData)
import           Data.Text       (Text)
import qualified Data.Text       as T
import           GHC.Generics    (Generic)


data AccountLimit
  = NoRestriction
  | OnlyPositive
  | OnlyNegative
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
instance NFData AccountLimit

-- | An account with a unique identifier and nickname
data Account = Account
  { accountName  :: Text -- ^ unique name
  , accountLimit :: AccountLimit -- ^ limits for the account
  , accountColor :: Text -- ^ hex color
  } deriving (Show, Read, Generic)
instance NFData Account
instance Eq Account where
  (Account x _ _) == (Account y _ _) = x == y
instance Ord Account where
  compare (Account x _ _) (Account y _ _) = compare x y

blankAccount :: Account
blankAccount = Account "" NoRestriction ""

outOfLimitError :: Account -> Dollar -> Maybe Text
outOfLimitError a@(Account name limit _) v = case limit of
  NoRestriction -> Nothing
  OnlyPositive
    | v < 0 ->
      Just $ "Account " <> T.pack (show name) <> " is Only Positive but has value of $" <> dollarPrinter v
    | otherwise -> Nothing
  OnlyNegative
    | v > 0 ->
      Just $ "Account " <> T.pack (show name) <> " is Only Negative but has value of $" <> dollarPrinter v
    | otherwise -> Nothing

validate :: Account -> Dollar -> Maybe Text
validate a@(Account name _ _) v
  | name == "" = Just "Account name can't be left blank"
  | otherwise = outOfLimitError a v
