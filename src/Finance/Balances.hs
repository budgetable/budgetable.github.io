{-# LANGUAGE OverloadedStrings #-}

module Finance.Balances where

import           Finance.Account (Account (..), validate)
import           Finance.Dollar  (Dollar)

import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Maybe      (isJust)
import           Data.Text       (Text)
import qualified Data.Text       as T


-- | A set of accounts with their balance
type Balances = Map Account Dollar

addAccount :: Balances -> Account -> Dollar -> Maybe Balances
addAccount acc a@(Account name limit _) v
  | isJust (validate a v) = Nothing
  | Map.null (Map.filterWithKey (\a' _ -> accountName a' == name) acc) =
      pure $ Map.insert a v acc
  | otherwise = Nothing

mkBalances :: [(Account, Dollar)] -> Maybe Balances
mkBalances [] = Just Map.empty
mkBalances ((a,v):xs) = do
  acc <- mkBalances xs
  addAccount acc a v
