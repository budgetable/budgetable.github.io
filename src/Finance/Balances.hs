{-# LANGUAGE OverloadedStrings #-}

module Finance.Balances where

import           Finance.Account (Account (..), validate)
import           Finance.Dollar  (Dollar)

import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Data.Maybe      (maybe)
import           Data.Text       (Text)
import qualified Data.Text       as T


-- | A set of accounts with their balance
type Balances = Map Account Dollar

addAccount :: Balances -> Account -> Dollar -> Either Text Balances
addAccount acc a@(Account name limit _) v
  | Map.null (Map.filterWithKey (\a' _ -> accountName a' == name) acc) =
      maybe continue Left (validate a v)
  | otherwise = Left $ "Account " <> T.pack (show name) <> " already exists in balances"
  where
    continue = pure $ Map.insert a v acc

mkBalances :: [(Account, Dollar)] -> Either Text Balances
mkBalances [] = Right Map.empty
mkBalances ((a,v):xs) = do
  acc <- mkBalances xs
  addAccount acc a v
