{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}

module Finance.Account where

import           Finance.Dollar     (Dollar, dollarPrinter)
import           Finance.Interest   (CompoundingInterest (..),
                                     makeInterestIfApplicable)
import           Utils.ChartChange  (CausesChartChange (..))

import           Control.DeepSeq    (NFData)
import           Data.Aeson         (FromJSON, ToJSON)
import           Data.Binary        (Binary)
import           Data.Default       (Default (..))
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Maybe         (isJust, mapMaybe)
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
  , accountAuxEditable :: Bool
  , accountAuxDisabled :: Bool
  } deriving (Show, Read, Eq, Ord, Generic)
instance NFData AccountAux
instance Binary AccountAux
instance CausesChartChange AccountAux where
  chartChangeEq
    (AccountAux xLimit xColor xBalance xInterest _ xDisabled)
    (AccountAux yLimit yColor yBalance yInterest _ yDisabled)
    = xLimit `chartChangeEq` yLimit
    && xColor `chartChangeEq` yColor
    && xBalance `chartChangeEq` yBalance
    && xInterest `chartChangeEq` yInterest
    && xDisabled `chartChangeEq` yDisabled
instance Default AccountAux where
  def = AccountAux
    { accountAuxLimit    = def
    , accountAuxColor    = ""
    , accountAuxBalance  = def
    , accountAuxInterest = Nothing
    , accountAuxEditable = True
    , accountAuxDisabled = False
    }


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
instance Default AccountLimit where
  def = NoRestriction
instance CausesChartChange AccountLimit where
  chartChangeEq x y = x == y

-- | An account with a unique identifier
newtype AccountId = AccountId {getAccountId :: Text}
  deriving (Show, Read, Eq, Ord, Generic, NFData, IsString, ToJSON, FromJSON, Binary, CausesChartChange)
instance Default AccountId where
  def = ""

blankAccount :: (AccountId, AccountAux)
blankAccount = (def, def)

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
mkAccounts = Just . Map.unions . mapMaybe (uncurry (addAccount Map.empty))
-- NOTE it used to fail if one was bad. mkAccounts = foldlM (uncurry . addAccount) Map.empty
