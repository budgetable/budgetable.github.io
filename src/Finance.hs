{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , MultiWayIf
  , FlexibleInstances
  , DeriveGeneric
  , OverloadedStrings
  #-}

module Finance where

import           Control.DeepSeq             (NFData)
import           Control.Applicative         ((<|>))
import           Data.Time.Calendar          (Day, toGregorian, gregorianMonthLength, isLeapYear, addDays)
import           Data.Time.Calendar.MonthDay (monthAndDayToDayOfYear)
import           Data.Time.Calendar.WeekDate (toWeekDate)
import           Data.Attoparsec.Text        (Parser, digit, char, many1, parseOnly)
import           Data.Map                    (Map)
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Foldable               (foldl')
import           Text.Read                   (Read (readsPrec))
import           GHC.Generics                (Generic)


data DayOfWeek = Sun | Mon | Tue | Wed | Thu | Fri | Sat
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)
instance NFData DayOfWeek
dayOfWeekNum :: DayOfWeek -> Int
dayOfWeekNum x = case x of
  Sun -> 7
  Mon -> 1
  Tue -> 2
  Wed -> 3
  Thu -> 4
  Fri -> 5
  Sat -> 6
prettyPrintDayOfWeek :: DayOfWeek -> Text
prettyPrintDayOfWeek x = case x of
  Sun -> "Sunday"
  Mon -> "Monday"
  Tue -> "Tuesday"
  Wed -> "Wednesday"
  Thu -> "Thursday"
  Fri -> "Friday"
  Sat -> "Saturday"
type DayOfMonth = Int
type DayOfYear = Int

-- | Repeating transfers happen over some period of time (assuming forever)
data RepeatingTransfer
  = RepeatingDaily -- ^ everyday
  | RepeatingWeekly DayOfWeek -- ^ once a week on a specific weekday
  | RepeatingMonthly DayOfMonth -- ^ once a month
  | RepeatingYearly DayOfYear -- ^ on some day of the year
  deriving (Show, Read, Eq, Ord, Generic)
instance NFData RepeatingTransfer

-- | The schedule of a transfer
data ScheduledTransfer
  = RepeatingTransfer RepeatingTransfer -- ^ on some repeating schedule
  | DateTransfer Day -- ^ on some specific day (should be in the future)
  deriving (Show, Read, Eq, Ord, Generic)
instance NFData ScheduledTransfer
isRepeating :: ScheduledTransfer -> Bool
isRepeating (RepeatingTransfer _) = True
isRepeating _ = False

class Schedulable a where
  isApplicableOn :: a -> Day -> Bool
instance Schedulable ScheduledTransfer where
  isApplicableOn t day = case t of
    DateTransfer day' -> day == day'
    RepeatingTransfer r -> case r of
      RepeatingDaily -> True
      RepeatingWeekly w ->
        let (_,_,weekDay) = toWeekDate day
        in  weekDay == dayOfWeekNum w
      RepeatingMonthly dayInMonth ->
        let (y,m,d) = toGregorian day
        in  d == dayInMonth || gregorianMonthLength y m < dayInMonth
      RepeatingYearly dayInYear ->
        let (y,m,d) = toGregorian day
        in  dayInYear == monthAndDayToDayOfYear (isLeapYear y) m d

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
  } deriving (Show, Read, Eq, Ord, Generic)
instance NFData Account
blankAccount :: Account
blankAccount = Account "" NoRestriction ""

-- | A set of accounts with their balance
type Balances = Map Account Dollar

outOfLimitError :: Account -> Dollar -> Maybe Text
outOfLimitError a@(Account name limit _) v
  | name == "" = Just "Account name can't be left blank"
  | otherwise = case limit of
    NoRestriction -> Nothing
    OnlyPositive
      | v < 0 ->
        Just $ "Account " <> T.pack (show name) <> " is OnlyPositive but has value of " <> T.pack (show v)
      | otherwise -> Nothing
    OnlyNegative
      | v > 0 ->
        Just $ "Account " <> T.pack (show name) <> " is OnlyNegative but has value of " <> T.pack (show v)
      | otherwise -> Nothing

addAccount :: Balances -> Account -> Dollar -> Either Text Balances
addAccount acc a@(Account name limit _) v
  | Map.null (Map.filterWithKey (\a' _ -> accountName a' == name) acc) = case outOfLimitError a v of
      Nothing -> continue
      Just e -> Left e
  | otherwise = Left $ "Account " <> T.pack (show name) <> " already exists in balances"
  where
    continue = pure $ Map.insert a v acc

mkBalances :: [(Account, Dollar)] -> Either Text Balances
mkBalances [] = Right Map.empty
mkBalances ((a,v):xs) = do
  acc <- mkBalances xs
  addAccount acc a v

class ApplyTransaction a where
  applyTransaction :: Balances -> a -> Balances

-- | A transfer with its schedule, accounts, and value
data Transfer = Transfer
  { transferFromAccount :: Account -- ^ the account transferring value from
  , transferToAccount   :: Account -- ^ the account receiving the value
  , transferSchedule    :: ScheduledTransfer -- ^ when the transfer is scheduled
  , transferValue       :: Dollar -- ^ the value being transferred between accounts
  , transferNote        :: Text -- ^ optional note for reference
  } deriving (Show, Read, Eq, Ord, Generic)
instance NFData Transfer
instance Schedulable Transfer where
  isApplicableOn (Transfer _ _ s _ _) d = isApplicableOn s d
instance ApplyTransaction Transfer where
  applyTransaction bs (Transfer f t _ x _) = case (Map.lookup f bs, Map.lookup t bs) of
    (Just fromBal, Just toBal)
      | accountLimit f == OnlyPositive && x > fromBal ->
        Map.insert f 0 . Map.insert t (toBal + fromBal) $ bs -- cannibalizes `from`
      | accountLimit t == OnlyNegative && x > (negate toBal) ->
        Map.insert f (fromBal - (negate toBal)) . Map.insert t 0 $ bs -- pays off `to`
      | otherwise ->
        Map.insert f (fromBal - x) . Map.insert t (toBal + x) $ bs
    _ -> bs -- fail when non-existent

-- | Genesis of value for accounts
data Income = Income
  { incomeAccount  :: Account -- ^ the account receiving the value
  , incomeSchedule :: ScheduledTransfer -- ^ when the transfer will happen
  , incomeValue    :: Dollar -- ^ the value transferred
  , incomeNote     :: Text -- ^ optional note for reference
  } deriving (Show, Read, Eq, Ord, Generic)
instance NFData Income
instance Schedulable Income where
  isApplicableOn (Income _ s _ _) d = isApplicableOn s d
instance ApplyTransaction Income where
  applyTransaction bs (Income a _ x _) = case Map.lookup a bs of
    Just ys
      | accountLimit a == OnlyNegative && x > (negate ys) ->
        Map.insert (Account "__unclaimed_income" OnlyPositive "") (x - (negate ys)) . Map.insert a 0 $ bs
      | otherwise -> Map.insert a (ys + x) bs
    Nothing -> bs

-- | Burning of value for accounts
data Cost = Cost
  { costAccount  :: Account -- ^ the account losing the value
  , costSchedule :: ScheduledTransfer -- ^ when the transfer will happen
  , costValue    :: Dollar -- ^ the value transferred
  , costNote     :: Text -- ^ optional note for reference
  } deriving (Show, Read, Eq, Ord, Generic)
instance NFData Cost
instance Schedulable Cost where
  isApplicableOn (Cost _ s _ _) d = isApplicableOn s d
instance ApplyTransaction Cost where
  applyTransaction bs (Cost a _ x _) = case Map.lookup a bs of
    Just ys
      | accountLimit a == OnlyPositive && x > ys ->
        Map.insert (Account "__unpaid_cost" OnlyNegative "") (x - ys) . Map.insert a 0 $ bs
      | otherwise -> Map.insert a (ys - x) bs
    Nothing -> bs

data FinancePlan
  = FinancePlanTransfer Transfer
  | FinancePlanIncome   Income
  | FinancePlanCost     Cost
  deriving (Show, Read, Eq, Ord, Generic)
instance NFData FinancePlan
instance Schedulable FinancePlan where
  isApplicableOn f d = case f of
    FinancePlanTransfer x -> isApplicableOn x d
    FinancePlanIncome x -> isApplicableOn x d
    FinancePlanCost x -> isApplicableOn x d
instance ApplyTransaction FinancePlan where
  applyTransaction bs x = case x of
    FinancePlanTransfer y -> applyTransaction bs y
    FinancePlanIncome y -> applyTransaction bs y
    FinancePlanCost y -> applyTransaction bs y

-- | Money, in terms of cent (lossless compared to 'Double')
newtype Cent = Cent {getCent :: Integer}
  deriving (Show, Read, Eq, Ord, Num, Generic)
instance NFData Cent

-- | Cent, but with a prettier format
newtype Dollar = Dollar {getDollar :: Cent}
  deriving (Show, Read, Eq, Ord, Num, Generic)
instance NFData Dollar
dollarPrinter :: Dollar -> Text
dollarPrinter (Dollar (Cent xs)) =
  if  | xsLen == 1 -> "0.0" <> xs'
      | xsLen == 2 -> "0." <> xs'
      | otherwise  ->
        let lenWithoutCent = xsLen - 2
            (pfx,sfx) = T.splitAt lenWithoutCent xs'
        in  pfx <> "." <> sfx
  where
    xs' = T.pack $ show xs
    xsLen = T.length xs'
dollarParser :: Parser Dollar
dollarParser =
  let noDecimal = do
        ds <- many1 digit
        pure . Dollar . Cent . read $ ds <> "00"
      oneDecimal = do
        ds <- many1 digit
        _ <- char '.'
        dimes <- digit
        pure . Dollar . Cent . read $ ds <> [dimes,'0']
      twoDecimal = do
        ds <- many1 digit
        _ <- char '.'
        dimes <- digit
        pennies <- digit
        pure . Dollar . Cent . read $ ds <> [dimes,pennies]
      isNegative x = do
        _ <- char '-'
        negate <$> x
      allPositives = twoDecimal <|> oneDecimal <|> noDecimal
  in  isNegative allPositives <|> allPositives

-- | Creates an infinite list of balances indexed by the day
balancesOverTime :: Day -- ^ Today
                 -> Balances -- ^ Initial state
                 -> [FinancePlan] -- ^ Things that happen
                 -> [(Day, Balances)]
balancesOverTime today accounts financePlans =
  (today, newAccounts) : balancesOverTime (addDays 1 today) newAccounts financePlans
  where
    newAccounts =
      let go acc f
            | f `isApplicableOn` today = applyTransaction acc f
            | otherwise = acc
      in  foldl' go accounts financePlans
