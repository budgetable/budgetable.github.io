{-# LANGUAGE
    OverloadedStrings
  , OverloadedLabels
  , RankNTypes
  , ScopedTypeVariables
  #-}

module FinancePlanView where

import Finance (FinancePlan (..), Transfer (..), Income (..), Cost (..), Account (..),
                AccountLimit (NoRestriction), ScheduledTransfer (DateTransfer), blankAccount)
import AccountView (accountView)
import ScheduledTransferView (scheduledTransferView, scheduledTransferEdit)
import DollarView (dollarView, dollarEdit)

import Shpadoinkle (Html, text)
import Shpadoinkle.Html (table_, tr_, td_, th, textProperty', value, option, select, onOption, onOptionM,
                         selected, hidden, disabled, input', styleProp, onInput, placeholder)
import Shpadoinkle.Lens (onRecord, onSum)

import Control.Monad.IO.Class (MonadIO (liftIO))
import qualified Data.Text as T
import Data.Generics.Labels ()
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Time.Clock (getCurrentTime, utctDay)


data FinancePlanPicker
  = PickerFinancePlanTransfer
  | PickerFinancePlanIncome
  | PickerFinancePlanCost
  deriving (Show, Read, Eq, Ord, Enum, Bounded)
financePlanToPicker :: FinancePlan -> FinancePlanPicker
financePlanToPicker f = case f of
  FinancePlanTransfer _ -> PickerFinancePlanTransfer
  FinancePlanIncome _ -> PickerFinancePlanIncome
  FinancePlanCost _ -> PickerFinancePlanCost
isFinancePlanPickedDifferent :: FinancePlan -> FinancePlanPicker -> Bool
isFinancePlanPickedDifferent f p = case (f,p) of
  (FinancePlanTransfer _, PickerFinancePlanTransfer) -> False
  (FinancePlanIncome _, PickerFinancePlanIncome) -> False
  (FinancePlanCost _, PickerFinancePlanCost) -> False
  _ -> True


financePlanEdit :: forall m. MonadIO m => Set Account -> FinancePlan -> [Html m FinancePlan]
financePlanEdit accounts f =
  [ select
    [ value . T.pack . show $ financePlanToPicker f
    , onOptionM (pickedFinancePlan . read . T.unpack)
    ] (mkFinancePicker <$> [minBound .. maxBound])
  ] <>
    ( case f of
        FinancePlanTransfer t -> map (onSum #_FinancePlanTransfer) (transferEdit t)
        FinancePlanIncome t   -> map (onSum #_FinancePlanIncome) (incomeEdit t)
        FinancePlanCost t     -> map (onSum #_FinancePlanCost) (costEdit t)
    )
  where
    transferEdit :: Transfer -> [Html m Transfer]
    transferEdit (Transfer from to s v note) =
      [ onRecord #transferFromAccount (accountPicker from)
      , onRecord #transferToAccount (accountPicker to)
      ] <> map (onRecord #transferSchedule) (scheduledTransferEdit s)
        <> map (onRecord #transferValue) (dollarEdit v)
        <> [onRecord #transferNote $ input' [value note, onInput (const . id), placeholder "Optional Note"]]
    incomeEdit :: Income -> [Html m Income]
    incomeEdit (Income a s v note) =
      [onRecord #incomeAccount (accountPicker a)]
      <> map (onRecord #incomeSchedule) (scheduledTransferEdit s)
      <> map (onRecord #incomeValue) (dollarEdit v)
      <> [onRecord #incomeNote $ input' [value note, onInput (const . id), placeholder "Optional Note"]]
    costEdit :: Cost -> [Html m Cost]
    costEdit (Cost a s v note) =
      [onRecord #costAccount (accountPicker a)]
      <> map (onRecord #costSchedule) (scheduledTransferEdit s)
      <> map (onRecord #costValue) (dollarEdit v)
      <> [onRecord #costNote $ input' [value note, onInput (const . id), placeholder "Optional Note"]]
    accountPicker :: Account -> Html m Account
    accountPicker a =
      select [value . T.pack $ show a, onOption (const . read . T.unpack)] $
        ( option
          [ value . T.pack $ show blankAccount
          , hidden True
          , disabled True
          , selected (a == blankAccount)
          ]
          ["-- Select a saved Account --"]
        )
        : (mkAccount <$> Set.toList accounts)
      where
        mkAccount a'@(Account name _ color) =
          option [value . T.pack $ show a', selected (a' == a), styleProp [("background",color)]] [text name]
    mkFinancePicker :: FinancePlanPicker -> Html m FinancePlan
    mkFinancePicker p =
      option [value . T.pack $ show p]
        [ case p of
            PickerFinancePlanTransfer -> "Transfer"
            PickerFinancePlanIncome -> "Income"
            PickerFinancePlanCost -> "Cost"
        ]
    pickedFinancePlan :: FinancePlanPicker -> m (FinancePlan -> FinancePlan)
    pickedFinancePlan p = do
      today <- utctDay <$> liftIO getCurrentTime
      pure $ \fOld ->
        if isFinancePlanPickedDifferent fOld p
        then case p of
              PickerFinancePlanTransfer ->
                FinancePlanTransfer $
                  Transfer blankAccount blankAccount (DateTransfer today) 0 ""
              PickerFinancePlanIncome ->
                FinancePlanIncome $
                  Income blankAccount (DateTransfer today) 0 ""
              PickerFinancePlanCost ->
                FinancePlanCost $
                  Cost blankAccount (DateTransfer today) 0 ""
        else fOld


financePlanView :: FinancePlan -> Html m a
financePlanView p = case p of
  FinancePlanTransfer (Transfer from to s v note) -> table_
    [ tr_ [th [textProperty' "colspan" "2"] ["Transfer"]]
    , tr_ [td_ ["From:"], td_ [accountView from]]
    , tr_ [td_ ["To:"], td_ [accountView to]]
    , tr_ [td_ ["Schedule:"], td_ [scheduledTransferView s]]
    , tr_ [td_ ["Value:"], td_ [dollarView v]]
    , tr_ [td_ ["Note:"], td_ [text note]]
    ]
  FinancePlanIncome (Income a s v note) -> table_
    [ tr_ [th [textProperty' "colspan" "2"] ["Income"]]
    , tr_ [td_ ["Income:"], td_ [accountView a]]
    , tr_ [td_ ["Schedule:"], td_ [scheduledTransferView s]]
    , tr_ [td_ ["Value:"], td_ [dollarView v]]
    , tr_ [td_ ["Note:"], td_ [text note]]
    ]
  FinancePlanCost (Cost a s v note) -> table_
    [ tr_ [th [textProperty' "colspan" "2"] ["Cost"]]
    , tr_ [td_ ["Cost:"], td_ [accountView a]]
    , tr_ [td_ ["Schedule:"], td_ [scheduledTransferView s]]
    , tr_ [td_ ["Value:"], td_ [dollarView v]]
    , tr_ [td_ ["Note:"], td_ [text note]]
    ]
