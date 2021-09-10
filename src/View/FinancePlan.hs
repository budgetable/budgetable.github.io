{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module View.FinancePlan where

import           Debouncer                   (Debouncer)
import           Finance.Account             (AccountAux (..), AccountId (..),
                                              AccountLimit (NoRestriction),
                                              blankAccount)
import           Finance.Plan                (Cost (..), FinancePlan (..),
                                              FinancePlanType (..), Income (..),
                                              Transfer (..))
import           Finance.Schedule            (ScheduledTransfer (DateTransfer))
import           View.Account                (accountView)
import           View.Dollar                 (DollarEdit (..), dollarEdit,
                                              dollarView)
import           View.ScheduledTransfer      (scheduledTransferEdit,
                                              scheduledTransferView)

import           Prelude                     hiding (div)
import           Shpadoinkle                 (Html, MonadJSM, RawNode (..),
                                              listenRaw, text)
import           Shpadoinkle.Continuation    (pur)
import           Shpadoinkle.Html            (className, debounceRaw, disabled,
                                              div, hidden, input', label_,
                                              onInput, onOption, onOptionM,
                                              option, placeholder, select,
                                              selected, styleProp, table_, td_,
                                              textProperty', th, tr_, value)
import           Shpadoinkle.Lens            (onRecord, onSum)

import           Control.Monad.IO.Class      (MonadIO (liftIO))
import           Data.Generics.Labels        ()
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import qualified Data.Text                   as T
import           Data.Time.Clock             (getCurrentTime, utctDay)
import           Language.Javascript.JSaddle (JSVal, fromJSValUnchecked,
                                              makeObject, toJSVal,
                                              unsafeGetProp)


data FinancePlanPicker
  = PickerFinancePlanTransfer
  | PickerFinancePlanIncome
  | PickerFinancePlanCost
  deriving (Show, Read, Eq, Ord, Enum, Bounded)
financePlanToPicker :: FinancePlanType -> FinancePlanPicker
financePlanToPicker t = case t of
  FinancePlanTypeTransfer _ -> PickerFinancePlanTransfer
  FinancePlanTypeIncome _   -> PickerFinancePlanIncome
  FinancePlanTypeCost _     -> PickerFinancePlanCost
isFinancePlanPickedDifferent :: FinancePlanType -> FinancePlanPicker -> Bool
isFinancePlanPickedDifferent f p = case (f,p) of
  (FinancePlanTypeTransfer _, PickerFinancePlanTransfer) -> False
  (FinancePlanTypeIncome _, PickerFinancePlanIncome)     -> False
  (FinancePlanTypeCost _, PickerFinancePlanCost)         -> False
  _                                                      -> True


financePlanEdit :: forall m
                 . MonadJSM m
                => MonadIO m
                => Set AccountId
                -> Debouncer m T.Text
                -> FinancePlan
                -> Html m FinancePlan
financePlanEdit accounts debouncer (FinancePlan t s v note) = div [className "row"]
  [ div [className "row"] $
    let viewFinancePlan = map (onRecord #financePlanType) $ case t of
          FinancePlanTypeTransfer t ->
            map (onSum #_FinancePlanTypeTransfer) (transferEdit t)
          FinancePlanTypeIncome t ->
            map (onSum #_FinancePlanTypeIncome) (incomeEdit t)
          FinancePlanTypeCost t ->
            map (onSum #_FinancePlanTypeCost) (costEdit t)
    in  financePlanPicker : viewFinancePlan
  , div [className "row"] $
    map (onRecord #financePlanSchedule) (scheduledTransferEdit s)
  , div [className "row"]
      [ div [className "col"] . (: []) $
        let params = DollarEdit
              { dollarEditIsPositive = True
              , dollarEditIsValid = True
              , dollarEditInvalidFeedback = ""
              }
        in  onRecord #financePlanValue (dollarEdit params v)
      , div [className "col"] . (: []) $
        onRecord #financePlanNote $
        input'
          [ value note
          , listenRaw "input" . debouncer $ \(RawNode n) _ -> do
              o <- makeObject n
              v <- unsafeGetProp "value" o
              t <- fromJSValUnchecked v
              pure . pur $ const t
          , placeholder "Optional Note"
          , className "form-control"
          ]
      ]
  ]
  where
    financePlanPicker :: Html m FinancePlan
    financePlanPicker =
      div [className "col"] . (: []) $ div [className "form-group"]
        [ label_ ["Type:"]
        , select
          [ value . T.pack . show $ financePlanToPicker t
          , onOptionM (pickedFinancePlan . read . T.unpack)
          , className "form-select"
          ] (mkFinancePicker <$> [minBound .. maxBound])
        ]
      where
        mkFinancePicker :: FinancePlanPicker -> Html m FinancePlan
        mkFinancePicker p =
          option [value . T.pack $ show p, selected (p == financePlanToPicker t)]
            [ case p of
                PickerFinancePlanTransfer -> "Transfer"
                PickerFinancePlanIncome   -> "Income"
                PickerFinancePlanCost     -> "Cost"
            ]
        pickedFinancePlan :: FinancePlanPicker -> m (FinancePlan -> FinancePlan)
        pickedFinancePlan p = do
          today <- utctDay <$> liftIO getCurrentTime
          pure $ \fOld ->
            if isFinancePlanPickedDifferent (financePlanType fOld) p
            then case p of
                  PickerFinancePlanTransfer ->
                    fOld { financePlanType = FinancePlanTypeTransfer
                           (uncurry (uncurry Transfer blankAccount) blankAccount) } -- FIXME more elegance
                  PickerFinancePlanIncome ->
                    fOld { financePlanType = FinancePlanTypeIncome (uncurry Income blankAccount) }
                  PickerFinancePlanCost ->
                    fOld { financePlanType = FinancePlanTypeCost (uncurry Cost blankAccount) }
            else fOld
    transferEdit :: Transfer -> [Html m Transfer]
    transferEdit (Transfer from _ to _) =
      [ div [className "col"] . (: []) $ div [className "form-group"]
        [ label_ ["From:"]
        , onRecord #transferFromAccount (accountPicker from)
        ]
      , div [className "col"] . (: []) $ div [className "form-group"]
        [ label_ ["To:"]
        , onRecord #transferToAccount (accountPicker to)
        ]
      ]

    incomeEdit :: Income -> [Html m Income]
    incomeEdit (Income a _) =
      [ div [className "col"] . (: []) $ div [className "form-group"]
        [ label_ ["Account:"]
        , onRecord #incomeAccount (accountPicker a)
        ]
      ]

    costEdit :: Cost -> [Html m Cost]
    costEdit (Cost a _) = -- FIXME use named field puns
      [ div [className "col"] . (: []) $ div [className "form-group"]
        [ label_ ["Account:"]
        , onRecord #costAccount (accountPicker a)
        ]
      ]

    accountPicker :: AccountId -> Html m AccountId
    accountPicker a =
      select
        [ value . T.pack $ show a
        , onOption (const . read . T.unpack)
        , className "form-select"
        ] $
        option
          [ value . T.pack . show $ fst blankAccount
          , hidden True
          , disabled True
          , selected (a == fst blankAccount)
          ]
          ["-- Select a saved Account --"]
            : (mkAccount <$> Set.toList accounts)
      where
        mkAccount a'@(AccountId name) =
          option [value . T.pack $ show a', selected (a' == a)] [text name]


financePlanView :: FinancePlan -> Html m a
financePlanView (FinancePlan t s v note) = div [className "row"]
  [ div [className "col-sm-2"] [text note]
  , case t of
      FinancePlanTypeTransfer (Transfer from (AccountAux fromLimit _ _) to (AccountAux toLimit _ _)) ->
        div [className "col-sm-6"] . (: []) $ div [className "row"]
          [ centered $ accountView from fromLimit
          , arrow
          , centered $ dollarView v
          , arrow
          , centered $ accountView to toLimit
          ]
      FinancePlanTypeIncome (Income a (AccountAux aLimit _ _)) -> -- FIXME use puns
        div [className "col-sm-6"] . (: []) $ div [className "row"]
          [ div [className "col"] []
          , div [className "col-sm-1"] []
          , centered $ dollarView v
          , arrow
          , centered $ accountView a aLimit
          ]
      FinancePlanTypeCost (Cost a (AccountAux aLimit _ _)) ->
        div [className "col-sm-6"] . (: []) $ div [className "row"]
          [ centered $ accountView a aLimit
          , arrow
          , centered $ dollarView v
          , div [className "col-sm-1"] []
          , div [className "col"] []
          ]
  , div [className "col-sm-4"] [scheduledTransferView s]
  ]
  where
    arrow = div [className "col-sm-1"] . (: []) $ "&#8594;"
    centered = div [className "col", styleProp [("text-align", "center")]] . (: [])
