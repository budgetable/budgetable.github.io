{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module View.FinancePlan where

import           View.Account                (accountView)
import           View.Dollar                 (dollarEdit, dollarView)
import           View.ScheduledTransfer      (scheduledTransferEdit,
                                              scheduledTransferView)
import           Debouncer                   (Debouncer)
import           Finance                     (Account (..),
                                              AccountLimit (NoRestriction),
                                              Cost (..), FinancePlan (..),
                                              Income (..),
                                              ScheduledTransfer (DateTransfer),
                                              Transfer (..), blankAccount)

import           Prelude                     hiding (div)
import           Shpadoinkle                 (Html, RawNode (..), listenRaw,
                                              text)
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
financePlanToPicker :: FinancePlan -> FinancePlanPicker
financePlanToPicker f = case f of
  FinancePlanTransfer _ -> PickerFinancePlanTransfer
  FinancePlanIncome _   -> PickerFinancePlanIncome
  FinancePlanCost _     -> PickerFinancePlanCost
isFinancePlanPickedDifferent :: FinancePlan -> FinancePlanPicker -> Bool
isFinancePlanPickedDifferent f p = case (f,p) of
  (FinancePlanTransfer _, PickerFinancePlanTransfer) -> False
  (FinancePlanIncome _, PickerFinancePlanIncome)     -> False
  (FinancePlanCost _, PickerFinancePlanCost)         -> False
  _                                                  -> True


financePlanEdit :: forall m
                 . MonadIO m
                => Set Account
                -> Debouncer m T.Text
                -> FinancePlan
                -> Html m FinancePlan
financePlanEdit accounts debouncer f = div [className "row"] $ case f of
  FinancePlanTransfer t ->
    let (topR,midR,botR) = transferEdit t
    in
      [ div [className "row"] $
        financePlanPicker : map (onSum #_FinancePlanTransfer) topR
      , onSum #_FinancePlanTransfer midR
      , onSum #_FinancePlanTransfer botR
      ]
  FinancePlanIncome t ->
    let (topR,midR,botR) = incomeEdit t
    in
      [ div [className "row"] $
        financePlanPicker : map (onSum #_FinancePlanIncome) topR
      , onSum #_FinancePlanIncome midR
      , onSum #_FinancePlanIncome botR
      ]
  FinancePlanCost t ->
    let (topR,midR,botR) = costEdit t
    in
      [ div [className "row"] $
        financePlanPicker : map (onSum #_FinancePlanCost) topR
      , onSum #_FinancePlanCost midR
      , onSum #_FinancePlanCost botR
      ]
  where
    financePlanPicker :: Html m FinancePlan
    financePlanPicker =
      div [className "col"] . (: []) $ div [className "form-group"]
        [ label_ ["Type:"]
        , select
          [ value . T.pack . show $ financePlanToPicker f
          , onOptionM (pickedFinancePlan . read . T.unpack)
          , className "form-select"
          ] (mkFinancePicker <$> [minBound .. maxBound])
        ]
      where
        mkFinancePicker :: FinancePlanPicker -> Html m FinancePlan
        mkFinancePicker p =
          option [value . T.pack $ show p, selected (p == financePlanToPicker f)]
            [ case p of
                PickerFinancePlanTransfer -> "Transfer"
                PickerFinancePlanIncome   -> "Income"
                PickerFinancePlanCost     -> "Cost"
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
    transferEdit :: Transfer -> ([Html m Transfer], Html m Transfer, Html m Transfer)
    transferEdit (Transfer from to s v note) =
      ( [ div [className "col"] . (: []) $ div [className "form-group"]
          [ label_ ["From:"]
          , onRecord #transferFromAccount (accountPicker from)
          ]
        , div [className "col"] . (: []) $ div [className "form-group"]
          [ label_ ["To:"]
          , onRecord #transferToAccount (accountPicker to)
          ]
        ]
      , div [className "row"] $
        map (onRecord #transferSchedule) (scheduledTransferEdit s)
      , div [className "row"]
        [ div [className "col"] . (: []) $
          onRecord #transferValue (dollarEdit True v)
        , div [className "col"] . (: []) $
          onRecord #transferNote $
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
      )
    incomeEdit :: Income -> ([Html m Income], Html m Income, Html m Income)
    incomeEdit (Income a s v note) =
      ( [ div [className "col"] . (: []) $ div [className "form-group"]
          [ label_ ["Account:"]
          , onRecord #incomeAccount (accountPicker a)
          ]
        ]
      , div [className "row"] $
        map (onRecord #incomeSchedule) (scheduledTransferEdit s)
      , div [className "row"]
          [ div [className "col"] . (: []) $
            onRecord #incomeValue (dollarEdit True v)
          , div [className "col"] . (: []) $
            onRecord #incomeNote $
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
      )
    costEdit :: Cost -> ([Html m Cost], Html m Cost, Html m Cost)
    costEdit (Cost a s v note) =
      ( [ div [className "col"] . (: []) $ div [className "form-group"]
          [ label_ ["Account:"]
          , onRecord #costAccount (accountPicker a)
          ]
        ]
      , div [className "row"] $
        map (onRecord #costSchedule) (scheduledTransferEdit s)
      , div [className "row"]
          [ div [className "col"] . (: []) $
            onRecord #costValue (dollarEdit True v)
          , div [className "col"] . (: []) $
            onRecord #costNote $
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
      )
    accountPicker :: Account -> Html m Account
    accountPicker a =
      select
        [ value . T.pack $ show a
        , onOption (const . read . T.unpack)
        , className "form-select"
        ] $
        option
          [ value . T.pack $ show blankAccount
          , hidden True
          , disabled True
          , selected (a == blankAccount)
          ]
          ["-- Select a saved Account --"]
            : (mkAccount <$> Set.toList accounts)
      where
        mkAccount a'@(Account name _ color) =
          option [value . T.pack $ show a', selected (a' == a), styleProp [("background",color)]] [text name]


financePlanView :: FinancePlan -> Html m a
financePlanView p = div [className "row"] $ case p of
  FinancePlanTransfer (Transfer from to s v note) ->
    [ div [className "col-sm-2"] [text note]
    , div [className "col-sm-6"] . (: []) $ div [className "row"]
      [ centered $ accountView from
      , arrow
      , centered $ dollarView v
      , arrow
      , centered $ accountView to
      ]
    , div [className "col-sm-4"] [scheduledTransferView s]
    ]
  FinancePlanIncome (Income a s v note) ->
    [ div [className "col-sm-2"] [text note]
    , div [className "col-sm-6"] . (: []) $ div [className "row"]
      [ div [className "col"] []
      , div [className "col-sm-1"] []
      , centered $ dollarView v
      , arrow
      , centered $ accountView a
      ]
    , div [className "col-sm-4"] [scheduledTransferView s]
    ]
  FinancePlanCost (Cost a s v note) ->
    [ div [className "col-sm-2"] [text note]
    , div [className "col-sm-6"] . (: []) $ div [className "row"]
      [ centered $ accountView a
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
