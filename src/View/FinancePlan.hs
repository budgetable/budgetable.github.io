{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module View.FinancePlan where

import           Debouncer                   (Debouncer)
import           Finance.Account             (AccountAux (..), AccountId (..),
                                              AccountLimit (..), Accounts,
                                              blankAccount)
import           Finance.Plan                (Cost (..), FinancePlan (..),
                                              FinancePlanType (..), Income (..),
                                              Transfer (..))
import           View.Account                (accountView)
import           View.Dollar                 (DollarEdit (..), dollarEdit,
                                              dollarView)
import           View.Schedule               (scheduleEdit, scheduleView)

import           Prelude                     hiding (div)
import           Shpadoinkle                 (Html, MonadJSM, RawNode (..),
                                              listenRaw, text)
import           Shpadoinkle.Continuation    (pur)
import           Shpadoinkle.Html            (className, disabled, div, hidden,
                                              input', label_, onOption, option,
                                              placeholder, select, selected,
                                              styleProp, value)
import           Shpadoinkle.Lens            (onRecord, onSum)

import           Control.Lens                (Lens', lens, (.~))
import           Data.Generics.Labels        ()
import qualified Data.Map                    as Map
import qualified Data.Text                   as T
import           Language.Javascript.JSaddle (fromJSValUnchecked, makeObject,
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
                => Accounts
                -> Debouncer m T.Text
                -> FinancePlan
                -> Html m FinancePlan
financePlanEdit accounts debouncer FinancePlan{..} = div [className "row"]
  [ div [className "row"] $
    let viewFinancePlan = map (onRecord #financePlanType) $ case financePlanType of
          FinancePlanTypeTransfer t ->
            map (onSum #_FinancePlanTypeTransfer) (transferEdit t)
          FinancePlanTypeIncome t ->
            map (onSum #_FinancePlanTypeIncome) (incomeEdit t)
          FinancePlanTypeCost t ->
            map (onSum #_FinancePlanTypeCost) (costEdit t)
    in  financePlanPicker : viewFinancePlan
  , div [className "row"] $
    map (onRecord #financePlanSchedule) (scheduleEdit financePlanSchedule)
  , div [className "row"]
      [ div [className "col"] . (: []) $
        let params = DollarEdit
              { dollarEditIsPositive = True
              , dollarEditIsValid = True
              , dollarEditInvalidFeedback = ""
              }
        in  onRecord #financePlanValue (dollarEdit params financePlanValue)
      , div [className "col"] . (: []) $
        onRecord #financePlanNote $
        input'
          [ value financePlanNote
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
          [ value . T.pack . show $ financePlanToPicker financePlanType
          , onOption (pickedFinancePlan . read . T.unpack)
          , className "form-select"
          ] (mkFinancePicker <$> [minBound .. maxBound])
        ]
      where
        mkFinancePicker :: forall a. FinancePlanPicker -> Html m a
        mkFinancePicker p =
          option [value . T.pack $ show p, selected (p == financePlanToPicker financePlanType)]
            [ case p of
                PickerFinancePlanTransfer -> "Transfer"
                PickerFinancePlanIncome   -> "Income"
                PickerFinancePlanCost     -> "Cost"
            ]
        pickedFinancePlan :: FinancePlanPicker -> FinancePlan -> FinancePlan
        pickedFinancePlan p fOld@FinancePlan{financePlanType = fpType}
          | isFinancePlanPickedDifferent fpType p = case p of
              -- sets to a new finance plan without any information
              PickerFinancePlanTransfer ->
                fOld { financePlanType = FinancePlanTypeTransfer
                        (uncurry (uncurry Transfer blankAccount) blankAccount) } -- TODO more elegance
              PickerFinancePlanIncome ->
                fOld { financePlanType = FinancePlanTypeIncome (uncurry Income blankAccount) }
              PickerFinancePlanCost ->
                fOld { financePlanType = FinancePlanTypeCost (uncurry Cost blankAccount) }
          | otherwise = fOld
    getAccount aId = case Map.lookup aId accounts of
      Nothing -> error $ "Couldn't find " <> show aId <> " in " <> show accounts
      Just x  -> x
    transferEdit :: Transfer -> [Html m Transfer]
    transferEdit Transfer{transferFromAccount = from, transferToAccount = to} =
      [ div [className "col"] . (: []) $ div [className "form-group"]
        [ label_ ["From:"]
        , onRecord transferFromLens (accountPicker (const True) from)
        ]
      , div [className "col"] . (: []) $ div [className "form-group"]
        [ label_ ["To:"]
        , onRecord transferToLens (accountPicker (const True) to)
        ]
      ]
      where
        transferFromLens :: Lens' Transfer AccountId
        transferFromLens = lens transferFromAccount $ \t aId ->
          (#transferFromAccount .~ aId) . (#transferFromAccountAux .~ getAccount aId) $ t
        transferToLens :: Lens' Transfer AccountId
        transferToLens = lens transferToAccount $ \t aId ->
          (#transferToAccount .~ aId) . (#transferToAccountAux .~ getAccount aId) $ t

    incomeEdit :: Income -> [Html m Income]
    incomeEdit Income{incomeAccount = a} =
      [ div [className "col"] . (: []) $ div [className "form-group"]
        [ label_ ["Account:"]
        , onRecord incomeLens $
          accountPicker (\AccountAux{accountAuxLimit} -> accountAuxLimit /= OnlyNegative) a
        ]
      ]
      where
        incomeLens :: Lens' Income AccountId
        incomeLens = lens incomeAccount $ \i aId ->
          (#incomeAccount .~ aId) . (#incomeAccountAux .~ getAccount aId) $ i

    costEdit :: Cost -> [Html m Cost]
    costEdit Cost{costAccount = a} =
      [ div [className "col"] . (: []) $ div [className "form-group"]
        [ label_ ["Account:"]
        , onRecord costLens $
          accountPicker (\AccountAux{accountAuxLimit} -> accountAuxLimit /= OnlyPositive) a
        ]
      ]
      where
        costLens :: Lens' Cost AccountId
        costLens = lens costAccount $ \c aId ->
          (#costAccount .~ aId) . (#costAccountAux .~ getAccount aId) $ c

    accountPicker :: (AccountAux -> Bool) -> AccountId -> Html m AccountId
    accountPicker withQualifier a =
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
            : (mkAccount <$> Map.keys (Map.filter withQualifier accounts))
      where
        mkAccount a'@(AccountId name) =
          option [value . T.pack $ show a', selected (a' == a)] [text name]


financePlanView :: FinancePlan -> Html m a
financePlanView (FinancePlan t s v note) = div [className "row"]
  [ div [className "col-sm-2"] [text note]
  , case t of
      FinancePlanTypeTransfer
        ( Transfer
          from
          AccountAux{accountAuxLimit = fromLimit}
          to
          AccountAux{accountAuxLimit = toLimit}
        ) ->
        div [className "col-sm-6"] . (: []) $ div [className "row"]
          [ centered $ accountView from fromLimit
          , arrow
          , centered $ dollarView v
          , arrow
          , centered $ accountView to toLimit
          ]
      FinancePlanTypeIncome (Income a AccountAux{accountAuxLimit}) ->
        div [className "col-sm-6"] . (: []) $ div [className "row"]
          [ div [className "col"] []
          , div [className "col-sm-1"] []
          , centered $ dollarView v
          , arrow
          , centered $ accountView a accountAuxLimit
          ]
      FinancePlanTypeCost (Cost a AccountAux{accountAuxLimit}) ->
        div [className "col-sm-6"] . (: []) $ div [className "row"]
          [ centered $ accountView a accountAuxLimit
          , arrow
          , centered $ dollarView v
          , div [className "col-sm-1"] []
          , div [className "col"] []
          ]
  , div [className "col-sm-4"] [scheduleView s]
  ]
  where
    arrow = div [className "col-sm-1"] . (: []) $ "&#8594;"
    centered = div [className "col", styleProp [("text-align", "center")]] . (: [])
