{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module View.Plan where

import           Bootstrap.Modal             (modal)
import           Debouncer                   (Debouncer)
import           Finance.Account             (AccountAux (..), AccountId (..),
                                              AccountLimit (..), Accounts,
                                              blankAccount)
import           Finance.Plan                (Cost (..), FinancePlan (..),
                                              FinancePlanType (..), Income (..),
                                              Transfer (..))
import           Finance.Schedule            (Schedule (DateSchedule))
import           Utils.List                  (dropIndex, moveDown, moveUp)
import           View.Account                (accountView)
import           View.Dollar                 (DollarEdit (..), dollarEdit,
                                              dollarView)
import           View.Schedule               (scheduleEdit, scheduleView)

import           Prelude                     hiding (div)
import           Shpadoinkle                 (Html, MonadJSM, RawNode (..),
                                              listenRaw, text)
import           Shpadoinkle.Continuation    (pur)
import           Shpadoinkle.Html            (button, className, disabled, div,
                                              hidden, input', label_, onClick,
                                              onClickM, onOption, option, p_,
                                              placeholder, select, selected,
                                              styleProp, textProperty, value)
import           Shpadoinkle.Lens            (onRecord, onSum)

import           Control.Lens                (Lens', lens, (%~), (.~))
import           Control.Lens.At             (ix)
import           Control.Lens.Combinators    (imap)
import           Control.Monad.IO.Class      (MonadIO (liftIO))
import           Data.Default                (def)
import           Data.Generics.Labels        ()
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time.Clock             (getCurrentTime, utctDay)
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
                -> Text
                -> FinancePlan
                -> Html m FinancePlan
financePlanEdit accounts debouncer ident FinancePlan{..} = div [className "row finance-plan-details"] $
  [ div [className "row"] $
    let viewFinancePlan = map (onRecord #financePlanType) $ case financePlanType of
          FinancePlanTypeTransfer t ->
            map (onSum #_FinancePlanTypeTransfer) (transferEdit t)
          FinancePlanTypeIncome t ->
            map (onSum #_FinancePlanTypeIncome) (incomeEdit t)
          FinancePlanTypeCost t ->
            map (onSum #_FinancePlanTypeCost) (costEdit t)
    in  financePlanPicker : viewFinancePlan
  ] <> schedules <>
  [ div [className "row"]
    [ let clickedAddSchedule = do
            today <- utctDay <$> liftIO getCurrentTime
            pure $ #financePlanSchedule %~ (<> [DateSchedule today])
      in  button
            [ onClickM clickedAddSchedule
            , className "btn btn-secondary"
            ]
            ["Add Schedule"]
    ]
  , div [className "row"]
      [ div [className "col"] . (: []) $ div [className "form-group"]
        [ label_ ["Value:"]
        , let params = DollarEdit
                { dollarEditIsPositive = True
                , dollarEditIsValid = True
                , dollarEditInvalidFeedback = ""
                }
          in  onRecord #financePlanValue (dollarEdit params financePlanValue)
        ]
      , div [className "col"] . (: []) $ div [className "form-group"]
        [ label_ ["Note:"]
        , onRecord #financePlanNote $
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
  ]
  where
    schedules :: [Html m FinancePlan]
    schedules =
      onRecord #financePlanSchedule <$> imap scheduleEdit' financePlanSchedule
      where
        scheduleEdit' :: Int -> Schedule -> Html m [Schedule]
        scheduleEdit' idx s =
          div
            [ className "row finance-plan-schedule"
            , styleProp $ [("padding","0.5em 0")] <> [("border-top", "solid 5px #fff") | idx > 0]
            ] $
          (onSum (ix idx) <$> scheduleEdit (ident <> T.pack (show idx)) s) <> listButtons
          where
            modalIdent = "dialog-finance-plan-schedule-delete-" <> ident <> T.pack (show idx)
            listButtons
              | length financePlanSchedule < 2 = []
              | otherwise =
                [ div [className "col-12 col-md-4 d-grid"] . (: []) $
                    button
                      [ className "btn btn-outline-danger"
                      , textProperty "data-bs-toggle" ("modal" :: Text)
                      , textProperty "data-bs-target" ("#" <> modalIdent)
                      , styleProp [("margin-top","0.5em")]
                      ] ["Un-Schedule"]
                , div [className "col-12 col-md-4 d-grid"] . (: []) $
                    button
                      [ className "btn btn-outline-secondary"
                      , styleProp [("margin-top","0.5em")]
                      , onClick $ moveUp idx
                      ] ["&#8593;"]
                , div [className "col-12 col-md-4 d-grid"] . (: []) $
                    button
                      [ className "btn btn-outline-secondary"
                      , styleProp [("margin-top","0.5em")]
                      , onClick $ moveDown idx
                      ] ["&#8595;"]
                , modal
                    []
                    modalIdent
                    "Are you sure?"
                    [p_ ["Are you sure you want to delete this schedule?"]]
                    (\dismiss ->
                      [button
                        [ className "btn btn-danger"
                        , onClick $ dropIndex idx
                        , dismiss]
                        ["Yes, delete this schedule"]
                      ])
                ]
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
                fOld { financePlanType = FinancePlanTypeTransfer def }
              PickerFinancePlanIncome ->
                fOld { financePlanType = FinancePlanTypeIncome def }
              PickerFinancePlanCost ->
                fOld { financePlanType = FinancePlanTypeCost def }
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
financePlanView (FinancePlan t s v note) = div [className "row finance-plan"]
  [ div [className "col-12 col-sm-2"] [text note]
  , div [className "col-12 col-sm-6"] . (: []) . div [className "row"] $ case t of
      FinancePlanTypeTransfer (Transfer from fromAux to toAux) ->
        [ centered $ accountView from fromAux
        , arrow
        , centered [dollarView v]
        , arrow
        , centered $ accountView to toAux
        ]
      FinancePlanTypeIncome (Income a aux) ->
        [ div [className "col"] []
        , div [className "col-1"] []
        , centered [dollarView v]
        , arrow
        , centered $ accountView a aux
        ]
      FinancePlanTypeCost (Cost a aux) ->
        [ centered $ accountView a aux
        , arrow
        , centered [dollarView v]
        , div [className "col-1"] []
        , div [className "col"] []
        ]
  , div [className "col-12 col-sm-4"] $
    scheduleView <$> s
  ]
  where
    arrow = div [className "col-1"] . (: []) $ "&#8594;"
    centered = div
      [ className "col"
      , styleProp
        [ ("text-align", "center")
        , ("overflow-x", "auto")
        ]
      ]
