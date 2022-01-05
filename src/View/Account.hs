{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module View.Account where

import           Bootstrap.Popover           (popoverDismissable)
import           Debouncer                   (Debouncer)
import           Finance.Account             (AccountAux (..), AccountId (..),
                                              AccountLimit (..),
                                              outOfLimitError)
import           Finance.Interest            (CompoundingInterest (..))
import           Finance.Schedule            (RepeatingInterval (RepeatingMonthly))
import           View.Dollar                 (DollarEdit (..), dollarEdit,
                                              dollarView)
import           View.Interest               (compoundingInterestEdit,
                                              compoundingInterestView)

import           Prelude                     hiding (div, span)
import           Shpadoinkle                 (Html, RawNode (..), listenRaw,
                                              text)
import           Shpadoinkle.Continuation    (pur)
import           Shpadoinkle.Html            (checked, className, div, input',
                                              label, label_, onCheck, onOption,
                                              option, placeholder, select,
                                              selected, span, styleProp, type',
                                              value)
import           Shpadoinkle.Lens            (onRecord, onSum)

import           Control.Lens                ((.~))
import           Control.Lens.Tuple          (_1, _2)
import           Control.Monad.IO.Class      (MonadIO)
import           Data.Generics.Labels        ()
import           Data.Maybe                  (isJust, isNothing)
import qualified Data.Text                   as T
import           Language.Javascript.JSaddle (fromJSValUnchecked, makeObject,
                                              unsafeGetProp)


accountEdit :: forall m
             . MonadIO m
            => Bool
            -> Debouncer m T.Text
            -> AccountId
            -> AccountAux
            -> [Html m (AccountId, AccountAux)]
accountEdit isUnique debouncer name@(AccountId nameRaw) AccountAux{..} =
  [ div [className "col-6 col-lg-3"]
    [ onRecord (_1 . #getAccountId) $ div [className "form-group"]
      [ label_ ["Name:"]
      , input'
        [ value nameRaw
        , listenRaw "input" . debouncer $ \(RawNode n) _ -> do
            o <- makeObject n
            v <- unsafeGetProp "value" o
            t <- fromJSValUnchecked v
            pure . pur $ const t
        , placeholder "Account Name"
        , className $
            let validity
                  | name == "" || not isUnique = " is-invalid"
                  | otherwise = ""
            in  "form-control" <> validity
        ]
      , div [className "invalid-feedback"]
        [ if
            | name == ""   -> "Account name can't be left blank"
            | not isUnique -> "Account name should be unique"
            | otherwise    -> ""
        ]
      ]
    ]
  , div [className "col-6 col-lg-3"]
    [ onRecord (_2 . #accountAuxLimit) $ div [className "form-group"]
      [ label_
        [ "Limit: "
        , popoverDismissable
          "Account Limits"
          "Some accounts naturally have limits; credit cards only hold <em>negative</em> balances, while most medium-term savings accounts only hold <em>positive</em> balances. Checking accounts, however, usually don't have a restriction."
          [className "badge rounded-pill bg-light text-dark"]
          ["?"]
        ]
      , accountLimitEdit accountAuxLimit
      ]
    ]
  , div [className "col-6 col-lg-3"]
    [ onRecord (_2 . #accountAuxColor) $ div [className "form-group"]
      [ label_
        [ "Color: "
        , popoverDismissable
          "Graph Colors"
          "You can use any HTML-compatible color you'd like. Any <a href=\"https://htmlcolorcodes.com/color-names/\" target=\"_blank\">HTML color names</a> will work, as well as <a href=\"https://htmlcolorcodes.com\" target=\"_blank\">normal HTML color codes</a>."
          [className "badge rounded-pill bg-light text-dark"]
          ["?"]
        ]
      , input'
        [ value accountAuxColor
        , listenRaw "input" . debouncer $ \(RawNode n) _ -> do
            o <- makeObject n
            v <- unsafeGetProp "value" o
            t <- fromJSValUnchecked v
            pure . pur $ const t
        , placeholder "Color"
        , className "form-control color-input"
        , styleProp [("background-color", accountAuxColor)]
        ]
      ]
    ]
  , div [className "col-6 col-lg-3"]
    [ onRecord (_2 . #accountAuxBalance) $ div [className "form-group"]
      [ label_ ["Value:"]
      , let mOutOfLimit = outOfLimitError name accountAuxLimit accountAuxBalance
            params = DollarEdit
              { dollarEditIsPositive = False
              , dollarEditIsValid = isNothing mOutOfLimit
              , dollarEditInvalidFeedback = case mOutOfLimit of
                  Nothing -> ""
                  Just outOfLimit -> div [className "invalid-feedback"] [text outOfLimit]
              }
        in  dollarEdit params accountAuxBalance
      ]
    ]
  , div [className "col-12"]
    [ div [className "row"] $
      let interestRateEdit = case accountAuxInterest of
            Nothing -> []
            Just i -> map (onSum (_2 . #accountAuxInterest . #_Just)) (compoundingInterestEdit debouncer i)
      in  [ div [className "col-6 col-md-4"]
            [ div [className "row"] . (: []) . div [className "col"] . (: []) $
                div [className "form-check form-switch"]
                  [ input'
                    [ type' "checkbox"
                    , checked (isJust accountAuxInterest)
                    , onCheck $ \c ->
                      let go | c = Just . CompoundingInterest 5 $ RepeatingMonthly 1
                            | otherwise = Nothing
                      in  _2 . #accountAuxInterest .~ go
                    , className "form-check-input"
                    ]
                  , label [className "form-check-label"] ["Has Interest Rate?"]
                  ]
            , div [className "row"] . (: []) . div [className "col"] . (: []) $
                div [className "form-check form-switch"]
                  [ onSum (_2 . #accountAuxDisabled) $
                      input'
                      [ type' "checkbox"
                      , checked accountAuxDisabled
                      , onCheck const
                      , className "form-check-input"
                      ]
                  , label
                    [ className "form-check-label"
                    ]
                    [ "Is Disabled? "
                    , popoverDismissable
                      "Disabling Accounts"
                      "If you would like to compare hypothetical accounts (for instance, comparing prospect auto loans for two different vehicles), disabling it is a good method to remove the account from your budget <em>without deleting it</em>."
                      [className "badge rounded-pill bg-light text-dark"]
                      ["?"]
                    ]
                  ]
            ]
          ] <> interestRateEdit
    ]
  ]

accountLimitEdit :: AccountLimit -> Html m AccountLimit
accountLimitEdit l =
  select
    [ value . T.pack $ show l
    , onOption $ const . read . T.unpack
    , className "form-select"
    ]
    (accountLimitOption <$> [minBound .. maxBound])
  where
    accountLimitOption o = option [value . T.pack $ show o, selected (o == l)]
      [ case o of
          NoRestriction -> "No Restriction"
          OnlyPositive  -> "Only Positive"
          OnlyNegative  -> "Only Negative"
      ]


accountView :: AccountId -> AccountAux -> [Html m a]
accountView (AccountId name) AccountAux{accountAuxLimit, accountAuxColor, accountAuxDisabled} =
  [ span
    [ styleProp $ [("background-color",c)]
      <> [("text-decoration","line-through") | accountAuxDisabled]
    , className "badge account-label"
    ]
    [text $ name <> " (" <> l <> ")"]
  ]
  where
    c  | accountAuxDisabled = "#777"
       | accountAuxColor == "" = "#666666"
       | otherwise = accountAuxColor
    l = case accountAuxLimit of
      NoRestriction -> "&plusmn;"
      OnlyPositive  -> "&plus;"
      OnlyNegative  -> "&minus;"


accountViewFull :: AccountId -> AccountAux -> [Html m a]
accountViewFull (AccountId name) AccountAux{..} =
  [ span
    [ styleProp $ [("background-color",c)]
      <> [("text-decoration","line-through") | accountAuxDisabled]
    , className "badge account-label"
    ]
    [ text $ name <> " (" <> l <> ") "
    , dollarView accountAuxBalance
    ]
  , " "
  ] <> maybe [] compoundingInterestView accountAuxInterest
  where
    c  | accountAuxDisabled = "#777"
       | accountAuxColor == "" = "#666666"
       | otherwise = accountAuxColor
    l = case accountAuxLimit of
      NoRestriction -> "&plusmn;"
      OnlyPositive  -> "&plus;"
      OnlyNegative  -> "&minus;"
