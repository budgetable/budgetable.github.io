{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module View.Account where

import           Debouncer                   (Debouncer)
import           Finance.Account             (AccountAux (..), AccountId (..),
                                              AccountLimit (..),
                                              outOfLimitError)
import           Finance.Dollar              (Dollar)
import           View.Dollar                 (DollarEdit (..), dollarEdit)

import           Prelude                     hiding (div, span)
import           Shpadoinkle                 (Html, RawNode (..), listenRaw,
                                              text)
import           Shpadoinkle.Continuation    (pur)
import           Shpadoinkle.Html            (className, div, input', onInput,
                                              onOption, option, placeholder,
                                              select, selected, span, styleProp,
                                              value)
import           Shpadoinkle.Lens            (onRecord)

import           Control.Lens.Tuple          (_1, _2)
import           Control.Monad.IO.Class      (MonadIO)
import           Data.Generics.Labels        ()
import           Data.Maybe                  (isNothing)
import qualified Data.Text                   as T
import           Language.Javascript.JSaddle (JSVal, fromJSValUnchecked,
                                              makeObject, toJSVal,
                                              unsafeGetProp)


accountEdit :: forall m
             . MonadIO m
            => Bool
            -> Debouncer m T.Text
            -> (AccountId, AccountAux)
            -> [Html m (AccountId, AccountAux)]
accountEdit isUnique debouncer (name@(AccountId nameRaw), AccountAux limit color v) =
  [ div [className "col-xs-12 col-sm-6 col-lg-3"]
    [ onRecord (_1 . #getAccountId) $ input'
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
      [ if | name == ""   -> "Account name can't be left blank"
           | not isUnique -> "Account name should be unique"
           | otherwise    -> ""
      ]
    ]
  , div [className "col-xs-12 col-sm-6 col-lg-3"] . (: []) $
    onRecord (_2 . #accountAuxLimit) $ accountLimitEdit limit
  , div [className "col-xs-12 col-sm-4 col-lg-2"] . (: []) $
    onRecord (_2 . #accountAuxColor) $ input'
      [ value color
      , listenRaw "input" . debouncer $ \(RawNode n) _ -> do
          o <- makeObject n
          v <- unsafeGetProp "value" o
          t <- fromJSValUnchecked v
          pure . pur $ const t
      , placeholder "Color"
      , className "form-control"
      ]
  , div [className "col-xs-12 col-sm-4 col-lg-3"] . (: []) .
    onRecord (_2 . #accountAuxBalance) $
      let mOutOfLimit = outOfLimitError name limit v
          params = DollarEdit
            { dollarEditIsPositive = False
            , dollarEditIsValid = isNothing mOutOfLimit
            , dollarEditInvalidFeedback = case mOutOfLimit of
                Nothing -> ""
                Just outOfLimit -> div [className "invalid-feedback"] [text outOfLimit]
            }
      in  dollarEdit params v
  ]
  where
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


accountView :: AccountId -> AccountLimit -> Html m a
accountView (AccountId name) limit = text $ name <> " (" <> l <> ")"
  where
    l = case limit of
      NoRestriction -> "&plusmn;"
      OnlyPositive  -> "&plus;"
      OnlyNegative  -> "&minus;"
