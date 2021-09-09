{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module View.Account where

import           Debouncer                   (Debouncer)
import           Finance.Account             (Account (..), AccountLimit (..), outOfLimitError)
import Finance.Dollar (Dollar)
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

import Control.Lens.Tuple (_1, _2)
import Control.Monad.IO.Class (MonadIO)
import           Data.Generics.Labels        ()
import qualified Data.Text                   as T
import Data.Maybe (isNothing)
import           Language.Javascript.JSaddle (JSVal, fromJSValUnchecked,
                                              makeObject, toJSVal,
                                              unsafeGetProp)


accountEdit :: forall m
             . MonadIO m
            => Debouncer m T.Text
            -> (Account, Dollar)
            -> [Html m (Account, Dollar)]
accountEdit debouncer (a@(Account name limit color), v) =
  [ div [className "col-xs-12 col-sm-6 col-lg-3"]
    [ onRecord (_1 . #accountName) $ input'
        [ value name
        , listenRaw "input" . debouncer $ \(RawNode n) _ -> do
            o <- makeObject n
            v <- unsafeGetProp "value" o
            t <- fromJSValUnchecked v
            pure . pur $ const t
        , placeholder "Account Name"
        , className $ "form-control" <> if name == "" then " is-invalid" else ""
        ]
    , div [className "invalid-feedback"] ["Account name can't be left blank"] -- FIXME and should be unique
    ]
  , div [className "col-xs-12 col-sm-6 col-lg-3"] . (: []) $
    onRecord (_1 . #accountLimit) $ accountLimitEdit limit
  , div [className "col-xs-12 col-sm-4 col-lg-2"] . (: []) $
    onRecord (_1 . #accountColor) $ input'
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
    onRecord _2 $
      let mOutOfLimit = outOfLimitError a v
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


accountView :: Account -> Html m a
accountView (Account name limit _) = text $ name <> " (" <> l <> ")"
  where
    l = case limit of
      NoRestriction -> "&plusmn;"
      OnlyPositive  -> "&plus;"
      OnlyNegative  -> "&minus;"
