{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AccountView where

import           Debouncer                   (Debouncer)
import           Finance                     (Account (..), AccountLimit (..))

import           Prelude                     hiding (div, span)
import           Shpadoinkle                 (Html, RawNode (..), listenRaw,
                                              text)
import           Shpadoinkle.Continuation    (pur)
import           Shpadoinkle.Html            (className, div, input', onInput,
                                              onOption, option, placeholder,
                                              select, selected, span, styleProp,
                                              value)
import           Shpadoinkle.Lens            (onRecord)

import           Data.Generics.Labels        ()
import qualified Data.Text                   as T
import           Language.Javascript.JSaddle (JSVal, fromJSValUnchecked,
                                              makeObject, toJSVal,
                                              unsafeGetProp)


accountEdit :: forall m
             . Functor m
            => Debouncer m T.Text
            -> Account
            -> [Html m Account]
accountEdit debouncer (Account name limit color) =
  [ div [className "col-xs-12 col-sm-6 col-lg-3"] . (: []) $
    onRecord #accountName $ input'
      [ value name
      , listenRaw "input" . debouncer $ \(RawNode n) _ -> do
          o <- makeObject n
          v <- unsafeGetProp "value" o
          t <- fromJSValUnchecked v
          pure . pur $ const t
      , placeholder "Account Name"
      , className "form-control"
      ]
  , div [className "col-xs-12 col-sm-6 col-lg-3"] . (: []) $
    onRecord #accountLimit $ accountLimitEdit limit
  , div [className "col-xs-12 col-sm-4 col-lg-2"] . (: []) $
    onRecord #accountColor $ input'
      [ value color
      , listenRaw "input" . debouncer $ \(RawNode n) _ -> do
          o <- makeObject n
          v <- unsafeGetProp "value" o
          t <- fromJSValUnchecked v
          pure . pur $ const t
      , placeholder "Color"
      , className "form-control"
      ]
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
accountView (Account name limit color) = span [styleProp [("background",color)]] [text $ name <> " (" <> l <> ")"]
  where
    l = case limit of
      NoRestriction -> "&plusmn;"
      OnlyPositive  -> "&plus;"
      OnlyNegative  -> "&minus;"
