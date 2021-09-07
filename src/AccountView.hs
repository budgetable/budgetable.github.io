{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module AccountView where

import           Finance              (Account (..), AccountLimit (..))

import           Prelude              hiding (div, span)
import           Shpadoinkle          (Html, text)
import           Shpadoinkle.Html     (className, div, input', onInput,
                                       onOption, option, placeholder, select,
                                       selected, span, styleProp, value)
import           Shpadoinkle.Lens     (onRecord)

import           Data.Generics.Labels ()
import qualified Data.Text            as T


accountEdit :: forall m. Functor m => Account -> [Html m Account]
accountEdit (Account name limit color) =
  [ div [className "col"] . (: []) $
    onRecord #accountName $ input'
    [ value name
    , onInput $ const . id
    , placeholder "Account Name"
    , className "form-control"
    ]
  , div [className "col"] . (: []) $
    onRecord #accountLimit $ accountLimitEdit limit
  , div [className "col"] . (: []) $
    onRecord #accountColor $ input'
    [ value color
    , onInput $ const . id
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
