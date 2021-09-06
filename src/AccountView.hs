{-# LANGUAGE
    OverloadedLabels
  , OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  #-}

module AccountView where

import Finance (Account (..), AccountLimit (..))

import Prelude hiding (span)
import Shpadoinkle (Html, text)
import Shpadoinkle.Html (input', select, onInput, option, value, onOption, span, styleProp, placeholder)
import Shpadoinkle.Lens (onRecord)

import Data.Generics.Labels ()
import qualified Data.Text as T


accountEdit :: forall m. Functor m => Account -> [Html m Account]
accountEdit (Account name limit color) =
  [ onRecord #accountName $ input'
    [ value name
    , onInput $ const . id
    , placeholder "Account Name"
    ]
  , onRecord #accountLimit $ accountLimitEdit limit
  , onRecord #accountColor $ input'
    [ value color
    , onInput $ const . id
    , placeholder "Color"
    ]
  ]
  where
    accountLimitEdit :: AccountLimit -> Html m AccountLimit
    accountLimitEdit l =
      select
        [ value . T.pack $ show l
        , onOption $ const . read . T.unpack
        ]
        (accountLimitOption <$> [minBound .. maxBound])
      where
        accountLimitOption o = option [value . T.pack $ show o]
          [ case o of
              NoRestriction -> "No Restriction"
              OnlyPositive -> "Only Positive"
              OnlyNegative -> "Only Negative"
          ]


accountView :: Account -> Html m a
accountView (Account name limit color) = span [styleProp [("background",color)]] [text $ name <> " (" <> l <> ")"]
  where
    l = case limit of
      NoRestriction -> "&plusmn;"
      OnlyPositive -> "&plus;"
      OnlyNegative -> "&minus;"
