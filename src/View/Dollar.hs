{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module View.Dollar where

import           Finance.Dollar              (Dollar (..), dollarParser,
                                              dollarPrinter)

import           Prelude                     hiding (div, min, span)
import           Shpadoinkle                 (Html, JSM, RawEvent, RawNode (..),
                                              listenRaw, text)
import           Shpadoinkle.Console         (warn)
import           Shpadoinkle.Continuation    (Continuation, done, pur)
import           Shpadoinkle.Html            (className, div, input', min, span,
                                              step, type', value)

import           Data.Attoparsec.Text        (parseOnly)
import           Language.Javascript.JSaddle (ToJSVal, fromJSValUnchecked,
                                              makeObject, unsafeGetProp)


data DollarEdit m = DollarEdit
  { dollarEditIsPositive      :: Bool
  , dollarEditIsValid         :: Bool
  , dollarEditInvalidFeedback :: Html m Dollar
  }


dollarEdit :: forall m. DollarEdit m -> Dollar -> Html m Dollar
dollarEdit DollarEdit{..} d = div [className "input-group mb-3"]
  [ div [className "input-group-prepend"] [span [className "input-group-text"] ["$"]]
  , input' $
    [ type' "number"
    , step "0.01"
    , value (dollarPrinter d)
    , className $ "form-control" <> if dollarEditIsValid then "" else " is-invalid"
    , listenRaw "blur" parse
    , listenRaw "change" parse
    ] <> [min "0" | dollarEditIsPositive]
  , dollarEditInvalidFeedback
  ]
  where
    parse :: RawNode -> RawEvent -> JSM (Continuation m Dollar)
    parse (RawNode n) _ = do
      o <- makeObject n
      v <- unsafeGetProp "value" o
      t <- fromJSValUnchecked v
      case parseOnly dollarParser t of
        Left e -> do
          warn @ToJSVal e
          pure done
        Right new -> pure . pur $ const new


dollarView :: Dollar -> Html m a
dollarView x = text $ "$" <> dollarPrinter x
