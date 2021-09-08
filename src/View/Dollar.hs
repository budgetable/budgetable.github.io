{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module View.Dollar where

import           Finance                     (Cent (..), Dollar (..),
                                              dollarParser, dollarPrinter)

import           Prelude                     hiding (div, span, min)
import           Shpadoinkle                 (Html, JSM, MonadJSM, RawEvent,
                                              RawNode (..), liftJSM, listenRaw,
                                              text)
import           Shpadoinkle.Console         (warn)
import           Shpadoinkle.Continuation    (Continuation, done, pur)
import           Shpadoinkle.Html            (className, div, i, input',
                                              onBlurC, onChange, span, step,
                                              type', value, min)

import           Control.Arrow               (second)
import           Data.Attoparsec.Text        (parseOnly)
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Language.Javascript.JSaddle (ToJSVal, fromJSValUnchecked,
                                              makeObject, unsafeGetProp)



dollarEdit :: forall m. MonadJSM m => Bool -> Dollar -> Html m Dollar
dollarEdit isPositiveOnly d = div [className "input-group mb-3"]
  [ div [className "input-group-prepend"] [span [className "input-group-text"] ["$"]]
  , input' $
    [ type' "number"
    , step "0.01"
    , value (dollarPrinter d)
    , className "form-control"
    , listenRaw "blur" parse
    , listenRaw "change" parse
    ] <> [min "0" | isPositiveOnly]
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
