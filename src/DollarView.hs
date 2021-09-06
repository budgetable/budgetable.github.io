{-# LANGUAGE
    OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  , TypeApplications
  #-}

module DollarView where

import Finance (Dollar (..), Cent (..), dollarParser, dollarPrinter)

import Shpadoinkle (Html, text, MonadJSM, liftJSM, listenRaw, RawNode (..), RawEvent, JSM)
import Shpadoinkle.Continuation (Continuation, done, pur)
import Shpadoinkle.Html (input', type', step, value, onInput, span_, onBlurC)
import Shpadoinkle.Console (warn)

import Control.Arrow (second)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text (parseOnly)
import Language.Javascript.JSaddle (ToJSVal, makeObject, unsafeGetProp, fromJSValUnchecked)



dollarEdit :: forall m. MonadJSM m => Dollar -> [Html m Dollar]
dollarEdit d =
  [ span_ ["$"]
  , input'
    [ type' "number"
    , step "0.01"
    , value (dollarPrinter d)
    , listenRaw "blur" parse
    , onInput $ \t old -> case parseOnly dollarParser t of
        Left _ -> old
        Right new -> new
    ]
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
