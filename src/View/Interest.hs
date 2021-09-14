{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module View.Interest where

import           Debouncer                   (Debouncer)
import           Finance.Interest            (CompoundingInterest (..),
                                              Interest, getInterest)
import           View.Schedule               (repeatingIntervalEdit,
                                              repeatingIntervalView)

import           Prelude                     hiding (div, span)
import           Shpadoinkle                 (Continuation, Html, JSM, MonadJSM,
                                              RawEvent, RawNode (..), text)
import           Shpadoinkle.Console         (ToJSVal, warn)
import           Shpadoinkle.Continuation    (contIso, done, pur)
import           Shpadoinkle.Html            (className, div, input', label_,
                                              listenRaw, span, step, type',
                                              value)
import           Shpadoinkle.Lens            (onRecord)

import           Data.Attoparsec.Text        (parseOnly, scientific)
import           Data.Generics.Labels        ()
import           Data.Scientific             (toRealFloat)
import qualified Data.Text                   as T
import           Language.Javascript.JSaddle (fromJSValUnchecked, makeObject,
                                              unsafeGetProp)
import           Text.Printf                 (printf)


interestEdit :: forall m. MonadJSM m => Debouncer m T.Text -> Interest -> Html m Interest
interestEdit debouncer i = div [className "form-group"]
  [ label_ ["APR:"]
  , div [className "input-group mb-3"]
    [ input'
      [ type' "number"
      , step "0.001"
      , value . T.pack . show $ getInterest i
      , className "form-control"
      , listenRaw "blur" (debouncer' parse)
      , listenRaw "change" (debouncer' parse)
      ]
    , div [className "input-group-append"] [span [className "input-group-text"] ["%"]]
    ]
  ]
  where
    debouncer' :: Debouncer m Interest
    debouncer' f n e = toI <$> debouncer (\n' e' ->  fromI <$> f n' e') n e
      where
        toI :: Continuation m T.Text -> Continuation m Interest
        toI = contIso (read . T.unpack) (T.pack . show)
        fromI :: Continuation m Interest -> Continuation m T.Text
        fromI = contIso (T.pack . show) (read . T.unpack)
    parse :: RawNode -> RawEvent -> JSM (Continuation m Interest)
    parse (RawNode n) _ = do
      o <- makeObject n
      v <- unsafeGetProp "value" o
      t <- fromJSValUnchecked v
      case parseOnly (toRealFloat <$> scientific) t of
        Left e -> do
          warn @ToJSVal e
          pure done
        Right new -> pure . pur $ const new

interestView :: Interest -> Html m a
interestView i = text $ T.pack (printf "0.3f" i) <> "%"

compoundingInterestEdit :: MonadJSM m => Debouncer m T.Text -> CompoundingInterest -> [Html m CompoundingInterest]
compoundingInterestEdit debouncer (CompoundingInterest interest r) =
  [ div [className "col"] . (: []) $
    onRecord #compoundingInterestAnnualRate (interestEdit debouncer interest)
  , div [className "col"] $
    map (onRecord #compoundingInterestInterval) (repeatingIntervalEdit r)
  ]

compoundingInterestView :: CompoundingInterest -> Html m a
compoundingInterestView (CompoundingInterest interest r) = div [className "row"]
  [ interestView interest
  , repeatingIntervalView r
  ]
