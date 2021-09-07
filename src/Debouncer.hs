{-# LANGUAGE RankNTypes #-}

module Debouncer where

import           Shpadoinkle (Continuation, JSM, MonadJSM, RawEvent, RawNode)


type Debouncer m a = (RawNode -> RawEvent -> JSM (Continuation m a))
                  -> RawNode -> RawEvent -> JSM (Continuation m a)
