{-# LANGUAGE RankNTypes #-}

module Debouncer where

import Shpadoinkle (RawNode, RawEvent, Continuation, MonadJSM, JSM)


type Debouncer m a = (RawNode -> RawEvent -> JSM (Continuation m a))
                  -> RawNode -> RawEvent -> JSM (Continuation m a)
