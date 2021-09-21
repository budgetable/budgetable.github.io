{-# LANGUAGE OverloadedStrings #-}

module View.Progress where

import           Shpadoinkle                  (Continuation, Html, RawNode (..),
                                               baked, shpadoinkle)
import           Shpadoinkle.Backend.Snabbdom (runSnabbdom)
import           Shpadoinkle.Html             (className, div, div', span'_,
                                               styleProp, textProperty)

import           Control.Concurrent           (forkIO, threadDelay)
import           Control.Concurrent.STM       (STM, TVar, retry)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           GHCJS.DOM                    (currentDocumentUnchecked)
import           GHCJS.DOM.Document           (createElement)
import           GHCJS.DOM.Element            (setId)
import           Language.Javascript.JSaddle  (JSM, toJSVal)
import           Prelude                      hiding (div)


progress :: TVar (Int, Int) -> Html m a
progress model = baked mkProgress
  where
    mkProgress :: JSM (RawNode, STM (Continuation m a))
    mkProgress = do
      doc <- currentDocumentUnchecked
      elm <- createElement doc ("div" :: Text)
      setId elm ("progress" :: Text)
      raw <- RawNode <$> toJSVal elm
      _ <- forkIO $ do
        threadDelay 1
        shpadoinkle id runSnabbdom model view (pure raw)
      pure (raw, retry)

    view :: (Int, Int) -> Html n (Int, Int)
    view (soFar, total)
      | soFar == total = span'_
      | otherwise =
        div [className "progress"]
          [ div'
            [ className "progress-bar progress-bar-striped progress-bar-animated"
            , textProperty "role" ("progressbar" :: Text)
            , textProperty "aria-valuenow" . T.pack $ show valuePercent
            , textProperty "aria-valuemin" ("0" :: Text)
            , textProperty "aria-valuemax" ("100" :: Text)
            , styleProp [("width", T.pack (show valuePercent) <> "%")]
            ]
          ]
      where
        valuePercent :: Double
        valuePercent = 100 * (fromIntegral soFar / fromIntegral total)
