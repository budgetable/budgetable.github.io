{-# LANGUAGE DeriveGeneric, TupleSections, RecordWildCards, OverloadedStrings, ExtendedDefaultRules, OverloadedLabels, RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module View.Day.Modal where

import Bootstrap.Modal (modal)
import Shpadoinkle (Html, baked, JSM, RawNode (..), shpadoinkle, text, liftJSM, MonadJSM)
import Shpadoinkle.Html (button, className, onClickM_, div, styleProp, textProperty, onClick)
import Shpadoinkle.Backend.Snabbdom (runSnabbdom)

import Prelude hiding (div)
import           GHCJS.DOM                               (currentDocumentUnchecked)
import           GHCJS.DOM.Document                      (createElement)
import           GHCJS.DOM.Element                       (setId)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day, toGregorian, fromGregorian)
import Control.DeepSeq (NFData)
import Control.Lens ((%~))
import           Data.Generics.Labels        ()
import GHC.Generics (Generic)
import Control.Concurrent.STM (retry, newTVarIO)
import Control.Concurrent (threadDelay, forkIO)
import Language.Javascript.JSaddle (toJSVal)

default (Text)


prevMonth :: (Integer, Int) -> (Integer, Int)
prevMonth (y,m)
  | m == 1 = (y-1, 12)
  | otherwise = (y, m-1)

nextMonth :: (Integer, Int) -> (Integer, Int)
nextMonth (y,m)
  | m == 12 = (y+1, 1)
  | otherwise = (y, m+1)

data ModalState = ModalState
  { year :: Integer
  , month :: Int
  , day :: Int
  } deriving (Show, Eq, Generic)
instance NFData ModalState

datePicker :: Day
           -> (Day -> JSM ())
           -> Text
           -> Html m a
datePicker today onPickedDate ident =
  baked $ (,retry) <$> mkModal
  where
    mkModal :: JSM RawNode
    mkModal = do
      doc <- currentDocumentUnchecked
      elm <- createElement doc "div"
      setId elm ("container-" <> ident)
      model <- newTVarIO $
        let (y,m,d) = toGregorian today
        in  ModalState y m d
      raw <- RawNode <$> toJSVal elm
      _ <- forkIO $ do
        threadDelay 1
        shpadoinkle id runSnabbdom model view (pure raw)
      pure raw

    view :: forall n. MonadJSM n => ModalState -> Html n ModalState
    view ModalState{..} =
      modal
        []
        ident
        "Pick Date"
        [ div [className "row"]
          [ div [className "col-auto"] . (: []) $
              button
                [ className "btn btn-secondary"
                , onClick $ #year %~ (\y -> y - 1)
                ] ["&#8592;"]
          , div [className "col", styleProp [("text-align","center")]] [text . T.pack $ show year]
          , div [className "col-auto"] . (: []) $
              button
                [ className "btn btn-secondary"
                , onClick $ #year %~ (+ 1)
                ] ["&#8594;"]
          ]
        , button
          [ className "btn btn-outline-primary"
          , dismiss
          , let newDay :: Day
                newDay = fromGregorian year month day
            in  onClickM_ . liftJSM $ onPickedDate newDay -- FIXME make day clicked
          ] [text . T.pack $ show day]
        ]
        (const [])
      where
        dismiss = textProperty "data-bs-dismiss" "modal"
