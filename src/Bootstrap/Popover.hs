{-# LANGUAGE OverloadedStrings #-}

module Bootstrap.Popover where

import           Shpadoinkle.Core (Html, Prop)
import           Shpadoinkle.Html (a, tabIndex, textProperty)

import           Data.Text        (Text)


popoverDismissable :: Text -- ^ Title
                   -> Text -- ^ Content
                   -> [(Text, Prop m a)]
                   -> [Html m a]
                   -> Html m a
popoverDismissable title content ps =
  a $
    [ tabIndex 0
    , textProperty "role" ("button" :: Text)
    , textProperty "data-bs-toggle" ("popover" :: Text)
    , textProperty "data-bs-trigger" ("focus" :: Text)
    , textProperty "title" title
    , textProperty "data-bs-content" content
    ]
    <> ps
