{-# LANGUAGE OverloadedStrings #-}

module Bootstrap.Modal where

import           Shpadoinkle      (Html)
import           Shpadoinkle.Core (Prop)
import           Shpadoinkle.Html (button, button', className, div, h5, id',
                                   tabIndex, textProperty)

import           Data.Text        (Text)
import           Prelude          hiding (div)


modal :: [(Text, Prop m a)] -- ^ extra props
      -> Text -- ^ Identifier of modal
      -> Html m a -- ^ Title
      -> [Html m a] -- ^ Content
      -> ((Text, Prop m a) -> [Html m a]) -- ^ Footer button
      -> Html m a
modal ps ident title content footerButton =
  div
    ([ className "modal fade"
    , tabIndex (-1)
    , id' ident
    ] <> ps)
    [ div [className "modal-dialog"]
      [ div [className "modal-content"] $
        let dismiss = textProperty "data-bs-dismiss" ("modal" :: Text)
        in  [ div [className "modal-header"]
              [ h5 [className "modal-title"] [title]
              , button' [className "btn-close", dismiss]
              ]
            , div [className "modal-body"] content
            , div [className "modal-footer"] $
              [ button [className "btn btn-secondary", dismiss]
                ["Cancel"]
              ] <> footerButton dismiss
            ]
      ]
    ]
