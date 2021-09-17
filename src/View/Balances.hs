{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module View.Balances where

import           Bootstrap.Modal          (modal)
import           Debouncer                (Debouncer)
import           Finance.Account          (AccountAux (..), AccountId (..),
                                           Accounts, blankAccount)
import           Utils.List               (dropIndex, moveDown, moveUp)
import           View.Account             (accountEdit, accountViewFull)

import           Prelude                  hiding (div)
import           Shpadoinkle              (Html, MonadJSM, text)
import           Shpadoinkle.Html         (button, checked, className, div, id',
                                           input', label, onCheck, onClick, p_,
                                           styleProp, textProperty, type')
import           Shpadoinkle.Lens         (onSum)

import           Control.Lens.At          (ix)
import           Control.Lens.Combinators (imap)
import           Control.Lens.Tuple       (_2)
import           Data.Generics.Labels     ()
import qualified Data.Map                 as Map
import           Data.Text                (Text)
import qualified Data.Text                as T


balancesEdit :: forall m
              . MonadJSM m
             => Debouncer m Text
             -> [(AccountId, AccountAux)]
             -> Html m [(AccountId, AccountAux)]
balancesEdit debouncer xs = div [id' "balances-edit"] $ imap balanceEdit xs <>
  [ div [className "row d-grid", styleProp [("padding","0.5rem 0")]] [newButton]
  ]
  where
    balanceEdit :: Int -> (AccountId, AccountAux) -> Html m [(AccountId, AccountAux)]
    balanceEdit idx (a@(AccountId aId),aux@AccountAux{accountAuxEditable}) = div [className "row account"]
      [ div [className "col-12 col-lg-11"] . (: []) . div [className "row"] $
        if accountAuxEditable
        then onSum (ix idx)
               <$> accountEdit (all (\(a',_) -> a' /= a) (take idx xs <> drop (idx + 1) xs)) debouncer a aux
        else [div [className "col"] $ accountViewFull a aux]
      , div [className "col-12 col-lg-1"] $
        [ div [className "row"] . (: []) . div [className "col"] . (: []) $
            div [className "form-check form-switch"]
              [ onSum (ix idx . _2 . #accountAuxEditable) $
                  input'
                  [ type' "checkbox"
                  , checked accountAuxEditable
                  , onCheck const
                  , className "form-check-input"
                  ]
              , label [className "form-check-label"] ["Edit"]
              ]
        ]
        <>  if not accountAuxEditable
            then  []
            else  [ div [className "row d-grid"] . (: []) $
                    button
                    [ className "btn btn-outline-danger"
                    , textProperty "data-bs-toggle" ("modal" :: Text)
                    , textProperty "data-bs-target" ("#dialog-account-delete-" <> T.pack (show idx))
                    ]
                    ["Delete"]
                  , div [className "row d-grid"] . (: []) $
                    button
                      [ className "btn btn-outline-secondary"
                      , styleProp [("margin-top","0.5em")]
                      , onClick $ moveUp idx
                      ] ["&#8593;"]
                  , div [className "row d-grid"] . (: []) $
                    button
                      [ className "btn btn-outline-secondary"
                      , styleProp [("margin-top","0.5em")]
                      , onClick $ moveDown idx
                      ] ["&#8595;"]
                  , modal
                    []
                    ("dialog-account-delete-" <> T.pack (show idx))
                    "Are you sure?"
                    [ p_ . (: []) . text $
                        let accName
                              | aId == "" = "this account with no name"
                              | otherwise = T.pack (show aId)
                        in  "Are you sure you want to delete " <> accName <> "?"
                    ]
                    (\dismiss ->
                      [ button
                        [ className "btn btn-danger"
                        , dismiss
                        , onClick $ dropIndex idx
                        ]
                        ["Yes, delete this account"]
                      ])
                  ]
                ]
    newButton :: Html m [(AccountId, AccountAux)]
    newButton =
      button
        [ className "btn btn-secondary"
        , onClick (<> [blankAccount])
        ]
        ["Add New Account"]

balancesView :: Accounts -> Html m a -- FIXME make term name reference Accounts? Not balances?
balancesView bs = div [className "row"] $ balanceView <$> Map.toList bs
  where
    balanceView :: (AccountId, AccountAux) -> Html m a
    balanceView (a,v) = div [className "col-12"] $ accountViewFull a v
