{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module View.Balances where

import           Debouncer                (Debouncer)
import           Finance.Account          (AccountAux (..), AccountId (..),
                                           Accounts, blankAccount)
import           View.Account             (accountEdit, accountView)
import           View.Dollar              (dollarView)

import           Prelude                  hiding (div)
import           Shpadoinkle              (Html, MonadJSM, text)
import           Shpadoinkle.Html         (button, button', className, div, h5,
                                           id', onClick, p_, styleProp,
                                           tabIndex, table_, td_, textProperty,
                                           tr_)
import           Shpadoinkle.Lens         (onSum)

import           Control.Lens.At          (ix)
import           Control.Lens.Combinators (imap)
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
    balanceEdit idx x@(a@(AccountId aId),_) = div [className "row account"]
      [ div [className "col-12 col-lg-11"] . (: []) . div [className "row"] $
        onSum (ix idx)
        <$> accountEdit (all (\(a',_) -> a' /= a) (take idx xs <> drop (idx + 1) xs)) debouncer x
      , div [className "col-12 col-lg-1"] . (: []) $ div [className "row d-grid"]
        [ button
          [ className "btn btn-outline-danger"
          , textProperty "data-bs-toggle" ("modal" :: Text)
          , textProperty "data-bs-target" ("#dialog-account-delete-" <> T.pack (show idx))
          ]
          ["Delete"]
        , button
            [ className "btn btn-outline-secondary"
            , styleProp [("margin-top","0.5em")]
            , onClick $ \xs' ->
                if idx == 0 then xs'
                else take (idx - 1) xs' -- everything before the next one up
                  <> [xs' !! idx] -- me
                  <> take 1 (drop (idx - 1) xs') -- the one that was the next one up
                  <> drop (idx + 1) xs' -- everything after me
            ] ["&#8593;"]
        , button
            [ className "btn btn-outline-secondary"
            , styleProp [("margin-top","0.5em")]
            , onClick $ \xs' ->
                if idx == length xs' - 1 then xs'
                else take idx xs' -- everything before me
                  <> take 1 (drop (idx + 1) xs') -- the next one after me
                  <> [xs' !! idx] -- me
                  <> drop (idx + 2) xs' -- everything after the next one down
            ] ["&#8595;"]
        , div
          [ className "modal fade"
          , tabIndex (-1)
          , id' $ "dialog-account-delete-" <> T.pack (show idx)
          ]
          [ div [className "modal-dialog"]
            [ div [className "modal-content"] $
              let dismiss = textProperty "data-bs-dismiss" ("modal" :: Text)
              in  [ div [className "modal-header"]
                    [ h5 [className "modal-title"] ["Are you sure?"]
                    , button' [className "btn-close", dismiss]
                    ]
                  , div [className "modal-body"] . (: []) . p_ . (: []) . text $
                    let accName
                          | aId == "" = "this account with no name"
                          | otherwise = T.pack (show aId)
                    in  "Are you sure you want to delete " <> accName <> "?"
                  , div [className "modal-footer"]
                    [ button [className "btn btn-secondary", dismiss]
                      ["Cancel"]
                    , button
                      [ className "btn btn-danger"
                      , dismiss
                      , onClick $ \xsOld -> take idx xsOld <> drop (idx + 1) xsOld
                      ]
                      ["Yes, delete this account"]
                    ]
                  ]
            ]
          ]
        ]
      ]
    newButton :: Html m [(AccountId, AccountAux)]
    newButton =
      button
        [ className "btn btn-secondary"
        , onClick (<> [blankAccount])
        ]
        [text $ "Add New Account" <> if null xs then " (click me)" else ""]

balancesView :: Accounts -> Html m a -- FIXME make term name reference Accounts? Not balances?
balancesView bs = table_ $ balanceView <$> Map.toList bs
  where
    balanceView :: (AccountId, AccountAux) -> Html m a
    balanceView (a,v) = tr_
      [ td_ $
        accountView a v <>
        [": "]
      , td_ [dollarView (accountAuxBalance v)]
      ]
