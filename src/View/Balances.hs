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
import           Shpadoinkle.Html         (button, className, div, id', onClick,
                                           styleProp, table_, td_, tr_)
import           Shpadoinkle.Lens         (onSum)

import           Control.Lens.At          (ix)
import           Control.Lens.Combinators (imap)
import qualified Data.Map                 as Map
import           Data.Text                (Text)


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
    balanceEdit idx x@(a,_) = div [className "row account"] $
      (onSum (ix idx) <$> accountEdit (all (\(a',_) -> a' /= a) (take idx xs <> drop (idx + 1) xs)) debouncer x)
      <>
      [ div
        [ className "col-xs-12 d-grid"
        , styleProp [("margin-top", "0"),("padding-bottom", "0.5rem")]
        ]
        [ button
          [ className "btn btn-secondary"
          , onClick $ \xsOld -> take idx xsOld <> drop (idx + 1) xsOld
          ]
          ["Delete"]
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
      [ td_
        [ accountView a (accountAuxLimit v)
        , ": "
        ]
      , td_ [dollarView (accountAuxBalance v)]
      ]
