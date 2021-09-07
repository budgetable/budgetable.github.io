{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BalancesView where

import           AccountView              (accountEdit, accountView)
import           Debouncer                (Debouncer)
import           DollarView               (dollarEdit, dollarView)
import           Finance                  (Account (..),
                                           AccountLimit (NoRestriction),
                                           Balances, Dollar, blankAccount,
                                           outOfLimitError)

import           Prelude                  hiding (div)
import           Shpadoinkle              (Html, MonadJSM, text)
import           Shpadoinkle.Html         (button, className, div, div_, form_,
                                           i, id', li_, onClick, span_,
                                           styleProp, table_, td_, tr_, ul_)
import           Shpadoinkle.Lens         (onSum)

import           Control.Lens.At          (ix)
import           Control.Lens.Combinators (imap)
import           Control.Lens.Tuple       (_1, _2)
import qualified Data.Map                 as Map
import           Data.Text                (Text)


balancesEdit :: forall m
              . MonadJSM m
             => Debouncer m Text
             -> [(Account, Dollar)]
             -> Html m [(Account, Dollar)]
balancesEdit debouncer xs = div [id' "balances-edit"] $ imap balanceEdit xs <>
  [ div [className "row d-grid", styleProp [("padding","0.5rem 0")]] [newButton]
  ]
  where
    balanceEdit :: Int -> (Account, Dollar) -> Html m [(Account, Dollar)]
    balanceEdit idx (a,v) = div [className "row account"]
      [ div [className "row g-2"] $
        (onSum (ix idx . _1) <$> accountEdit debouncer a)
        <>
        [ div [className "col-xs-12 col-sm-4 col-lg-3"] . (: []) $ onSum (ix idx . _2) (dollarEdit v)
        , div
          [ className "col-xs-12 col-sm-4 col-lg-1 d-grid"
          , styleProp [("margin-top", "0"),("padding-bottom", "0.5rem")]
          ]
          [ button
            [ className "btn btn-secondary"
            , onClick $ \xs -> take idx xs <> drop (idx + 1) xs
            ]
            ["Delete"]
          ]
        ]
      , div [className "row"] $
          case outOfLimitError a v of
            Nothing -> []
            Just e  -> [span_ [text e]]
      ]
    newButton :: Html m [(Account, Dollar)]
    newButton =
      button
        [ className "btn btn-secondary"
        , onClick (<> [(blankAccount, 0)])
        ]
        ["Add New Account"]

balancesView :: Balances -> Html m a
balancesView bs = table_ $ balanceView <$> Map.toList bs
  where
    balanceView :: (Account, Dollar) -> Html m a
    balanceView (a,v) = tr_
      [ td_
        [ accountView a
        , ": "
        ]
      , td_ [dollarView v]
      ]
