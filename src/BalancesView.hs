{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module BalancesView where

import           AccountView              (accountEdit, accountView)
import           DollarView               (dollarEdit, dollarView)
import           Finance                  (Account (..),
                                           AccountLimit (NoRestriction),
                                           Balances, Dollar, blankAccount,
                                           outOfLimitError)

import           Prelude                  hiding (div)
import           Shpadoinkle              (Html, MonadJSM, text)
import           Shpadoinkle.Html         (button, className, div, div_, form_,
                                           i, li_, onClick, span_, table_, td_,
                                           tr_, ul_)
import           Shpadoinkle.Lens         (onSum)

import           Control.Lens.At          (ix)
import           Control.Lens.Combinators (imap)
import           Control.Lens.Tuple       (_1, _2)
import qualified Data.Map                 as Map


balancesEdit :: forall m. MonadJSM m => [(Account, Dollar)] -> Html m [(Account, Dollar)]
balancesEdit xs = div_ $ (imap balanceEdit xs) <> [newButton]
  where
    balanceEdit :: Int -> (Account, Dollar) -> Html m [(Account, Dollar)]
    balanceEdit idx (a,v) = div [className "row"]
      [ div [className "row"] $
        (onSum (ix idx . _1) <$> accountEdit a)
        <>
        [ div [className "col"] . (: []) $ onSum (ix idx . _2) (dollarEdit v)
        , div [className "col-md-1"]
          [ button
            [ className "btn btn-secondary"
            , onClick $ \xs -> take idx xs <> drop (idx + 1) xs
            ]
            ["Delete"]
          ]
        ]
      , div [className "row"] $
          ( case outOfLimitError a v of
              Nothing -> []
              Just e  -> [span_ [text e]]
          )
      ]
      where
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
