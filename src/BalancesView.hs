{-# LANGUAGE
    OverloadedStrings
  , RankNTypes
  , ScopedTypeVariables
  #-}

module BalancesView where

import Finance (Account (..), AccountLimit (NoRestriction), Dollar, Balances, outOfLimitError, blankAccount)
import AccountView (accountEdit, accountView)
import DollarView (dollarEdit, dollarView)

import Shpadoinkle (Html, text, MonadJSM)
import Shpadoinkle.Html (span_, table_, tr_, td_, button, onClick, ul_, li_, div_)
import Shpadoinkle.Lens (onSum)

import Control.Lens.Tuple (_1, _2)
import Control.Lens.Combinators (imap)
import Control.Lens.At (ix)
import qualified Data.Map as Map


balancesEdit :: forall m. MonadJSM m => [(Account, Dollar)] -> Html m [(Account, Dollar)]
balancesEdit xs = div_
  [ ul_ $ imap balanceEdit xs
  , newButton
  ]
  where
    balanceEdit :: Int -> (Account, Dollar) -> Html m [(Account, Dollar)]
    balanceEdit idx (a,v) = li_ $
         (onSum (ix idx . _1) <$> accountEdit a)
      <> (onSum (ix idx . _2) <$> dollarEdit v)
      <> [ button [onClick $ \xs -> take idx xs <> drop (idx + 1) xs] ["Delete"]
         ]
      <> ( case outOfLimitError a v of
              Nothing -> []
              Just e -> [span_ [text e]]
         )
      where
    newButton :: Html m [(Account, Dollar)]
    newButton = button [onClick (<> [(blankAccount, 0)])] ["Add New Account"]

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
