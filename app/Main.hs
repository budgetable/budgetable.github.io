{-# LANGUAGE
    OverloadedStrings
  , OverloadedLabels
  , RecordWildCards
  , NamedFieldPuns
  , DeriveGeneric
  , RankNTypes
  , ScopedTypeVariables
  , StandaloneDeriving
  #-}


module Main where

import           Finance                       (Balances, FinancePlan (FinancePlanTransfer), Account (..),
                                                Dollar, Transfer (..), ScheduledTransfer (DateTransfer),
                                                mkBalances, blankAccount, balancesOverTime)
import           BalancesView                  (balancesEdit, balancesView)
import           FinancePlanView               (financePlanEdit)
import           DayView                       (dayEdit)

import           Prelude                       hiding (min)
import           Shpadoinkle                   (Html, JSM, MonadJSM, text, shpadoinkle, RawNode (..), listenRaw)
import           Shpadoinkle.Backend.Snabbdom  (runSnabbdom, stage)
import           Shpadoinkle.Continuation      (done, pur)
import           Shpadoinkle.Html              (div_, button, onClick, p_, h1_, h2_, ul_, li_, table_, tr_, td_,
                                                input', type', step, min, value)
import           Shpadoinkle.Html.LocalStorage (getStorage, manageLocalStorage)
import           Shpadoinkle.Run               (live, runJSorWarp)
import           Shpadoinkle.Lens              (onRecord, onSum)

import           Control.Monad.IO.Class        (MonadIO (liftIO))
import           Control.DeepSeq               (NFData)
import           Control.Lens.Combinators      (imap)
import           Control.Lens.At               (ix)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time.Clock               (getCurrentTime, utctDay)
import           Data.Time.Calendar            (Day)
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromMaybe)
import           Data.Generics.Labels          ()
import           GHC.Generics                  (Generic)
import           Language.Javascript.JSaddle   (makeObject, unsafeGetProp, fromJSValUnchecked)
import           Text.Read                     (readMaybe)




data Model = Model
  { balancesInEdit      :: [(Account, Dollar)]
  , balancesInEditError :: Maybe Text
  , balancesSaved       :: Balances
  , startDate           :: Day
  , financePlans        :: [FinancePlan]
  , daysToCompute       :: Int
  } deriving (Eq, Ord, Show, Read, Generic)
instance NFData Model


view :: forall m. MonadJSM m => Day -> Model -> Html m Model
view today (Model {..}) = div_ $
  [ h1_ ["Budgetable"]
  , div_ $ map (onRecord #startDate) (dayEdit startDate)
  , onRecord #balancesInEdit $ balancesEdit balancesInEdit
  , saveBalancesButton
  , showBalancesError
  , onRecord #financePlans $ listOfFinancePlansEdit financePlans
  , input'
    [ type' "number"
    , step "1"
    , min "0"
    , value . T.pack $ show daysToCompute
    , listenRaw "change" $ \(RawNode n) _ -> do
        o <- makeObject n
        v <- unsafeGetProp "value" o
        t <- fromJSValUnchecked v
        case readMaybe t of
          Nothing -> pure done
          Just newDays -> pure . pur $ \m -> m {daysToCompute = newDays}
    ]
  , h2_ ["Computed"]
  , table_ $
    let daysComputed = take daysToCompute $ balancesOverTime startDate balancesSaved financePlans
        mkComputedRow (day,b) = tr_ [td_ [text . T.pack $ show day], td_ [balancesView b]]
    in  map mkComputedRow daysComputed
  ]
  where
    showBalancesError = case balancesInEditError of
      Nothing -> ""
      Just e -> p_ [text e]
    saveBalancesButton = button [onClick saveBalances] ["Save Balances"]
      where
        saveBalances m@(Model {balancesInEdit = inEdit}) = case mkBalances inEdit of
          Left e -> m {balancesInEditError = Just e}
          Right bs -> m {balancesInEditError = Nothing, balancesSaved = bs}
    listOfFinancePlansEdit :: [FinancePlan] -> Html m [FinancePlan]
    listOfFinancePlansEdit fs = div_
      [ ul_ $ imap itemFinancePlansEdit fs
      , newButton
      ]
      where
        itemFinancePlansEdit :: Int -> FinancePlan -> Html m [FinancePlan]
        itemFinancePlansEdit idx f = li_ $
             (map (onSum (ix idx)) (financePlanEdit (Map.keysSet balancesSaved) f))
          <> [button [onClick $ \xs -> take idx xs <> drop (idx + 1) xs] ["Delete"]]
        newButton :: Html m [FinancePlan]
        newButton = button [onClick (<> [blankFinancePlan])] ["Add New Finance Plan"]
          where
            blankFinancePlan =
              FinancePlanTransfer $ Transfer
                blankAccount
                blankAccount
                (DateTransfer today)
                0
                ""


app :: JSM ()
app = do
  today <- utctDay <$> liftIO getCurrentTime
  let emptyState = Model [] Nothing Map.empty today [] 0
  initialState <- fromMaybe emptyState <$> getStorage "budgetable"
  print initialState
  model <- manageLocalStorage "budgetable" initialState
  shpadoinkle id runSnabbdom model (view today) stage


dev :: IO ()
dev = live 8080 app


main :: IO ()
main = do
  putStrLn "\nhi, my name is budgetable"
  putStrLn "happy point of view on http://localhost:8080\n"
  runJSorWarp 8080 app
