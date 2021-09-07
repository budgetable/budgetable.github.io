{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}


module Main where

import           BalancesView                  (balancesEdit)
import           DayView                       (dayEdit)
import           Finance                       (Account (..), Balances, Dollar,
                                                FinancePlan (FinancePlanTransfer),
                                                ScheduledTransfer (DateTransfer),
                                                Transfer (..), balancesOverTime,
                                                blankAccount, mkBalances)
import           FinancePlanView               (financePlanEdit)
import           Chart                         (ChartData (..), InitialChart (..))
import           Debouncer                     (Debouncer)

import           Prelude                       hiding (div, min)
import           Shpadoinkle                   (Html, JSM, MonadJSM,
                                                RawNode (..), listenRaw,
                                                shpadoinkle, text)
import           Shpadoinkle.Backend.Snabbdom  (runSnabbdom, stage)
import           Shpadoinkle.Continuation      (done, pur, shouldUpdate)
import           Shpadoinkle.Html              (button, canvas', className, div,
                                                div_, h1_, h2_, h3_, height,
                                                hr'_, id', input', min, onClick,
                                                p_, step, styleProp,
                                                type', value, width, debounceRaw)
import           Shpadoinkle.Html.LocalStorage (getStorage, setStorage)
import           Shpadoinkle.Lens              (onRecord, onSum)
import           Shpadoinkle.Run               (live, runJSorWarp)

import           Control.Concurrent            (forkIO, threadDelay)
import           Control.Concurrent.STM        (newTVarIO)
import           Control.DeepSeq               (NFData)
import           Control.Lens.At               (ix)
import           Control.Lens.Combinators      (imap)
import           Control.Monad                 (void)
import           Control.Monad.IO.Class        (MonadIO (liftIO))
import           Data.Aeson                    (toJSON)
import           Data.Generics.Labels          ()
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromMaybe)
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time.Calendar            (Day)
import           Data.Time.Clock               (getCurrentTime, utctDay)
import           GHC.Generics                  (Generic)
import           Language.Javascript.JSaddle   (JSVal, toJSVal, fromJSValUnchecked, makeObject,
                                                unsafeGetProp)
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

daysComputed :: Model -> [(Day, Balances)]
daysComputed Model{..} = take daysToCompute (balancesOverTime startDate balancesSaved financePlans)

foreign import javascript unsafe "$r = document.getElementById('graphed-income').getContext('2d');" getContext :: IO JSVal
foreign import javascript unsafe "$r = new Chart($1, $2);" newChart :: JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe "$1.update();" updateChart :: JSVal -> IO ()
foreign import javascript unsafe "$1.data = $2;" assignChartData :: JSVal -> JSVal -> IO ()

view :: forall m
      . MonadJSM m
     => Day
     -> Debouncer m Text
     -> Model
     -> Html m Model
view today debouncer (Model {..}) = div [className "container"] $
  [ h1_ ["Budgetable"]
  , hr'_
  , h3_ ["Start Date"]
  , onRecord #startDate (dayEdit startDate)
  , hr'_
  , h3_ ["Active Accounts"]
  , onRecord #balancesInEdit $ balancesEdit balancesInEdit
  , saveBalancesButton
  , showBalancesError
  , hr'_
  , h3_ ["Finance Plans"]
  , onRecord #financePlans $ listOfFinancePlansEdit financePlans
  , hr'_
  , h3_ ["Days to Compute"]
  , input'
    [ type' "number"
    , step "1"
    , min "0"
    , value . T.pack $ show daysToCompute
    , className "form-control"
    , listenRaw "change" $ \(RawNode n) _ -> do
        o <- makeObject n
        v <- unsafeGetProp "value" o
        t <- fromJSValUnchecked v
        case readMaybe t of
          Nothing      -> pure done
          Just newDays -> pure . pur $ \m -> m {daysToCompute = newDays}
    ]
  , hr'_
  , h2_ ["Computed"]
  , canvas' [id' "graphed-income", width 400, height 400]
  ]
  where
    showBalancesError = case balancesInEditError of
      Nothing -> ""
      Just e  -> p_ [text e]
    saveBalancesButton = button [className "btn btn-primary", onClick saveBalances] ["Save Balances"]
      where
        saveBalances m@(Model {balancesInEdit = inEdit}) = case mkBalances inEdit of
          Left e   -> m {balancesInEditError = Just e}
          Right bs -> m {balancesInEditError = Nothing, balancesSaved = bs}
    listOfFinancePlansEdit :: [FinancePlan] -> Html m [FinancePlan]
    listOfFinancePlansEdit fs = div_ $ (imap itemFinancePlansEdit fs) <> [newButton]
      where
        itemFinancePlansEdit :: Int -> FinancePlan -> Html m [FinancePlan]
        itemFinancePlansEdit idx f = div [className "row"]
          [ div [className "col-md-11"] . (: []) $
            onSum (ix idx) (financePlanEdit (Map.keysSet balancesSaved) debouncer f)
          , div [className "col-md-1"] . (: []) $
            button
            [ onClick $ \xs -> take idx xs <> drop (idx + 1) xs
            , className "btn btn-secondary"
            , styleProp [("margin-top","4rem")]
            ] ["Delete"]
          ]
        newButton :: Html m [FinancePlan]
        newButton =
          button [onClick (<> [blankFinancePlan]), className "btn btn-secondary"] ["Add New Finance Plan"]
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
  model <- newTVarIO initialState
  debouncer <- debounceRaw 1
  shpadoinkle id runSnabbdom model (view today debouncer) stage

  threadDelay 1000

  context <- getContext
  let initialChart = InitialChart . ChartData $ daysComputed initialState
  chart <- newChart context =<< toJSVal (toJSON initialChart)
  let go () newState = do
        setStorage "budgetable" newState
        threadDelay 1000
        let newChartData = ChartData (daysComputed newState)
        assignChartData chart =<< toJSVal (toJSON newChartData)
        updateChart chart
  void $ forkIO $ shouldUpdate go () model


dev :: IO ()
dev = live 8080 app


main :: IO ()
main = do
  putStrLn "\nhi, my name is budgetable"
  putStrLn "happy point of view on http://localhost:8080\n"
  runJSorWarp 8080 app
