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
import           Chart                         (ChartData (..),
                                                InitialChart (..))
import           DayView                       (dayEdit)
import           Debouncer                     (Debouncer)
import           Finance                       (Account (..), Balances, Dollar,
                                                FinancePlan (FinancePlanTransfer),
                                                ScheduledTransfer (DateTransfer),
                                                Transfer (..), balancesOverTime,
                                                blankAccount, everyMonth,
                                                everyWeek, everyYear,
                                                mkBalances)
import           FinancePlanView               (financePlanEdit)

import           Prelude                       hiding (div, min)
import           Shpadoinkle                   (Html, JSM, MonadJSM,
                                                RawNode (..), listenRaw,
                                                shpadoinkle, text)
import           Shpadoinkle.Backend.Snabbdom  (runSnabbdom, stage)
import           Shpadoinkle.Continuation      (done, pur, shouldUpdate)
import           Shpadoinkle.Html              (button, canvas', className,
                                                debounceRaw, div, h1_, h2_, h3_,
                                                h4_, height, hr'_, id', input',
                                                min, onClick, onOption, option,
                                                p_, select, selected, step,
                                                type', value, width, styleProp)
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
import           Language.Javascript.JSaddle   (JSVal, fromJSValUnchecked,
                                                makeObject, toJSVal,
                                                unsafeGetProp)
import           Text.Read                     (readMaybe)



data ComputeBatchPicker
  = PickerComputeDaily
  | PickerComputeWeekly
  | PickerComputeMonthly
  | PickerComputeYearly
  deriving (Eq, Ord, Show, Read, Generic, Enum, Bounded)
instance NFData ComputeBatchPicker

data Model = Model
  { balancesInEdit      :: [(Account, Dollar)]
  , balancesInEditError :: Maybe Text
  , balancesSaved       :: Balances
  , startDate           :: Day
  , financePlans        :: [FinancePlan]
  , numberToCompute     :: Int
  , computeBatch        :: ComputeBatchPicker
  } deriving (Eq, Ord, Show, Read, Generic)
instance NFData Model

batchComputed :: Model -> [(Day, Balances)]
batchComputed Model{..} = take numberToCompute $
  let daysComputed = balancesOverTime startDate balancesSaved financePlans
  in  case computeBatch of
    PickerComputeDaily   -> daysComputed
    PickerComputeWeekly  -> everyWeek daysComputed
    PickerComputeMonthly -> everyMonth daysComputed
    PickerComputeYearly  -> everyYear daysComputed

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
  , h3_ ["Active Accounts"]
  , onRecord #balancesInEdit $ balancesEdit debouncer balancesInEdit
  , div [className "row d-grid"]
    [saveBalancesButton]
  , showBalancesError
  , hr'_
  , h3_ ["Finance Plans"]
  , onRecord #financePlans $ listOfFinancePlansEdit financePlans
  , hr'_
  , h2_ ["Computed"]
  , h4_ ["Start Date"]
  , onRecord #startDate (dayEdit startDate)
  , h4_ ["Interval"]
  , div [className "row"]
    [ div [className "col"] . (: []) $
        select
          [ className "form-select"
          , value . T.pack $ show computeBatch
          , onOption $ \t m -> let x = read $ T.unpack t in m { computeBatch = x }
          ] $
          let mkComputePicker :: ComputeBatchPicker -> Html m Model
              mkComputePicker p = option [selected (p == computeBatch), value . T.pack $ show p]
                [ case p of
                    PickerComputeDaily   -> "Daily"
                    PickerComputeWeekly  -> "Weekly"
                    PickerComputeMonthly -> "Monthly"
                    PickerComputeYearly  -> "Yearly"
                ]
          in  mkComputePicker <$> [minBound .. maxBound]
    , div [className "col"] . (: []) $
        input'
          [ type' "number"
          , step "1"
          , min "0"
          , value . T.pack $ show numberToCompute
          , className "form-control"
          , listenRaw "change" $ \(RawNode n) _ -> do
              o <- makeObject n
              v <- unsafeGetProp "value" o
              t <- fromJSValUnchecked v
              case readMaybe t of
                Nothing      -> pure done
                Just newDays -> pure . pur $ \m -> m {numberToCompute = newDays}
          ]
    ]
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
    listOfFinancePlansEdit fs = div [id' "finance-plans"] $
      (imap itemFinancePlansEdit fs) <>
      [ div [className "row d-grid", styleProp [("padding-top","0.5rem")]] [newButton]
      ]
      where
        itemFinancePlansEdit :: Int -> FinancePlan -> Html m [FinancePlan]
        itemFinancePlansEdit idx f = div [className "row finance-plan"]
          [ div [className "col-xs-12 col-lg-11"] . (: []) $
            onSum (ix idx) (financePlanEdit (Map.keysSet balancesSaved) debouncer f)
          , div [className "col-xs-12 col-lg-1 d-grid", styleProp [("padding","0.5rem")]] . (: []) $
            button
            [ onClick $ \xs -> take idx xs <> drop (idx + 1) xs
            , className "btn btn-secondary"
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
  let emptyState = Model [] Nothing Map.empty today [] 7 PickerComputeDaily
  initialState <- fromMaybe emptyState <$> getStorage "budgetable"
  model <- newTVarIO initialState
  debouncer <- debounceRaw 1
  shpadoinkle id runSnabbdom model (view today debouncer) stage

  threadDelay 1000

  context <- getContext
  let initialChart = InitialChart . ChartData $ batchComputed initialState
  chart <- newChart context =<< toJSVal (toJSON initialChart)
  let go () newState = do
        setStorage "budgetable" newState
        threadDelay 1000
        let newChartData = ChartData (batchComputed newState)
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
