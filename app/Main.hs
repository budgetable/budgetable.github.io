{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where


import           Chart                         (ChartData (..),
                                                InitialChart (..))
import           Debouncer                     (Debouncer)
import           Finance                       (balancesOverTime, everyMonth,
                                                everyWeek, everyYear)
import           Finance.Account               (AccountAux (accountAuxColor),
                                                AccountId, Accounts,
                                                blankAccount, mkAccounts)
import           Finance.Plan                  (FinancePlan (..),
                                                FinancePlanType (FinancePlanTypeTransfer),
                                                Transfer (..))
import           Finance.Schedule              (Schedule (DateSchedule))
import           View.Balances                 (balancesEdit)
import           View.Day                      (dayEdit)
import           View.FinancePlan              (financePlanEdit,
                                                financePlanView)

import           Prelude                       hiding (div, min)
import           Shpadoinkle                   (Html, JSM, MonadJSM,
                                                RawNode (..), listenRaw,
                                                shpadoinkle, text)
import           Shpadoinkle.Backend.Snabbdom  (runSnabbdom, stage)
import           Shpadoinkle.Continuation      (done, pur, shouldUpdate)
import           Shpadoinkle.Html              (a, button, canvas', checked,
                                                className, debounceRaw, div,
                                                h1_, h2_, h3_, h4_, height,
                                                hr'_, href, id', input', label,
                                                min, onCheck, onClick, onOption,
                                                option, p, select, selected,
                                                step, styleProp, target, type',
                                                value, width, h5, textProperty, button', tabIndex, p_)
import           Shpadoinkle.Html.LocalStorage (getStorage, setStorage)
import           Shpadoinkle.Lens              (onRecord, onSum)
import           Shpadoinkle.Run               (live, runJSorWarp)

import           Control.DeepSeq               (NFData)
import           Control.Lens                  (Lens', lens, (^.))
import           Control.Lens.At               (ix)
import           Control.Lens.Combinators      (imap)
import           Control.Lens.Tuple            (_1, _2, _3)
import           Control.Monad                 (void, when, (<=<))
import           Control.Monad.IO.Class        (MonadIO (liftIO))
import           Data.Aeson                    (toJSON)
import           Data.Binary                   (Binary)
import qualified Data.Binary as B
import           Data.Generics.Labels          ()
import qualified Data.Map                      as Map
import           Data.Maybe                    (fromJust)
import Data.Monoid (First (..))
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Data.Text.Encoding            as T
import           Data.Time.Calendar            (Day)
import           Data.Time.Clock               (getCurrentTime, utctDay)
import qualified Data.ByteString.Base64        as BS64
import qualified Data.ByteString.Lazy          as LBS
import           GHC.Generics                  (Generic)
import           Language.Javascript.JSaddle   (JSVal, fromJSValUnchecked,
                                                makeObject, toJSVal,
                                                unsafeGetProp)
import           Text.Read                     (readMaybe)
import           UnliftIO                      (newTVarIO, readTVarIO)
import           UnliftIO.Concurrent           (forkIO, threadDelay)



data ComputeBatchPicker
  = PickerComputeDaily
  | PickerComputeWeekly
  | PickerComputeMonthly
  | PickerComputeYearly
  deriving (Eq, Ord, Show, Read, Generic, Enum, Bounded)
instance NFData ComputeBatchPicker
instance Binary ComputeBatchPicker

data Model = Model
  { balancesInEdit  :: [(AccountId, AccountAux)]
  , balancesSaved   :: Accounts
  , startDate       :: Day
  , financePlans    :: [(FinancePlan, Bool)]
  , numberToCompute :: Int
  , computeBatch    :: ComputeBatchPicker
  } deriving (Eq, Ord, Show, Read, Generic)
instance NFData Model
instance Binary Model

emptyModel :: Day -> Model
emptyModel day = Model
  { balancesInEdit  = []
  , balancesSaved   = Map.empty
  , startDate       = day
  , financePlans    = []
  , numberToCompute = 7
  , computeBatch    = PickerComputeDaily
  }

batchComputed :: Model -> ChartData
batchComputed Model{..} =
  let computed = take numberToCompute $
        let daysComputed = balancesOverTime startDate balancesSaved (fst <$> financePlans)
        in  case computeBatch of
          PickerComputeDaily   -> daysComputed
          PickerComputeWeekly  -> everyWeek daysComputed
          PickerComputeMonthly -> everyMonth daysComputed
          PickerComputeYearly  -> everyYear daysComputed
  in  ChartData computed (accountAuxColor <$> balancesSaved)

#ifndef ghcjs_HOST_OS
getContext :: IO JSVal
getContext = error "Must use in GHCjs"
newChart :: JSVal -> JSVal -> IO JSVal
newChart = error "Must use in GHCjs"
updateChart :: JSVal -> IO ()
updateChart = error "Must use in GHCjs"
assignChartData :: JSVal -> JSVal -> IO ()
assignChartData = error "Must use in GHCjs"
getHash :: IO Text
getHash = error "Must use in GHCjs"
#else
foreign import javascript unsafe "$r = document.getElementById('graphed-income').getContext('2d');" getContext :: IO JSVal
foreign import javascript unsafe "$r = new Chart($1, $2);" newChart :: JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe "$1.update();" updateChart :: JSVal -> IO ()
foreign import javascript unsafe "$1.data = $2;" assignChartData :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe "$r = window.location.hash;" getHash' :: IO JSVal
getHash :: IO Text
getHash = fromJSValUnchecked =<< getHash'
#endif

view :: forall m
      . MonadJSM m
     => Day
     -> Debouncer m Text
     -> Model
     -> Html m Model
view today debouncer Model{..} = div [className "container"]
  [ div [className "row"]
    [ div [className "col"] . (: []) $ h1_ ["Budgetable.org"]
    , div [className "col", styleProp [("text-align","right")]]
      [ button
        [ className "btn btn-secondary"
        , textProperty "data-bs-toggle" ("modal" :: Text)
        , textProperty "data-bs-target" ("#dialog-new" :: Text)
        ] ["New"]
      , div [className "modal fade", tabIndex (-1), id' "dialog-new"]
        [ div [className "modal-dialog"]
          [ div [className "modal-content"] $
            let dismiss = textProperty "data-bs-dismiss" ("modal" :: Text)
            in  [ div [className "modal-header"]
                  [ h5 [className "modal-title"] ["Are you sure?"]
                  , button' [className "btn-close", dismiss]
                  ]
                , div [className "modal-body"] . (: []) . p_ . (: []) $
                  "Making a new budget will delete everything so far. Are you sure you want to start a new one?"
                , div [className "modal-footer"]
                  [ button [className "btn btn-secondary", dismiss]
                    ["Cancel"]
                  , button [className "btn btn-danger", onClick . const $ emptyModel today, dismiss]
                    ["Yes, create a new budget"]
                  ]
                ]
          ]
        ]
      ]
    ]
  , hr'_
  , h3_ ["Active Accounts"]
  , let editBalancesLens :: Lens' Model [(AccountId, AccountAux)]
        editBalancesLens = lens (^. #balancesInEdit) $ \m inEdit -> case mkAccounts inEdit of
          Nothing -> m {balancesInEdit = inEdit}
          Just bs -> m {balancesInEdit = inEdit, balancesSaved = bs}
    in  onRecord editBalancesLens $ balancesEdit debouncer balancesInEdit
  , hr'_
  , h3_ ["Finance Plans"]
  , onRecord #financePlans $ listOfFinancePlansEdit financePlans
  , hr'_
  , h2_ ["Computed"]
  , h4_ ["Start Date"]
  , onRecord #startDate $ dayEdit startDate
  , h4_ ["Interval"]
  , div [className "row"]
    [ div [className "col"] . (: []) $
        select
          [ className "form-select"
          , value . T.pack $ show computeBatch
          , onOption $ \t m -> let x = read $ T.unpack t in m { computeBatch = x }
          ] $
          let mkComputePicker :: ComputeBatchPicker -> Html m Model
              mkComputePicker p' = option [selected (p' == computeBatch), value . T.pack $ show p']
                [ case p' of
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
  , hr'_
  , p [styleProp [("text-align","center")]]
    [ "Budgetable is built with the great-and-powerful "
    , a [href "https://shpadoinkle.org", target "_blank"] ["Shpadoinkle"]
    , ", "
    , a [href "https://chartjs.org", target "_blank"] ["Chart.js"]
    , ", and "
    , a [href "https://getbootstrap.com", target "_blank"] ["Bootstrap"]
    , "."
    ]
  , p [styleProp [("text-align","center")]]
    [ "It is Free Software released with NO WARRANTY, under the "
    , a [href "https://www.gnu.org/licenses/gpl-3.0.en.html", target "_blank"] ["GNU GPL v3"]
    , "."
    ]
  , p [styleProp [("text-align","center")]]
    [ "Check us out on "
    , a [href "https://github.com/budgetable/budgetable.github.io", target "_blank"] ["GitHub"]
    , "!"
    ]
  ]
  where
    listOfFinancePlansEdit :: [(FinancePlan, Bool)] -> Html m [(FinancePlan, Bool)]
    listOfFinancePlansEdit fs = div [id' "finance-plans"] $
      imap itemFinancePlansEdit fs <>
      [ div [className "row d-grid", styleProp [("padding-top","0.5rem")]] [newButton]
      ]
      where
        itemFinancePlansEdit :: Int -> (FinancePlan, Bool) -> Html m [(FinancePlan, Bool)]
        itemFinancePlansEdit idx (f,isEditable) = div [className "row finance-plan"]
          [ div [className "col-xs-12 col-lg-11"] . (: []) $
              if isEditable
              then onSum (ix idx . _1) (financePlanEdit balancesSaved debouncer f)
              else financePlanView f
          , div [className "col-xs-12 col-lg-1"] $
            [ div [className "row"] . (: []) $ div [className "form-check"]
                [ onSum (ix idx . _2) $
                    input' [type' "checkbox", checked isEditable, onCheck const, className "form-check-input"]
                , label [className "form-check-label"] ["Edit"]
                ]
            ]
            <>
              [ div [className "row d-grid"] . (: []) $
                  button
                    [ onClick $ \xs -> take idx xs <> drop (idx + 1) xs
                    , className "btn btn-secondary"
                    ] ["Delete"]
              | isEditable
              ] -- , styleProp [("padding","0.5rem")]] . (: []) $
          ]
        newButton :: Html m [(FinancePlan, Bool)]
        newButton =
          button
            [onClick (<> [(blankFinancePlan, True)]), className "btn btn-secondary"]
            [text $ "Add New Finance Plan" <> if null fs then " (click me)" else ""]
          where
            blankFinancePlan =
              FinancePlan
                (FinancePlanTypeTransfer $
                   uncurry (uncurry Transfer blankAccount)
                      blankAccount)
                (DateSchedule today)
                0
                ""

app :: JSM ()
app = do
  today <- utctDay <$> liftIO getCurrentTime
  initialState <- do
    let hush (Left _) = Nothing
        hush (Right x) = Just x
        hashToModel :: Text -> Maybe Model
        hashToModel = getModel <=< getByteString
          where
            getModel :: LBS.ByteString -> Maybe Model
            getModel =
                fmap (^. _3)
              . hush
              . B.decodeOrFail
            getByteString :: Text -> Maybe LBS.ByteString
            getByteString x
              | T.null x = Nothing
              | otherwise =
                  fmap LBS.fromStrict
                . hush -- either to maybe
                . BS64.decode -- decode base64 into bytes
                . T.encodeUtf8 -- turn into strict bytestring
                $ T.tail x -- take away first #
    mHash <- First . hashToModel <$> getHash
    mStored <- First <$> getStorage "budgetable"
    let justEmptyModel = First . Just $ emptyModel today
    pure . fromJust . getFirst $ mHash <> mStored <> justEmptyModel
  model <- newTVarIO initialState
  debouncer <- debounceRaw 1
  shpadoinkle id runSnabbdom model (view today debouncer) stage

  threadDelay 1000

  context <- getContext
  let initialChartData = batchComputed initialState
  chartDataVar <- newTVarIO initialChartData
  let initialChart = InitialChart initialChartData
  chart <- newChart context =<< toJSVal (toJSON initialChart)
  let go () newState = do
        setStorage "budgetable" newState
        threadDelay 1000
        let newChartData = batchComputed newState
        oldChartData <- readTVarIO chartDataVar
        when (newChartData /= oldChartData) $ do
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
