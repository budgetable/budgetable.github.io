{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Main where

import           Chart                         (ChartData, InitialChart (..))
import           Model                         (batchComputed, decodeFromHash,
                                                emptyModel)
import           Utils.ChartChange             (chartChangeEq)
import           View                          (view)

import           Shpadoinkle                   (JSM, shpadoinkle)
import           Shpadoinkle.Backend.Snabbdom  (runSnabbdom, stage)
import           Shpadoinkle.Continuation      (shouldUpdate)
import           Shpadoinkle.Html              (debounceRaw)
import           Shpadoinkle.Html.LocalStorage (getStorage, setStorage)
import           Shpadoinkle.Run               (runJSorWarp)

import           Control.Monad                 (unless, void)
import           Control.Monad.IO.Class        (MonadIO (liftIO))
import           Data.Aeson                    (toJSON)
import           Data.Generics.Labels          ()
import           Data.Maybe                    (fromJust)
import           Data.Monoid                   (First (..))
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time.Clock               (getCurrentTime, utctDay)
import           Language.Javascript.JSaddle   (JSVal, fromJSValUnchecked,
                                                toJSVal)
import           UnliftIO                      (atomically, newTVarIO,
                                                readTVarIO, writeTVar)
import           UnliftIO.Concurrent           (forkIO, threadDelay)


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
getHrefWithoutHash :: IO Text
getHrefWithoutHash = error "Must use in GHCjs"
resetHash :: IO ()
resetHash = error "Must use in GHCjs"
initializePopovers :: IO ()
initializePopovers = error "Must use in GHCjs"
setBadgeTextColor :: IO ()
setBadgeTextColor = error "Must use in GHCjs"
#else
foreign import javascript unsafe "$r = document.getElementById('graphed-income').getContext('2d');" getContext :: IO JSVal
foreign import javascript unsafe "$r = new Chart($1, $2);" newChart :: JSVal -> JSVal -> IO JSVal
foreign import javascript unsafe "$1.update();" updateChart :: JSVal -> IO ()
foreign import javascript unsafe "assignChartData($1,$2);" assignChartData :: JSVal -> JSVal -> IO ()
foreign import javascript unsafe "$r = window.location.hash;" getHash' :: IO JSVal
foreign import javascript unsafe "$r = window.location.href;" getHref' :: IO JSVal
foreign import javascript unsafe "history['replaceState']('',document.title,window.location.pathname + window.location.search);" resetHash :: IO ()
foreign import javascript unsafe "initializePopovers();" initializePopovers :: IO ()
foreign import javascript unsafe "setBadgeTextColor();" setBadgeTextColor :: IO ()
getHash :: IO Text
getHash = fromJSValUnchecked =<< getHash'
getHrefWithoutHash :: IO Text
getHrefWithoutHash = T.takeWhile (/= '#') <$> (fromJSValUnchecked =<< getHref')
#endif

app :: JSM ()
app = do
  today <- utctDay <$> liftIO getCurrentTime
  -- get initial state
  initialState <- do
    -- decode from URL fragment
    mHash <- fmap First $ do
      mHash' <- decodeFromHash =<< getHash
      case mHash' of
        Nothing -> pure ()
        Just exists  -> do
          resetHash
          setStorage "budgetable" exists
      pure mHash'
    -- fetch from local storage
    mStored <- First <$> getStorage "budgetable"
    let justEmptyModel = First . Just $ emptyModel today
    pure . fromJust . getFirst $ mHash <> mStored <> justEmptyModel
  model <- newTVarIO initialState
  modelOldVar <- newTVarIO initialState
  debouncer <- debounceRaw 1
  currentHref <- getHrefWithoutHash
  progressVar <- newTVarIO (7,7)
  shpadoinkle id runSnabbdom model (view today currentHref debouncer progressVar) stage

  threadDelay 500

  -- build chart
  context <- getContext
  (initialChartData :: ChartData) <- batchComputed initialState progressVar
  let initialChart :: InitialChart
      initialChart = InitialChart initialChartData
  chart <- newChart context =<< toJSVal (toJSON initialChart)
  initializePopovers
  setBadgeTextColor
  -- go is called every time the state changes
  let go () newState = do
        -- store in local storage
        setStorage "budgetable" newState
        threadDelay 500
        oldState <- readTVarIO modelOldVar
        -- only assign new chart data when the model would cause different data
        unless (oldState `chartChangeEq` newState) $ do
          -- get new chart data
          (newChartData :: ChartData) <- batchComputed newState progressVar
          assignChartData chart =<< toJSVal (toJSON newChartData)
          updateChart chart
        initializePopovers
        setBadgeTextColor
        atomically $ writeTVar modelOldVar newState
  void $ forkIO $ shouldUpdate go () model


main :: IO ()
main = do
  putStrLn "\nhi, my name is budgetable"
  runJSorWarp 8080 app
