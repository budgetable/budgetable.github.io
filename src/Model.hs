{-# LANGUAGE CPP             #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Model where

import           Chart                       (ChartData (..))
import           Finance                     (balancesOverTime, everyMonth,
                                              everyWeek, everyYear)
import           Finance.Account             (AccountAux (accountAuxColor),
                                              AccountId, Accounts)
import           Finance.Plan                (FinancePlan)
import           Utils.ChartChange           (CausesChartChange (..))

import           Control.DeepSeq             (NFData, deepseq)
import           Control.Lens                ((^.))
import           Control.Lens.Tuple          (_3)
import           Control.Monad               ((<=<))
import           Data.Binary                 (Binary)
import qualified Data.Binary                 as B
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Base64      as BS64
import qualified Data.ByteString.Lazy        as LBS
import           Data.Foldable               (foldlM)
import qualified Data.Map                    as Map
import qualified Data.Set                    as Set
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import           Data.Time.Calendar          (Day)
import           GHC.Generics                (Generic)
import qualified JavaScript.Array.Internal   as A
import qualified JavaScript.TypedArray       as TA
import           Language.Javascript.JSaddle (MonadJSM, fromJSValUnchecked,
                                              toJSVal)
import           UnliftIO.Concurrent         (threadDelay)
import           UnliftIO.STM                (TVar, atomically, writeTVar)

#ifndef ghcjs_HOST_OS
deflatePako :: TA.Uint8Array -> IO TA.Uint8Array
deflatePako = error "Must use in GHCjs"
inflatePako :: TA.Uint8Array -> IO TA.Uint8Array
inflatePako = error "Must use in GHCjs"
uint8ArrayToArray :: TA.Uint8Array -> IO A.JSArray
uint8ArrayToArray = error "Must use in GHCjs"
arrayToUint8Array :: A.JSArray -> IO TA.Uint8Array
arrayToUint8Array = error "Must use in GHCjs"
#else
foreign import javascript unsafe "$r = pako['deflate']($1);" deflatePako :: TA.Uint8Array -> IO TA.Uint8Array
foreign import javascript unsafe "$r = pako['inflate']($1);" inflatePako :: TA.Uint8Array -> IO TA.Uint8Array
foreign import javascript unsafe "$r = Array.from($1);" uint8ArrayToArray :: TA.Uint8Array -> IO A.JSArray
foreign import javascript unsafe "$r = new Uint8Array($1);" arrayToUint8Array :: A.JSArray -> IO TA.Uint8Array
#endif

data ComputeBatchPicker
  = PickerComputeDaily
  | PickerComputeWeekly
  | PickerComputeMonthly
  | PickerComputeYearly
  deriving (Eq, Ord, Show, Read, Generic, Enum, Bounded)
instance NFData ComputeBatchPicker
instance Binary ComputeBatchPicker
instance CausesChartChange ComputeBatchPicker where
  chartChangeEq x y = x == y

data Model = Model
  { balancesInEdit  :: [(AccountId, AccountAux)]
  , balancesSaved   :: Accounts
  , startDate       :: Day
  , financePlans    :: [(FinancePlan, Bool)] -- ^ Bool being whether or not it's being edited FIXME
  , numberToCompute :: Int
  , computeBatch    :: ComputeBatchPicker
  } deriving (Eq, Ord, Show, Read, Generic)
instance NFData Model
instance Binary Model
instance CausesChartChange Model where
  chartChangeEq
    (Model _ xSaved xStartDate xFinancePlans xNumberToCompute xComputeBatch)
    (Model _ ySaved yStartDate yFinancePlans yNumberToCompute yComputeBatch)
    = chartChangeEq xSaved ySaved
    && chartChangeEq xStartDate yStartDate
    && ( and (zipWith (\(x,_) (y,_) -> chartChangeEq x y) xFinancePlans yFinancePlans) -- edited text
       || Set.fromList (fst <$> xFinancePlans) == Set.fromList (fst <$> xFinancePlans) -- just reordered
       )
    && chartChangeEq xNumberToCompute yNumberToCompute
    && chartChangeEq xComputeBatch yComputeBatch

modelToUint8Array :: Model -> IO TA.Uint8Array
modelToUint8Array =
  deflatePako <=< arrayToUint8Array <=< A.fromListIO <=< traverse toJSVal . BS.unpack . LBS.toStrict . B.encode

uint8ArrayToModel :: TA.Uint8Array -> IO (Maybe Model)
uint8ArrayToModel b = do
  inflated <- fmap BS.pack $ traverse fromJSValUnchecked =<< A.toListIO =<< uint8ArrayToArray =<< inflatePako b
  pure $ case B.decodeOrFail (LBS.fromStrict inflated) of
    Left _        -> Nothing
    Right (_,_,x) -> Just x

bufferOverByteString :: (TA.Uint8Array -> IO TA.Uint8Array) -> BS.ByteString -> IO BS.ByteString
bufferOverByteString f b = do
  buff <- do
    arr <- A.fromListIO =<< traverse toJSVal (BS.unpack b)
    arrayToUint8Array arr
  newBuff <- f buff
  fmap BS.pack $ traverse fromJSValUnchecked =<< A.toListIO =<< uint8ArrayToArray newBuff

encodeForHash :: Model -> IO Text
encodeForHash m = do
  deflated <- bufferOverByteString deflatePako . LBS.toStrict $ B.encode m
  pure . T.decodeUtf8 $ BS64.encode deflated

decodeFromHash :: Text -> IO (Maybe Model)
decodeFromHash t = do
  let mCompressed = getByteString t
  mDecompressed <- sequence $ bufferOverByteString inflatePako <$> mCompressed
  pure $ getModel =<< mDecompressed
  where
    hush (Left _)  = Nothing
    hush (Right x) = Just x
    getModel :: BS.ByteString -> Maybe Model
    getModel =
        fmap (^. _3)
      . hush
      . B.decodeOrFail
      . LBS.fromStrict
    getByteString :: Text -> Maybe BS.ByteString
    getByteString x
      | T.null x = Nothing
      | otherwise =
          hush -- either to maybe
        . BS64.decode -- decode base64 into bytes
        . T.encodeUtf8 -- turn into strict bytestring
        $ T.tail x -- take away first #

emptyModel :: Day -> Model
emptyModel day = Model
  { balancesInEdit  = []
  , balancesSaved   = Map.empty
  , startDate       = day
  , financePlans    = []
  , numberToCompute = 7
  , computeBatch    = PickerComputeDaily
  }


batchComputed :: MonadJSM m => Model -> TVar (Int, Int) -> m ChartData
batchComputed Model{..} progressVar = do
  let computed = take numberToCompute $
        let daysComputed = balancesOverTime startDate balancesSaved (fst <$> financePlans)
        in  case computeBatch of
          PickerComputeDaily   -> daysComputed
          PickerComputeWeekly  -> everyWeek daysComputed
          PickerComputeMonthly -> everyMonth daysComputed
          PickerComputeYearly  -> everyYear daysComputed

  atomically $ writeTVar progressVar (0, numberToCompute)

  let go prevIdx nextComputed = do
        deepseq nextComputed .
          atomically $ writeTVar progressVar (prevIdx + 1, numberToCompute)
        threadDelay 1
        pure $ prevIdx + 1
  _ <- foldlM go 0 computed

  atomically $ writeTVar progressVar (numberToCompute, numberToCompute)

  pure $ ChartData computed (accountAuxColor <$> balancesSaved)
