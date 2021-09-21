{-# LANGUAGE
    DeriveGeneric
  , TupleSections
  , RecordWildCards
  , OverloadedStrings
  , ExtendedDefaultRules
  , OverloadedLabels
  , RankNTypes
  , ScopedTypeVariables
  #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module View.Day.Modal where

import Bootstrap.Modal (modal)
import Shpadoinkle (Html, Continuation, baked, JSM, RawNode (..), shpadoinkle, text, liftJSM, MonadJSM, pur)
import Shpadoinkle.Html (button, className, onClickM, div, styleProp, textProperty, onClick)
import Shpadoinkle.Backend.Snabbdom (runSnabbdom)

import Prelude hiding (div)
import           GHCJS.DOM                               (currentDocumentUnchecked)
import           GHCJS.DOM.Document                      (createElement)
import           GHCJS.DOM.Element                       (setId)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Calendar (Day, toGregorian, fromGregorian, gregorianMonthLength)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Control.DeepSeq (NFData)
import Control.Lens ((%~), (.~))
import Control.Lens.Tuple (_2)
import           Data.Generics.Labels        ()
import GHC.Generics (Generic)
import Control.Concurrent.STM (STM, TMVar, atomically, newTVarIO, newEmptyTMVarIO, takeTMVar, putTMVar)
import Control.Concurrent (threadDelay, forkIO)
import Language.Javascript.JSaddle (toJSVal)

default (Text)


prevMonth :: (Integer, Int) -> (Integer, Int)
prevMonth (y,m)
  | m == 1 = (y-1, 12)
  | otherwise = (y, m-1)

nextMonth :: (Integer, Int) -> (Integer, Int)
nextMonth (y,m)
  | m == 12 = (y+1, 1)
  | otherwise = (y, m+1)

-- | Returns a list of whether the day number is within the month, and the day number
weeksOfMonth :: (Integer, Int) -- ^ Year / Month
             -> [[(Bool, Int)]] -- Inner list should be length 7
weeksOfMonth (y,m) =
  [ firstWeek
  , secondWeek
  , thirdWeek
  , fourthWeek
  ] <>
    ( case mFifthWeek of
        Nothing -> []
        Just xs -> [xs]
    )
    <>
    ( case mSixthWeek of
        Nothing -> []
        Just xs -> [xs]
    )
  where
    maxMonth = gregorianMonthLength y m
    firstWeek :: [(Bool, Int)]
    firstWeek
      | weekDayOf1st == 7 = (True,) <$> [1..7]
      | otherwise =
        let prevMonthWeek :: [(Bool, Int)]
            prevMonthWeek =
              (False,)
                <$>
                [ lastDayOfPrevMonth - (daysBefore - 1)
                  .. lastDayOfPrevMonth
                ]
              where
                daysBefore = weekDayOf1st
                lastDayOfPrevMonth =
                  let (y',m') = prevMonth (y,m)
                  in  gregorianMonthLength y' m'
        in  prevMonthWeek <> ((True,) <$> [1 .. (7 - weekDayOf1st)])
      where
        (_,_,weekDayOf1st) = toWeekDate (fromGregorian y m 1)
    secondWeek :: [(Bool, Int)]
    secondWeek = (True,) <$> [lastDayFirstWeek + 1 .. lastDayFirstWeek + 7]
      where
        (_,lastDayFirstWeek) = last firstWeek
    thirdWeek :: [(Bool, Int)]
    thirdWeek = (_2 %~ (+ 7)) <$> secondWeek
    fourthWeek :: [(Bool, Int)]
    fourthWeek = (_2 %~ (+ 7)) <$> thirdWeek
    mFifthWeek :: Maybe [(Bool, Int)]
    mFifthWeek
      | lastDayFourthWeek == maxMonth = Nothing
      | otherwise = Just $
        let inMonth =
              (True,)
                <$>
                [ lastDayFourthWeek + 1
                  .. ( if maxMonth <= lastDayFourthWeek + 7
                       then maxMonth
                       else lastDayFourthWeek + 7 -- won't have any in nextMonthWeek
                     )
                ]
            nextMonthWeek = (False,) <$> [1 .. 7 - length inMonth]
        in  inMonth <> nextMonthWeek
      where
        (_,lastDayFourthWeek) = last fourthWeek
    mSixthWeek :: Maybe [(Bool, Int)]
    mSixthWeek = case mFifthWeek of
      Nothing -> Nothing
      Just xs ->
        let (isStillInMonth,lastDaySoFar) = last xs
        in  if not isStillInMonth || lastDaySoFar == maxMonth then Nothing
            else  Just $
              let inMonth = (True,) <$> [lastDaySoFar + 1 .. maxMonth]
                  nextMonthWeek = (False,) <$> [1 .. 7 - length inMonth]
              in  inMonth <> nextMonthWeek

data ModalState = ModalState
  { year :: Integer
  , month :: Int
  , day :: Int
  , pickedDay :: Day
  } deriving (Show, Eq, Generic)
instance NFData ModalState

datePicker :: forall m
            . Text
           -> Day
           -> Html m Day
datePicker ident today =
  baked mkModal
  where
    mkModal :: JSM (RawNode, STM (Continuation m Day))
    mkModal = do
      doc <- currentDocumentUnchecked
      elm <- createElement doc "div"
      setId elm ("container-" <> ident)
      model <- newTVarIO $
        let (y,m,d) = toGregorian today
        in  ModalState y m d today
      raw <- RawNode <$> toJSVal elm
      nextDayVar <- newEmptyTMVarIO
      _ <- forkIO $ do
        threadDelay 1
        shpadoinkle id runSnabbdom model (view nextDayVar) (pure raw)
      pure
        ( raw
        , pur . const <$> takeTMVar nextDayVar
        )

    view :: forall n. MonadJSM n => TMVar Day -> ModalState -> Html n ModalState
    view nextDayVar ModalState{..} =
      modal
        []
        ident
        "Pick Date"
        ( [ div [className "row"]
            [ div [className "col-auto"] . (: []) $
                button
                  [ className "btn btn-secondary"
                  , onClick $ #year %~ (\y -> y - 1)
                  ] ["&#8592;"]
            , div [className "col", styleProp [("text-align","center")]] [text . T.pack $ show year]
            , div [className "col-auto"] . (: []) $
                button
                  [ className "btn btn-secondary"
                  , onClick $ #year %~ (+ 1)
                  ] ["&#8594;"]
            ]
          , div [className "row"]
            [ div [className "col-auto"] . (: []) $
                button
                  [ className "btn btn-secondary"
                  , onClick $ \(ModalState y' m' d' today') ->
                      let (y'', m'') = prevMonth (y',m')
                      in  ModalState y'' m'' d' today'
                  ] ["&#8592;"]
            , div [className "col", styleProp [("text-align","center")]]
              [ text $ case month of
                  1 -> "January"
                  2 -> "February"
                  3 -> "March"
                  4 -> "April"
                  5 -> "May"
                  6 -> "June"
                  7 -> "July"
                  8 -> "August"
                  9 -> "September"
                  10 -> "October"
                  11 -> "November"
                  12 -> "December"
                  _ -> error $ "Month not in range: " <> show month
              ]
            , div [className "col-auto"] . (: []) $
                button
                  [ className "btn btn-secondary"
                  , onClick $ \(ModalState y' m' d' today') ->
                      let (y'', m'') = nextMonth (y',m')
                      in  ModalState y'' m'' d' today'
                  ] ["&#8594;"]
            ]
          , div [className "row"] $
            let mkCol = div [className "col"] . (: [])
            in  mkCol <$> ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"]
          ] <> allDays
        )
        (const [])
      where
        allDays = mkRow <$> weeksOfMonth (year,month)
          where
            mkRow :: [(Bool, Int)] -> Html n ModalState
            mkRow xs = div [className "row"] $ uncurry dayButton <$> xs
        dayButton :: Bool -> Int -> Html n ModalState
        dayButton inMonth d =
          div [className "col"] . (: []) .
            div [className "row d-grid"] . (: []) $
              button
              [ className $
                let btnType
                      | not inMonth = "btn-"
                      | otherwise = "btn-outline-"
                in  "btn "
                    <> btnType
                    <> ( if fromGregorian year month d == pickedDay
                         then "primary" else "secondary"
                       )
              , dismiss
              , onClickM $ do
                  liftJSM . atomically . putTMVar nextDayVar $ fromGregorian year month d
                  pure $
                    if inMonth
                    then
                      let newDay = fromGregorian year month d
                      in  (#day .~ d) . (#pickedDay .~ newDay)
                    else
                      let (y',m')
                            | d < 15 = prevMonth (year,month)
                            | otherwise = nextMonth (year,month)
                          newDay = fromGregorian y' m' d
                      in  (#year .~ y') . (#month .~ m') . (#day .~ d) . (#pickedDay .~ newDay)
              ] [text . T.pack $ show d]
          where
            dismiss = textProperty "data-bs-dismiss" "modal"
