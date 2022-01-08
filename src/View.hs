{-# LANGUAGE CPP                 #-}
{-# LANGUAGE OverloadedLabels    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module View where

import           Bootstrap.Modal             (modal)
import           Debouncer                   (Debouncer)
import           Finance.Account             (AccountAux, AccountId, mkAccounts)
import           Finance.Plan                (Cost (..), FinancePlan (..),
                                              FinancePlanType (..), Income (..),
                                              Transfer (..), defFinancePlan)
import           Model                       (ComputeBatchPicker (..),
                                              Model (..), emptyModel,
                                              encodeForHash, modelToUint8Array,
                                              uint8ArrayToModel)
import           Utils.List                  (dropIndex, moveDown, moveUp)
import           View.Balances               (balancesEdit)
import           View.Day                    (dayEdit)
import           View.Plan                   (financePlanEdit, financePlanView)
import           View.Progress               (progress)

import           Prelude                     hiding (div, log, min, span)
import           Shpadoinkle                 (Html, MonadJSM, RawNode (..),
                                              listenRaw, text)
import           Shpadoinkle.Continuation    (done, pur)
import           Shpadoinkle.Html            (a, accept, button, canvas',
                                              checked, className, div, em_,
                                              footer_, h1_, h2_, h3_, h4_,
                                              header, height, hr', href, id',
                                              input', label, label_, main', min,
                                              onCheck, onClick, onClickM,
                                              onOption, option, p, p_, section_,
                                              select, selected, span, step,
                                              styleProp, target, textProperty,
                                              type', value, width, title)
import           Shpadoinkle.Lens            (onRecord, onSum)

import           Control.Lens                (Lens', lens, (%~), (^.))
import           Control.Lens.At             (ix)
import           Control.Lens.Combinators    (imap)
import           Control.Lens.Tuple          (_1, _2)
import           Control.Monad.IO.Class      (MonadIO (liftIO))
import           Data.Generics.Labels        ()
import           Data.List                   (intersperse)
import qualified Data.Map                    as Map
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time.Calendar          (Day)
import           GHCJS.Foreign.Callback      (Callback, asyncCallback1)
import qualified JavaScript.TypedArray       as TA
import           Language.Javascript.JSaddle (JSString, JSVal,
                                              fromJSValUnchecked, makeObject,
                                              textFromJSString, unsafeGetProp)
import           System.IO.Unsafe            (unsafePerformIO)
import           Text.Read                   (readMaybe)
import           UnliftIO                    (TVar, atomically, newEmptyTMVarIO,
                                              putTMVar, takeTMVar)




#ifndef ghcjs_HOST_OS
-- getContext :: IO JSVal
-- getContext = error "Must use in GHCjs"
-- newChart :: JSVal -> JSVal -> IO JSVal
-- newChart = error "Must use in GHCjs"
-- updateChart :: JSVal -> IO ()
-- updateChart = error "Must use in GHCjs"
-- assignChartData :: JSVal -> JSVal -> IO ()
-- assignChartData = error "Must use in GHCjs"
-- getHash :: IO Text
-- getHash = error "Must use in GHCjs"
-- getHrefWithoutHash :: IO Text
-- getHrefWithoutHash = error "Must use in GHCjs"
dataToUrl :: TA.Uint8Array -> IO Text
dataToUrl = error "Must use in GHCjs"
getFiles :: IO JSVal
getFiles = error "Must use in GHCjs"
filesLength :: JSVal -> IO Int
filesLength = error "Must use in GHCjs"
firstFile :: JSVal -> IO JSVal
firstFile = error "Must use in GHCjs"
arrayBufferToUint8Array :: JSVal -> IO TA.Uint8Array
arrayBufferToUint8Array = error "Must use in GHCjs"
fileToArrayBuffer :: JSVal -> Callback (JSVal -> IO ()) -> IO ()
fileToArrayBuffer = error "Must use in GHCjs"
-- resetHash :: IO ()
-- resetHash = error "Must use in GHCjs"
-- initializePopovers :: IO ()
-- initializePopovers = error "Must use in GHCjs"
-- setBadgeTextColor :: IO ()
-- setBadgeTextColor = error "Must use in GHCjs"
#else
-- foreign import javascript unsafe "$r = document.getElementById('graphed-income').getContext('2d');" getContext :: IO JSVal
-- foreign import javascript unsafe "$r = new Chart($1, $2);" newChart :: JSVal -> JSVal -> IO JSVal
-- foreign import javascript unsafe "$1.update();" updateChart :: JSVal -> IO ()
-- foreign import javascript unsafe "assignChartData($1,$2);" assignChartData :: JSVal -> JSVal -> IO ()
-- foreign import javascript unsafe "$r = window.location.hash;" getHash' :: IO JSVal
-- foreign import javascript unsafe "$r = window.location.href;" getHref' :: IO JSVal
foreign import javascript unsafe "$r = URL['createObjectURL'](new File([$1],'budget.bgt',{'type':'application/budgetable'}));" dataToUrl' :: TA.Uint8Array -> IO JSString
foreign import javascript unsafe "$r = document['getElementById']('import-file')['files'];" getFiles :: IO JSVal
foreign import javascript unsafe "$r = $1.length;" filesLength :: JSVal -> IO Int
foreign import javascript unsafe "$r = $1[0];" firstFile :: JSVal -> IO JSVal
foreign import javascript safe "$1['arrayBuffer']().then($2);" fileToArrayBuffer :: JSVal -> Callback (JSVal -> IO ()) -> IO ()
foreign import javascript unsafe "$r = new Uint8Array($1);" arrayBufferToUint8Array :: JSVal -> IO TA.Uint8Array
-- foreign import javascript unsafe "history['replaceState']('',document.title,window.location.pathname + window.location.search);" resetHash :: IO ()
-- foreign import javascript unsafe "initializePopovers();" initializePopovers :: IO ()
-- foreign import javascript unsafe "setBadgeTextColor();" setBadgeTextColor :: IO ()
-- getHash :: IO Text
-- getHash = fromJSValUnchecked =<< getHash'
-- getHrefWithoutHash :: IO Text
-- getHrefWithoutHash = T.takeWhile (/= '#') <$> (fromJSValUnchecked =<< getHref')
dataToUrl :: TA.Uint8Array -> IO Text
dataToUrl = fmap textFromJSString . dataToUrl'
#endif


view :: forall m
      . MonadJSM m
     => Day
     -> Text
     -> Debouncer m Text
     -> TVar (Int, Int)
     -> Model
     -> Html m Model
view today currentHref debouncer progressVar currentModel@Model{..} = main' [className "container"]
  [ header [className "row"]
    [ div [className "col-12 col-sm-6"]
      [ h1_ ["Budgetable.org"]
      , p [styleProp [("font-size","1.25rem"),("font-style","italic")]]
        [ a [href "https://github.com/budgetable/budgetable.github.io", target "_blank"] ["Open-Source"]
        , " Budget Visualization Software"
        ]
      ]
    , div [className "col-12 col-sm-6", styleProp [("text-align","right")]] $
      let alignLeft = styleProp [("text-align","left")]
      in  [ button
            [ className "btn btn-secondary"
            , textProperty "data-bs-toggle" ("modal" :: Text)
            , textProperty "data-bs-target" ("#dialog-import" :: Text)
            ] ["Import"]
          , "&nbsp;"
          , modal
              [alignLeft]
              "dialog-import"
              "Import"
              [ p_ ["Select a budget file"]
              , input' [type' "file", accept ".bgt", id' "import-file", className "form-control"]
              ]
              (\dismiss ->
                [ button
                  [ className "btn btn-primary"
                  , dismiss
                  , onClickM . liftIO $ do
                      files <- getFiles
                      filesLen <- filesLength files
                      if filesLen == 0 then pure id
                      else do
                        file <- firstFile files
                        stateModifierVar <- newEmptyTMVarIO
                        cb <- asyncCallback1 $ \buff -> do
                          buff' <- arrayBufferToUint8Array buff
                          mModel <- uint8ArrayToModel buff'
                          atomically . putTMVar stateModifierVar $ maybe id const mModel
                        fileToArrayBuffer file cb
                        atomically $ takeTMVar stateModifierVar
                  ]
                  ["Import"]
                ])
          , button
            [ className "btn btn-secondary"
            , textProperty "data-bs-toggle" ("modal" :: Text)
            , textProperty "data-bs-target" ("#dialog-export" :: Text)
            ] ["Export"]
          , "&nbsp;"
          , let shareLink = currentHref <> "#" <> unsafePerformIO (encodeForHash currentModel)
            in  modal
                  [alignLeft]
                  "dialog-export"
                  "Export"
                  [ p [styleProp [("overflow-x","auto")]]
                    [ "Share link: "
                    , a [ href shareLink
                        , target "_blank"
                        , styleProp [("white-space","nowrap")]
                        ] [text shareLink]
                    ]
                  ]
                  (const
                    [ a
                      [ className "btn btn-primary"
                      , href . unsafePerformIO $ dataToUrl =<< modelToUint8Array currentModel
                      , textProperty "download" ("budget.bgt" :: Text)
                      ]
                      ["Export"]
                    ])
          , button
            [ className "btn btn-secondary"
            , textProperty "data-bs-toggle" ("modal" :: Text)
            , textProperty "data-bs-target" ("#dialog-new" :: Text)
            ] ["New"]
          , modal
              [alignLeft]
              "dialog-new"
              "Are you sure?"
              [p_ ["Making a new budget will delete everything so far. Are you sure you want to start a new one?"]]
              (\dismiss ->
                [ button [className "btn btn-danger", onClick . const $ emptyModel today, dismiss]
                  ["Yes, create a new budget"]
                ])
          ]
    ]
  , hr' [styleProp [("height", "3px")]]
  , section_
    [ h3_ ["Accounts"]
    , let editBalancesLens :: Lens' Model [(AccountId, AccountAux)]
          editBalancesLens = lens (^. #balancesInEdit) $ \m@Model{financePlans = ps} inEdit ->
            let updateAux :: AccountId -> AccountAux -> FinancePlan -> FinancePlan
                updateAux aId aux f@(FinancePlan t _ _ _) =
                  f {financePlanType = case t of
                      FinancePlanTypeIncome (Income aId' _)
                        | aId' == aId -> FinancePlanTypeIncome $ Income aId aux
                      FinancePlanTypeCost (Cost aId' _)
                        | aId' == aId -> FinancePlanTypeCost $ Cost aId aux
                      FinancePlanTypeTransfer t' -> FinancePlanTypeTransfer $ case t' of
                        Transfer tId _ fId _ | aId == tId && aId == fId -> Transfer aId aux aId aux
                        Transfer aId' _ x y | aId' == aId -> Transfer aId aux x y
                        Transfer x y aId' _ | aId' == aId -> Transfer x y aId aux
                        _ -> t'
                      _ -> t
                    }
            in case mkAccounts inEdit of
              Nothing -> m {balancesInEdit = inEdit}
              Just bs ->
                m { balancesInEdit = inEdit
                  , balancesSaved = bs
                  , financePlans = Map.foldrWithKey (\k aux -> map (_1 %~ updateAux k aux)) ps bs
                  }
      in  onRecord editBalancesLens $ balancesEdit debouncer balancesInEdit
    ]
  , hr' [styleProp [("height", "3px")]]
  , section_
    [ h3_ ["Finance Plans"]
    , onRecord #financePlans $ listOfFinancePlansEdit financePlans
    ]
  , hr' [styleProp [("height", "3px")]]
  , section_
    [ h2_ ["Forecast"]
    , h4_ ["Start Date"]
    , onRecord #startDate $ dayEdit "forecast" startDate
    , div [className "row"]
      [ div [className "col"] . (: []) $ div [className "form-group"]
        [ label_ ["Interval:"]
        , select
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
        ]
      , div [className "col"] . (: []) $ div [className "form-group"]
        [ label_
          [ text $ "Number of " <> case computeBatch of
              PickerComputeDaily   -> "Days:"
              PickerComputeWeekly  -> "Weeks:"
              PickerComputeMonthly -> "Months:"
              PickerComputeYearly  -> "Years:"
          ]
        , input'
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
      ]
    , progress progressVar
    , canvas' [id' "graphed-income", width 400, height 400]
    ]
  , hr' [styleProp [("height", "3px")]]
  , footer_
    [ p [styleProp [("text-align","center")]]
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
    , p [styleProp [("text-align","center")]]
      [ "Want to help me with "
      , em_ ["my"]
      , " budget? "
      , a [href "https://www.buymeacoffee.com/athanclark", target "_blank"]
        [ "Buy me a "
        , span [styleProp [("text-decoration","line-through")]] ["coffee"]
        , " beer! &#127867;"
        ]
      ]
    ]
  ]
  where
    listOfFinancePlansEdit :: [(FinancePlan, Bool)] -> Html m [(FinancePlan, Bool)]
    listOfFinancePlansEdit fs = div [id' "finance-plans"] $
      intersperse (hr' [styleProp [("height", "2px")]]) (imap itemFinancePlansEdit fs) <>
        [ div [className "row d-grid", styleProp [("padding-top","0.5rem")]] [newButton]
        ]
      where
        itemFinancePlansEdit :: Int -> (FinancePlan, Bool) -> Html m [(FinancePlan, Bool)]
        itemFinancePlansEdit idx (f,isEditable) = div [className "row finance-plan"]
          [ div [className "col-12 col-lg-11"] . (: []) $
              if isEditable
              then onSum (ix idx . _1) (financePlanEdit balancesSaved debouncer (T.pack (show idx)) f)
              else financePlanView f
          , div [className "col-12 col-lg-1"] $
            [ div [className "row"] . (: []) . div [className "col"] . (: []) $
                div [className "form-check form-switch"]
                [ onSum (ix idx . _2) $
                    input'
                      [ type' "checkbox"
                      , checked isEditable
                      , onCheck const
                      , className "form-check-input"
                      , title $ (if isEditable then "Close" else "Open") <> " configuration menu"
                      ]
                , label [className "form-check-label"] ["Edit"]
                ]
            ]
            <>
            ( if not isEditable then []
              else
                let dialogIdent = "dialog-finance-plan-delete-" <> T.pack (show idx)
                in  [ div [className "row d-grid"] . (: []) $
                        button
                          [ className "btn btn-outline-danger"
                          , textProperty "data-bs-toggle" ("modal" :: Text)
                          , textProperty "data-bs-target" ("#" <> dialogIdent)
                          , title "Delete this finance plan"
                          ] ["Delete"]
                    , div [className "row d-grid"] . (: []) $
                        button
                          [ className "btn btn-outline-secondary"
                          , styleProp [("margin-top","0.5em")]
                          , onClick $ moveUp idx
                          , title "Move this finance plan up"
                          ] ["&#8593;"]
                    , div [className "row d-grid"] . (: []) $
                        button
                          [ className "btn btn-outline-secondary"
                          , styleProp [("margin-top","0.5em")]
                          , onClick $ moveDown idx
                          , title "Move this finance plan down"
                          ] ["&#8595;"]
                    , modal
                      []
                      dialogIdent
                      "Are you sure?"
                      [p_ ["Are you sure you want to delete this finance plan?"]]
                      (\dismiss ->
                        [ button
                          [ className "btn btn-danger"
                          , onClick $ dropIndex idx
                          , dismiss]
                          ["Yes, delete this finance plan"]
                        ])
                    ]
            )
          ]
        newButton :: Html m [(FinancePlan, Bool)]
        newButton =
          button
            [onClick (<> [(defFinancePlan today, True)]), className "btn btn-secondary"]
            ["Add New Finance Plan"]
