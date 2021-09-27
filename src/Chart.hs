{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Chart where

import           Finance.Account    (AccountId, Balances)
import           Finance.Dollar     (Dollar, dollarPrinter)

import           Data.Aeson         (ToJSON (..), Value (String), object, (.=))
import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Text          (Text)
import           Data.Time.Calendar (Day)

default (Text)


-- | For updating Chart.js
data ChartData = ChartData
  { chartData       :: [(Day, Balances)]
  , chartDataColors :: Map AccountId Text -- ^ Color / AccountId mapping
  } deriving (Eq)

-- | For setting-up Chart.js
newtype InitialChart = InitialChart {getInitialChart :: ChartData}

instance ToJSON InitialChart where
  toJSON (InitialChart xs) = object
    [ "type" .= String "line"
    , "data" .= xs
    , "options" .= object
      [ "responsive" .= True
      , "plugins" .= object
        [ "legend" .= object ["position" .= String "top"]
        , "title" .= object
          [ "display" .= True
          , "text" .= String "Graphed Budget"
          ]
        , "zoom" .= object
          [ "pan" .= object
            [ "enabled" .= True
            , "mode" .= "xy"
            ]
          , "zoom" .= object
            [ "wheel" .= object ["enabled" .= True]
            , "pinch" .= object ["enabled" .= True]
            , "mode" .= "xy"
            , "overScaleMode" .= "y"
            ]
          ]
        ]
      ]
    ]

transpose :: [Balances] -> Map AccountId [Dollar]
transpose = Map.unionsWith (<>) . map (fmap (: []))

instance ToJSON ChartData where
  toJSON (ChartData data' aux) = object
    [ "labels" .= (show . fst <$> data')
    , "datasets" .=
      ( let transposedData :: [(AccountId, [Dollar])]
            transposedData = Map.toList . transpose $ snd <$> data'
            mkDataset :: (AccountId, [Dollar]) -> Value
            mkDataset (name, dataSet) = case Map.lookup name aux of
              Nothing -> error $ "Can't find color " <> show name <> " in " <> show aux
              Just color -> object
                [ "label" .= name
                , "data" .= (dollarPrinter <$> dataSet)
                , "borderColor" .= color
                , "backgroundColor" .= color
                ]
        in  mkDataset <$> transposedData
      )
    ]
