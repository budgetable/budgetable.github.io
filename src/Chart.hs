{-# LANGUAGE
    OverloadedStrings
  #-}

module Chart where

import Finance (Account (..), Balances, Dollar, dollarPrinter)

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Aeson (ToJSON (..), object, (.=), Value (String))
import Data.Time.Calendar (Day)


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
        ]
      ]
    ]


newtype ChartData = ChartData {getChartData :: [(Day, Balances)]}

transpose :: [Balances] -> Map Account [Dollar]
transpose = Map.unionsWith (<>) . map (fmap (: []))

instance ToJSON ChartData where
  toJSON (ChartData xs) = object
    [ "labels" .= (show . fst <$> xs)
    , "datasets" .=
      ( let ys = Map.toList . transpose $ snd <$> xs
            mkDataset :: (Account, [Dollar]) -> Value
            mkDataset (Account name _ color, data') = object
              [ "label" .= name
              , "data" .= (dollarPrinter <$> data')
              , "borderColor" .= color
              , "backgroundColor" .= color
              ]
        in  mkDataset <$> ys
      )
    ]
