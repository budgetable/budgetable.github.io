module Utils.ChartChange where

import           Data.Map           (Map)
import qualified Data.Map           as Map
import           Data.Text          (Text)
import           Data.Time.Calendar (Day)


class CausesChartChange a where
  chartChangeEq :: a -> a -> Bool

instance CausesChartChange Int where
  chartChangeEq x y = x == y

instance CausesChartChange Integer where
  chartChangeEq x y = x == y

instance CausesChartChange Double where
  chartChangeEq x y = x == y

instance CausesChartChange Text where
  chartChangeEq x y = x == y

instance CausesChartChange Day where
  chartChangeEq x y = x == y

instance (CausesChartChange k, CausesChartChange a) => CausesChartChange (Map k a) where
  chartChangeEq x y = chartChangeEq (Map.toList x) (Map.toList y)

instance (CausesChartChange a, CausesChartChange b) => CausesChartChange (a, b) where
  chartChangeEq (xa, xb) (ya, yb) = chartChangeEq xa ya && chartChangeEq xb yb

instance CausesChartChange a => CausesChartChange [a] where
  chartChangeEq xs ys = and $ zipWith chartChangeEq xs ys

instance CausesChartChange a => CausesChartChange (Maybe a) where
  chartChangeEq x y = case (x,y) of
    (Nothing,Nothing)  -> True
    (Just x', Just y') -> chartChangeEq x' y'
    _                  -> False

instance CausesChartChange Bool where
  chartChangeEq x y = x == y
