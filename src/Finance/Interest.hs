{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric#-}

module Finance.Interest where

import Control.DeepSeq (NFData)
import Data.Scientific (Scientific)
import GHC.Generics (Generic)


newtype Interest = Interest {getInterest :: Scientific}
  deriving (Eq, Ord, Show, Read, Num, NFData, Real, RealFrac, Fractional, Generic)
