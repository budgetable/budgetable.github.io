{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-identities #-}

module Finance.Dollar where

import           Control.Applicative  ((<|>))
import           Control.DeepSeq      (NFData)
import           Data.Attoparsec.Text (Parser, char, digit, many1)
import           Data.Text            (Text)
import qualified Data.Text            as T
import           GHC.Generics         (Generic)


-- | Money, in terms of cent (lossless compared to 'Double')
newtype Cent = Cent {getCent :: Integer}
  deriving (Show, Read, Eq, Ord, Num, Generic, Integral, Real, Enum)
instance NFData Cent

-- | Cent, but with a prettier format
newtype Dollar = Dollar {getDollar :: Cent}
  deriving (Show, Read, Eq, Ord, Num, Generic, Integral, Real, Enum)
instance NFData Dollar
dollarPrinter :: Dollar -> Text
dollarPrinter (Dollar (Cent xs)) =
  if  | xsLen == 1 -> "0.0" <> xs'
      | xsLen == 2 -> "0." <> xs'
      | otherwise  ->
        let lenWithoutCent = xsLen - 2
            (pfx,sfx) = T.splitAt lenWithoutCent xs'
        in  pfx <> "." <> sfx
  where
    xs' = T.pack $ show xs
    xsLen = T.length xs'
dollarParser :: Parser Dollar
dollarParser =
  let noDecimal = do
        ds <- many1 digit
        pure . Dollar . Cent . read $ ds <> "00"
      oneDecimal = do
        ds <- many1 digit
        _ <- char '.'
        dimes <- digit
        pure . Dollar . Cent . read $ ds <> [dimes,'0']
      twoDecimal = do
        ds <- many1 digit
        _ <- char '.'
        dimes <- digit
        pennies <- digit
        pure . Dollar . Cent . read $ ds <> [dimes,pennies]
      isNegative x = do
        _ <- char '-'
        negate <$> x
      allPositives = twoDecimal <|> oneDecimal <|> noDecimal
  in  isNegative allPositives <|> allPositives
