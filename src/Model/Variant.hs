{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Variant
  ( Variant(..)
  , getVariant
  , calculateConversionRate
  ) where

import           Import
import           Model.ConversionRate
import           Text.Read            (readMaybe)

data Variant =
  Variant
    { visitors    :: Float
    , conversions :: Float
    }

getVariant :: [(Text, Text)] -> Int -> Maybe Variant
getVariant parameters index = do
  visitors <- lookupValue parameters "visitors" index >>= parseNumber
  conversions <- lookupValue parameters "conversions" index >>= parseNumber
  Just $ Variant visitors conversions

lookupValue :: [(Text, Text)] -> Text -> Int -> Maybe Text
lookupValue parameters key index = lookup (getKey key index) parameters

getKey :: Text -> Int -> Text
getKey key index =
  "variants[" `mappend` (pack $ show index) `mappend` "][" `mappend` key `mappend` "]"

calculateConversionRate :: Variant -> ConversionRate
calculateConversionRate variant = ConversionRate $ conversions variant / visitors variant

parseNumber :: Text -> Maybe Float
parseNumber text = readMaybe $ unpack text
