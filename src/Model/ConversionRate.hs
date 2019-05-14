{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.ConversionRate
  ( ConversionRate(..)
  , displayConversionRate
  , calculateConversionRate
  , calculateLowerConversionRate
  , calculateUpperConversionRate
  ) where

import           Import
import           Model.Record
import           Model.Times

newtype ConversionRate =
  ConversionRate
    { getConversionRate :: Float
    }

displayConversionRate :: ConversionRate -> Text
displayConversionRate conversionRate =
  (pack . show $ (fromIntegral . ceiling $ getConversionRate conversionRate * 10000) / 100) `mappend`
  "%"

calculateConversionRate :: Record -> ConversionRate
calculateConversionRate = ConversionRate . mean

calculateLowerConversionRate :: Record -> ConversionRate
calculateLowerConversionRate record = ConversionRate $ mean record - 2 * standardDeviation record

calculateUpperConversionRate :: Record -> ConversionRate
calculateUpperConversionRate record = ConversionRate $ mean record + 2 * standardDeviation record
