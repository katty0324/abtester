{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.ConversionRate
  ( ConversionRate(..)
  , displayConversionRate
  , calculateConversionRate
  ) where

import           Import
import           Model.Times
import           Model.Record

newtype ConversionRate =
  ConversionRate
    { getConversionRate :: Float
    }

displayConversionRate :: ConversionRate -> Text
displayConversionRate conversionRate =
  (pack . show $ (fromIntegral . ceiling $ getConversionRate conversionRate * 10000) / 100) `mappend`
  "%"

calculateConversionRate :: Record -> ConversionRate
calculateConversionRate record =
  ConversionRate $
  (fromIntegral (getTimes (success record) :: Int)) /
  (fromIntegral (getTimes (total record) :: Int))
