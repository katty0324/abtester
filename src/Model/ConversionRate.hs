{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.ConversionRate
  ( ConversionRate(..)
  , displayConversionRate
  , calculateConversionRate
  ) where

import           Import
import           Model.Times
import           Model.Variant

newtype ConversionRate =
  ConversionRate
    { getConversionRate :: Float
    }
  deriving (Show)

displayConversionRate :: ConversionRate -> Text
displayConversionRate conversionRate =
  (pack . show $ (fromIntegral . ceiling $ getConversionRate conversionRate * 10000) / 100) `mappend`
  "%"

calculateConversionRate :: Variant -> ConversionRate
calculateConversionRate variant =
  ConversionRate $
  (fromIntegral (getTimes (conversions variant) :: Int)) /
  (fromIntegral (getTimes (visitors variant) :: Int))
