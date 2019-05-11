{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.ConversionRate
  ( ConversionRate(..)
  , showConversionRate
  ) where

import           Import

newtype ConversionRate =
  ConversionRate
    { getConversionRate :: Float
    }
  deriving (Show)

showConversionRate :: ConversionRate -> Text
showConversionRate conversionRate =
  (pack . show $ (fromIntegral . ceiling $ getConversionRate conversionRate * 10000) / 100) `mappend`
  "%"
