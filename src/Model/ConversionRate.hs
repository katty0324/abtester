{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.ConversionRate
  ( ConversionRate(..)
  , displayConversionRate
  ) where

import           Import

type ConversionRate = Float

displayConversionRate :: ConversionRate -> Text
displayConversionRate conversionRate =
  (pack . show $ (fromIntegral . ceiling $ conversionRate * 10000) / 100) `mappend` "%"
