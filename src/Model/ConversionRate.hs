{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.ConversionRate
  ( ConversionRate
  , displayConversionRate
  ) where

import           Import
import           Model.Math

type ConversionRate = Double

displayConversionRate :: ConversionRate -> Text
displayConversionRate conversionRate =
  (pack . show . ceilingDigit 2 $ conversionRate * 100) `mappend` "%"
