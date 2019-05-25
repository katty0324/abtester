{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Record
  ( Record(..)
  , mean
  , standardDeviation
  , probabilityToBeast
  , conversionRate
  , probabilityDistributionPoints
  ) where

import           Data.Number.Erf
import           Import
import           Model.ConversionRate
import           Model.Probability
import           Model.Times

data Record =
  Record
    { session    :: Times
    , conversion :: Times
    }

mean :: Record -> Double
mean record = (fromIntegral . conversion) record / (fromIntegral . session) record

standardDeviation :: Record -> Double
standardDeviation record = sqrt $ mean record * (1 - mean record) / (fromIntegral . session) record

variance :: Record -> Double
variance record = standardDeviation record ^ (2 :: Int)

conversionRate :: Double -> Record -> ConversionRate
conversionRate sigma record = mean record + sigma * standardDeviation record

probabilityToBeast :: Record -> Record -> Probability
probabilityToBeast originalRecord targetRecord =
  1 / 2 *
  (1 -
   erf
     (-(mean targetRecord - mean originalRecord) /
       sqrt (2 * (variance targetRecord + variance originalRecord))))

probabilityDistributionPoints :: Record -> [(Probability, Double)]
probabilityDistributionPoints record = zip xs (map (probabilityDistribution record) xs)
  where
    left = mean record - 4 * standardDeviation record
    right = mean record + 4 * standardDeviation record
    xs = [left,left + (right - left) / 20 .. right]

probabilityDistribution :: Record -> Probability -> Double
probabilityDistribution record probability =
  1 / sqrt (2 * pi * variance record) *
  exp (-(probability - mean record) ^ 2 / (2 * variance record))
