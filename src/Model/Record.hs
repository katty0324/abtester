{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Record
  ( Record(..)
  , mean
  , standardDeviation
  , probabilityToBeast
  , conversionRate
  ) where

import           Data.Number.Erf
import           Import
import          Model.ConversionRate
import           Model.Probability
import           Model.Times

data Record =
  Record
    { session    :: Times
    , conversion :: Times
    }

mean :: Record -> Float
mean record = (fromIntegral . conversion) record / (fromIntegral . session) record

standardDeviation :: Record -> Float
standardDeviation record = sqrt $ mean record * (1 - mean record) / (fromIntegral . session) record

variance :: Record -> Float
variance record = standardDeviation record ^ 2

conversionRate :: Float -> Record -> ConversionRate
conversionRate sigma record = mean record + sigma * (standardDeviation record)

probabilityToBeast :: Record -> Record -> Probability
probabilityToBeast originalRecord targetRecord =
  1 / 2 *
  (1 -
   erf
     (-(mean targetRecord - mean originalRecord) /
       sqrt (2 * (variance targetRecord + variance originalRecord))))
