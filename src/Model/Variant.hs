{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Variant
  ( Variant(..)
  , conversionRate
  , probabilityToBeast
  , session
  , conversion
  ) where

import           Import
import           Model.ConversionRate
import           Model.Probability
import qualified Model.Record
import           Model.Times

data Variant =
  Variant
    { record :: Model.Record.Record
    }

conversionRate :: Double -> Variant -> ConversionRate
conversionRate sigma variant = Model.Record.conversionRate sigma $ record variant

probabilityToBeast :: Variant -> Variant -> Probability
probabilityToBeast originalVariant targetVariant =
  Model.Record.probabilityToBeast (record originalVariant) (record targetVariant)

session :: Variant -> Times
session = Model.Record.session . record

conversion :: Variant -> Times
conversion = Model.Record.conversion . record
