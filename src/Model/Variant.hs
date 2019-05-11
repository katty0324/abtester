{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Variant
  ( Variant(..)
  , variant
  ) where

import           Import
import           Model.ConversionRate
import           Model.Record
import           Model.Times

data Variant =
  Variant
    { record         :: Record
    , conversionRate :: ConversionRate
    }

variant :: Record -> Variant
variant record = Variant record (calculateConversionRate record)
