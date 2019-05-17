{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Probability
  ( Probability
  , displayProbability
  ) where

import           Import
import           Model.Math

type Probability = Double

displayProbability :: Probability -> Text
displayProbability probability = (pack . show . ceilingDigit 1 $ probability * 100) `mappend` "%"
