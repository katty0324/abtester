{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Probability
  ( Probability(..)
  , displayProbability
  ) where

import           Import

type Probability = Float

displayProbability :: Probability -> Text
displayProbability probability =
  (pack . show $ (fromIntegral . ceiling $ probability * 1000) / 10) `mappend` "%"
