{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Probability
  ( Probability(..)
  , displayProbability
  ) where

import           Import

newtype Probability =
  Probability
    { getProbability :: Float
    }

displayProbability :: Probability -> Text
displayProbability probability =
  (pack . show $ (fromIntegral . ceiling $ getProbability probability * 1000) / 10) `mappend` "%"
