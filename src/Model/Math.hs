{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Math
  ( ceilingDigit
  ) where

import           Import

ceilingDigit :: Int -> Double -> Double
ceilingDigit digit value = fromIntegral (ceiling $ value * 10 ^ digit :: Int) / 10 ^ digit
