{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Point
  ( Point
  ) where

import           Import
import           Model.Math
import           Model.Probability

type Point = (Probability, Double)
