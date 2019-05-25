{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Point
  ( Point
  , displayPoint
  , displayPoints
  ) where

import           Import
import           Model.Math
import           Model.Probability

type Point = (Probability, Double)

displayPoint :: Point -> Text
displayPoint point =
  mconcat ["{x:", (pack . show . (* 100) . fst) point, ",y:", (pack . show . snd) point, "}"]

displayPoints :: [Point] -> Text
displayPoints points = "[" `mappend` (intercalate "," $ fmap displayPoint points) `mappend` "]"
