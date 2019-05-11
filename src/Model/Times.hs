{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Times
  ( Times(..)
  , displayTimes
  ) where

import           Import

newtype Times =
  Times
    { getTimes :: Int
    }
  deriving (Show)

displayTimes :: Times -> Text
displayTimes times = pack . show $ getTimes times
