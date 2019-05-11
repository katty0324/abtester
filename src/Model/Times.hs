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

displayTimes :: Times -> Text
displayTimes times = pack . show $ getTimes times
