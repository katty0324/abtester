{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Times
  ( Times
  , displayTimes
  ) where

import           Import

type Times = Int

displayTimes :: Times -> Text
displayTimes = pack . show
