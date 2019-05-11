{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Record
  ( Record(..)
  , extractRecord
  ) where

import           Import
import           Model.Times
import           Text.Read   (readMaybe)

data Record =
  Record
    { total   :: Times
    , success :: Times
    }

extractRecord :: [(Text, Text)] -> Int -> Maybe Record
extractRecord parameters index = do
  total <- lookupValue parameters "total" index >>= parseTimes
  success <- lookupValue parameters "success" index >>= parseTimes
  Just $ Record total success

lookupValue :: [(Text, Text)] -> Text -> Int -> Maybe Text
lookupValue parameters key index = lookup (getKey key index) parameters

getKey :: Text -> Int -> Text
getKey key index =
  "records[" `mappend` (pack $ show index) `mappend` "][" `mappend` key `mappend` "]"

parseTimes :: Text -> Maybe Times
parseTimes text =
  case readMaybe $ unpack text of
    Just a  -> Just $ Times a
    Nothing -> Nothing
