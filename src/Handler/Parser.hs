{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Parser
  ( extractRecord
  ) where

import           Import
import           Model.Record
import           Model.Times
import           Text.Read    (readMaybe)

extractRecord :: [(Text, Text)] -> Int -> Maybe Record
extractRecord parameters index = do
  session <- lookupValue parameters "session" index >>= parseTimes
  conversion <- lookupValue parameters "conversion" index >>= parseTimes
  Just $ Record session conversion

lookupValue :: [(Text, Text)] -> Text -> Int -> Maybe Text
lookupValue parameters key index = lookup (getKey key index) parameters

getKey :: Text -> Int -> Text
getKey key index =
  "records[" `mappend` (pack $ show index) `mappend` "][" `mappend` key `mappend` "]"

parseTimes :: Text -> Maybe Times
parseTimes = readMaybe . unpack
