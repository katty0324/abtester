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
extractRecord parameters i = do
  session' <- lookupValue parameters "session" i >>= parseTimes
  conversion' <- lookupValue parameters "conversion" i >>= parseTimes
  Just $ Record session' conversion'

lookupValue :: [(Text, Text)] -> Text -> Int -> Maybe Text
lookupValue parameters key i = lookup (getKey key i) parameters

getKey :: Text -> Int -> Text
getKey key i = mconcat ["records[", pack . show $ i, "][", key, "]"]

parseTimes :: Text -> Maybe Times
parseTimes = readMaybe . unpack
