{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Record
  ( Record(..)
  , extractRecord
  , mean
  , standardDeviation
  , probabilityToBeast
  ) where

import           Data.Number.Erf
import           Import
import           Model.Probability
import           Model.Times
import           Text.Read         (readMaybe)

data Record =
  Record
    { session    :: Times
    , conversion :: Times
    }

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
parseTimes text =
  case readMaybe $ unpack text of
    Just a  -> Just $ Times a
    Nothing -> Nothing

mean :: Record -> Float
mean record =
  (fromIntegral . getTimes . conversion) record / (fromIntegral . getTimes . session) record

standardDeviation :: Record -> Float
standardDeviation record =
  sqrt $ mean record * (1 - mean record) / (fromIntegral . getTimes . session) record

variance :: Record -> Float
variance record = standardDeviation record ^ 2

probabilityToBeast :: Record -> Record -> Probability
probabilityToBeast record1 record2 =
  Probability $
  1 / 2 *
  (1 - erf (-(mean record1 - mean record2) / sqrt (2 * (variance record1 + variance record2))))
