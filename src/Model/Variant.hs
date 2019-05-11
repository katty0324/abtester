{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Variant
  ( Variant(..)
  , extractVariant
  ) where

import           Import
import           Model.Times
import           Text.Read   (readMaybe)

data Variant =
  Variant
    { total   :: Times
    , success :: Times
    }

extractVariant :: [(Text, Text)] -> Int -> Maybe Variant
extractVariant parameters index = do
  total <- lookupValue parameters "total" index >>= parseTimes
  success <- lookupValue parameters "success" index >>= parseTimes
  Just $ Variant total success

lookupValue :: [(Text, Text)] -> Text -> Int -> Maybe Text
lookupValue parameters key index = lookup (getKey key index) parameters

getKey :: Text -> Int -> Text
getKey key index =
  "variants[" `mappend` (pack $ show index) `mappend` "][" `mappend` key `mappend` "]"

parseTimes :: Text -> Maybe Times
parseTimes text =
  case readMaybe $ unpack text of
    Just a  -> Just $ Times a
    Nothing -> Nothing
