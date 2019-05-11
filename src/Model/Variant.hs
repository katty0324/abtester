{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Model.Variant
  ( Variant(..)
  , getVariant
  ) where

import           Import
import           Model.Times
import           Text.Read   (readMaybe)

data Variant =
  Variant
    { visitors    :: Times
    , conversions :: Times
    }

getVariant :: [(Text, Text)] -> Int -> Maybe Variant
getVariant parameters index = do
  visitors <- lookupValue parameters "visitors" index >>= parseNumber
  conversions <- lookupValue parameters "conversions" index >>= parseNumber
  Just $ Variant visitors conversions

lookupValue :: [(Text, Text)] -> Text -> Int -> Maybe Text
lookupValue parameters key index = lookup (getKey key index) parameters

getKey :: Text -> Int -> Text
getKey key index =
  "variants[" `mappend` (pack $ show index) `mappend` "][" `mappend` key `mappend` "]"

parseNumber :: Text -> Maybe Times
parseNumber text =
  case readMaybe $ unpack text of
    Just a  -> Just $ Times a
    Nothing -> Nothing
