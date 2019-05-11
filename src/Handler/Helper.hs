{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module Handler.Helper
  ( showMaybe
  ) where

import           Import

showMaybe :: (a -> Text) -> Maybe a -> Text -> Text
showMaybe f maybeValue fallbackText =
  case maybeValue of
    Just value -> f value
    Nothing    -> fallbackText
