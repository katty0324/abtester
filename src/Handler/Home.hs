{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Home where

import           Import
import           Model.ConversionRate
import           Model.Variant

getHomeR :: Handler Html
getHomeR = do
  parameters <- reqGetParams <$> getRequest
  let variant0 = getVariant parameters 0
      variant1 = getVariant parameters 1
      conversionRate0 = fmap calculateConversionRate variant0
      conversionRate1 = fmap calculateConversionRate variant1
  defaultLayout $ do
    setTitle "AB Tester"
    $(widgetFile "homepage")

showMaybe :: Maybe a -> (a -> Text) -> Text
showMaybe x f =
  case x of
    Just y  -> f y
    Nothing -> "-"

showMaybeFloat :: Maybe Float -> Text
showMaybeFloat maybeFloat = showMaybeText $ fmap (pack . show) maybeFloat

showMaybeText :: Maybe Text -> Text
showMaybeText maybeText =
  case maybeText of
    Just text -> text
    Nothing   -> ""
