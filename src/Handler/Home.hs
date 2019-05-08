{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Home where

import           Import
import           Text.Read             (readMaybe)
import           Yesod.Form.Bootstrap3 (BootstrapFormLayout (..),
                                        renderBootstrap3)

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
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

parseNumber :: Text -> Maybe Float
parseNumber text = readMaybe $ unpack text

showPercent :: Maybe Float -> Text
showPercent maybeFloat =
  case maybeFloat of
    Just float -> (pack $ show $ (fromIntegral $ ceiling $ float * 10000) / 100) `mappend` "%"
    Nothing -> "-"

showMaybeFloat :: Maybe Float -> Text
showMaybeFloat maybeFloat = showMaybeText $ fmap (pack . show) maybeFloat

showMaybeText :: Maybe Text -> Text
showMaybeText maybeText =
  case maybeText of
    Just text -> text
    Nothing   -> ""

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

calculateConversionRate :: Variant -> Float
calculateConversionRate variant = conversions variant / visitors variant

data Variant =
  Variant
    { visitors    :: Float
    , conversions :: Float
    }
