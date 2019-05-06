{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import Text.Read (readMaybe)

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    visitos0Text <- lookupGetParam "variants[0][visitors]"
    conversions0Text <- lookupGetParam "variants[0][conversions]"
    visitos1Text <- lookupGetParam "variants[1][visitors]"
    conversions1Text <- lookupGetParam "variants[1][conversions]"
    let conversionRate0 = (/) <$> parseNumber conversions0Text <*> parseNumber visitos0Text
        conversionRate1 = (/) <$> parseNumber conversions1Text <*> parseNumber visitos1Text
    defaultLayout $ do
        setTitle "AB Tester"
        $(widgetFile "homepage")

parseNumber :: Maybe Text -> Maybe Float
parseNumber maybeText =
    case maybeText of
        Just text -> readMaybe $ unpack text
        Nothing -> Nothing

showPercent :: Maybe Float -> Text
showPercent maybeFloat =
    case maybeFloat of
        Just float -> (pack $ show $ (fromIntegral $ ceiling $ float * 10000) / 100) `mappend` "%"
        Nothing -> "-"

showMaybeText :: Maybe Text -> Text
showMaybeText maybeText =
    case maybeText of
        Just text -> text
        Nothing -> ""
