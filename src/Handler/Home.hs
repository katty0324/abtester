{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Home where

import           Handler.Parser
import           Import
import           Model.ConversionRate
import           Model.Point
import           Model.Probability
import           Model.Times
import           Model.Variant

mkMessage "App" "messages" "en"

getHomeR :: Handler Html
getHomeR = do
  parameters <- reqGetParams <$> getRequest
  let originalMaybeVariant = Variant `fmap` extractRecord parameters 0
      pattern1MaybeVariant = Variant `fmap` extractRecord parameters 1
      pattern2MaybeVariant = Variant `fmap` extractRecord parameters 2
      probabilityToBeastOriginal variant = do
        originalVariant <- originalMaybeVariant
        Just $ Model.Variant.probabilityToBeast originalVariant variant
      beastOriginal maybeVariant =
        case maybeVariant >>= probabilityToBeastOriginal of
          Just probability -> probability > 0.99
          Nothing          -> False
  defaultLayout $ do
    setTitle "AB Tester"
    $(widgetFile "homepage")
