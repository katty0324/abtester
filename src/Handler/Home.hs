{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Home where

import           Handler.Helper
import           Handler.Parser
import           Import
import           Model.ConversionRate
import           Model.Probability
import           Model.Times
import           Model.Variant

mkMessage "App" "messages" "en"

getHomeR :: Handler Html
getHomeR = do
  parameters <- reqGetParams <$> getRequest
  let originalMaybeVariant = Variant <$> extractRecord parameters 0
      pattern1MaybeVariant = Variant <$> extractRecord parameters 1
      probabilityToBeastOriginal :: Variant -> Maybe Probability
      probabilityToBeastOriginal maybeVariant = do
        originalVariant <- originalMaybeVariant
        variant <- maybeVariant
        Just $ Model.Variant.probabilityToBeast originalVariant variant
  defaultLayout $ do
    setTitle "AB Tester"
    $(widgetFile "homepage")
