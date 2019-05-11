{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Home where

import           Handler.Helper
import           Import
import           Model.ConversionRate
import           Model.Times
import           Model.Variant

getHomeR :: Handler Html
getHomeR = do
  parameters <- reqGetParams <$> getRequest
  let variant0 = extractVariant parameters 0
      variant1 = extractVariant parameters 1
      conversionRate0 = fmap calculateConversionRate variant0
      conversionRate1 = fmap calculateConversionRate variant1
  defaultLayout $ do
    setTitle "AB Tester"
    $(widgetFile "homepage")
