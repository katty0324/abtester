{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Home where

import           Handler.Helper
import           Import
import           Model.ConversionRate
import           Model.Record
import           Model.Times
import           Model.Variant

mkMessage "App" "messages" "en"

getHomeR :: Handler Html
getHomeR = do
  parameters <- reqGetParams <$> getRequest
  let variant0 = fmap variant $ extractRecord parameters 0
      variant1 = fmap variant $ extractRecord parameters 1
  defaultLayout $ do
    setTitle "AB Tester"
    $(widgetFile "homepage")
