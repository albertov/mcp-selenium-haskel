{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MCP.Selenium.WebDriver
  ( SeleniumAction (..),
    seleniumAction,
    SeleniumError (..),
  )
where

import Control.Exception (Exception, SomeException, try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Object, Value (..), object, (.=))
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as H
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Test.WebDriver
import Test.WebDriver.Config

data SeleniumAction
  = StartBrowser (Maybe Text) (Maybe Bool)
  | NavigateTo Text
  | FindElement Text
  | ClickElement Text
  | TypeText Text Text
  | GetText Text
  | GetAttribute Text Text
  | TakeScreenshot
  | CloseBrowser
  deriving (Show, Generic)

data SeleniumError
  = WebDriverError String
  | ConfigError String
  | ElementNotFound String
  | TimeoutError String
  deriving (Show, Generic)

instance Exception SeleniumError

seleniumAction :: SeleniumAction -> IO (Either SeleniumError Text)
seleniumAction action = do
  result <- try $ runWebDriverSession action
  case result of
    Left (e :: SomeException) -> return $ Left (WebDriverError $ show e)
    Right r -> return r

runWebDriverSession :: SeleniumAction -> IO (Either SeleniumError Text)
runWebDriverSession action = do
  let wdConfig =
        defaultConfig
          { wdHost = "localhost",
            wdPort = 4444,
            wdBasePath = "/wd/hub",
            wdCapabilities = defaultCaps
          }

  case action of
    StartBrowser browserType headless -> do
      let finalConfig = wdConfig -- Just use default config for now
      result <- try $ runSession finalConfig $ do
        return "Session started successfully"

      case result of
        Left (e :: SomeException) -> return $ Left (WebDriverError $ show e)
        Right sessionId -> return $ Right sessionId
    NavigateTo url -> do
      result <- try $ runSession wdConfig $ do
        openPage $ T.unpack url
        return "Navigation successful"

      case result of
        Left (e :: SomeException) -> return $ Left (WebDriverError $ show e)
        Right _ -> return $ Right "Navigation successful"
    FindElement selector -> do
      result <- try $ runSession wdConfig $ do
        element <- findElem (ByCSS selector)
        return $ T.pack $ show element

      case result of
        Left (e :: SomeException) -> return $ Left (ElementNotFound $ "Element not found: " ++ T.unpack selector)
        Right elementId -> return $ Right elementId
    ClickElement selector -> do
      result <- try $ runSession wdConfig $ do
        element <- findElem (ByCSS selector)
        click element
        return "Click successful"

      case result of
        Left (e :: SomeException) -> return $ Left (ElementNotFound $ "Could not click element: " ++ T.unpack selector)
        Right _ -> return $ Right "Click successful"
    TypeText selector text -> do
      result <- try $ runSession wdConfig $ do
        element <- findElem (ByCSS selector)
        sendKeys text element
        return "Text input successful"

      case result of
        Left (e :: SomeException) -> return $ Left (ElementNotFound $ "Could not type into element: " ++ T.unpack selector)
        Right _ -> return $ Right "Text input successful"
    GetText selector -> do
      result <- try $ runSession wdConfig $ do
        element <- findElem (ByCSS selector)
        getText element

      case result of
        Left (e :: SomeException) -> return $ Left (ElementNotFound $ "Could not get text from element: " ++ T.unpack selector)
        Right text -> return $ Right text
    GetAttribute selector attrName -> do
      result <- try $ runSession wdConfig $ do
        element <- findElem (ByCSS selector)
        maybeValue <- attr element attrName
        case maybeValue of
          Just value -> return value
          Nothing -> return ""

      case result of
        Left (e :: SomeException) -> return $ Left (ElementNotFound $ "Could not get attribute from element: " ++ T.unpack selector)
        Right value -> return $ Right value
    TakeScreenshot -> do
      result <- try $ runSession wdConfig $ do
        TE.decodeUtf8 . Base64.encode . BL.toStrict <$> screenshot

      case result of
        Left (e :: SomeException) -> return $ Left (WebDriverError $ "Could not take screenshot: " ++ show e)
        Right screenshotData -> return $ Right screenshotData
    CloseBrowser -> do
      result <- try $ runSession wdConfig $ do
        closeSession
        return "Browser closed"

      case result of
        Left (e :: SomeException) -> return $ Left (WebDriverError $ show e)
        Right _ -> return $ Right "Browser closed"
