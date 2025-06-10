{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module MCP.Selenium.Tools
  ( SeleniumTool (..),
    ToolInput (..),
    ToolResult (..),
    availableTools,
    executeTool,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.:), (.:?), (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import MCP.Selenium.WebDriver (SeleniumAction (..), seleniumAction)

data SeleniumTool
  = StartBrowserTool
  | NavigateToTool
  | FindElementTool
  | ClickElementTool
  | TypeTextTool
  | GetTextTool
  | GetAttributeTool
  | TakeScreenshotTool
  | CloseBrowserTool
  deriving (Show, Eq, Generic)

instance ToJSON SeleniumTool

instance FromJSON SeleniumTool

data ToolInput = ToolInput
  { inputUrl :: Maybe Text,
    inputSelector :: Maybe Text,
    inputText :: Maybe Text,
    inputAttribute :: Maybe Text,
    inputBrowser :: Maybe Text,
    inputHeadless :: Maybe Bool,
    inputWaitTime :: Maybe Int
  }
  deriving (Show, Generic)

instance FromJSON ToolInput where
  parseJSON = Aeson.withObject "ToolInput" $ \o ->
    ToolInput
      <$> o .:? "url"
      <*> o .:? "selector"
      <*> o .:? "text"
      <*> o .:? "attribute"
      <*> o .:? "browser"
      <*> o .:? "headless"
      <*> o .:? "wait_time"

data ToolResult = ToolResult
  { resultSuccess :: Bool,
    resultMessage :: Text,
    resultData :: Maybe Value
  }
  deriving (Show, Generic)

instance ToJSON ToolResult

availableTools :: [SeleniumTool]
availableTools =
  [ StartBrowserTool,
    NavigateToTool,
    FindElementTool,
    ClickElementTool,
    TypeTextTool,
    GetTextTool,
    GetAttributeTool,
    TakeScreenshotTool,
    CloseBrowserTool
  ]

executeTool :: SeleniumTool -> ToolInput -> IO ToolResult
executeTool tool input = do
  case tool of
    StartBrowserTool -> do
      result <- seleniumAction (StartBrowser (inputBrowser input) (inputHeadless input))
      return $ case result of
        Right sessionId -> ToolResult True "Browser started successfully" (Just $ object ["session_id" .= sessionId])
        Left err -> ToolResult False (T.pack $ show err) Nothing
    NavigateToTool -> do
      case inputUrl input of
        Nothing -> return $ ToolResult False "URL is required" Nothing
        Just url -> do
          result <- seleniumAction (NavigateTo url)
          return $ case result of
            Right _ -> ToolResult True ("Navigated to " <> url) Nothing
            Left err -> ToolResult False (T.pack $ show err) Nothing
    FindElementTool -> do
      case inputSelector input of
        Nothing -> return $ ToolResult False "Selector is required" Nothing
        Just selector -> do
          result <- seleniumAction (FindElement selector)
          return $ case result of
            Right elementId -> ToolResult True "Element found" (Just $ object ["element_id" .= elementId])
            Left err -> ToolResult False (T.pack $ show err) Nothing
    ClickElementTool -> do
      case inputSelector input of
        Nothing -> return $ ToolResult False "Selector is required" Nothing
        Just selector -> do
          result <- seleniumAction (ClickElement selector)
          return $ case result of
            Right _ -> ToolResult True ("Clicked element: " <> selector) Nothing
            Left err -> ToolResult False (T.pack $ show err) Nothing
    TypeTextTool -> do
      case (inputSelector input, inputText input) of
        (Just selector, Just text) -> do
          result <- seleniumAction (TypeText selector text)
          return $ case result of
            Right _ -> ToolResult True ("Typed text into: " <> selector) Nothing
            Left err -> ToolResult False (T.pack $ show err) Nothing
        _ -> return $ ToolResult False "Both selector and text are required" Nothing
    GetTextTool -> do
      case inputSelector input of
        Nothing -> return $ ToolResult False "Selector is required" Nothing
        Just selector -> do
          result <- seleniumAction (GetText selector)
          return $ case result of
            Right text -> ToolResult True "Text retrieved" (Just $ object ["text" .= text])
            Left err -> ToolResult False (T.pack $ show err) Nothing
    GetAttributeTool -> do
      case (inputSelector input, inputAttribute input) of
        (Just selector, Just attribute) -> do
          result <- seleniumAction (GetAttribute selector attribute)
          return $ case result of
            Right value -> ToolResult True "Attribute retrieved" (Just $ object ["value" .= value])
            Left err -> ToolResult False (T.pack $ show err) Nothing
        _ -> return $ ToolResult False "Both selector and attribute are required" Nothing
    TakeScreenshotTool -> do
      result <- seleniumAction TakeScreenshot
      return $ case result of
        Right screenshot -> ToolResult True "Screenshot taken" (Just $ object ["screenshot" .= screenshot])
        Left err -> ToolResult False (T.pack $ show err) Nothing
    CloseBrowserTool -> do
      result <- seleniumAction CloseBrowser
      return $ case result of
        Right _ -> ToolResult True "Browser closed" Nothing
        Left err -> ToolResult False (T.pack $ show err) Nothing
