{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | MCP Tools for Selenium browser automation
module MCP.Selenium.Tools
  ( SeleniumTools (..),
    StartBrowserParams (..),
    NavigateParams (..),
    FindElementParams (..),
    ClickElementParams (..),
    SendKeysParams (..),
    GetElementTextParams (..),
    HoverParams (..),
    DragAndDropParams (..),
    DoubleClickParams (..),
    RightClickParams (..),
    PressKeyParams (..),
    UploadFileParams (..),
    TakeScreenshotParams (..),
    CloseSessionParams (..),
    createSeleniumTools,
    handleStartBrowser,
    handleNavigate,
    handleFindElement,
    handleClickElement,
    handleSendKeys,
    handleGetElementText,
    handleHover,
    handleDragAndDrop,
    handleDoubleClick,
    handleRightClick,
    handlePressKey,
    handleUploadFile,
    handleTakeScreenshot,
    handleCloseSession,
  )
where

import Control.Concurrent.STM (TVar, atomically, newTVar, readTVar, writeTVar)
import Control.Exception (SomeException, catch)
import Data.Aeson (FromJSON, ToJSON, Value (..), object, (.=))
import qualified Data.Text as T
import GHC.Generics (Generic)
import MCP.Selenium.WebDriver
import Network.MCP.Server.Types (CallToolRequest (..), CallToolResult (..), Tool (..), ToolCallHandler)

-- | Tool parameter types
data StartBrowserParams = StartBrowserParams
  { browser :: Browser,
    options :: Maybe BrowserOptions
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data NavigateParams = NavigateParams
  { url :: T.Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data FindElementParams = FindElementParams
  { by :: T.Text,
    value :: T.Text,
    timeout :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ClickElementParams = ClickElementParams
  { by :: T.Text,
    value :: T.Text,
    timeout :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data SendKeysParams = SendKeysParams
  { by :: T.Text,
    value :: T.Text,
    text :: T.Text,
    timeout :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data GetElementTextParams = GetElementTextParams
  { by :: T.Text,
    value :: T.Text,
    timeout :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data HoverParams = HoverParams
  { by :: T.Text,
    value :: T.Text,
    timeout :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data DragAndDropParams = DragAndDropParams
  { by :: T.Text,
    value :: T.Text,
    targetBy :: T.Text,
    targetValue :: T.Text,
    timeout :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data DoubleClickParams = DoubleClickParams
  { by :: T.Text,
    value :: T.Text,
    timeout :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data RightClickParams = RightClickParams
  { by :: T.Text,
    value :: T.Text,
    timeout :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data PressKeyParams = PressKeyParams
  { key :: T.Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data UploadFileParams = UploadFileParams
  { by :: T.Text,
    value :: T.Text,
    filePath :: T.Text,
    timeout :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data TakeScreenshotParams = TakeScreenshotParams
  { outputPath :: Maybe T.Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data CloseSessionParams = CloseSessionParams
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Selenium tools container
data SeleniumTools = SeleniumTools
  { sessionVar :: TVar (Maybe SeleniumSession)
  }

-- | Parse locator strategy from text
parseLocatorStrategy :: T.Text -> T.Text -> LocatorStrategy
parseLocatorStrategy "id" val = ById val
parseLocatorStrategy "css" val = ByCss val
parseLocatorStrategy "xpath" val = ByXPath val
parseLocatorStrategy "name" val = ByName val
parseLocatorStrategy "tag" val = ByTag val
parseLocatorStrategy "class" val = ByClass val
parseLocatorStrategy _ val = ById val -- Default to ID

-- | Create success result
successResult :: T.Text -> CallToolResult
successResult msg =
  CallToolResult
    { content = [object ["type" .= ("text" :: T.Text), "text" .= msg]],
      isError = Just False
    }

-- | Create error result
errorResult :: T.Text -> CallToolResult
errorResult msg =
  CallToolResult
    { content = [object ["type" .= ("text" :: T.Text), "text" .= msg]],
      isError = Just True
    }

-- | Handle start_browser tool
handleStartBrowser :: SeleniumTools -> StartBrowserParams -> IO CallToolResult
handleStartBrowser tools params = do
  let opts = case options params of
        Nothing -> BrowserOptions Nothing Nothing
        Just o -> o
  result <-
    catch
      ( do
          session <- initializeSession (browser params) opts
          atomically $ writeTVar (sessionVar tools) (Just session)
          return $ successResult $ "Browser " <> T.pack (show $ browser params) <> " started successfully"
      )
      (\e -> return $ errorResult $ "Failed to start browser: " <> T.pack (show (e :: SomeException)))
  return result

-- | Handle navigate tool
handleNavigate :: SeleniumTools -> NavigateParams -> IO CallToolResult
handleNavigate tools params = do
  sessionMaybe <- atomically $ readTVar (sessionVar tools)
  case sessionMaybe of
    Nothing -> return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            navigateToUrl session (url params)
            return $ successResult $ "Navigated to " <> url params
        )
        (\e -> return $ errorResult $ "Navigation failed: " <> T.pack (show (e :: SomeException)))

-- | Handle find_element tool
handleFindElement :: SeleniumTools -> FindElementParams -> IO CallToolResult
handleFindElement tools params = do
  sessionMaybe <- atomically $ readTVar (sessionVar tools)
  case sessionMaybe of
    Nothing -> return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            let locator = parseLocatorStrategy (by params) (value params)
                timeoutMs = maybe 10000 id (timeout params)
            _ <- findElementByLocator session locator timeoutMs
            return $ successResult $ "Element found with " <> by params <> "='" <> value params <> "'"
        )
        (\e -> return $ errorResult $ "Element not found: " <> T.pack (show (e :: SomeException)))

-- | Handle click_element tool
handleClickElement :: SeleniumTools -> ClickElementParams -> IO CallToolResult
handleClickElement tools params = do
  sessionMaybe <- atomically $ readTVar (sessionVar tools)
  case sessionMaybe of
    Nothing -> return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            let locator = parseLocatorStrategy (by params) (value params)
                timeoutMs = maybe 10000 id (timeout params)
            clickElement session locator timeoutMs
            return $ successResult $ "Clicked element with " <> by params <> "='" <> value params <> "'"
        )
        (\e -> return $ errorResult $ "Click failed: " <> T.pack (show (e :: SomeException)))

-- | Handle send_keys tool
handleSendKeys :: SeleniumTools -> SendKeysParams -> IO CallToolResult
handleSendKeys tools params = do
  sessionMaybe <- atomically $ readTVar (sessionVar tools)
  case sessionMaybe of
    Nothing -> return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            let locator = parseLocatorStrategy (by params) (value params)
                timeoutMs = maybe 10000 id (timeout params)
            sendKeysToElement session locator (text params) timeoutMs
            return $ successResult $ "Sent keys to element with " <> by params <> "='" <> value params <> "'"
        )
        (\e -> return $ errorResult $ "Send keys failed: " <> T.pack (show (e :: SomeException)))

-- | Handle get_element_text tool
handleGetElementText :: SeleniumTools -> GetElementTextParams -> IO CallToolResult
handleGetElementText tools params = do
  sessionMaybe <- atomically $ readTVar (sessionVar tools)
  case sessionMaybe of
    Nothing -> return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            let locator = parseLocatorStrategy (by params) (value params)
                timeoutMs = maybe 10000 id (timeout params)
            elementText <- getElementText session locator timeoutMs
            return $ successResult $ "Element text: " <> elementText
        )
        (\e -> return $ errorResult $ "Get text failed: " <> T.pack (show (e :: SomeException)))

-- | Handle hover tool
handleHover :: SeleniumTools -> HoverParams -> IO CallToolResult
handleHover tools params = do
  sessionMaybe <- atomically $ readTVar (sessionVar tools)
  case sessionMaybe of
    Nothing -> return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            let locator = parseLocatorStrategy (by params) (value params)
                timeoutMs = maybe 10000 id (timeout params)
            hoverElement session locator timeoutMs
            return $ successResult $ "Hovered over element with " <> by params <> "='" <> value params <> "'"
        )
        (\e -> return $ errorResult $ "Hover failed: " <> T.pack (show (e :: SomeException)))

-- | Handle drag_and_drop tool
handleDragAndDrop :: SeleniumTools -> DragAndDropParams -> IO CallToolResult
handleDragAndDrop tools params = do
  sessionMaybe <- atomically $ readTVar (sessionVar tools)
  case sessionMaybe of
    Nothing -> return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            let sourceLocator = parseLocatorStrategy (by params) (value params)
                targetLocator = parseLocatorStrategy (targetBy params) (targetValue params)
                timeoutMs = maybe 10000 id (timeout params)
            dragAndDropElements session sourceLocator targetLocator timeoutMs
            return $
              successResult $
                "Dragged element from "
                  <> by params
                  <> "='"
                  <> value params
                  <> "' to "
                  <> targetBy params
                  <> "='"
                  <> targetValue params
                  <> "'"
        )
        (\e -> return $ errorResult $ "Drag and drop failed: " <> T.pack (show (e :: SomeException)))

-- | Handle double_click tool
handleDoubleClick :: SeleniumTools -> DoubleClickParams -> IO CallToolResult
handleDoubleClick tools params = do
  sessionMaybe <- atomically $ readTVar (sessionVar tools)
  case sessionMaybe of
    Nothing -> return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            let locator = parseLocatorStrategy (by params) (value params)
                timeoutMs = maybe 10000 id (timeout params)
            doubleClickElement session locator timeoutMs
            return $ successResult $ "Double-clicked element with " <> by params <> "='" <> value params <> "'"
        )
        (\e -> return $ errorResult $ "Double click failed: " <> T.pack (show (e :: SomeException)))

-- | Handle right_click tool
handleRightClick :: SeleniumTools -> RightClickParams -> IO CallToolResult
handleRightClick tools params = do
  sessionMaybe <- atomically $ readTVar (sessionVar tools)
  case sessionMaybe of
    Nothing -> return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            let locator = parseLocatorStrategy (by params) (value params)
                timeoutMs = maybe 10000 id (timeout params)
            rightClickElement session locator timeoutMs
            return $ successResult $ "Right-clicked element with " <> by params <> "='" <> value params <> "'"
        )
        (\e -> return $ errorResult $ "Right click failed: " <> T.pack (show (e :: SomeException)))

-- | Handle press_key tool
handlePressKey :: SeleniumTools -> PressKeyParams -> IO CallToolResult
handlePressKey tools params = do
  sessionMaybe <- atomically $ readTVar (sessionVar tools)
  case sessionMaybe of
    Nothing -> return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            pressKey session (key params)
            return $ successResult $ "Pressed key: " <> key params
        )
        (\e -> return $ errorResult $ "Press key failed: " <> T.pack (show (e :: SomeException)))

-- | Handle upload_file tool
handleUploadFile :: SeleniumTools -> UploadFileParams -> IO CallToolResult
handleUploadFile tools params = do
  sessionMaybe <- atomically $ readTVar (sessionVar tools)
  case sessionMaybe of
    Nothing -> return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            let locator = parseLocatorStrategy (by params) (value params)
                timeoutMs = maybe 10000 id (timeout params)
            uploadFileToElement session locator (filePath params) timeoutMs
            return $
              successResult $
                "Uploaded file "
                  <> filePath params
                  <> " to element with "
                  <> by params
                  <> "='"
                  <> value params
                  <> "'"
        )
        (\e -> return $ errorResult $ "Upload file failed: " <> T.pack (show (e :: SomeException)))

-- | Handle take_screenshot tool
handleTakeScreenshot :: SeleniumTools -> TakeScreenshotParams -> IO CallToolResult
handleTakeScreenshot tools params = do
  sessionMaybe <- atomically $ readTVar (sessionVar tools)
  case sessionMaybe of
    Nothing -> return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            result <- takeScreenshot session (outputPath params)
            return $ successResult $ "Screenshot captured: " <> result
        )
        (\e -> return $ errorResult $ "Screenshot failed: " <> T.pack (show (e :: SomeException)))

-- | Handle close_session tool
handleCloseSession :: SeleniumTools -> CloseSessionParams -> IO CallToolResult
handleCloseSession tools _ = do
  sessionMaybe <- atomically $ readTVar (sessionVar tools)
  case sessionMaybe of
    Nothing -> return $ successResult "No active session to close"
    Just session ->
      catch
        ( do
            closeSession session
            atomically $ writeTVar (sessionVar tools) Nothing
            return $ successResult "Browser session closed successfully"
        )
        (\e -> return $ errorResult $ "Close session failed: " <> T.pack (show (e :: SomeException)))

-- | Create selenium tools instance
createSeleniumTools :: IO SeleniumTools
createSeleniumTools = do
  sessionVar <- atomically $ newTVar Nothing
  return $ SeleniumTools sessionVar
