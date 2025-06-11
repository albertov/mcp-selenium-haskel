{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
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
    GetConsoleLogsParams (..),
    GetAvailableLogTypesParams (..),
    InjectConsoleLoggerParams (..),
    GetInjectedConsoleLogsParams (..),
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
    handleGetConsoleLogs,
    handleGetAvailableLogTypes,
    handleInjectConsoleLogger,
    handleGetInjectedConsoleLogs,
  )
where

import Control.Concurrent.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)
import Control.Exception (SomeException, catch)
import Data.Aeson (FromJSON, ToJSON, encode, object, parseJSON, toJSON, (.=))
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import MCP.Selenium.Utils (debugLog)
import MCP.Selenium.WebDriver
import Network.MCP.Types (CallToolResult (..), ToolContent (..), ToolContentType (..))

-- | Tool parameter types
data StartBrowserParams = StartBrowserParams
  { browser :: Browser,
    options :: Maybe BrowserOptions,
    enableLogging :: Maybe Bool
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype NavigateParams = NavigateParams
  { url :: T.Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data FindElementParams = FindElementParams
  { by :: Maybe T.Text,
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

newtype PressKeyParams = PressKeyParams
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
  deriving (Eq, Show, Generic, ToJSON)

instance FromJSON TakeScreenshotParams where
  parseJSON _ = pure TakeScreenshotParams

data CloseSessionParams = CloseSessionParams
  deriving (Eq, Show, Generic, ToJSON)

instance FromJSON CloseSessionParams where
  parseJSON _ = pure CloseSessionParams

-- Console logging parameter types
data GetConsoleLogsParams = GetConsoleLogsParams
  { logLevel :: Maybe T.Text,
    maxEntries :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data GetAvailableLogTypesParams = GetAvailableLogTypesParams
  deriving (Eq, Show, Generic, ToJSON)

instance FromJSON GetAvailableLogTypesParams where
  parseJSON _ = pure GetAvailableLogTypesParams

newtype InjectConsoleLoggerParams = InjectConsoleLoggerParams
  { timeout :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype GetInjectedConsoleLogsParams = GetInjectedConsoleLogsParams
  { clear :: Maybe Bool
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Selenium tools container
newtype SeleniumTools = SeleniumTools
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
  CallToolResult [ToolContent TextualContent (Just msg)] False

-- | Create error result
errorResult :: T.Text -> CallToolResult
errorResult msg =
  CallToolResult [ToolContent TextualContent (Just msg)] True

-- | Handle start_browser tool
handleStartBrowser :: SeleniumTools -> StartBrowserParams -> IO CallToolResult
handleStartBrowser tools (StartBrowserParams browserVal optionsVal enableLoggingVal) = do
  debugLog "HANDLER: start_browser called"
  let loggingEnabled = fromMaybe False enableLoggingVal
      finalOpts = case optionsVal of
        Nothing -> BrowserOptions Nothing Nothing (Just loggingEnabled)
        Just opts -> opts {MCP.Selenium.WebDriver.enableLogging = Just loggingEnabled}
  catch
    ( do
        readTVarIO (sessionVar tools) >>= \case
          Just oldSession -> closeSeleniumSession oldSession
          Nothing -> pure ()
        session <- initializeSession browserVal finalOpts
        atomically $ writeTVar (sessionVar tools) (Just session)
        let message =
              if loggingEnabled
                then "Browser " <> T.pack (show browserVal) <> " started successfully with logging enabled"
                else "Browser " <> T.pack (show browserVal) <> " started successfully"
        return $ successResult message
    )
    ( \e -> do
        return $ errorResult $ "Failed to start browser: " <> T.pack (show (e :: SomeException))
    )

-- | Handle navigate tool
handleNavigate :: SeleniumTools -> NavigateParams -> IO CallToolResult
handleNavigate tools params = do
  debugLog "HANDLER: navigate called"
  sessionMaybe <- readTVarIO (sessionVar tools)
  case sessionMaybe of
    Nothing -> do
      debugLog "HANDLER: No session in navigate"
      return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            navigateToUrl session (url params)
            return $ successResult $ "Navigated to " <> url params
        )
        (\e -> return $ errorResult $ "Navigation failed: " <> T.pack (show (e :: SomeException)))

-- | Handle find_element tool
handleFindElement :: SeleniumTools -> FindElementParams -> IO CallToolResult
handleFindElement tools (FindElementParams byVal valueVal timeoutVal) = do
  debugLog "HANDLER: find_element called"
  sessionMaybe <- readTVarIO (sessionVar tools)
  -- Debug logging
  case sessionMaybe of
    Nothing -> do
      debugLog "HANDLER: No session in find_element"
      return $ errorResult "No active browser session"
    Just session -> do
      catch
        ( do
            let byStrategy = fromMaybe "id" byVal -- default to "id" if not provided
                locator = parseLocatorStrategy byStrategy valueVal
                timeoutMs = fromMaybe 10000 timeoutVal
            element <- findElementByLocator session locator timeoutMs
            -- Return element information with proper JSON encoding
            let elementIdText = T.pack (show element)
                responseJson = object [("elementId", toJSON elementIdText), ("found", toJSON True)]
                responseText = TE.decodeUtf8 $ BSL.toStrict $ encode responseJson
            return $ CallToolResult [ToolContent TextualContent (Just responseText)] False
        )
        ( \e -> do
            return $ errorResult $ "Element not found: " <> T.pack (show (e :: SomeException))
        )

-- | Handle click_element tool
handleClickElement :: SeleniumTools -> ClickElementParams -> IO CallToolResult
handleClickElement tools (ClickElementParams byVal valueVal timeoutVal) = do
  sessionMaybe <- readTVarIO (sessionVar tools)
  case sessionMaybe of
    Nothing -> return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            let locator = parseLocatorStrategy byVal valueVal
                timeoutMs = fromMaybe 10000 timeoutVal
            clickElement session locator timeoutMs
            return $ successResult $ "Clicked element with " <> byVal <> "='" <> valueVal <> "'"
        )
        (\e -> return $ errorResult $ "Click failed: " <> T.pack (show (e :: SomeException)))

-- | Handle send_keys tool
handleSendKeys :: SeleniumTools -> SendKeysParams -> IO CallToolResult
handleSendKeys tools (SendKeysParams byVal valueVal textVal timeoutVal) = do
  sessionMaybe <- readTVarIO (sessionVar tools)
  case sessionMaybe of
    Nothing -> return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            let locator = parseLocatorStrategy byVal valueVal
                timeoutMs = fromMaybe 10000 timeoutVal
            sendKeysToElement session locator textVal timeoutMs
            return $ successResult $ "Sent keys to element with " <> byVal <> "='" <> valueVal <> "'"
        )
        (\e -> return $ errorResult $ "Send keys failed: " <> T.pack (show (e :: SomeException)))

-- | Handle get_element_text tool
handleGetElementText :: SeleniumTools -> GetElementTextParams -> IO CallToolResult
handleGetElementText tools (GetElementTextParams byVal valueVal timeoutVal) = do
  sessionMaybe <- readTVarIO (sessionVar tools)
  case sessionMaybe of
    Nothing -> return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            let locator = parseLocatorStrategy byVal valueVal
                timeoutMs = fromMaybe 10000 timeoutVal
            elementText <- getElementText session locator timeoutMs
            return $ successResult elementText
        )
        (\e -> return $ errorResult $ "Get text failed: " <> T.pack (show (e :: SomeException)))

-- | Handle hover tool
handleHover :: SeleniumTools -> HoverParams -> IO CallToolResult
handleHover tools (HoverParams byVal valueVal timeoutVal) = do
  sessionMaybe <- readTVarIO (sessionVar tools)
  case sessionMaybe of
    Nothing -> return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            let locator = parseLocatorStrategy byVal valueVal
                timeoutMs = fromMaybe 10000 timeoutVal
            hoverElement session locator timeoutMs
            return $ successResult $ "Hovered over element with " <> byVal <> "='" <> valueVal <> "'"
        )
        (\e -> return $ errorResult $ "Hover failed: " <> T.pack (show (e :: SomeException)))

-- | Handle drag_and_drop tool
handleDragAndDrop :: SeleniumTools -> DragAndDropParams -> IO CallToolResult
handleDragAndDrop tools (DragAndDropParams byVal valueVal targetByVal targetValueVal timeoutVal) = do
  sessionMaybe <- readTVarIO (sessionVar tools)
  case sessionMaybe of
    Nothing -> return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            let sourceLocator = parseLocatorStrategy byVal valueVal
                targetLocator = parseLocatorStrategy targetByVal targetValueVal
                timeoutMs = fromMaybe 10000 timeoutVal
            dragAndDropElements session sourceLocator targetLocator timeoutMs
            return $
              successResult $
                "Dragged element from "
                  <> byVal
                  <> "='"
                  <> valueVal
                  <> "' to "
                  <> targetByVal
                  <> "='"
                  <> targetValueVal
                  <> "'"
        )
        (\e -> return $ errorResult $ "Drag and drop failed: " <> T.pack (show (e :: SomeException)))

-- | Handle double_click tool
handleDoubleClick :: SeleniumTools -> DoubleClickParams -> IO CallToolResult
handleDoubleClick tools (DoubleClickParams byVal valueVal timeoutVal) = do
  sessionMaybe <- readTVarIO (sessionVar tools)
  case sessionMaybe of
    Nothing -> return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            let locator = parseLocatorStrategy byVal valueVal
                timeoutMs = fromMaybe 10000 timeoutVal
            doubleClickElement session locator timeoutMs
            return $ successResult $ "Double-clicked element with " <> byVal <> "='" <> valueVal <> "'"
        )
        (\e -> return $ errorResult $ "Double click failed: " <> T.pack (show (e :: SomeException)))

-- | Handle right_click tool
handleRightClick :: SeleniumTools -> RightClickParams -> IO CallToolResult
handleRightClick tools (RightClickParams byVal valueVal timeoutVal) = do
  sessionMaybe <- readTVarIO (sessionVar tools)
  case sessionMaybe of
    Nothing -> return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            let locator = parseLocatorStrategy byVal valueVal
                timeoutMs = fromMaybe 10000 timeoutVal
            rightClickElement session locator timeoutMs
            return $ successResult $ "Right-clicked element with " <> byVal <> "='" <> valueVal <> "'"
        )
        (\e -> return $ errorResult $ "Right click failed: " <> T.pack (show (e :: SomeException)))

-- | Handle press_key tool
handlePressKey :: SeleniumTools -> PressKeyParams -> IO CallToolResult
handlePressKey tools (PressKeyParams keyVal) = do
  sessionMaybe <- readTVarIO (sessionVar tools)
  case sessionMaybe of
    Nothing -> return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            pressKey session keyVal
            return $ successResult $ "Pressed key: " <> keyVal
        )
        (\e -> return $ errorResult $ "Press key failed: " <> T.pack (show (e :: SomeException)))

-- | Handle upload_file tool
handleUploadFile :: SeleniumTools -> UploadFileParams -> IO CallToolResult
handleUploadFile tools (UploadFileParams byVal valueVal filePathVal timeoutVal) = do
  sessionMaybe <- readTVarIO (sessionVar tools)
  case sessionMaybe of
    Nothing -> return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            let locator = parseLocatorStrategy byVal valueVal
                timeoutMs = fromMaybe 10000 timeoutVal
            uploadFileToElement session locator filePathVal timeoutMs
            return $
              successResult $
                "Uploaded file "
                  <> filePathVal
                  <> " to element with "
                  <> byVal
                  <> "='"
                  <> valueVal
                  <> "'"
        )
        (\e -> return $ errorResult $ "Upload file failed: " <> T.pack (show (e :: SomeException)))

-- | Handle take_screenshot tool
handleTakeScreenshot :: SeleniumTools -> TakeScreenshotParams -> IO CallToolResult
handleTakeScreenshot tools TakeScreenshotParams = do
  sessionMaybe <- readTVarIO (sessionVar tools)
  case sessionMaybe of
    Nothing -> return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            result <- takeScreenshot session Nothing
            return $ successResult $ "Screenshot captured: " <> result
        )
        (\e -> return $ errorResult $ "Screenshot failed: " <> T.pack (show (e :: SomeException)))

-- | Handle close_session tool
handleCloseSession :: SeleniumTools -> CloseSessionParams -> IO CallToolResult
handleCloseSession tools _ = do
  sessionMaybe <- readTVarIO (sessionVar tools)
  case sessionMaybe of
    Nothing -> return $ successResult "No active session to close"
    Just session ->
      catch
        ( do
            closeSeleniumSession session
            atomically $ writeTVar (sessionVar tools) Nothing
            return $ successResult "Browser session closed successfully"
        )
        (\e -> return $ errorResult $ "Close session failed: " <> T.pack (show (e :: SomeException)))

-- | Handle get_console_logs tool
handleGetConsoleLogs :: SeleniumTools -> GetConsoleLogsParams -> IO CallToolResult
handleGetConsoleLogs tools (GetConsoleLogsParams logLevelVal maxEntriesVal) = do
  debugLog "HANDLER: get_console_logs called"
  sessionMaybe <- readTVarIO (sessionVar tools)
  case sessionMaybe of
    Nothing -> do
      debugLog "HANDLER: No session in get_console_logs"
      return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            debugLog "HANDLER: Getting logs from WebDriver"
            logs <- getConsoleLogs session logLevelVal maxEntriesVal
            debugLog ("HANDLER: Got " ++ show (length logs) ++ " log entries")
            let formattedLogs =
                  map
                    ( \(LogEntry timestamp level message) ->
                        object
                          [ "level" .= show level,
                            "message" .= message,
                            "timestamp" .= timestamp
                          ]
                    )
                    logs
                responseText = TE.decodeUtf8 $ BSL.toStrict $ encode $ object ["logs" .= formattedLogs]
            debugLog ("HANDLER: Returning response: " ++ T.unpack responseText)
            return $ successResult responseText
        )
        ( \e -> do
            debugLog ("HANDLER: Exception in get_console_logs: " ++ show (e :: SomeException))
            return $ errorResult $ "Get console logs failed: " <> T.pack (show (e :: SomeException))
        )

-- | Handle get_available_log_types tool
handleGetAvailableLogTypes :: SeleniumTools -> GetAvailableLogTypesParams -> IO CallToolResult
handleGetAvailableLogTypes tools _ = do
  debugLog "HANDLER: get_available_log_types called"
  sessionMaybe <- readTVarIO (sessionVar tools)
  case sessionMaybe of
    Nothing -> do
      debugLog "HANDLER: No session in get_available_log_types"
      return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            debugLog "HANDLER: Getting log types from WebDriver"
            logTypes <- getAvailableLogTypes session
            debugLog ("HANDLER: Got log types: " ++ show logTypes)
            let responseText = TE.decodeUtf8 $ BSL.toStrict $ encode $ object ["logTypes" .= logTypes]
            debugLog ("HANDLER: Returning log types response: " ++ T.unpack responseText)
            return $ successResult responseText
        )
        ( \e -> do
            debugLog ("HANDLER: Exception in get_available_log_types: " ++ show (e :: SomeException))
            return $ errorResult $ "Get available log types failed: " <> T.pack (show (e :: SomeException))
        )

-- | Handle inject_console_logger tool
handleInjectConsoleLogger :: SeleniumTools -> InjectConsoleLoggerParams -> IO CallToolResult
handleInjectConsoleLogger tools (InjectConsoleLoggerParams timeoutVal) = do
  debugLog "HANDLER: inject_console_logger called"
  sessionMaybe <- readTVarIO (sessionVar tools)
  case sessionMaybe of
    Nothing -> do
      debugLog "HANDLER: No session in inject_console_logger"
      return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            debugLog "HANDLER: Injecting console logger"
            let timeoutMs = fromMaybe 60000 timeoutVal -- Default to 60 seconds
            injectConsoleLogger session timeoutMs
            debugLog "HANDLER: Console logger injected successfully"
            return $ successResult "Console logger injected successfully"
        )
        ( \e -> do
            debugLog ("HANDLER: Exception in inject_console_logger: " ++ show (e :: SomeException))
            return $ errorResult $ "Inject console logger failed: " <> T.pack (show (e :: SomeException))
        )

-- | Handle get_injected_console_logs tool
handleGetInjectedConsoleLogs :: SeleniumTools -> GetInjectedConsoleLogsParams -> IO CallToolResult
handleGetInjectedConsoleLogs tools (GetInjectedConsoleLogsParams clearVal) = do
  debugLog "HANDLER: get_injected_console_logs called"
  sessionMaybe <- readTVarIO (sessionVar tools)
  case sessionMaybe of
    Nothing -> do
      debugLog "HANDLER: No session in get_injected_console_logs"
      return $ errorResult "No active browser session"
    Just session ->
      catch
        ( do
            let shouldClear = fromMaybe False clearVal
            debugLog ("HANDLER: Getting injected logs, clear=" ++ show shouldClear)
            logsJson <- getInjectedConsoleLogs session shouldClear
            debugLog ("HANDLER: Got injected logs: " ++ T.unpack logsJson)
            return $ successResult logsJson
        )
        ( \e -> do
            debugLog ("HANDLER: Exception in get_injected_console_logs: " ++ show (e :: SomeException))
            return $ errorResult $ "Get injected console logs failed: " <> T.pack (show (e :: SomeException))
        )

-- | Create selenium tools instance
createSeleniumTools :: IO SeleniumTools
createSeleniumTools = do
  sessionState <- newTVarIO Nothing
  debugLog "Creating SeleniumTools instance"
  return $ SeleniumTools sessionState
