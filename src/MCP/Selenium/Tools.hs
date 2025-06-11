{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | MCP Tools for Selenium browser automation
module MCP.Selenium.Tools
  ( SeleniumTools (..),
    SessionId,
    SessionData (..),
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
    generateSessionId,
    lookupSession,
    insertSession,
    removeSession,
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

import Control.Concurrent.STM (TVar, atomically, modifyTVar, newTVarIO, readTVarIO)
import Control.Exception (SomeException, catch)
import Data.Aeson (FromJSON, ToJSON, encode, object, toJSON, (.=))
import qualified Data.ByteString.Lazy as BSL
import qualified Data.HashMap.Strict as HashMap
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4
import GHC.Generics (Generic)
import MCP.Selenium.Utils (debugLog)
import MCP.Selenium.WebDriver
import Network.MCP.Types (CallToolResult (..), ToolContent (..), ToolContentType (..))

-- | Session management types
type SessionId = UUID

data SessionData = SessionData
  { sessionKey :: SessionId,
    seleniumSession :: SeleniumSession
  }
  deriving (Generic)

instance Show SessionData where
  show (SessionData sessKey _) = "SessionData { sessionKey = " ++ show sessKey ++ " }"

instance Eq SessionData where
  (SessionData key1 _) == (SessionData key2 _) = key1 == key2

-- | Tool parameter types
data StartBrowserParams = StartBrowserParams
  { browser :: Browser,
    options :: Maybe BrowserOptions,
    enableLogging :: Maybe Bool
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data NavigateParams = NavigateParams
  { session_id :: SessionId,
    url :: T.Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data FindElementParams = FindElementParams
  { session_id :: SessionId,
    by :: Maybe T.Text,
    value :: T.Text,
    timeout :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data ClickElementParams = ClickElementParams
  { session_id :: SessionId,
    by :: T.Text,
    value :: T.Text,
    timeout :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data SendKeysParams = SendKeysParams
  { session_id :: SessionId,
    by :: T.Text,
    value :: T.Text,
    text :: T.Text,
    timeout :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data GetElementTextParams = GetElementTextParams
  { session_id :: SessionId,
    by :: T.Text,
    value :: T.Text,
    timeout :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data HoverParams = HoverParams
  { session_id :: SessionId,
    by :: T.Text,
    value :: T.Text,
    timeout :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data DragAndDropParams = DragAndDropParams
  { session_id :: SessionId,
    by :: T.Text,
    value :: T.Text,
    targetBy :: T.Text,
    targetValue :: T.Text,
    timeout :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data DoubleClickParams = DoubleClickParams
  { session_id :: SessionId,
    by :: T.Text,
    value :: T.Text,
    timeout :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data RightClickParams = RightClickParams
  { session_id :: SessionId,
    by :: T.Text,
    value :: T.Text,
    timeout :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data PressKeyParams = PressKeyParams
  { session_id :: SessionId,
    key :: T.Text
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data UploadFileParams = UploadFileParams
  { session_id :: SessionId,
    by :: T.Text,
    value :: T.Text,
    filePath :: T.Text,
    timeout :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype TakeScreenshotParams = TakeScreenshotParams
  { session_id :: SessionId
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype CloseSessionParams = CloseSessionParams
  { session_id :: SessionId
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- Console logging parameter types
data GetConsoleLogsParams = GetConsoleLogsParams
  { session_id :: SessionId,
    logLevel :: Maybe T.Text,
    maxEntries :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

newtype GetAvailableLogTypesParams = GetAvailableLogTypesParams
  { session_id :: SessionId
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data InjectConsoleLoggerParams = InjectConsoleLoggerParams
  { session_id :: SessionId,
    timeout :: Maybe Int
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

data GetInjectedConsoleLogsParams = GetInjectedConsoleLogsParams
  { session_id :: SessionId,
    clear :: Maybe Bool
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Selenium tools container
newtype SeleniumTools = SeleniumTools
  { sessionsVar :: TVar (HashMap.HashMap SessionId SessionData)
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

-- | Session management helper functions
lookupSession :: SeleniumTools -> SessionId -> IO (Maybe SessionData)
lookupSession tools searchKey = do
  sessions <- readTVarIO (sessionsVar tools)
  return $ HashMap.lookup searchKey sessions

insertSession :: SeleniumTools -> SessionId -> SessionData -> IO ()
insertSession tools insertKey sessionData = do
  atomically $ modifyTVar (sessionsVar tools) (HashMap.insert insertKey sessionData)

removeSession :: SeleniumTools -> SessionId -> IO ()
removeSession tools removeKey = do
  atomically $ modifyTVar (sessionsVar tools) (HashMap.delete removeKey)

generateSessionId :: IO SessionId
generateSessionId = UUID4.nextRandom

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
        session <- initializeSession browserVal finalOpts
        newSessionId <- generateSessionId
        let sessionData = SessionData newSessionId session
        insertSession tools newSessionId sessionData
        let sessionIdText = T.pack $ UUID.toString newSessionId
            responseJson =
              object
                [ "sessionId" .= sessionIdText,
                  "browser" .= show browserVal,
                  "success" .= True,
                  "message" .= ("Browser " <> T.pack (show browserVal) <> " started successfully" :: T.Text)
                ]
            responseText = TE.decodeUtf8 $ BSL.toStrict $ encode responseJson
        debugLog $ "HANDLER: Created session with ID: " ++ UUID.toString newSessionId
        return $ CallToolResult [ToolContent TextualContent (Just responseText)] False
    )
    ( \e -> do
        return $ errorResult $ "Failed to start browser: " <> T.pack (show (e :: SomeException))
    )

-- | Handle navigate tool
handleNavigate :: SeleniumTools -> NavigateParams -> IO CallToolResult
handleNavigate tools (NavigateParams sessId urlVal) = do
  debugLog "HANDLER: navigate called"
  sessionMaybe <- lookupSession tools sessId
  case sessionMaybe of
    Nothing -> do
      debugLog "HANDLER: Session not found in navigate"
      return $ errorResult "Session not found"
    Just sessionData ->
      catch
        ( do
            navigateToUrl (seleniumSession sessionData) urlVal
            return $ successResult $ "Navigated to " <> urlVal
        )
        (\e -> return $ errorResult $ "Navigation failed: " <> T.pack (show (e :: SomeException)))

-- | Handle find_element tool
handleFindElement :: SeleniumTools -> FindElementParams -> IO CallToolResult
handleFindElement tools (FindElementParams sessId byVal valueVal timeoutVal) = do
  debugLog "HANDLER: find_element called"
  sessionMaybe <- lookupSession tools sessId
  case sessionMaybe of
    Nothing -> do
      debugLog "HANDLER: Session not found in find_element"
      return $ errorResult "Session not found"
    Just sessionData -> do
      catch
        ( do
            let byStrategy = fromMaybe "id" byVal -- default to "id" if not provided
                locator = parseLocatorStrategy byStrategy valueVal
                timeoutMs = fromMaybe 10000 timeoutVal
            element <- findElementByLocator (seleniumSession sessionData) locator timeoutMs
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
handleClickElement tools (ClickElementParams sessionId byVal valueVal timeoutVal) = do
  sessionMaybe <- lookupSession tools sessionId
  case sessionMaybe of
    Nothing -> return $ errorResult "Session not found"
    Just sessionData ->
      catch
        ( do
            let locator = parseLocatorStrategy byVal valueVal
                timeoutMs = fromMaybe 10000 timeoutVal
            clickElement (seleniumSession sessionData) locator timeoutMs
            return $ successResult $ "Clicked element with " <> byVal <> "='" <> valueVal <> "'"
        )
        (\e -> return $ errorResult $ "Click failed: " <> T.pack (show (e :: SomeException)))

-- | Handle send_keys tool
handleSendKeys :: SeleniumTools -> SendKeysParams -> IO CallToolResult
handleSendKeys tools (SendKeysParams sessionId byVal valueVal textVal timeoutVal) = do
  sessionMaybe <- lookupSession tools sessionId
  case sessionMaybe of
    Nothing -> return $ errorResult "Session not found"
    Just sessionData ->
      catch
        ( do
            let locator = parseLocatorStrategy byVal valueVal
                timeoutMs = fromMaybe 10000 timeoutVal
            sendKeysToElement (seleniumSession sessionData) locator textVal timeoutMs
            return $ successResult $ "Sent keys to element with " <> byVal <> "='" <> valueVal <> "'"
        )
        (\e -> return $ errorResult $ "Send keys failed: " <> T.pack (show (e :: SomeException)))

-- | Handle get_element_text tool
handleGetElementText :: SeleniumTools -> GetElementTextParams -> IO CallToolResult
handleGetElementText tools (GetElementTextParams sessionId byVal valueVal timeoutVal) = do
  sessionMaybe <- lookupSession tools sessionId
  case sessionMaybe of
    Nothing -> return $ errorResult "Session not found"
    Just sessionData ->
      catch
        ( do
            let locator = parseLocatorStrategy byVal valueVal
                timeoutMs = fromMaybe 10000 timeoutVal
            elementText <- getElementText (seleniumSession sessionData) locator timeoutMs
            return $ successResult elementText
        )
        (\e -> return $ errorResult $ "Get text failed: " <> T.pack (show (e :: SomeException)))

-- | Handle hover tool
handleHover :: SeleniumTools -> HoverParams -> IO CallToolResult
handleHover tools (HoverParams sessionId byVal valueVal timeoutVal) = do
  sessionMaybe <- lookupSession tools sessionId
  case sessionMaybe of
    Nothing -> return $ errorResult "Session not found"
    Just sessionData ->
      catch
        ( do
            let locator = parseLocatorStrategy byVal valueVal
                timeoutMs = fromMaybe 10000 timeoutVal
            hoverElement (seleniumSession sessionData) locator timeoutMs
            return $ successResult $ "Hovered over element with " <> byVal <> "='" <> valueVal <> "'"
        )
        (\e -> return $ errorResult $ "Hover failed: " <> T.pack (show (e :: SomeException)))

-- | Handle drag_and_drop tool
handleDragAndDrop :: SeleniumTools -> DragAndDropParams -> IO CallToolResult
handleDragAndDrop tools (DragAndDropParams sessionId byVal valueVal targetByVal targetValueVal timeoutVal) = do
  sessionMaybe <- lookupSession tools sessionId
  case sessionMaybe of
    Nothing -> return $ errorResult "Session not found"
    Just sessionData ->
      catch
        ( do
            let sourceLocator = parseLocatorStrategy byVal valueVal
                targetLocator = parseLocatorStrategy targetByVal targetValueVal
                timeoutMs = fromMaybe 10000 timeoutVal
            dragAndDropElements (seleniumSession sessionData) sourceLocator targetLocator timeoutMs
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
handleDoubleClick tools (DoubleClickParams sessionId byVal valueVal timeoutVal) = do
  sessionMaybe <- lookupSession tools sessionId
  case sessionMaybe of
    Nothing -> return $ errorResult "Session not found"
    Just sessionData ->
      catch
        ( do
            let locator = parseLocatorStrategy byVal valueVal
                timeoutMs = fromMaybe 10000 timeoutVal
            doubleClickElement (seleniumSession sessionData) locator timeoutMs
            return $ successResult $ "Double-clicked element with " <> byVal <> "='" <> valueVal <> "'"
        )
        (\e -> return $ errorResult $ "Double click failed: " <> T.pack (show (e :: SomeException)))

-- | Handle right_click tool
handleRightClick :: SeleniumTools -> RightClickParams -> IO CallToolResult
handleRightClick tools (RightClickParams sessionId byVal valueVal timeoutVal) = do
  sessionMaybe <- lookupSession tools sessionId
  case sessionMaybe of
    Nothing -> return $ errorResult "Session not found"
    Just sessionData ->
      catch
        ( do
            let locator = parseLocatorStrategy byVal valueVal
                timeoutMs = fromMaybe 10000 timeoutVal
            rightClickElement (seleniumSession sessionData) locator timeoutMs
            return $ successResult $ "Right-clicked element with " <> byVal <> "='" <> valueVal <> "'"
        )
        (\e -> return $ errorResult $ "Right click failed: " <> T.pack (show (e :: SomeException)))

-- | Handle press_key tool
handlePressKey :: SeleniumTools -> PressKeyParams -> IO CallToolResult
handlePressKey tools (PressKeyParams sessionId keyVal) = do
  sessionMaybe <- lookupSession tools sessionId
  case sessionMaybe of
    Nothing -> return $ errorResult "Session not found"
    Just sessionData ->
      catch
        ( do
            pressKey (seleniumSession sessionData) keyVal
            return $ successResult $ "Pressed key: " <> keyVal
        )
        (\e -> return $ errorResult $ "Press key failed: " <> T.pack (show (e :: SomeException)))

-- | Handle upload_file tool
handleUploadFile :: SeleniumTools -> UploadFileParams -> IO CallToolResult
handleUploadFile tools (UploadFileParams sessionId byVal valueVal filePathVal timeoutVal) = do
  sessionMaybe <- lookupSession tools sessionId
  case sessionMaybe of
    Nothing -> return $ errorResult "Session not found"
    Just sessionData ->
      catch
        ( do
            let locator = parseLocatorStrategy byVal valueVal
                timeoutMs = fromMaybe 10000 timeoutVal
            uploadFileToElement (seleniumSession sessionData) locator filePathVal timeoutMs
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
handleTakeScreenshot tools (TakeScreenshotParams sessionId) = do
  sessionMaybe <- lookupSession tools sessionId
  case sessionMaybe of
    Nothing -> return $ errorResult "Session not found"
    Just sessionData ->
      catch
        ( do
            result <- takeScreenshot (seleniumSession sessionData) Nothing
            return $ successResult $ "Screenshot captured: " <> result
        )
        (\e -> return $ errorResult $ "Screenshot failed: " <> T.pack (show (e :: SomeException)))

-- | Handle close_session tool
handleCloseSession :: SeleniumTools -> CloseSessionParams -> IO CallToolResult
handleCloseSession tools (CloseSessionParams sessionId) = do
  sessionMaybe <- lookupSession tools sessionId
  case sessionMaybe of
    Nothing -> return $ successResult "Session not found or already closed"
    Just sessionData ->
      catch
        ( do
            closeSeleniumSession (seleniumSession sessionData)
            removeSession tools sessionId
            return $ successResult "Browser session closed successfully"
        )
        (\e -> return $ errorResult $ "Close session failed: " <> T.pack (show (e :: SomeException)))

-- | Handle get_console_logs tool
handleGetConsoleLogs :: SeleniumTools -> GetConsoleLogsParams -> IO CallToolResult
handleGetConsoleLogs tools (GetConsoleLogsParams sessionId logLevelVal maxEntriesVal) = do
  debugLog "HANDLER: get_console_logs called"
  sessionMaybe <- lookupSession tools sessionId
  case sessionMaybe of
    Nothing -> do
      debugLog "HANDLER: Session not found in get_console_logs"
      return $ errorResult "Session not found"
    Just sessionData ->
      catch
        ( do
            debugLog "HANDLER: Getting logs from WebDriver"
            logs <- getConsoleLogs (seleniumSession sessionData) logLevelVal maxEntriesVal
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
handleGetAvailableLogTypes tools (GetAvailableLogTypesParams sessionId) = do
  debugLog "HANDLER: get_available_log_types called"
  sessionMaybe <- lookupSession tools sessionId
  case sessionMaybe of
    Nothing -> do
      debugLog "HANDLER: Session not found in get_available_log_types"
      return $ errorResult "Session not found"
    Just sessionData ->
      catch
        ( do
            debugLog "HANDLER: Getting log types from WebDriver"
            logTypes <- getAvailableLogTypes (seleniumSession sessionData)
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
handleInjectConsoleLogger tools (InjectConsoleLoggerParams sessionId timeoutVal) = do
  debugLog "HANDLER: inject_console_logger called"
  sessionMaybe <- lookupSession tools sessionId
  case sessionMaybe of
    Nothing -> do
      debugLog "HANDLER: Session not found in inject_console_logger"
      return $ errorResult "Session not found"
    Just sessionData ->
      catch
        ( do
            debugLog "HANDLER: Injecting console logger"
            let timeoutMs = fromMaybe 60000 timeoutVal -- Default to 60 seconds
            injectConsoleLogger (seleniumSession sessionData) timeoutMs
            debugLog "HANDLER: Console logger injected successfully"
            return $ successResult "Console logger injected successfully"
        )
        ( \e -> do
            debugLog ("HANDLER: Exception in inject_console_logger: " ++ show (e :: SomeException))
            return $ errorResult $ "Inject console logger failed: " <> T.pack (show (e :: SomeException))
        )

-- | Handle get_injected_console_logs tool
handleGetInjectedConsoleLogs :: SeleniumTools -> GetInjectedConsoleLogsParams -> IO CallToolResult
handleGetInjectedConsoleLogs tools (GetInjectedConsoleLogsParams sessionId clearVal) = do
  debugLog "HANDLER: get_injected_console_logs called"
  sessionMaybe <- lookupSession tools sessionId
  case sessionMaybe of
    Nothing -> do
      debugLog "HANDLER: Session not found in get_injected_console_logs"
      return $ errorResult "Session not found"
    Just sessionData ->
      catch
        ( do
            let shouldClear = fromMaybe False clearVal
            debugLog ("HANDLER: Getting injected logs, clear=" ++ show shouldClear)
            logsJson <- getInjectedConsoleLogs (seleniumSession sessionData) shouldClear
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
  sessionsState <- newTVarIO HashMap.empty
  debugLog "Creating SeleniumTools instance with session management"
  return $ SeleniumTools sessionsState
