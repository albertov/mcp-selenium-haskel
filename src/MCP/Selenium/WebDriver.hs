{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module: MCP.Selenium.WebDriver
-- Description: WebDriver operations wrapper for Selenium automation
--
-- This module provides a Haskell wrapper around the Selenium WebDriver for browser automation.
-- It abstracts common browser operations and provides a type-safe interface for web automation tasks.
--
-- = Key Features
--
-- * Type-safe browser configuration and session management
-- * Support for Chrome and Firefox browsers with comprehensive options
-- * Element location using multiple strategies (ID, CSS, XPath, etc.)
-- * Advanced browser interactions (hover, drag-and-drop, right-click)
-- * Console logging and JavaScript injection capabilities
-- * Screenshot capture and page source retrieval
--
-- = Browser Support
--
-- Currently supported browsers:
--
-- * **Chrome**: Full feature support with extensive configuration options
-- * **Firefox**: Basic support with standard configuration options
--
-- = Configuration
--
-- The WebDriver connects to a Selenium server specified by environment variables:
--
-- * @SELENIUM_HOST@: Server hostname (default: "127.0.0.1")
-- * @SELENIUM_PORT@: Server port (default: "4444")
--
-- = Example Usage
--
-- @
-- import MCP.Selenium.WebDriver
--
-- -- Create a browser session
-- session <- initializeSession Chrome (Just defaultChromeOptions)
--
-- -- Navigate to a page
-- navigateToUrl session "https://example.com"
--
-- -- Find and interact with elements
-- element <- findElementByLocator session (CSSSelector "#button")
-- clickElement session (CSSSelector "#button") 5000
--
-- -- Take a screenshot
-- screenshot <- takeScreenshot session Nothing
--
-- -- Clean up
-- closeSeleniumSession session
-- @
--
-- = Error Handling
--
-- All operations can throw 'SeleniumException' for WebDriver-related errors.
-- Callers should handle these exceptions appropriately.
--
-- = Thread Safety
--
-- Individual sessions are not thread-safe, but multiple sessions can be used
-- concurrently from different threads safely.
module MCP.Selenium.WebDriver
  ( -- * Core Types
    SeleniumSession (..),
    BrowserOptions (..),
    Browser (..),
    LocatorStrategy (..),
    LogEntry (..),
    LogLevel (..),

    -- * Session Management
    initializeSession,
    closeSeleniumSession,

    -- * Navigation
    navigateToUrl,

    -- * Element Operations
    findElementByLocator,
    clickElement,
    sendKeysToElement,
    getElementText,

    -- * Advanced Actions
    hoverElement,
    dragAndDropElements,
    doubleClickElement,
    rightClickElement,
    pressKey,

    -- * File Operations
    uploadFileToElement,

    -- * Utility Operations
    takeScreenshot,
    getPageSource,

    -- * Console Logging
    getConsoleLogs,
    getAvailableLogTypes,
    injectConsoleLogger,
    getInjectedConsoleLogs,
  )
where

import Control.Exception (Exception)
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import GHC.Generics (Generic)
import System.Environment (lookupEnv)
import qualified Test.WebDriver as WD
import Test.WebDriver.Commands (LogEntry (..), LogLevel (..), LogType, executeJS, getLogs, getSource)
import Test.WebDriver.Session (WDSession (..), getSession)
import Text.RawString.QQ (r)

-- | Browser type enumeration
data Browser = Chrome | Firefox
  deriving (Eq, Show, Generic)

-- | Custom JSON instances for case-insensitive browser names
instance ToJSON Browser where
  toJSON Chrome = toJSON ("chrome" :: T.Text)
  toJSON Firefox = toJSON ("firefox" :: T.Text)

instance FromJSON Browser where
  parseJSON v = do
    str <- parseJSON v :: Parser T.Text
    case T.toLower str of
      "chrome" -> return Chrome
      "firefox" -> return Firefox
      _ -> fail $ "Unknown browser: " ++ T.unpack str

-- | Browser configuration options
data BrowserOptions = BrowserOptions
  { headless :: Maybe Bool,
    arguments :: Maybe [T.Text],
    enableLogging :: Maybe Bool
  }
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Element locator strategies
data LocatorStrategy
  = ById T.Text
  | ByCss T.Text
  | ByXPath T.Text
  | ByName T.Text
  | ByTag T.Text
  | ByClass T.Text
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Selenium session state
data SeleniumSession = SeleniumSession
  { browser :: Browser,
    wdSession :: WDSession
  }
  deriving (Generic)

-- | Custom exception type for WebDriver errors
newtype SeleniumError = SeleniumError T.Text
  deriving (Show)

instance Exception SeleniumError

-- | Convert LocatorStrategy to WebDriver's Selector
locatorToBy :: LocatorStrategy -> WD.Selector
locatorToBy (ById t) = WD.ById t
locatorToBy (ByCss t) = WD.ByCSS t
locatorToBy (ByXPath t) = WD.ByXPath t
locatorToBy (ByName t) = WD.ByName t
locatorToBy (ByTag t) = WD.ByTag t
locatorToBy (ByClass t) = WD.ByClass t

-- | Create WebDriver config for given browser and options
createWebDriverConfig :: Browser -> BrowserOptions -> IO WD.WDConfig
createWebDriverConfig browserType opts = do
  -- Read host and port from environment variables with defaults
  hostStr <- fromMaybe "127.0.0.1" <$> lookupEnv "SELENIUM_HOST"
  portStr <- fromMaybe "4444" <$> lookupEnv "SELENIUM_PORT"
  let port = case TR.decimal (T.pack portStr) of
        Right (p, _) -> p
        Left _ -> 4444 -- Default port if parsing fails
      baseConfig = case browserType of
        Chrome -> WD.defaultConfig {WD.wdCapabilities = WD.defaultCaps {WD.browser = WD.chrome}}
        Firefox -> WD.defaultConfig {WD.wdCapabilities = WD.defaultCaps {WD.browser = WD.firefox}}
      -- Increase HTTP response timeout and retry count for CI environments
      -- This is especially important for JavaScript execution calls like injectConsoleLogger
      configWithHost = baseConfig {WD.wdHost = hostStr, WD.wdPort = port, WD.wdHTTPRetryCount = 5}
      chromeCapabilities :: WD.Capabilities
      chromeCapabilities =
        WD.defaultCaps
          { WD.browser =
              WD.Chrome
                { chromeDriverVersion = mempty,
                  chromeBinary = mempty,
                  chromeOptions =
                    ["--width=1024", "--height=768", "--enable-logging", "--log-level=0", "--v=1", "--enable-network-service-logging", "--no-sandbox", "--disable-dev-shm-usage"]
                      <> if fromMaybe False (headless opts)
                        then ["--headless=new", "--disable-gpu"]
                        else [],
                  chromeExtensions = mempty,
                  chromeExperimentalOptions = mempty
                },
            WD.additionalCaps =
              [ ( "goog:loggingPrefs",
                  [aesonQQ|
                    {
                      "browser": "ALL",
                      "driver": "ALL",
                      "performance": "ALL"
                    }
                  |]
                ),
                ( "loggingPrefs",
                  [aesonQQ|
                    {
                      "browser": "ALL",
                      "driver": "ALL",
                      "performance": "ALL"
                    }
                  |]
                )
              ]
          }
  return $ case browserType of
    Chrome ->
      configWithHost
        { WD.wdCapabilities = chromeCapabilities
        }
    Firefox -> configWithHost

-- | Initialize a new WebDriver session
initializeSession :: Browser -> BrowserOptions -> IO SeleniumSession
initializeSession browserType opts = do
  config <- createWebDriverConfig browserType opts
  session <- WD.runSession config getSession
  return $ SeleniumSession browserType session

-- | Close the WebDriver session
closeSeleniumSession :: SeleniumSession -> IO ()
closeSeleniumSession (SeleniumSession _ session) = WD.runWD session WD.closeSession

-- | Navigate to URL
navigateToUrl :: SeleniumSession -> T.Text -> IO ()
navigateToUrl (SeleniumSession _ session) url = do
  WD.runWD session $
    WD.openPage (T.unpack url)

-- | Find element by locator strategy
findElementByLocator :: SeleniumSession -> LocatorStrategy -> Int -> IO WD.Element
findElementByLocator (SeleniumSession _ session) locator timeoutMs = do
  WD.runWD session $ do
    WD.setImplicitWait (fromIntegral timeoutMs)
    WD.findElem (locatorToBy locator)

-- | Click an element
clickElement :: SeleniumSession -> LocatorStrategy -> Int -> IO ()
clickElement (SeleniumSession _ session) locator timeoutMs = do
  WD.runWD session $ do
    WD.setImplicitWait (fromIntegral timeoutMs)
    element <- WD.findElem (locatorToBy locator)
    WD.click element

-- | Send keys to an element
sendKeysToElement :: SeleniumSession -> LocatorStrategy -> T.Text -> Int -> IO ()
sendKeysToElement (SeleniumSession _ session) locator text timeoutMs = do
  WD.runWD session $ do
    WD.setImplicitWait (fromIntegral timeoutMs)
    element <- WD.findElem (locatorToBy locator)
    WD.sendKeys text element

-- | Get text content of an element
getElementText :: SeleniumSession -> LocatorStrategy -> Int -> IO T.Text
getElementText (SeleniumSession _ session) locator timeoutMs = do
  WD.runWD session $ do
    WD.setImplicitWait (fromIntegral timeoutMs)
    element <- WD.findElem (locatorToBy locator)
    WD.getText element

-- | Hover over an element
hoverElement :: SeleniumSession -> LocatorStrategy -> Int -> IO ()
hoverElement (SeleniumSession _ session) locator timeoutMs = do
  WD.runWD session $ do
    WD.setImplicitWait (fromIntegral timeoutMs)
    element <- WD.findElem (locatorToBy locator)
    WD.moveToCenter element

-- | Drag and drop between elements
dragAndDropElements :: SeleniumSession -> LocatorStrategy -> LocatorStrategy -> Int -> IO ()
dragAndDropElements (SeleniumSession _ session) sourceLocator targetLocator timeoutMs = do
  WD.runWD session $ do
    WD.setImplicitWait (fromIntegral timeoutMs)
    sourceElement <- WD.findElem (locatorToBy sourceLocator)
    targetElement <- WD.findElem (locatorToBy targetLocator)
    WD.moveToCenter sourceElement
    WD.mouseDown
    WD.moveToCenter targetElement
    WD.mouseUp

-- | Double click an element
doubleClickElement :: SeleniumSession -> LocatorStrategy -> Int -> IO ()
doubleClickElement (SeleniumSession _ session) locator timeoutMs = do
  WD.runWD session $ do
    WD.setImplicitWait (fromIntegral timeoutMs)
    element <- WD.findElem (locatorToBy locator)
    WD.moveToCenter element
    WD.doubleClick

-- | Right click an element
rightClickElement :: SeleniumSession -> LocatorStrategy -> Int -> IO ()
rightClickElement (SeleniumSession _ session) locator timeoutMs = do
  WD.runWD session $ do
    WD.setImplicitWait (fromIntegral timeoutMs)
    element <- WD.findElem (locatorToBy locator)
    -- Perform right-click using JavaScript since WebDriver doesn't have direct right-click support
    (_ :: Maybe ()) <-
      executeJS
        [WD.JSArg element]
        [r|
      var element = arguments[0];
      var event = new MouseEvent('contextmenu', {
        view: window,
        bubbles: true,
        cancelable: true,
        button: 2
      });
      element.dispatchEvent(event);
    |]
    return ()

-- | Press a key
pressKey :: SeleniumSession -> T.Text -> IO ()
pressKey (SeleniumSession _ session) key = do
  WD.runWD session $ do
    -- Use JavaScript to dispatch keyboard events for better compatibility
    -- This ensures the events are triggered on the document
    (_ :: Maybe ()) <-
      executeJS
        [WD.JSArg key]
        [r|
      var key = arguments[0];
      var event = new KeyboardEvent('keydown', {
        key: key,
        code: key,
        bubbles: true,
        cancelable: true
      });
      document.dispatchEvent(event);

      // Also dispatch on the body element for additional compatibility
      var bodyEvent = new KeyboardEvent('keydown', {
        key: key,
        code: key,
        bubbles: true,
        cancelable: true
      });
      document.body.dispatchEvent(bodyEvent);
    |]
    return ()

-- | Upload file to input element
uploadFileToElement :: SeleniumSession -> LocatorStrategy -> T.Text -> Int -> IO ()
uploadFileToElement (SeleniumSession _ session) locator filePath timeoutMs = do
  WD.runWD session $ do
    WD.setImplicitWait (fromIntegral timeoutMs)
    element <- WD.findElem (locatorToBy locator)
    WD.sendKeys filePath element

-- | Take screenshot and return base64 encoded data
takeScreenshot :: SeleniumSession -> Maybe T.Text -> IO T.Text
takeScreenshot (SeleniumSession _ session) outputPath = do
  screenshotDataLazy <- WD.runWD session WD.screenshot
  let screenshotData = BSL.toStrict screenshotDataLazy
  case outputPath of
    Just path -> do
      BS.writeFile (T.unpack path) screenshotData
      return $ T.pack $ T.unpack path ++ " saved"
    Nothing -> return $ TE.decodeUtf8 $ B64.encode screenshotData

-- | Get console logs from the browser
getConsoleLogs :: SeleniumSession -> Maybe T.Text -> Maybe Int -> IO [LogEntry]
getConsoleLogs (SeleniumSession _ session) logLevelFilter maxEntries = do
  -- For console logs, always try "browser" first since that's where JavaScript console messages go
  -- Even if it's not listed as available, it often still works
  allLogs <- WD.runWD session $ getLogs "browser"
  let filteredLogs = case logLevelFilter of
        Nothing -> allLogs
        Just levelStr -> filter (\(LogEntry _ level _) -> T.pack (show level) == levelStr) allLogs
      limitedLogs = case maxEntries of
        Nothing -> filteredLogs
        Just n -> take n (reverse filteredLogs)
  return limitedLogs

-- | Get available log types for the current session
getAvailableLogTypes :: SeleniumSession -> IO [LogType]
getAvailableLogTypes (SeleniumSession _ session) = WD.runWD session WD.getLogTypes

-- | Inject JavaScript console logger to capture console messages
injectConsoleLogger :: SeleniumSession -> Int -> IO ()
injectConsoleLogger (SeleniumSession _ session) timeoutMs = do
  WD.runWD session $ do
    -- Set script timeout based on the provided parameter
    WD.setScriptTimeout (fromIntegral timeoutMs)
    (_ :: Maybe ()) <-
      executeJS
        []
        [r|
if (!window._consoleLogsCaptured) {
  window._consoleLogsCaptured = [];
  window._originalConsole = {
    log: console.log,
    warn: console.warn,
    error: console.error,
    info: console.info,
    debug: console.debug
  };
  ['log', 'warn', 'error', 'info', 'debug'].forEach(function(method) {
    console[method] = function() {
      window._originalConsole[method].apply(console, arguments);
      window._consoleLogsCaptured.push({
        level: method,
        message: Array.from(arguments).map(function(arg) {
          return typeof arg === 'object' ? JSON.stringify(arg) : String(arg);
        }).join(' '),
        timestamp: Date.now()
      });
    };
  });
} else { }
|]
    return ()

-- | Get console logs captured by the injected logger
getInjectedConsoleLogs :: SeleniumSession -> Bool -> IO T.Text
getInjectedConsoleLogs (SeleniumSession _ session) clearLogs = do
  result <- WD.runWD session $ do
    (jsResult :: Maybe T.Text) <-
      executeJS [] $
        "var logs = window._consoleLogsCaptured || [];\
        \"
          <> (if clearLogs then "window._consoleLogsCaptured = [];" else "")
          <> "return JSON.stringify(logs);"
    return jsResult
  case result of
    Just s -> return s
    Nothing -> return "[]"

-- | Get the current page source
getPageSource :: SeleniumSession -> IO T.Text
getPageSource (SeleniumSession _ session) = do
  WD.runWD session getSource
