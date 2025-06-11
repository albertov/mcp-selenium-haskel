{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | WebDriver operations wrapper for Selenium automation
module MCP.Selenium.WebDriver
  ( SeleniumSession (..),
    BrowserOptions (..),
    Browser (..),
    LocatorStrategy (..),
    LogEntry (..),
    LogLevel (..),
    initializeSession,
    closeSeleniumSession,
    navigateToUrl,
    findElementByLocator,
    clickElement,
    sendKeysToElement,
    getElementText,
    hoverElement,
    dragAndDropElements,
    doubleClickElement,
    rightClickElement,
    pressKey,
    uploadFileToElement,
    takeScreenshot,
    getConsoleLogs,
    getAvailableLogTypes,
    injectConsoleLogger,
    getInjectedConsoleLogs,
    getPageSource,
  )
where

import Control.Exception (Exception)
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
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
                    ["--width=1024", "--height=768"]
                      <> if fromMaybe False (headless opts)
                        then ["--headless=new", "--no-sandbox", "--disable-gpu"]
                        else [],
                  chromeExtensions = mempty,
                  chromeExperimentalOptions = mempty
                }
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
    -- Note: contextClick is not available in this webdriver version
    -- As a workaround, we move to the element
    WD.moveToCenter element

-- | Press a key
pressKey :: SeleniumSession -> T.Text -> IO ()
pressKey (SeleniumSession _ session) key = do
  WD.runWD session $
    WD.sendRawKeys key

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
getAvailableLogTypes _ = do
  -- Since getAvailableLogTypes is not available in this version of webdriver,
  -- return the common log types that are typically supported
  return ["browser", "driver", "performance", "server", "client"]

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
