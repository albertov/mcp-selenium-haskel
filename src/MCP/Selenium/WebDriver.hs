{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module MCP.Selenium.WebDriver
  ( initializeWebDriver
  , createBrowserSession
  , navigateToUrl
  , findElement
  , clickElement
  , sendKeysToElement
  , getElementText
  , hoverOverElement
  , dragAndDrop
  , doubleClickElement
  , rightClickElement
  , pressKey
  , uploadFile
  , takeScreenshot
  , takeElementScreenshot
  , executeScript
  , getConsoleLogs
  , injectConsoleLogger
  , getInjectedConsoleLogs
  , closeBrowserSession
  , getCurrentSession
  , setCurrentSession
  , toWebDriverLocator
  ) where

import Control.Exception (try, catch, SomeException)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value(..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Base64 as B64
import qualified Test.WebDriver as WD
import qualified Test.WebDriver.Capabilities as WD
import qualified Test.WebDriver.Config as WD
import qualified Test.WebDriver.Firefox.Profile as FF
import qualified Test.WebDriver.Chrome.Extension as Chrome

import MCP.Selenium.Types

initializeWebDriver :: IO ()
initializeWebDriver = pure ()

toWebDriverLocator :: LocatorStrategy -> Text -> WD.Selector
toWebDriverLocator ById value = WD.ById value
toWebDriverLocator ByCss value = WD.ByCSS value
toWebDriverLocator ByXPath value = WD.ByXPath value
toWebDriverLocator ByName value = WD.ByName value
toWebDriverLocator ByTagName value = WD.ByTag value
toWebDriverLocator ByClassName value = WD.ByClass value

createBrowserSession :: Browser -> BrowserOptions -> Bool -> IO (Either SeleniumError SessionId)
createBrowserSession browser opts enableLogging = do
  result <- try $ case browser of
    Chrome -> createChromeSession opts enableLogging
    Firefox -> createFirefoxSession opts enableLogging
  case result of
    Left (e :: SomeException) -> pure $ Left $ BrowserError $ T.pack $ show e
    Right sessionId -> pure $ Right sessionId

createChromeSession :: BrowserOptions -> Bool -> IO SessionId
createChromeSession BrowserOptions{..} enableLogging = do
  let caps = WD.defaultCaps
      chromeOpts = WD.defaultChromeOptions
      config = WD.defaultConfig
        { WD.wdCapabilities = caps
          { WD.browser = WD.chrome chromeOpts
          }
        }

  -- Apply headless mode if specified
  let finalConfig = case headless of
        Just True -> config
          { WD.wdCapabilities = (WD.wdCapabilities config)
            { WD.browser = WD.chrome $ chromeOpts { WD.chromeHeadless = True }
            }
          }
        _ -> config

  WD.runSession finalConfig $ do
    sessionId <- WD.getSessionId
    pure $ SessionId $ T.pack $ show sessionId

createFirefoxSession :: BrowserOptions -> Bool -> IO SessionId
createFirefoxSession BrowserOptions{..} enableLogging = do
  let caps = WD.defaultCaps
      ffOpts = WD.defaultFirefoxOptions
      config = WD.defaultConfig
        { WD.wdCapabilities = caps
          { WD.browser = WD.firefox ffOpts
          }
        }

  -- Apply headless mode if specified
  let finalConfig = case headless of
        Just True -> config
          { WD.wdCapabilities = (WD.wdCapabilities config)
            { WD.browser = WD.firefox $ ffOpts { WD.ffHeadless = True }
            }
          }
        _ -> config

  WD.runSession finalConfig $ do
    sessionId <- WD.getSessionId
    pure $ SessionId $ T.pack $ show sessionId

navigateToUrl :: Text -> WD.WD (Either SeleniumError ())
navigateToUrl url = do
  result <- liftIO $ try $ WD.openPage url
  case result of
    Left (e :: SomeException) -> pure $ Left $ NavigationError $ T.pack $ show e
    Right _ -> pure $ Right ()

findElement :: ElementLocator -> WD.WD (Either SeleniumError WD.Element)
findElement ElementLocator{..} = do
  let selector = toWebDriverLocator strategy value
      timeoutMs = maybe 10000 id timeout
  result <- liftIO $ try $ WD.waitUntil timeoutMs $ WD.findElem selector
  case result of
    Left (e :: SomeException) -> pure $ Left $ ElementNotFound $ T.pack $ show e
    Right element -> pure $ Right element

clickElement :: WD.Element -> WD.WD (Either SeleniumError ())
clickElement element = do
  result <- liftIO $ try $ WD.click element
  case result of
    Left (e :: SomeException) -> pure $ Left $ ActionError $ T.pack $ show e
    Right _ -> pure $ Right ()

sendKeysToElement :: WD.Element -> Text -> WD.WD (Either SeleniumError ())
sendKeysToElement element text = do
  result <- liftIO $ try $ do
    WD.clearInput element
    WD.sendKeys text element
  case result of
    Left (e :: SomeException) -> pure $ Left $ ActionError $ T.pack $ show e
    Right _ -> pure $ Right ()

getElementText :: WD.Element -> WD.WD (Either SeleniumError Text)
getElementText element = do
  result <- liftIO $ try $ WD.getText element
  case result of
    Left (e :: SomeException) -> pure $ Left $ ActionError $ T.pack $ show e
    Right text -> pure $ Right text

hoverOverElement :: WD.Element -> WD.WD (Either SeleniumError ())
hoverOverElement element = do
  result <- liftIO $ try $ WD.moveToCenter element
  case result of
    Left (e :: SomeException) -> pure $ Left $ ActionError $ T.pack $ show e
    Right _ -> pure $ Right ()

dragAndDrop :: WD.Element -> WD.Element -> WD.WD (Either SeleniumError ())
dragAndDrop source target = do
  result <- liftIO $ try $ WD.dragAndDrop source target
  case result of
    Left (e :: SomeException) -> pure $ Left $ ActionError $ T.pack $ show e
    Right _ -> pure $ Right ()

doubleClickElement :: WD.Element -> WD.WD (Either SeleniumError ())
doubleClickElement element = do
  result <- liftIO $ try $ WD.doubleClick (Just element)
  case result of
    Left (e :: SomeException) -> pure $ Left $ ActionError $ T.pack $ show e
    Right _ -> pure $ Right ()

rightClickElement :: WD.Element -> WD.WD (Either SeleniumError ())
rightClickElement element = do
  result <- liftIO $ try $ WD.contextClick (Just element)
  case result of
    Left (e :: SomeException) -> pure $ Left $ ActionError $ T.pack $ show e
    Right _ -> pure $ Right ()

pressKey :: WD.Key -> WD.WD (Either SeleniumError ())
pressKey key = do
  result <- liftIO $ try $ WD.sendKeys (T.singleton $ WD.keyToChar key) =<< WD.findElem (WD.ByTag "body")
  case result of
    Left (e :: SomeException) -> pure $ Left $ ActionError $ T.pack $ show e
    Right _ -> pure $ Right ()

uploadFile :: WD.Element -> Text -> WD.WD (Either SeleniumError ())
uploadFile element filePath = do
  result <- liftIO $ try $ WD.sendKeys filePath element
  case result of
    Left (e :: SomeException) -> pure $ Left $ ActionError $ T.pack $ show e
    Right _ -> pure $ Right ()

takeScreenshot :: WD.WD (Either SeleniumError Text)
takeScreenshot = do
  result <- liftIO $ try $ WD.screenshot
  case result of
    Left (e :: SomeException) -> pure $ Left $ ActionError $ T.pack $ show e
    Right screenshotData -> pure $ Right $ TE.decodeUtf8 $ B64.encode screenshotData

takeElementScreenshot :: WD.Element -> WD.WD (Either SeleniumError Text)
takeElementScreenshot element = do
  result <- liftIO $ try $ WD.screenshotElem element
  case result of
    Left (e :: SomeException) -> pure $ Left $ ActionError $ T.pack $ show e
    Right screenshotData -> pure $ Right $ TE.decodeUtf8 $ B64.encode screenshotData

executeScript :: Text -> [Value] -> WD.WD (Either SeleniumError Value)
executeScript script args = do
  result <- liftIO $ try $ WD.executeJS [] script args
  case result of
    Left (e :: SomeException) -> pure $ Left $ ActionError $ T.pack $ show e
    Right value -> pure $ Right value

getConsoleLogs :: ConsoleLogLevel -> Maybe Int -> WD.WD (Either SeleniumError [ConsoleLogEntry])
getConsoleLogs level maxEntries = do
  result <- liftIO $ try $ WD.getLogs WD.Browser
  case result of
    Left (e :: SomeException) -> pure $ Left $ ActionError $ T.pack $ show e
    Right logs -> do
      let convertedLogs = map convertLogEntry logs
          filteredLogs = case level of
            LogAll -> convertedLogs
            _ -> filter (\entry -> logLevel entry == level) convertedLogs
          limitedLogs = case maxEntries of
            Nothing -> filteredLogs
            Just n -> take n $ reverse filteredLogs
      pure $ Right limitedLogs

convertLogEntry :: WD.LogEntry -> ConsoleLogEntry
convertLogEntry logEntry =
  ConsoleLogEntry
    { logLevel = convertLogLevel $ WD.logLevel logEntry
    , message = WD.logMessage logEntry
    , timestamp = fromIntegral $ WD.logTime logEntry
    , source = "browser"
    }

convertLogLevel :: WD.LogLevel -> ConsoleLogLevel
convertLogLevel WD.LogAll = LogAll
convertLogLevel WD.LogDebug = LogDebug
convertLogLevel WD.LogInfo = LogInfo
convertLogLevel WD.LogWarning = LogWarning
convertLogLevel WD.LogSevere = LogSevere

injectConsoleLogger :: WD.WD (Either SeleniumError ())
injectConsoleLogger = do
  let script = T.unlines
        [ "if (!window._consoleLogsCaptured) {"
        , "  window._consoleLogsCaptured = [];"
        , "  window._originalConsole = {"
        , "    log: console.log,"
        , "    warn: console.warn,"
        , "    error: console.error,"
        , "    info: console.info,"
        , "    debug: console.debug"
        , "  };"
        , "  "
        , "  ['log', 'warn', 'error', 'info', 'debug'].forEach(function(method) {"
        , "    console[method] = function() {"
        , "      window._originalConsole[method].apply(console, arguments);"
        , "      "
        , "      window._consoleLogsCaptured.push({"
        , "        level: method,"
        , "        message: Array.from(arguments).map(arg => "
        , "          typeof arg === 'object' ? JSON.stringify(arg) : String(arg)"
        , "        ).join(' '),"
        , "        timestamp: Date.now()"
        , "      });"
        , "    };"
        , "  });"
        , "}"
        ]
  result <- executeScript script []
  case result of
    Left err -> pure $ Left err
    Right _ -> pure $ Right ()

getInjectedConsoleLogs :: Bool -> WD.WD (Either SeleniumError [ConsoleLogEntry])
getInjectedConsoleLogs clear = do
  let script = T.unlines
        [ "var logs = window._consoleLogsCaptured || [];"
        , if clear then "window._consoleLogsCaptured = [];" else ""
        , "return logs;"
        ]
  result <- executeScript script []
  case result of
    Left err -> pure $ Left err
    Right (Array values) -> do
      let parsedLogs = mapM parseLogEntry values
      case parsedLogs of
        Left err -> pure $ Left $ ActionError $ T.pack err
        Right logs -> pure $ Right logs
    Right _ -> pure $ Right []

parseLogEntry :: Value -> Either String ConsoleLogEntry
parseLogEntry (Object obj) = do
  level <- case lookup "level" obj of
    Just (String "log") -> Right LogInfo
    Just (String "warn") -> Right LogWarning
    Just (String "error") -> Right LogSevere
    Just (String "info") -> Right LogInfo
    Just (String "debug") -> Right LogDebug
    _ -> Left "Invalid log level"

  message <- case lookup "message" obj of
    Just (String msg) -> Right msg
    _ -> Left "Invalid message"

  timestamp <- case lookup "timestamp" obj of
    Just (Number n) -> Right $ round n
    _ -> Left "Invalid timestamp"

  pure $ ConsoleLogEntry level message timestamp "injected"
parseLogEntry _ = Left "Invalid log entry format"

closeBrowserSession :: WD.WD (Either SeleniumError ())
closeBrowserSession = do
  result <- liftIO $ try WD.closeSession
  case result of
    Left (e :: SomeException) -> pure $ Left $ BrowserError $ T.pack $ show e
    Right _ -> pure $ Right ()

getCurrentSession :: IO (Maybe SessionId)
getCurrentSession = pure Nothing

setCurrentSession :: Maybe SessionId -> IO ()
setCurrentSession _ = pure ()
