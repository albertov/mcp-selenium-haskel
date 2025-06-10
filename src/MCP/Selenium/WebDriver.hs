{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | WebDriver operations wrapper for Selenium automation
module MCP.Selenium.WebDriver
  ( SeleniumSession (..),
    BrowserOptions (..),
    Browser (..),
    LocatorStrategy (..),
    initializeSession,
    closeSession,
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
  )
where

import Control.Concurrent.STM (STM, TVar, atomically, newTVar, readTVar, writeTVar)
import Control.Exception (Exception, throwIO)
import Data.Aeson (FromJSON, ToJSON, object, toJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text as T
import GHC.Generics (Generic)
import Test.WebDriver
import Test.WebDriver.Commands
import Test.WebDriver.Config

-- | Browser type enumeration
data Browser = Chrome | Firefox
  deriving (Eq, Show, Generic, ToJSON, FromJSON)

-- | Browser configuration options
data BrowserOptions = BrowserOptions
  { headless :: Maybe Bool,
    arguments :: Maybe [T.Text]
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
    session :: Maybe WDSession
  }
  deriving (Generic)

-- | Custom exception type for WebDriver errors
newtype SeleniumError = SeleniumError T.Text
  deriving (Show)

instance Exception SeleniumError

-- | Convert LocatorStrategy to WebDriver's By selector
locatorToBy :: LocatorStrategy -> By
locatorToBy (ById t) = ById (T.unpack t)
locatorToBy (ByCss t) = ByCSSSelector (T.unpack t)
locatorToBy (ByXPath t) = ByXPath (T.unpack t)
locatorToBy (ByName t) = ByName (T.unpack t)
locatorToBy (ByTag t) = ByTagName (T.unpack t)
locatorToBy (ByClass t) = ByClassName (T.unpack t)

-- | Create WebDriver config for given browser and options
createWebDriverConfig :: Browser -> BrowserOptions -> WDConfig
createWebDriverConfig browser opts =
  let baseConfig = case browser of
        Chrome -> defaultConfig {wdCapabilities = defaultCaps {browser = chrome}}
        Firefox -> defaultConfig {wdCapabilities = defaultCaps {browser = firefox}}
      chromeOpts = case arguments opts of
        Just args ->
          ["--" ++ T.unpack arg | arg <- args]
            ++ ["--headless" | headless opts == Just True]
        Nothing -> ["--headless" | headless opts == Just True]
   in case browser of
        Chrome ->
          baseConfig
            { wdCapabilities =
                (wdCapabilities baseConfig)
                  { additionalCaps =
                      [ ( "goog:chromeOptions",
                          object [("args", toJSON chromeOpts)]
                        )
                      ]
                  }
            }
        Firefox -> baseConfig

-- | Initialize a new WebDriver session
initializeSession :: Browser -> BrowserOptions -> IO SeleniumSession
initializeSession browser opts = do
  let config = createWebDriverConfig browser opts
  session <- runWD config getSession
  return $ SeleniumSession browser (Just session)

-- | Close the WebDriver session
closeSession :: SeleniumSession -> IO ()
closeSession (SeleniumSession _ Nothing) = return ()
closeSession (SeleniumSession _ (Just session)) = do
  runWD (defaultConfig {wdSession = Just session}) closeSession

-- | Navigate to URL
navigateToUrl :: SeleniumSession -> T.Text -> IO ()
navigateToUrl (SeleniumSession _ Nothing) _ =
  throwIO $ SeleniumError "No active session"
navigateToUrl (SeleniumSession _ (Just session)) url = do
  runWD (defaultConfig {wdSession = Just session}) $
    openPage (T.unpack url)

-- | Find element by locator strategy
findElementByLocator :: SeleniumSession -> LocatorStrategy -> Int -> IO Element
findElementByLocator (SeleniumSession _ Nothing) _ _ =
  throwIO $ SeleniumError "No active session"
findElementByLocator (SeleniumSession _ (Just session)) locator timeoutMs = do
  runWD (defaultConfig {wdSession = Just session}) $ do
    setImplicitWait timeoutMs
    findElem (locatorToBy locator)

-- | Click an element
clickElement :: SeleniumSession -> LocatorStrategy -> Int -> IO ()
clickElement session locator timeoutMs = do
  element <- findElementByLocator session locator timeoutMs
  case session of
    SeleniumSession _ (Just sess) ->
      runWD (defaultConfig {wdSession = Just sess}) $
        click element
    _ -> throwIO $ SeleniumError "No active session"

-- | Send keys to an element
sendKeysToElement :: SeleniumSession -> LocatorStrategy -> T.Text -> Int -> IO ()
sendKeysToElement session locator text timeoutMs = do
  element <- findElementByLocator session locator timeoutMs
  case session of
    SeleniumSession _ (Just sess) ->
      runWD (defaultConfig {wdSession = Just sess}) $
        sendKeys (T.unpack text) element
    _ -> throwIO $ SeleniumError "No active session"

-- | Get text content of an element
getElementText :: SeleniumSession -> LocatorStrategy -> Int -> IO T.Text
getElementText session locator timeoutMs = do
  element <- findElementByLocator session locator timeoutMs
  case session of
    SeleniumSession _ (Just sess) -> do
      text <-
        runWD (defaultConfig {wdSession = Just sess}) $
          getText element
      return $ T.pack text
    _ -> throwIO $ SeleniumError "No active session"

-- | Hover over an element
hoverElement :: SeleniumSession -> LocatorStrategy -> Int -> IO ()
hoverElement session locator timeoutMs = do
  element <- findElementByLocator session locator timeoutMs
  case session of
    SeleniumSession _ (Just sess) ->
      runWD (defaultConfig {wdSession = Just sess}) $
        moveToElement element
    _ -> throwIO $ SeleniumError "No active session"

-- | Drag and drop between elements
dragAndDropElements :: SeleniumSession -> LocatorStrategy -> LocatorStrategy -> Int -> IO ()
dragAndDropElements session sourceLocator targetLocator timeoutMs = do
  sourceElement <- findElementByLocator session sourceLocator timeoutMs
  targetElement <- findElementByLocator session targetLocator timeoutMs
  case session of
    SeleniumSession _ (Just sess) ->
      runWD (defaultConfig {wdSession = Just sess}) $
        dragAndDrop sourceElement targetElement
    _ -> throwIO $ SeleniumError "No active session"

-- | Double click an element
doubleClickElement :: SeleniumSession -> LocatorStrategy -> Int -> IO ()
doubleClickElement session locator timeoutMs = do
  element <- findElementByLocator session locator timeoutMs
  case session of
    SeleniumSession _ (Just sess) ->
      runWD (defaultConfig {wdSession = Just sess}) $
        doubleClick (Just element)
    _ -> throwIO $ SeleniumError "No active session"

-- | Right click an element
rightClickElement :: SeleniumSession -> LocatorStrategy -> Int -> IO ()
rightClickElement session locator timeoutMs = do
  element <- findElementByLocator session locator timeoutMs
  case session of
    SeleniumSession _ (Just sess) ->
      runWD (defaultConfig {wdSession = Just sess}) $
        contextClick (Just element)
    _ -> throwIO $ SeleniumError "No active session"

-- | Press a key
pressKey :: SeleniumSession -> T.Text -> IO ()
pressKey (SeleniumSession _ Nothing) _ =
  throwIO $ SeleniumError "No active session"
pressKey (SeleniumSession _ (Just session)) key = do
  runWD (defaultConfig {wdSession = Just session}) $
    sendRawKeys (T.unpack key)

-- | Upload file to input element
uploadFileToElement :: SeleniumSession -> LocatorStrategy -> T.Text -> Int -> IO ()
uploadFileToElement session locator filePath timeoutMs = do
  element <- findElementByLocator session locator timeoutMs
  case session of
    SeleniumSession _ (Just sess) ->
      runWD (defaultConfig {wdSession = Just sess}) $
        sendKeys (T.unpack filePath) element
    _ -> throwIO $ SeleniumError "No active session"

-- | Take screenshot and return base64 encoded data
takeScreenshot :: SeleniumSession -> Maybe T.Text -> IO T.Text
takeScreenshot (SeleniumSession _ Nothing) _ =
  throwIO $ SeleniumError "No active session"
takeScreenshot (SeleniumSession _ (Just session)) outputPath = do
  screenshotData <- runWD (defaultConfig {wdSession = Just session}) screenshot
  case outputPath of
    Just path -> do
      BS.writeFile (T.unpack path) screenshotData
      return $ T.pack $ T.unpack path ++ " saved"
    Nothing -> return $ T.decodeUtf8 $ B64.encode screenshotData
