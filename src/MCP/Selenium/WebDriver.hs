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
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Test.WebDriver hiding (Browser, ByClass, ById, ByName, ByTag, ByXPath, Chrome, Firefox)
import qualified Test.WebDriver as WD
import Test.WebDriver.Commands hiding (ByClass, ById, ByName, ByTag, ByXPath)
import qualified Test.WebDriver.Commands as WDC
import Test.WebDriver.Config
import Test.WebDriver.Session (WDSession)

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

-- | Convert LocatorStrategy to WebDriver's Selector
locatorToBy :: LocatorStrategy -> Selector
locatorToBy (ById t) = WDC.ById (T.unpack t)
locatorToBy (ByCss t) = WDC.ByCSS (T.unpack t)
locatorToBy (ByXPath t) = WDC.ByXPath (T.unpack t)
locatorToBy (ByName t) = WDC.ByName (T.unpack t)
locatorToBy (ByTag t) = WDC.ByTag (T.unpack t)
locatorToBy (ByClass t) = WDC.ByClass (T.unpack t)

-- | Create WebDriver config for given browser and options
createWebDriverConfig :: Browser -> BrowserOptions -> WDConfig
createWebDriverConfig browser opts =
  let baseConfig = case browser of
        Chrome -> defaultConfig {wdCapabilities = defaultCaps {browser = WD.chrome}}
        Firefox -> defaultConfig {wdCapabilities = defaultCaps {browser = WD.firefox}}
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
  session <- runWD config $ createSession (wdCapabilities config)
  return $ SeleniumSession browser (Just session)

-- | Close the WebDriver session
closeSession :: SeleniumSession -> IO ()
closeSession (SeleniumSession _ Nothing) = return ()
closeSession (SeleniumSession _ (Just session)) = do
  runWD defaultConfig $ withSession session WDC.closeSession

-- | Navigate to URL
navigateToUrl :: SeleniumSession -> T.Text -> IO ()
navigateToUrl (SeleniumSession _ Nothing) _ =
  throwIO $ SeleniumError "No active session"
navigateToUrl (SeleniumSession _ (Just session)) url = do
  runWD defaultConfig $
    withSession session $
      openPage (T.unpack url)

-- | Find element by locator strategy
findElementByLocator :: SeleniumSession -> LocatorStrategy -> Int -> IO Element
findElementByLocator (SeleniumSession _ Nothing) _ _ =
  throwIO $ SeleniumError "No active session"
findElementByLocator (SeleniumSession _ (Just session)) locator timeoutMs = do
  runWD defaultConfig $ withSession session $ do
    setImplicitWait timeoutMs
    findElem (locatorToBy locator)

-- | Click an element
clickElement :: SeleniumSession -> LocatorStrategy -> Int -> IO ()
clickElement session locator timeoutMs = do
  element <- findElementByLocator session locator timeoutMs
  case session of
    SeleniumSession _ (Just sess) ->
      runWD defaultConfig $
        withSession sess $
          click element
    _ -> throwIO $ SeleniumError "No active session"

-- | Send keys to an element
sendKeysToElement :: SeleniumSession -> LocatorStrategy -> T.Text -> Int -> IO ()
sendKeysToElement session locator text timeoutMs = do
  element <- findElementByLocator session locator timeoutMs
  case session of
    SeleniumSession _ (Just sess) ->
      runWD defaultConfig $
        withSession sess $
          sendKeys (T.unpack text) element
    _ -> throwIO $ SeleniumError "No active session"

-- | Get text content of an element
getElementText :: SeleniumSession -> LocatorStrategy -> Int -> IO T.Text
getElementText session locator timeoutMs = do
  element <- findElementByLocator session locator timeoutMs
  case session of
    SeleniumSession _ (Just sess) -> do
      text <-
        runWD defaultConfig $
          withSession sess $
            getText element
      return $ T.pack text
    _ -> throwIO $ SeleniumError "No active session"

-- | Hover over an element
hoverElement :: SeleniumSession -> LocatorStrategy -> Int -> IO ()
hoverElement session locator timeoutMs = do
  element <- findElementByLocator session locator timeoutMs
  case session of
    SeleniumSession _ (Just sess) ->
      runWD defaultConfig $
        withSession sess $
          moveToCenter element
    _ -> throwIO $ SeleniumError "No active session"

-- | Drag and drop between elements
dragAndDropElements :: SeleniumSession -> LocatorStrategy -> LocatorStrategy -> Int -> IO ()
dragAndDropElements session sourceLocator targetLocator timeoutMs = do
  sourceElement <- findElementByLocator session sourceLocator timeoutMs
  targetElement <- findElementByLocator session targetLocator timeoutMs
  case session of
    SeleniumSession _ (Just sess) ->
      runWD defaultConfig $
        withSession sess $
          dragAndDrop sourceElement targetElement
    _ -> throwIO $ SeleniumError "No active session"

-- | Double click an element
doubleClickElement :: SeleniumSession -> LocatorStrategy -> Int -> IO ()
doubleClickElement session locator timeoutMs = do
  element <- findElementByLocator session locator timeoutMs
  case session of
    SeleniumSession _ (Just sess) ->
      runWD defaultConfig $
        withSession sess $
          doubleClick (Just element)
    _ -> throwIO $ SeleniumError "No active session"

-- | Right click an element
rightClickElement :: SeleniumSession -> LocatorStrategy -> Int -> IO ()
rightClickElement session locator timeoutMs = do
  element <- findElementByLocator session locator timeoutMs
  case session of
    SeleniumSession _ (Just sess) ->
      runWD defaultConfig $
        withSession sess $
          contextClick (Just element)
    _ -> throwIO $ SeleniumError "No active session"

-- | Press a key
pressKey :: SeleniumSession -> T.Text -> IO ()
pressKey (SeleniumSession _ Nothing) _ =
  throwIO $ SeleniumError "No active session"
pressKey (SeleniumSession _ (Just session)) key = do
  runWD defaultConfig $
    withSession session $
      sendRawKeys (T.unpack key)

-- | Upload file to input element
uploadFileToElement :: SeleniumSession -> LocatorStrategy -> T.Text -> Int -> IO ()
uploadFileToElement session locator filePath timeoutMs = do
  element <- findElementByLocator session locator timeoutMs
  case session of
    SeleniumSession _ (Just sess) ->
      runWD defaultConfig $
        withSession sess $
          sendKeys (T.unpack filePath) element
    _ -> throwIO $ SeleniumError "No active session"

-- | Take screenshot and return base64 encoded data
takeScreenshot :: SeleniumSession -> Maybe T.Text -> IO T.Text
takeScreenshot (SeleniumSession _ Nothing) _ =
  throwIO $ SeleniumError "No active session"
takeScreenshot (SeleniumSession _ (Just session)) outputPath = do
  screenshotData <- runWD defaultConfig $ withSession session screenshot
  case outputPath of
    Just path -> do
      BS.writeFile (T.unpack path) screenshotData
      return $ T.pack $ T.unpack path ++ " saved"
    Nothing -> return $ TE.decodeUtf8 $ B64.encode screenshotData
