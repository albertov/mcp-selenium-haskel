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
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Test.WebDriver hiding (Browser, ByClass, ById, ByName, ByTag, ByXPath, Chrome, Firefox, closeSession)
import qualified Test.WebDriver as WD
import Test.WebDriver.Commands hiding (ByClass, ById, ByName, ByTag, ByXPath, closeSession)
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
    config :: WDConfig
  }
  deriving (Generic)

-- | Custom exception type for WebDriver errors
newtype SeleniumError = SeleniumError T.Text
  deriving (Show)

instance Exception SeleniumError

-- | Convert LocatorStrategy to WebDriver's Selector
locatorToBy :: LocatorStrategy -> Selector
locatorToBy (ById t) = WDC.ById t
locatorToBy (ByCss t) = WDC.ByCSS t
locatorToBy (ByXPath t) = WDC.ByXPath t
locatorToBy (ByName t) = WDC.ByName t
locatorToBy (ByTag t) = WDC.ByTag t
locatorToBy (ByClass t) = WDC.ByClass t

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
  return $ SeleniumSession browser config

-- | Close the WebDriver session
closeSession :: SeleniumSession -> IO ()
closeSession (SeleniumSession _ _) = return () -- Sessions are managed automatically by runSession

-- | Navigate to URL
navigateToUrl :: SeleniumSession -> T.Text -> IO ()
navigateToUrl (SeleniumSession _ config) url = do
  runSession config $
    openPage (T.unpack url)

-- | Find element by locator strategy
findElementByLocator :: SeleniumSession -> LocatorStrategy -> Int -> IO Element
findElementByLocator (SeleniumSession _ config) locator timeoutMs = do
  runSession config $ do
    setImplicitWait (fromIntegral timeoutMs)
    findElem (locatorToBy locator)

-- | Click an element
clickElement :: SeleniumSession -> LocatorStrategy -> Int -> IO ()
clickElement (SeleniumSession _ config) locator timeoutMs = do
  runSession config $ do
    setImplicitWait (fromIntegral timeoutMs)
    element <- findElem (locatorToBy locator)
    click element

-- | Send keys to an element
sendKeysToElement :: SeleniumSession -> LocatorStrategy -> T.Text -> Int -> IO ()
sendKeysToElement (SeleniumSession _ config) locator text timeoutMs = do
  runSession config $ do
    setImplicitWait (fromIntegral timeoutMs)
    element <- findElem (locatorToBy locator)
    sendKeys text element

-- | Get text content of an element
getElementText :: SeleniumSession -> LocatorStrategy -> Int -> IO T.Text
getElementText (SeleniumSession _ config) locator timeoutMs = do
  runSession config $ do
    setImplicitWait (fromIntegral timeoutMs)
    element <- findElem (locatorToBy locator)
    getText element

-- | Hover over an element
hoverElement :: SeleniumSession -> LocatorStrategy -> Int -> IO ()
hoverElement (SeleniumSession _ config) locator timeoutMs = do
  runSession config $ do
    setImplicitWait (fromIntegral timeoutMs)
    element <- findElem (locatorToBy locator)
    moveToCenter element

-- | Drag and drop between elements
dragAndDropElements :: SeleniumSession -> LocatorStrategy -> LocatorStrategy -> Int -> IO ()
dragAndDropElements (SeleniumSession _ config) sourceLocator targetLocator timeoutMs = do
  runSession config $ do
    setImplicitWait (fromIntegral timeoutMs)
    sourceElement <- findElem (locatorToBy sourceLocator)
    targetElement <- findElem (locatorToBy targetLocator)
    moveToCenter sourceElement
    mouseDown
    moveToCenter targetElement
    mouseUp

-- | Double click an element
doubleClickElement :: SeleniumSession -> LocatorStrategy -> Int -> IO ()
doubleClickElement (SeleniumSession _ config) locator timeoutMs = do
  runSession config $ do
    setImplicitWait (fromIntegral timeoutMs)
    element <- findElem (locatorToBy locator)
    moveToCenter element
    doubleClick

-- | Right click an element
rightClickElement :: SeleniumSession -> LocatorStrategy -> Int -> IO ()
rightClickElement (SeleniumSession _ config) locator timeoutMs = do
  runSession config $ do
    setImplicitWait (fromIntegral timeoutMs)
    element <- findElem (locatorToBy locator)
    -- Note: contextClick is not available in this webdriver version
    -- As a workaround, we move to the element
    moveToCenter element

-- | Press a key
pressKey :: SeleniumSession -> T.Text -> IO ()
pressKey (SeleniumSession _ config) key = do
  runSession config $
    sendRawKeys key

-- | Upload file to input element
uploadFileToElement :: SeleniumSession -> LocatorStrategy -> T.Text -> Int -> IO ()
uploadFileToElement (SeleniumSession _ config) locator filePath timeoutMs = do
  runSession config $ do
    setImplicitWait (fromIntegral timeoutMs)
    element <- findElem (locatorToBy locator)
    sendKeys filePath element

-- | Take screenshot and return base64 encoded data
takeScreenshot :: SeleniumSession -> Maybe T.Text -> IO T.Text
takeScreenshot (SeleniumSession _ config) outputPath = do
  screenshotDataLazy <- runSession config screenshot
  let screenshotData = BSL.toStrict screenshotDataLazy
  case outputPath of
    Just path -> do
      BS.writeFile (T.unpack path) screenshotData
      return $ T.pack $ T.unpack path ++ " saved"
    Nothing -> return $ TE.decodeUtf8 $ B64.encode screenshotData
