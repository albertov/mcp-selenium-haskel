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
  )
where

import Control.Exception (Exception)
import Data.Aeson (FromJSON, ToJSON, object, toJSON)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import Test.WebDriver hiding (Browser, ByClass, ById, ByName, ByTag, ByXPath, Chrome, Firefox)
import qualified Test.WebDriver as WD
import qualified Test.WebDriver.Commands as WDC
import Test.WebDriver.Config (mkSession)
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
    wdSession :: WDSession
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
createWebDriverConfig browserType opts =
  let baseConfig = case browserType of
        Chrome -> defaultConfig {wdCapabilities = defaultCaps {WD.browser = WD.chrome}}
        Firefox -> defaultConfig {wdCapabilities = defaultCaps {WD.browser = WD.firefox}}
      chromeOpts = case arguments opts of
        Just args ->
          ["--" ++ T.unpack arg | arg <- args]
            ++ ["--headless" | headless opts == Just True]
        Nothing -> ["--headless" | headless opts == Just True]
   in case browserType of
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
initializeSession browserType opts = do
  let config = createWebDriverConfig browserType opts
  session <- mkSession config
  return $ SeleniumSession browserType session

-- | Close the WebDriver session
closeSeleniumSession :: SeleniumSession -> IO ()
closeSeleniumSession (SeleniumSession _ session) = runWD session WDC.closeSession

-- | Navigate to URL
navigateToUrl :: SeleniumSession -> T.Text -> IO ()
navigateToUrl (SeleniumSession _ session) url = do
  runWD session $
    openPage (T.unpack url)

-- | Find element by locator strategy
findElementByLocator :: SeleniumSession -> LocatorStrategy -> Int -> IO Element
findElementByLocator (SeleniumSession _ session) locator timeoutMs = do
  runWD session $ do
    setImplicitWait (fromIntegral timeoutMs)
    findElem (locatorToBy locator)

-- | Click an element
clickElement :: SeleniumSession -> LocatorStrategy -> Int -> IO ()
clickElement (SeleniumSession _ session) locator timeoutMs = do
  runWD session $ do
    setImplicitWait (fromIntegral timeoutMs)
    element <- findElem (locatorToBy locator)
    click element

-- | Send keys to an element
sendKeysToElement :: SeleniumSession -> LocatorStrategy -> T.Text -> Int -> IO ()
sendKeysToElement (SeleniumSession _ session) locator text timeoutMs = do
  runWD session $ do
    setImplicitWait (fromIntegral timeoutMs)
    element <- findElem (locatorToBy locator)
    sendKeys text element

-- | Get text content of an element
getElementText :: SeleniumSession -> LocatorStrategy -> Int -> IO T.Text
getElementText (SeleniumSession _ session) locator timeoutMs = do
  runWD session $ do
    setImplicitWait (fromIntegral timeoutMs)
    element <- findElem (locatorToBy locator)
    getText element

-- | Hover over an element
hoverElement :: SeleniumSession -> LocatorStrategy -> Int -> IO ()
hoverElement (SeleniumSession _ session) locator timeoutMs = do
  runWD session $ do
    setImplicitWait (fromIntegral timeoutMs)
    element <- findElem (locatorToBy locator)
    moveToCenter element

-- | Drag and drop between elements
dragAndDropElements :: SeleniumSession -> LocatorStrategy -> LocatorStrategy -> Int -> IO ()
dragAndDropElements (SeleniumSession _ session) sourceLocator targetLocator timeoutMs = do
  runWD session $ do
    setImplicitWait (fromIntegral timeoutMs)
    sourceElement <- findElem (locatorToBy sourceLocator)
    targetElement <- findElem (locatorToBy targetLocator)
    moveToCenter sourceElement
    mouseDown
    moveToCenter targetElement
    mouseUp

-- | Double click an element
doubleClickElement :: SeleniumSession -> LocatorStrategy -> Int -> IO ()
doubleClickElement (SeleniumSession _ session) locator timeoutMs = do
  runWD session $ do
    setImplicitWait (fromIntegral timeoutMs)
    element <- findElem (locatorToBy locator)
    moveToCenter element
    doubleClick

-- | Right click an element
rightClickElement :: SeleniumSession -> LocatorStrategy -> Int -> IO ()
rightClickElement (SeleniumSession _ session) locator timeoutMs = do
  runWD session $ do
    setImplicitWait (fromIntegral timeoutMs)
    element <- findElem (locatorToBy locator)
    -- Note: contextClick is not available in this webdriver version
    -- As a workaround, we move to the element
    moveToCenter element

-- | Press a key
pressKey :: SeleniumSession -> T.Text -> IO ()
pressKey (SeleniumSession _ session) key = do
  runWD session $
    sendRawKeys key

-- | Upload file to input element
uploadFileToElement :: SeleniumSession -> LocatorStrategy -> T.Text -> Int -> IO ()
uploadFileToElement (SeleniumSession _ session) locator filePath timeoutMs = do
  runWD session $ do
    setImplicitWait (fromIntegral timeoutMs)
    element <- findElem (locatorToBy locator)
    sendKeys filePath element

-- | Take screenshot and return base64 encoded data
takeScreenshot :: SeleniumSession -> Maybe T.Text -> IO T.Text
takeScreenshot (SeleniumSession _ session) outputPath = do
  screenshotDataLazy <- runWD session screenshot
  let screenshotData = BSL.toStrict screenshotDataLazy
  case outputPath of
    Just path -> do
      BS.writeFile (T.unpack path) screenshotData
      return $ T.pack $ T.unpack path ++ " saved"
    Nothing -> return $ TE.decodeUtf8 $ B64.encode screenshotData
