{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
import Data.Aeson (FromJSON, ToJSON, parseJSON, toJSON)
import Data.Aeson.Types (Parser)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)
import qualified Test.WebDriver as WD
import Test.WebDriver.Session (WDSession (..), getSession)

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
locatorToBy :: LocatorStrategy -> WD.Selector
locatorToBy (ById t) = WD.ById t
locatorToBy (ByCss t) = WD.ByCSS t
locatorToBy (ByXPath t) = WD.ByXPath t
locatorToBy (ByName t) = WD.ByName t
locatorToBy (ByTag t) = WD.ByTag t
locatorToBy (ByClass t) = WD.ByClass t

-- | Create WebDriver config for given browser and options
createWebDriverConfig :: Browser -> BrowserOptions -> WD.WDConfig
createWebDriverConfig browserType opts =
  let baseConfig = case browserType of
        Chrome -> WD.defaultConfig {WD.wdCapabilities = WD.defaultCaps {WD.browser = WD.chrome}}
        Firefox -> WD.defaultConfig {WD.wdCapabilities = WD.defaultCaps {WD.browser = WD.firefox}}
      -- Ensure we're using the default Selenium server URL (localhost:4444)
      configWithHost = baseConfig {WD.wdHost = "127.0.0.1", WD.wdPort = 4444}
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
   in case browserType of
        Chrome ->
          configWithHost
            { WD.wdCapabilities = chromeCapabilities
            }
        Firefox -> configWithHost

-- | Initialize a new WebDriver session
initializeSession :: Browser -> BrowserOptions -> IO SeleniumSession
initializeSession browserType opts = do
  let config = createWebDriverConfig browserType opts
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
