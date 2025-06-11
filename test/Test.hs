{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (decode, encode)
import qualified Data.UUID as UUID
import MCP.Selenium.Server
import MCP.Selenium.Tools (ClickElementParams (..), CloseSessionParams (..), DoubleClickParams (..), DragAndDropParams (..), FindElementParams (..), GetAvailableLogTypesParams (..), GetConsoleLogsParams (..), GetElementTextParams (..), GetInjectedConsoleLogsParams (..), GetSourceParams (..), HoverParams (..), InjectConsoleLoggerParams (..), NavigateParams (..), PressKeyParams (..), RightClickParams (..), SendKeysParams (..), SessionId, StartBrowserParams (..), TakeScreenshotParams (..), UploadFileParams (..), createSeleniumTools)
import MCP.Selenium.WebDriver
import SessionTest (sessionTests)
import Test.Hspec

-- | Dummy session ID for tests
dummySessionId :: SessionId
dummySessionId = UUID.nil

-- | Test suite for mcp-selenium
main :: IO ()
main = hspec $ do
  describe "MCP.Selenium.WebDriver" $ do
    describe "Browser" $ do
      it "can be encoded and decoded as JSON" $ do
        let browserType = Chrome
        decode (encode browserType) `shouldBe` Just browserType

      it "supports both Chrome and Firefox" $ do
        encode Chrome `shouldNotBe` encode Firefox

    describe "BrowserOptions" $ do
      it "can be encoded and decoded as JSON" $ do
        let browserOpts = BrowserOptions (Just True) (Just ["--no-sandbox"]) Nothing
        decode (encode browserOpts) `shouldBe` Just browserOpts

      it "handles empty options" $ do
        let browserOpts = BrowserOptions Nothing Nothing Nothing
        decode (encode browserOpts) `shouldBe` Just browserOpts

    describe "LocatorStrategy" $ do
      it "can be encoded and decoded as JSON" $ do
        let locator = ById "test-id"
        decode (encode locator) `shouldBe` Just locator

      it "supports all locator types" $ do
        let locators =
              [ ById "test",
                ByCss ".test",
                ByXPath "//div",
                ByName "test",
                ByTag "div",
                ByClass "test"
              ]
        mapM_ (\loc -> decode (encode loc) `shouldBe` Just loc) locators

  describe "MCP.Selenium.Tools" $ do
    describe "StartBrowserParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = StartBrowserParams Chrome (Just $ BrowserOptions (Just True) Nothing Nothing) Nothing
        decode (encode params) `shouldBe` Just params

    describe "NavigateParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = NavigateParams dummySessionId "https://example.com"
        decode (encode params) `shouldBe` Just params

    describe "FindElementParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = FindElementParams dummySessionId (Just "id") "test-element" (Just 5000)
        decode (encode params) `shouldBe` Just params

    describe "ClickElementParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = ClickElementParams dummySessionId "css" ".button" Nothing
        decode (encode params) `shouldBe` Just params

    describe "SendKeysParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = SendKeysParams dummySessionId "name" "username" "testuser" (Just 3000)
        decode (encode params) `shouldBe` Just params

    describe "GetElementTextParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = GetElementTextParams dummySessionId "xpath" "//h1" Nothing
        decode (encode params) `shouldBe` Just params

    describe "HoverParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = HoverParams dummySessionId "class" "menu-item" (Just 2000)
        decode (encode params) `shouldBe` Just params

    describe "DragAndDropParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = DragAndDropParams dummySessionId "id" "draggable" "id" "droppable" Nothing
        decode (encode params) `shouldBe` Just params

    describe "DoubleClickParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = DoubleClickParams dummySessionId "tag" "button" (Just 1000)
        decode (encode params) `shouldBe` Just params

    describe "RightClickParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = RightClickParams dummySessionId "css" ".context-menu" Nothing
        decode (encode params) `shouldBe` Just params

    describe "PressKeyParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = PressKeyParams dummySessionId "Enter"
        decode (encode params) `shouldBe` Just params

    describe "UploadFileParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = UploadFileParams dummySessionId "id" "file-input" "/path/to/file.txt" (Just 5000)
        decode (encode params) `shouldBe` Just params

    describe "TakeScreenshotParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = TakeScreenshotParams dummySessionId
        decode (encode params) `shouldBe` Just params

      it "handles constructor with session_id" $ do
        let params = TakeScreenshotParams dummySessionId
        decode (encode params) `shouldBe` Just params

    describe "CloseSessionParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = CloseSessionParams dummySessionId
        decode (encode params) `shouldBe` Just params

    describe "GetConsoleLogsParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = GetConsoleLogsParams dummySessionId (Just "SEVERE") (Just 10)
        decode (encode params) `shouldBe` Just params

      it "handles optional fields" $ do
        let params = GetConsoleLogsParams dummySessionId Nothing Nothing
        decode (encode params) `shouldBe` Just params

    describe "GetAvailableLogTypesParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = GetAvailableLogTypesParams dummySessionId
        decode (encode params) `shouldBe` Just params

    describe "InjectConsoleLoggerParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = InjectConsoleLoggerParams dummySessionId (Just 30000)
        decode (encode params) `shouldBe` Just params

      it "handles optional timeout" $ do
        let params = InjectConsoleLoggerParams dummySessionId Nothing
        decode (encode params) `shouldBe` Just params

    describe "GetInjectedConsoleLogsParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = GetInjectedConsoleLogsParams dummySessionId (Just True)
        decode (encode params) `shouldBe` Just params

      it "handles optional clear flag" $ do
        let params = GetInjectedConsoleLogsParams dummySessionId Nothing
        decode (encode params) `shouldBe` Just params

    describe "GetSourceParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = GetSourceParams dummySessionId
        decode (encode params) `shouldBe` Just params

    describe "createSeleniumTools" $ do
      it "creates tools instance without errors" $ do
        tools <- createSeleniumTools
        -- Basic check that it doesn't throw
        tools `seq` return ()

  describe "MCP.Selenium.Server" $ do
    describe "createSeleniumServer" $ do
      it "creates server without errors" $ do
        server <- createSeleniumServer
        -- Basic check that it doesn't throw
        server `seq` return ()

  sessionTests
