{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (decode, encode)
import MCP.Selenium.Server
import MCP.Selenium.Tools
import MCP.Selenium.WebDriver
import Test.Hspec

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
        let browserOpts = BrowserOptions (Just True) (Just ["--no-sandbox"])
        decode (encode browserOpts) `shouldBe` Just browserOpts

      it "handles empty options" $ do
        let browserOpts = BrowserOptions Nothing Nothing
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
        let params = StartBrowserParams Chrome (Just $ BrowserOptions (Just True) Nothing) Nothing Nothing
        decode (encode params) `shouldBe` Just params

    describe "NavigateParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = NavigateParams "https://example.com"
        decode (encode params) `shouldBe` Just params

    describe "FindElementParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = FindElementParams (Just "id") Nothing "test-element" (Just 5000)
        decode (encode params) `shouldBe` Just params

    describe "ClickElementParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = ClickElementParams "css" ".button" Nothing
        decode (encode params) `shouldBe` Just params

    describe "SendKeysParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = SendKeysParams "name" "username" "testuser" (Just 3000)
        decode (encode params) `shouldBe` Just params

    describe "GetElementTextParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = GetElementTextParams "xpath" "//h1" Nothing
        decode (encode params) `shouldBe` Just params

    describe "HoverParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = HoverParams "class" "menu-item" (Just 2000)
        decode (encode params) `shouldBe` Just params

    describe "DragAndDropParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = DragAndDropParams "id" "draggable" "id" "droppable" Nothing
        decode (encode params) `shouldBe` Just params

    describe "DoubleClickParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = DoubleClickParams "tag" "button" (Just 1000)
        decode (encode params) `shouldBe` Just params

    describe "RightClickParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = RightClickParams "css" ".context-menu" Nothing
        decode (encode params) `shouldBe` Just params

    describe "PressKeyParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = PressKeyParams "Enter"
        decode (encode params) `shouldBe` Just params

    describe "UploadFileParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = UploadFileParams "id" "file-input" "/path/to/file.txt" (Just 5000)
        decode (encode params) `shouldBe` Just params

    describe "TakeScreenshotParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = TakeScreenshotParams (Just "/tmp/screenshot.png")
        decode (encode params) `shouldBe` Just params

      it "handles optional output path" $ do
        let params = TakeScreenshotParams Nothing
        decode (encode params) `shouldBe` Just params

    describe "CloseSessionParams" $ do
      it "can be encoded and decoded as JSON" $ do
        let params = CloseSessionParams
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
