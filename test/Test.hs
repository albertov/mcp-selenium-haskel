{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Text (Text)
import qualified Data.Text as T
import MCP.Selenium.Server (ServerConfig (..), defaultServerConfig)
import MCP.Selenium.WebDriver (SeleniumAction (..), SeleniumError (..))
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "MCP.Selenium.Server" $ do
    describe "ServerConfig" $ do
      it "has sensible defaults" $ do
        let config = defaultServerConfig
        serverName config `shouldBe` "mcp-selenium-haskell"
        serverVersion config `shouldBe` "1.0.0"
        webDriverUrl config `shouldBe` "http://localhost:4444/wd/hub"
        timeoutSeconds config `shouldBe` 30

  describe "MCP.Selenium.WebDriver" $ do
    describe "SeleniumAction" $ do
      it "can create StartBrowser action" $ do
        let action = StartBrowser (Just "chrome") (Just True)
        show action `shouldContain` "StartBrowser"

      it "can create NavigateTo action" $ do
        let action = NavigateTo "https://example.com"
        show action `shouldContain` "NavigateTo"

      it "can create FindElement action" $ do
        let action = FindElement "#test-element"
        show action `shouldContain` "FindElement"

      it "can create ClickElement action" $ do
        let action = ClickElement ".button"
        show action `shouldContain` "ClickElement"

      it "can create TypeText action" $ do
        let action = TypeText "input[name='username']" "testuser"
        show action `shouldContain` "TypeText"

      it "can create GetText action" $ do
        let action = GetText ".text-content"
        show action `shouldContain` "GetText"

      it "can create GetAttribute action" $ do
        let action = GetAttribute "a" "href"
        show action `shouldContain` "GetAttribute"

      it "can create TakeScreenshot action" $ do
        let action = TakeScreenshot
        show action `shouldContain` "TakeScreenshot"

      it "can create CloseBrowser action" $ do
        let action = CloseBrowser
        show action `shouldContain` "CloseBrowser"

    describe "SeleniumError" $ do
      it "can create WebDriverError" $ do
        let err = WebDriverError "Connection failed"
        show err `shouldContain` "WebDriverError"

      it "can create ConfigError" $ do
        let err = ConfigError "Invalid configuration"
        show err `shouldContain` "ConfigError"

      it "can create ElementNotFound error" $ do
        let err = ElementNotFound "Element not found"
        show err `shouldContain` "ElementNotFound"

      it "can create TimeoutError" $ do
        let err = TimeoutError "Operation timed out"
        show err `shouldContain` "TimeoutError"

  describe "Integration tests" $ do
    describe "Server configuration validation" $ do
      it "accepts valid server config" $ do
        let config =
              ServerConfig
                { serverName = "test-server",
                  serverVersion = "0.1.0",
                  webDriverUrl = "http://localhost:4444",
                  timeoutSeconds = 10
                }
        serverName config `shouldBe` "test-server"
        timeoutSeconds config `shouldBe` 10

      it "handles different browser types" $ do
        let chromeAction = StartBrowser (Just "chrome") (Just True)
            firefoxAction = StartBrowser (Just "firefox") (Just False)
        show chromeAction `shouldContain` "chrome"
        show firefoxAction `shouldContain` "firefox"
