{-# LANGUAGE OverloadedStrings #-}

module SessionTest (sessionTests) where

import Control.Concurrent.STM (readTVarIO)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.UUID.V4 as UUID4
import MCP.Selenium.Tools
import Test.Hspec

-- | Session management test cases
sessionTests :: Spec
sessionTests = describe "Session Management" $ do
  describe "SessionId" $ do
    it "can generate unique session IDs" $ do
      sessionId1 <- generateSessionId
      sessionId2 <- generateSessionId
      sessionId1 `shouldNotBe` sessionId2

  describe "SeleniumTools" $ do
    it "starts with empty session map" $ do
      tools <- createSeleniumTools
      sessions <- readTVarIO (sessionsVar tools)
      HashMap.size sessions `shouldBe` 0

    it "returns Nothing for non-existent sessions" $ do
      tools <- createSeleniumTools
      nonExistentId <- UUID4.nextRandom

      result <- lookupSession tools nonExistentId
      result `shouldBe` Nothing

    it "session management functions exist and can be called" $ do
      tools <- createSeleniumTools
      sessionId <- UUID4.nextRandom

      -- Test that removeSession doesn't crash on non-existent session
      removeSession tools sessionId

      -- Verify session still doesn't exist
      result <- lookupSession tools sessionId
      result `shouldBe` Nothing
