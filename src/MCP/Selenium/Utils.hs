-- |
-- Module: MCP.Selenium.Utils
-- Description: Shared utility functions for MCP Selenium
--
-- This module provides common utility functions used throughout the MCP Selenium implementation.
-- It includes logging helpers and other shared functionality.
--
-- = Logging Utilities
--
-- The module provides debug logging functionality that writes to stderr with automatic flushing.
-- This ensures that debug messages are immediately visible even in buffered output scenarios.
--
-- = Example Usage
--
-- @
-- import MCP.Selenium.Utils
--
-- main :: IO ()
-- main = do
--   debugLog "Starting selenium server"
--   -- ... application logic ...
--   debugLog "Server ready"
-- @
--
-- = Thread Safety
--
-- The logging functions are thread-safe and can be called concurrently from multiple threads.
module MCP.Selenium.Utils
  ( -- * Logging
    debugLog,
  )
where

import System.IO (hFlush, hPutStrLn, stderr)

-- | Debug logging helper that writes to stderr and flushes
debugLog :: String -> IO ()
debugLog msg = do
  hPutStrLn stderr msg
  hFlush stderr
