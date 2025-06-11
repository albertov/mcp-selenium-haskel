-- | Shared utility functions for MCP Selenium
module MCP.Selenium.Utils
  ( debugLog,
  )
where

import System.IO (hFlush, hPutStrLn, stderr)

-- | Debug logging helper that writes to stderr and flushes
debugLog :: String -> IO ()
debugLog msg = do
  hPutStrLn stderr msg
  hFlush stderr
