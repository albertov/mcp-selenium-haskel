-- | Main executable entry point for the MCP Selenium Server
module Main (main) where

import MCP.Selenium.Server (runSeleniumServer)

-- | Main function that starts the MCP Selenium server
main :: IO ()
main = runSeleniumServer
