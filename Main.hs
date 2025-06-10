{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import MCP.Selenium.Server (defaultServerConfig, runMCPServer)

main :: IO ()
main = do
  putStrLn "Starting MCP Selenium Haskell Server..."
  runMCPServer defaultServerConfig
