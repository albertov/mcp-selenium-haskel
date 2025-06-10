{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module MCP.Selenium.Server
  ( runMCPServer,
    ServerConfig (..),
    defaultServerConfig,
  )
where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

data ServerConfig = ServerConfig
  { serverName :: Text,
    serverVersion :: Text,
    webDriverUrl :: Text,
    timeoutSeconds :: Int
  }
  deriving (Show, Generic)

instance ToJSON ServerConfig

instance FromJSON ServerConfig

defaultServerConfig :: ServerConfig
defaultServerConfig =
  ServerConfig
    { serverName = "mcp-selenium-haskell",
      serverVersion = "1.0.0",
      webDriverUrl = "http://localhost:4444/wd/hub",
      timeoutSeconds = 30
    }

runMCPServer :: ServerConfig -> IO ()
runMCPServer config = do
  putStrLn $ "Starting MCP Selenium server: " <> T.unpack (serverName config)
  putStrLn $ "Version: " <> T.unpack (serverVersion config)
  putStrLn $ "WebDriver URL: " <> T.unpack (webDriverUrl config)
  putStrLn $ "Timeout: " <> show (timeoutSeconds config) <> " seconds"
  putStrLn "Server ready to handle MCP requests..."
  -- For now, just run indefinitely
  -- In a real implementation, this would handle MCP protocol messages
  forever $ do
    threadDelay 1000000 -- Sleep for 1 second
