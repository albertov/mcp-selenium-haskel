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
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.=))
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import MCP.Selenium.Tools (SeleniumTool (..), ToolInput (..), availableTools, executeTool)
import qualified MCP.Selenium.Tools as SeleniumTools
import Network.MCP.Server (ServerInfo (..), Tool (..), ToolHandler, mkServer, runStdIOServer)
import Network.MCP.Types (ToolCall (..), ToolResult (..))

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
  let serverInfo =
        ServerInfo
          { serverName = serverName config,
            serverVersion = serverVersion config
          }

      tools = map seleniumToolToMCPTool availableTools

      server = mkServer serverInfo tools (seleniumToolHandler config)

  putStrLn $ "Starting MCP Selenium server: " <> T.unpack (serverName config)
  putStrLn $ "Version: " <> T.unpack (serverVersion config)
  putStrLn $ "WebDriver URL: " <> T.unpack (webDriverUrl config)
  putStrLn $ "Timeout: " <> show (timeoutSeconds config) <> " seconds"
  putStrLn "Server ready to handle MCP requests..."

  runStdIOServer server

-- Convert Selenium tools to MCP tools
seleniumToolToMCPTool :: SeleniumTool -> Tool
seleniumToolToMCPTool StartBrowserTool =
  Tool
    { toolName = "start_browser",
      toolDescription = "Start a new browser session with optional browser type and headless mode",
      toolInputSchema =
        object
          [ "type" .= ("object" :: Text),
            "properties"
              .= object
                [ "browser"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "enum" .= (["chrome", "firefox"] :: [Text]),
                        "description" .= ("Browser type to start" :: Text)
                      ],
                  "headless"
                    .= object
                      [ "type" .= ("boolean" :: Text),
                        "description" .= ("Run browser in headless mode" :: Text)
                      ]
                ]
          ]
    }
seleniumToolToMCPTool NavigateToTool =
  Tool
    { toolName = "navigate_to",
      toolDescription = "Navigate to a specific URL",
      toolInputSchema =
        object
          [ "type" .= ("object" :: Text),
            "properties"
              .= object
                [ "url"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "description" .= ("URL to navigate to" :: Text)
                      ]
                ],
            "required" .= (["url"] :: [Text])
          ]
    }
seleniumToolToMCPTool FindElementTool =
  Tool
    { toolName = "find_element",
      toolDescription = "Find an element on the page using CSS selector",
      toolInputSchema =
        object
          [ "type" .= ("object" :: Text),
            "properties"
              .= object
                [ "selector"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "description" .= ("CSS selector to find the element" :: Text)
                      ]
                ],
            "required" .= (["selector"] :: [Text])
          ]
    }
seleniumToolToMCPTool ClickElementTool =
  Tool
    { toolName = "click_element",
      toolDescription = "Click on an element identified by CSS selector",
      toolInputSchema =
        object
          [ "type" .= ("object" :: Text),
            "properties"
              .= object
                [ "selector"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "description" .= ("CSS selector of the element to click" :: Text)
                      ]
                ],
            "required" .= (["selector"] :: [Text])
          ]
    }
seleniumToolToMCPTool TypeTextTool =
  Tool
    { toolName = "type_text",
      toolDescription = "Type text into an element identified by CSS selector",
      toolInputSchema =
        object
          [ "type" .= ("object" :: Text),
            "properties"
              .= object
                [ "selector"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "description" .= ("CSS selector of the element to type into" :: Text)
                      ],
                  "text"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "description" .= ("Text to type into the element" :: Text)
                      ]
                ],
            "required" .= (["selector", "text"] :: [Text])
          ]
    }
seleniumToolToMCPTool GetTextTool =
  Tool
    { toolName = "get_text",
      toolDescription = "Get text content from an element identified by CSS selector",
      toolInputSchema =
        object
          [ "type" .= ("object" :: Text),
            "properties"
              .= object
                [ "selector"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "description" .= ("CSS selector of the element to get text from" :: Text)
                      ]
                ],
            "required" .= (["selector"] :: [Text])
          ]
    }
seleniumToolToMCPTool GetAttributeTool =
  Tool
    { toolName = "get_attribute",
      toolDescription = "Get attribute value from an element identified by CSS selector",
      toolInputSchema =
        object
          [ "type" .= ("object" :: Text),
            "properties"
              .= object
                [ "selector"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "description" .= ("CSS selector of the element" :: Text)
                      ],
                  "attribute"
                    .= object
                      [ "type" .= ("string" :: Text),
                        "description" .= ("Attribute name to get" :: Text)
                      ]
                ],
            "required" .= (["selector", "attribute"] :: [Text])
          ]
    }
seleniumToolToMCPTool TakeScreenshotTool =
  Tool
    { toolName = "take_screenshot",
      toolDescription = "Take a screenshot of the current page",
      toolInputSchema =
        object
          [ "type" .= ("object" :: Text),
            "properties" .= object []
          ]
    }
seleniumToolToMCPTool CloseBrowserTool =
  Tool
    { toolName = "close_browser",
      toolDescription = "Close the current browser session",
      toolInputSchema =
        object
          [ "type" .= ("object" :: Text),
            "properties" .= object []
          ]
    }

-- Tool handler that maps MCP tool calls to Selenium actions
seleniumToolHandler :: ServerConfig -> ToolHandler
seleniumToolHandler config toolCall = do
  let toolName' = toolName toolCall
      arguments = toolArguments toolCall

  case toolName' of
    "start_browser" -> handleSeleniumTool StartBrowserTool arguments
    "navigate_to" -> handleSeleniumTool NavigateToTool arguments
    "find_element" -> handleSeleniumTool FindElementTool arguments
    "click_element" -> handleSeleniumTool ClickElementTool arguments
    "type_text" -> handleSeleniumTool TypeTextTool arguments
    "get_text" -> handleSeleniumTool GetTextTool arguments
    "get_attribute" -> handleSeleniumTool GetAttributeTool arguments
    "take_screenshot" -> handleSeleniumTool TakeScreenshotTool arguments
    "close_browser" -> handleSeleniumTool CloseBrowserTool arguments
    _ -> return $ ToolResult False (Just $ "Unknown tool: " <> toolName') Nothing
  where
    handleSeleniumTool seleniumTool args = do
      case Aeson.fromJSON args of
        Aeson.Success toolInput -> do
          result <- liftIO $ executeTool seleniumTool toolInput
          return $
            ToolResult
              (SeleniumTools.resultSuccess result)
              (if SeleniumTools.resultSuccess result then Nothing else Just (SeleniumTools.resultMessage result))
              (SeleniumTools.resultData result)
        Aeson.Error err ->
          return $ ToolResult False (Just $ "Invalid tool arguments: " <> T.pack err) Nothing
