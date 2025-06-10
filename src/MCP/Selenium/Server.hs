{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Main MCP server implementation for Selenium automation
module MCP.Selenium.Server
  ( createSeleniumServer,
    runSeleniumServer,
  )
where

import Control.Exception (SomeException, catch)
import Data.Aeson (FromJSON, ToJSON, Value (..), decode, withObject, (.:))
import Data.Aeson.Types (Parser, parseMaybe)
import qualified Data.Text as T
import GHC.Generics (Generic)
import MCP.Selenium.Tools
import MCP.Selenium.WebDriver
import Network.MCP.Server (Server (..))
import Network.MCP.Server.StdIO (runServerWithSTDIO)
import Network.MCP.Server.Types (ToolCallHandler)
import Network.MCP.Types (CallToolRequest (..), CallToolResult (..), Tool (..), ToolContent (..), ToolContentType (..))

-- | Create a complete Selenium MCP server with all tools
createSeleniumServer :: IO Server
createSeleniumServer = do
  tools <- createSeleniumTools
  let server = emptyServer "mcp-selenium-haskell" "1.0.0"

  -- Add all selenium tools
  server' <- addTool server startBrowserTool (createHandler tools handleStartBrowser)
  server'' <- addTool server' navigateTool (createHandler tools handleNavigate)
  server''' <- addTool server'' findElementTool (createHandler tools handleFindElement)
  server'''' <- addTool server''' clickElementTool (createHandler tools handleClickElement)
  server''''' <- addTool server'''' sendKeysTool (createHandler tools handleSendKeys)
  server'''''' <- addTool server''''' getElementTextTool (createHandler tools handleGetElementText)
  server''''''' <- addTool server'''''' hoverTool (createHandler tools handleHover)
  server'''''''' <- addTool server''''''' dragAndDropTool (createHandler tools handleDragAndDrop)
  server''''''''' <- addTool server'''''''' doubleClickTool (createHandler tools handleDoubleClick)
  server'''''''''' <- addTool server''''''''' rightClickTool (createHandler tools handleRightClick)
  server''''''''''' <- addTool server'''''''''' pressKeyTool (createHandler tools handlePressKey)
  server'''''''''''' <- addTool server''''''''''' uploadFileTool (createHandler tools handleUploadFile)
  server''''''''''''' <- addTool server'''''''''''' takeScreenshotTool (createHandler tools handleTakeScreenshot)
  finalServer <- addTool server''''''''''''' closeSessionTool (createHandler tools handleCloseSession)

  return finalServer

-- | Generic handler creator that parses JSON parameters
createHandler ::
  (FromJSON params) =>
  SeleniumTools ->
  (SeleniumTools -> params -> IO CallToolResult) ->
  ToolCallHandler
createHandler tools handler request = do
  case parseMaybe parseParams (arguments request) of
    Nothing ->
      return $ CallToolResult [ToolContent TextualContent (Just "Invalid parameters")] True
    Just params -> handler tools params
  where
    parseParams :: Value -> Parser params
    parseParams = withObject "parameters" $ \obj -> do
      -- Try to parse the entire object as the parameter type
      pure <$> parseJSON (Object obj)

-- | Tool definitions
startBrowserTool :: Tool
startBrowserTool =
  Tool
    { toolName = "start_browser",
      toolDescription = Just "Launches a browser session",
      toolInputSchema =
        object
          [ "type" .= ("object" :: T.Text),
            "properties"
              .= object
                [ "browser"
                    .= object
                      [ "type" .= ("string" :: T.Text),
                        "enum" .= (["chrome", "firefox"] :: [T.Text]),
                        "description" .= ("Browser to launch" :: T.Text)
                      ],
                  "options"
                    .= object
                      [ "type" .= ("object" :: T.Text),
                        "properties"
                          .= object
                            [ "headless"
                                .= object
                                  [ "type" .= ("boolean" :: T.Text),
                                    "description" .= ("Run browser in headless mode" :: T.Text)
                                  ],
                              "arguments"
                                .= object
                                  [ "type" .= ("array" :: T.Text),
                                    "items" .= object ["type" .= ("string" :: T.Text)],
                                    "description" .= ("Additional browser arguments" :: T.Text)
                                  ]
                            ]
                      ]
                ],
            "required" .= (["browser"] :: [T.Text])
          ]
    }

navigateTool :: Tool
navigateTool =
  Tool
    { toolName = "navigate",
      toolDescription = Just "Navigates to a URL",
      toolInputSchema =
        object
          [ "type" .= ("object" :: T.Text),
            "properties"
              .= object
                [ "url"
                    .= object
                      [ "type" .= ("string" :: T.Text),
                        "description" .= ("URL to navigate to" :: T.Text)
                      ]
                ],
            "required" .= (["url"] :: [T.Text])
          ]
    }

findElementTool :: Tool
findElementTool =
  Tool
    { toolName = "find_element",
      toolDescription = Just "Finds an element on the page",
      toolInputSchema =
        object
          [ "type" .= ("object" :: T.Text),
            "properties"
              .= object
                [ "by"
                    .= object
                      [ "type" .= ("string" :: T.Text),
                        "enum" .= (["id", "css", "xpath", "name", "tag", "class"] :: [T.Text]),
                        "description" .= ("Locator strategy" :: T.Text)
                      ],
                  "value"
                    .= object
                      [ "type" .= ("string" :: T.Text),
                        "description" .= ("Value for the locator strategy" :: T.Text)
                      ],
                  "timeout"
                    .= object
                      [ "type" .= ("number" :: T.Text),
                        "description" .= ("Maximum time to wait for element in milliseconds" :: T.Text),
                        "default" .= (10000 :: Int)
                      ]
                ],
            "required" .= (["by", "value"] :: [T.Text])
          ]
    }

clickElementTool :: Tool
clickElementTool =
  Tool
    { toolName = "click_element",
      toolDescription = Just "Clicks an element",
      toolInputSchema =
        object
          [ "type" .= ("object" :: T.Text),
            "properties"
              .= object
                [ "by"
                    .= object
                      [ "type" .= ("string" :: T.Text),
                        "enum" .= (["id", "css", "xpath", "name", "tag", "class"] :: [T.Text])
                      ],
                  "value"
                    .= object
                      [ "type" .= ("string" :: T.Text)
                      ],
                  "timeout"
                    .= object
                      [ "type" .= ("number" :: T.Text),
                        "default" .= (10000 :: Int)
                      ]
                ],
            "required" .= (["by", "value"] :: [T.Text])
          ]
    }

sendKeysTool :: Tool
sendKeysTool =
  Tool
    { toolName = "send_keys",
      toolDescription = Just "Sends keys to an element (typing)",
      toolInputSchema =
        object
          [ "type" .= ("object" :: T.Text),
            "properties"
              .= object
                [ "by"
                    .= object
                      [ "type" .= ("string" :: T.Text),
                        "enum" .= (["id", "css", "xpath", "name", "tag", "class"] :: [T.Text])
                      ],
                  "value"
                    .= object
                      [ "type" .= ("string" :: T.Text)
                      ],
                  "text"
                    .= object
                      [ "type" .= ("string" :: T.Text),
                        "description" .= ("Text to enter into the element" :: T.Text)
                      ],
                  "timeout"
                    .= object
                      [ "type" .= ("number" :: T.Text),
                        "default" .= (10000 :: Int)
                      ]
                ],
            "required" .= (["by", "value", "text"] :: [T.Text])
          ]
    }

getElementTextTool :: Tool
getElementTextTool =
  Tool
    { toolName = "get_element_text",
      toolDescription = Just "Gets the text content of an element",
      toolInputSchema =
        object
          [ "type" .= ("object" :: T.Text),
            "properties"
              .= object
                [ "by"
                    .= object
                      [ "type" .= ("string" :: T.Text),
                        "enum" .= (["id", "css", "xpath", "name", "tag", "class"] :: [T.Text])
                      ],
                  "value"
                    .= object
                      [ "type" .= ("string" :: T.Text)
                      ],
                  "timeout"
                    .= object
                      [ "type" .= ("number" :: T.Text),
                        "default" .= (10000 :: Int)
                      ]
                ],
            "required" .= (["by", "value"] :: [T.Text])
          ]
    }

hoverTool :: Tool
hoverTool =
  Tool
    { toolName = "hover",
      toolDescription = Just "Moves the mouse to hover over an element",
      toolInputSchema =
        object
          [ "type" .= ("object" :: T.Text),
            "properties"
              .= object
                [ "by"
                    .= object
                      [ "type" .= ("string" :: T.Text),
                        "enum" .= (["id", "css", "xpath", "name", "tag", "class"] :: [T.Text])
                      ],
                  "value"
                    .= object
                      [ "type" .= ("string" :: T.Text)
                      ],
                  "timeout"
                    .= object
                      [ "type" .= ("number" :: T.Text),
                        "default" .= (10000 :: Int)
                      ]
                ],
            "required" .= (["by", "value"] :: [T.Text])
          ]
    }

dragAndDropTool :: Tool
dragAndDropTool =
  Tool
    { toolName = "drag_and_drop",
      toolDescription = Just "Drags an element and drops it onto another element",
      toolInputSchema =
        object
          [ "type" .= ("object" :: T.Text),
            "properties"
              .= object
                [ "by"
                    .= object
                      [ "type" .= ("string" :: T.Text),
                        "enum" .= (["id", "css", "xpath", "name", "tag", "class"] :: [T.Text])
                      ],
                  "value"
                    .= object
                      [ "type" .= ("string" :: T.Text)
                      ],
                  "targetBy"
                    .= object
                      [ "type" .= ("string" :: T.Text),
                        "enum" .= (["id", "css", "xpath", "name", "tag", "class"] :: [T.Text])
                      ],
                  "targetValue"
                    .= object
                      [ "type" .= ("string" :: T.Text)
                      ],
                  "timeout"
                    .= object
                      [ "type" .= ("number" :: T.Text),
                        "default" .= (10000 :: Int)
                      ]
                ],
            "required" .= (["by", "value", "targetBy", "targetValue"] :: [T.Text])
          ]
    }

doubleClickTool :: Tool
doubleClickTool =
  Tool
    { toolName = "double_click",
      toolDescription = Just "Performs a double click on an element",
      toolInputSchema =
        object
          [ "type" .= ("object" :: T.Text),
            "properties"
              .= object
                [ "by"
                    .= object
                      [ "type" .= ("string" :: T.Text),
                        "enum" .= (["id", "css", "xpath", "name", "tag", "class"] :: [T.Text])
                      ],
                  "value"
                    .= object
                      [ "type" .= ("string" :: T.Text)
                      ],
                  "timeout"
                    .= object
                      [ "type" .= ("number" :: T.Text),
                        "default" .= (10000 :: Int)
                      ]
                ],
            "required" .= (["by", "value"] :: [T.Text])
          ]
    }

rightClickTool :: Tool
rightClickTool =
  Tool
    { toolName = "right_click",
      toolDescription = Just "Performs a right click (context click) on an element",
      toolInputSchema =
        object
          [ "type" .= ("object" :: T.Text),
            "properties"
              .= object
                [ "by"
                    .= object
                      [ "type" .= ("string" :: T.Text),
                        "enum" .= (["id", "css", "xpath", "name", "tag", "class"] :: [T.Text])
                      ],
                  "value"
                    .= object
                      [ "type" .= ("string" :: T.Text)
                      ],
                  "timeout"
                    .= object
                      [ "type" .= ("number" :: T.Text),
                        "default" .= (10000 :: Int)
                      ]
                ],
            "required" .= (["by", "value"] :: [T.Text])
          ]
    }

pressKeyTool :: Tool
pressKeyTool =
  Tool
    { toolName = "press_key",
      toolDescription = Just "Simulates pressing a keyboard key",
      toolInputSchema =
        object
          [ "type" .= ("object" :: T.Text),
            "properties"
              .= object
                [ "key"
                    .= object
                      [ "type" .= ("string" :: T.Text),
                        "description" .= ("Key to press (e.g., 'Enter', 'Tab', 'a', etc.)" :: T.Text)
                      ]
                ],
            "required" .= (["key"] :: [T.Text])
          ]
    }

uploadFileTool :: Tool
uploadFileTool =
  Tool
    { toolName = "upload_file",
      toolDescription = Just "Uploads a file using a file input element",
      toolInputSchema =
        object
          [ "type" .= ("object" :: T.Text),
            "properties"
              .= object
                [ "by"
                    .= object
                      [ "type" .= ("string" :: T.Text),
                        "enum" .= (["id", "css", "xpath", "name", "tag", "class"] :: [T.Text])
                      ],
                  "value"
                    .= object
                      [ "type" .= ("string" :: T.Text)
                      ],
                  "filePath"
                    .= object
                      [ "type" .= ("string" :: T.Text),
                        "description" .= ("Absolute path to the file to upload" :: T.Text)
                      ],
                  "timeout"
                    .= object
                      [ "type" .= ("number" :: T.Text),
                        "default" .= (10000 :: Int)
                      ]
                ],
            "required" .= (["by", "value", "filePath"] :: [T.Text])
          ]
    }

takeScreenshotTool :: Tool
takeScreenshotTool =
  Tool
    { toolName = "take_screenshot",
      toolDescription = Just "Captures a screenshot of the current page",
      toolInputSchema =
        object
          [ "type" .= ("object" :: T.Text),
            "properties"
              .= object
                [ "outputPath"
                    .= object
                      [ "type" .= ("string" :: T.Text),
                        "description" .= ("Path where to save the screenshot. If not provided, returns base64 data." :: T.Text)
                      ]
                ]
          ]
    }

closeSessionTool :: Tool
closeSessionTool =
  Tool
    { name = "close_session",
      description = "Closes the current browser session and cleans up resources",
      inputSchema =
        object
          [ "type" .= ("object" :: T.Text)
          ]
    }

-- | Run the Selenium MCP server using stdio transport
runSeleniumServer :: IO ()
runSeleniumServer = do
  server <- createSeleniumServer
  runServerWithSTDIO server
