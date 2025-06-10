{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Main MCP server implementation for Selenium automation
module MCP.Selenium.Server
  ( createSeleniumServer,
    runSeleniumServer,
  )
where

import Data.Aeson (FromJSON, object, parseJSON, (.=))
import Data.Aeson.Key (fromText)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import MCP.Selenium.Tools
import Network.MCP.Server (Server, createServer, registerToolCallHandler, registerTools)
import Network.MCP.Server.StdIO (runServerWithSTDIO)
import Network.MCP.Server.Types (ToolCallHandler)
import Network.MCP.Types (CallToolRequest (..), CallToolResult (..), Implementation (..), ServerCapabilities (..), Tool (..), ToolContent (..), ToolContentType (..), ToolsCapability (..))

-- | Create a complete Selenium MCP server with all tools
createSeleniumServer :: IO Server
createSeleniumServer = do
  tools <- createSeleniumTools

  let serverInfo = Implementation "mcp-selenium-haskell" "1.0.0"
      serverCapabilities =
        ServerCapabilities
          { resourcesCapability = Nothing,
            toolsCapability = Just (ToolsCapability True),
            promptsCapability = Nothing
          }
      instructions = "Selenium WebDriver automation server for browser automation tasks"

  server <- createServer serverInfo serverCapabilities instructions

  -- Register all tools
  let allTools =
        [ startBrowserTool,
          navigateTool,
          findElementTool,
          clickElementTool,
          sendKeysTool,
          getElementTextTool,
          hoverTool,
          dragAndDropTool,
          doubleClickTool,
          rightClickTool,
          pressKeyTool,
          uploadFileTool,
          takeScreenshotTool,
          closeSessionTool
        ]

  registerTools server allTools
  registerToolCallHandler server (createHandler tools)

  return server

-- | Generic handler that routes tool calls to appropriate handlers
createHandler :: SeleniumTools -> ToolCallHandler
createHandler tools request = do
  case callToolName request of
    "start_browser" -> parseAndHandle handleStartBrowser
    "navigate" -> parseAndHandle handleNavigate
    "find_element" -> parseAndHandle handleFindElement
    "click_element" -> parseAndHandle handleClickElement
    "send_keys" -> parseAndHandle handleSendKeys
    "get_element_text" -> parseAndHandle handleGetElementText
    "hover" -> parseAndHandle handleHover
    "drag_and_drop" -> parseAndHandle handleDragAndDrop
    "double_click" -> parseAndHandle handleDoubleClick
    "right_click" -> parseAndHandle handleRightClick
    "press_key" -> parseAndHandle handlePressKey
    "upload_file" -> parseAndHandle handleUploadFile
    "take_screenshot" -> parseAndHandle handleTakeScreenshot
    "close_session" -> parseAndHandle handleCloseSession
    _ -> return $ CallToolResult [ToolContent TextualContent (Just "Unknown tool")] True
  where
    parseAndHandle :: (FromJSON params) => (SeleniumTools -> params -> IO CallToolResult) -> IO CallToolResult
    parseAndHandle handler = do
      case parseMaybe parseJSON (object (map (\(k, v) -> fromText k .= v) (Map.toList (callToolArguments request)))) of
        Nothing ->
          return $ CallToolResult [ToolContent TextualContent (Just "Invalid parameters")] True
        Just params -> handler tools params

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
    { toolName = "close_session",
      toolDescription = Just "Closes the current browser session and cleans up resources",
      toolInputSchema =
        object
          [ "type" .= ("object" :: T.Text)
          ]
    }

-- | Run the Selenium MCP server using stdio transport
runSeleniumServer :: IO ()
runSeleniumServer = do
  server <- createSeleniumServer
  runServerWithSTDIO server
