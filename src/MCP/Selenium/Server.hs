{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Main MCP server implementation for Selenium automation
module MCP.Selenium.Server
  ( createSeleniumServer,
    runSeleniumServer,
  )
where

import Data.Aeson (FromJSON, object, parseJSON, (.=))
import Data.Aeson.Key (fromText)
import Data.Aeson.QQ (aesonQQ)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import MCP.Selenium.Tools
import Network.MCP.Server (Server, createServer, registerToolCallHandler, registerTools)
import Network.MCP.Server.StdIO (runServerWithSTDIO)
import Network.MCP.Server.Types (ToolCallHandler)
import Network.MCP.Types (CallToolRequest (..), CallToolResult (..), Implementation (..), ServerCapabilities (..), Tool (..), ToolContent (..), ToolContentType (..), ToolsCapability (..))
import System.IO (hFlush, hPutStrLn, stderr)

-- | Debug logging helper
debugLog :: String -> IO ()
debugLog msg = do
  hPutStrLn stderr $ "SERVER_DEBUG: " ++ msg
  hFlush stderr

-- | Create a complete Selenium MCP server with all tools
createSeleniumServer :: IO Server
createSeleniumServer = do
  tools <- createSeleniumTools
  debugLog "Created SeleniumTools instance"

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
          closeSessionTool,
          getConsoleLogsTool,
          getAvailableLogTypesTool,
          injectConsoleLoggerTool,
          getInjectedConsoleLogsTool
        ]

  registerTools server allTools

  -- Create handler with the same tools instance
  let handler = createHandler tools
  debugLog "Created handler with tools instance"
  registerToolCallHandler server handler

  return server

-- | Generic handler that routes tool calls to appropriate handlers
createHandler :: SeleniumTools -> ToolCallHandler
createHandler tools request = do
  debugLog $ "Handler called for tool: " ++ T.unpack (callToolName request)
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
    "get_console_logs" -> parseAndHandle handleGetConsoleLogs
    "get_available_log_types" -> parseAndHandle handleGetAvailableLogTypes
    "inject_console_logger" -> parseAndHandle handleInjectConsoleLogger
    "get_injected_console_logs" -> parseAndHandle handleGetInjectedConsoleLogs
    _ -> return $ CallToolResult [ToolContent TextualContent (Just "Unknown tool")] True
  where
    parseAndHandle :: (FromJSON params) => (SeleniumTools -> params -> IO CallToolResult) -> IO CallToolResult
    parseAndHandle handler = do
      let args = object (map (\(k, v) -> fromText k .= v) (Map.toList (callToolArguments request)))
      debugLog $ "Parsing arguments: " ++ show args
      case parseMaybe parseJSON args of
        Nothing -> do
          debugLog "Failed to parse parameters"
          return $ CallToolResult [ToolContent TextualContent (Just "Invalid parameters")] True
        Just params -> do
          debugLog "Successfully parsed parameters, calling handler"
          handler tools params

-- | Tool definitions
startBrowserTool :: Tool
startBrowserTool =
  Tool
    { toolName = "start_browser",
      toolDescription = Just "Launches a browser session",
      toolInputSchema =
        [aesonQQ|{
        "type": "object",
        "properties": {
          "browser": {
            "type": "string",
            "enum": ["chrome", "firefox"],
            "description": "Browser to launch"
          },
          "options": {
            "type": "object",
            "properties": {
              "headless": {
                "type": "boolean",
                "description": "Run browser in headless mode"
              },
              "arguments": {
                "type": "array",
                "items": {"type": "string"},
                "description": "Additional browser arguments"
              }
            }
          },
          "enableLogging": {
            "type": "boolean",
            "description": "Enable logging for debugging"
          }
        },
        "required": ["browser"]
      }|]
    }

navigateTool :: Tool
navigateTool =
  Tool
    { toolName = "navigate",
      toolDescription = Just "Navigates to a URL",
      toolInputSchema =
        [aesonQQ|{
        "type": "object",
        "properties": {
          "url": {
            "type": "string",
            "description": "URL to navigate to"
          }
        },
        "required": ["url"]
      }|]
    }

findElementTool :: Tool
findElementTool =
  Tool
    { toolName = "find_element",
      toolDescription = Just "Finds an element on the page",
      toolInputSchema =
        [aesonQQ|{
        "type": "object",
        "properties": {
          "strategy": {
            "type": "string",
            "enum": ["id", "css", "xpath", "name", "tag", "class"],
            "description": "Locator strategy"
          },
          "by": {
            "type": "string",
            "enum": ["id", "css", "xpath", "name", "tag", "class"],
            "description": "Locator strategy (alternative to strategy)"
          },
          "value": {
            "type": "string",
            "description": "Value for the locator strategy"
          },
          "timeout": {
            "type": "number",
            "description": "Maximum time to wait for element in milliseconds",
            "default": 10000
          }
        },
        "required": ["value"]
      }|]
    }

clickElementTool :: Tool
clickElementTool =
  Tool
    { toolName = "click_element",
      toolDescription = Just "Clicks an element",
      toolInputSchema =
        [aesonQQ|{
        "type": "object",
        "properties": {
          "by": {
            "type": "string",
            "enum": ["id", "css", "xpath", "name", "tag", "class"]
          },
          "value": {
            "type": "string"
          },
          "timeout": {
            "type": "number",
            "default": 10000
          }
        },
        "required": ["by", "value"]
      }|]
    }

sendKeysTool :: Tool
sendKeysTool =
  Tool
    { toolName = "send_keys",
      toolDescription = Just "Sends keys to an element (typing)",
      toolInputSchema =
        [aesonQQ|{
        "type": "object",
        "properties": {
          "by": {
            "type": "string",
            "enum": ["id", "css", "xpath", "name", "tag", "class"]
          },
          "value": {
            "type": "string"
          },
          "text": {
            "type": "string",
            "description": "Text to enter into the element"
          },
          "timeout": {
            "type": "number",
            "default": 10000
          }
        },
        "required": ["by", "value", "text"]
      }|]
    }

getElementTextTool :: Tool
getElementTextTool =
  Tool
    { toolName = "get_element_text",
      toolDescription = Just "Gets the text content of an element",
      toolInputSchema =
        [aesonQQ|{
        "type": "object",
        "properties": {
          "by": {
            "type": "string",
            "enum": ["id", "css", "xpath", "name", "tag", "class"]
          },
          "value": {
            "type": "string"
          },
          "timeout": {
            "type": "number",
            "default": 10000
          }
        },
        "required": ["by", "value"]
      }|]
    }

hoverTool :: Tool
hoverTool =
  Tool
    { toolName = "hover",
      toolDescription = Just "Moves the mouse to hover over an element",
      toolInputSchema =
        [aesonQQ|{
        "type": "object",
        "properties": {
          "by": {
            "type": "string",
            "enum": ["id", "css", "xpath", "name", "tag", "class"]
          },
          "value": {
            "type": "string"
          },
          "timeout": {
            "type": "number",
            "default": 10000
          }
        },
        "required": ["by", "value"]
      }|]
    }

dragAndDropTool :: Tool
dragAndDropTool =
  Tool
    { toolName = "drag_and_drop",
      toolDescription = Just "Drags an element and drops it onto another element",
      toolInputSchema =
        [aesonQQ|{
        "type": "object",
        "properties": {
          "by": {
            "type": "string",
            "enum": ["id", "css", "xpath", "name", "tag", "class"]
          },
          "value": {
            "type": "string"
          },
          "targetBy": {
            "type": "string",
            "enum": ["id", "css", "xpath", "name", "tag", "class"]
          },
          "targetValue": {
            "type": "string"
          },
          "timeout": {
            "type": "number",
            "default": 10000
          }
        },
        "required": ["by", "value", "targetBy", "targetValue"]
      }|]
    }

doubleClickTool :: Tool
doubleClickTool =
  Tool
    { toolName = "double_click",
      toolDescription = Just "Performs a double click on an element",
      toolInputSchema =
        [aesonQQ|{
        "type": "object",
        "properties": {
          "by": {
            "type": "string",
            "enum": ["id", "css", "xpath", "name", "tag", "class"]
          },
          "value": {
            "type": "string"
          },
          "timeout": {
            "type": "number",
            "default": 10000
          }
        },
        "required": ["by", "value"]
      }|]
    }

rightClickTool :: Tool
rightClickTool =
  Tool
    { toolName = "right_click",
      toolDescription = Just "Performs a right click (context click) on an element",
      toolInputSchema =
        [aesonQQ|{
        "type": "object",
        "properties": {
          "by": {
            "type": "string",
            "enum": ["id", "css", "xpath", "name", "tag", "class"]
          },
          "value": {
            "type": "string"
          },
          "timeout": {
            "type": "number",
            "default": 10000
          }
        },
        "required": ["by", "value"]
      }|]
    }

pressKeyTool :: Tool
pressKeyTool =
  Tool
    { toolName = "press_key",
      toolDescription = Just "Simulates pressing a keyboard key",
      toolInputSchema =
        [aesonQQ|{
        "type": "object",
        "properties": {
          "key": {
            "type": "string",
            "description": "Key to press (e.g., 'Enter', 'Tab', 'a', etc.)"
          }
        },
        "required": ["key"]
      }|]
    }

uploadFileTool :: Tool
uploadFileTool =
  Tool
    { toolName = "upload_file",
      toolDescription = Just "Uploads a file using a file input element",
      toolInputSchema =
        [aesonQQ|{
        "type": "object",
        "properties": {
          "by": {
            "type": "string",
            "enum": ["id", "css", "xpath", "name", "tag", "class"]
          },
          "value": {
            "type": "string"
          },
          "filePath": {
            "type": "string",
            "description": "Absolute path to the file to upload"
          },
          "timeout": {
            "type": "number",
            "default": 10000
          }
        },
        "required": ["by", "value", "filePath"]
      }|]
    }

takeScreenshotTool :: Tool
takeScreenshotTool =
  Tool
    { toolName = "take_screenshot",
      toolDescription = Just "Captures a screenshot of the current page",
      toolInputSchema =
        [aesonQQ|{
        "type": "object",
        "properties": {
          "outputPath": {
            "type": "string",
            "description": "Path where to save the screenshot. If not provided, returns base64 data."
          }
        }
      }|]
    }

closeSessionTool :: Tool
closeSessionTool =
  Tool
    { toolName = "close_session",
      toolDescription = Just "Closes the current browser session and cleans up resources",
      toolInputSchema =
        [aesonQQ|{
        "type": "object"
      }|]
    }

getConsoleLogsTool :: Tool
getConsoleLogsTool =
  Tool
    { toolName = "get_console_logs",
      toolDescription = Just "Retrieves JavaScript console logs from the browser",
      toolInputSchema =
        [aesonQQ|{
        "type": "object",
        "properties": {
          "logLevel": {
            "type": "string",
            "enum": ["ALL", "SEVERE", "WARNING", "INFO", "DEBUG"],
            "description": "Filter logs by level (default: ALL)"
          },
          "maxEntries": {
            "type": "number",
            "description": "Maximum number of log entries to return"
          }
        }
      }|]
    }

getAvailableLogTypesTool :: Tool
getAvailableLogTypesTool =
  Tool
    { toolName = "get_available_log_types",
      toolDescription = Just "Retrieves the available log types supported by the current browser",
      toolInputSchema =
        [aesonQQ|{
        "type": "object"
      }|]
    }

injectConsoleLoggerTool :: Tool
injectConsoleLoggerTool =
  Tool
    { toolName = "inject_console_logger",
      toolDescription = Just "Injects a script to capture all console messages including console.log, console.warn, etc.",
      toolInputSchema =
        [aesonQQ|{
        "type": "object",
        "properties": {
          "timeout": {
            "type": "number",
            "description": "Script execution timeout in milliseconds (default: 60000)",
            "default": 60000
          }
        }
      }|]
    }

getInjectedConsoleLogsTool :: Tool
getInjectedConsoleLogsTool =
  Tool
    { toolName = "get_injected_console_logs",
      toolDescription = Just "Retrieves console logs captured by the injected logger script",
      toolInputSchema =
        [aesonQQ|{
        "type": "object",
        "properties": {
          "clear": {
            "type": "boolean",
            "description": "Clear the captured logs after retrieving them (default: false)"
          }
        }
      }|]
    }

-- | Run the Selenium MCP server using stdio transport
runSeleniumServer :: IO ()
runSeleniumServer = do
  server <- createSeleniumServer
  runServerWithSTDIO server
