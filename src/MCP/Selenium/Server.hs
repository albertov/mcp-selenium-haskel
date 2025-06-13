{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
-- Module: MCP.Selenium.Server
-- Description: Main MCP server implementation for Selenium automation
--
-- This module provides the core MCP (Model Context Protocol) server implementation
-- for browser automation using Selenium WebDriver. It implements a multi-session
-- architecture where each browser session is identified by a unique UUID, allowing
-- multiple concurrent browser sessions to be managed independently.
--
-- = Key Features
--
-- * Multi-session browser management with UUID-based session IDs
-- * Support for Chrome and Firefox browsers
-- * Comprehensive tool suite for browser automation
-- * Session isolation and resource management
-- * Integration with Selenium WebDriver
--
-- = Session Architecture
--
-- The server uses a multi-session model where:
--
-- 1. Each browser session is identified by a unique UUID ('SessionId')
-- 2. Sessions are stored in a concurrent hash map for thread-safe access
-- 3. All tools (except start_browser) require a valid session_id parameter
-- 4. Sessions can be created, used, and closed independently
--
-- = Environment Configuration
--
-- The server can be configured using environment variables:
--
-- * @SELENIUM_HOST@: WebDriver server hostname (default: "127.0.0.1")
-- * @SELENIUM_PORT@: WebDriver server port (default: "4444")
--
-- = Example Usage
--
-- @
-- import MCP.Selenium.Server
--
-- main :: IO ()
-- main = do
--   server <- createSeleniumServer
--   runSeleniumServer server
-- @
--
-- = Tool Categories
--
-- The server provides tools organized into categories:
--
-- * Session Management: start_browser, close_browser
-- * Navigation: navigate
-- * Element Location: find_element
-- * Element Interaction: click_element, send_keys, get_element_text
-- * Advanced Actions: hover, double_click, right_click, drag_and_drop, press_key
-- * File Operations: upload_file
-- * Utility Operations: take_screenshot, get_source
-- * Console Logging: get_console_logs, inject_console_logger, etc.
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
import MCP.Selenium.Utils (debugLog)
import Network.MCP.Server (Server, createServer, registerToolCallHandler, registerTools)
import Network.MCP.Server.StdIO (runServerWithSTDIO)
import Network.MCP.Server.Types (ToolCallHandler)
import Network.MCP.Types (CallToolRequest (..), CallToolResult (..), Implementation (..), ServerCapabilities (..), Tool (..), ToolContent (..), ToolContentType (..), ToolsCapability (..))

-- | Create a complete Selenium MCP server with all tools
createSeleniumServer :: IO Server
createSeleniumServer = do
  tools <- createSeleniumTools
  debugLog "SERVER_DEBUG: Created SeleniumTools instance"

  let serverInfo = Implementation "mcp-selenium-haskell" "0.1.0"
      serverCapabilities =
        ServerCapabilities
          { resourcesCapability = Nothing,
            toolsCapability = Just (ToolsCapability True),
            promptsCapability = Nothing
          }
      instructions =
        "Selenium WebDriver automation server for browser automation tasks.\n\n"
          <> "SESSION MANAGEMENT PROTOCOL:\n"
          <> "1. Start a session: Call 'start_browser' tool to get a session_id\n"
          <> "2. Use session_id: All subsequent tools require the session_id parameter\n"
          <> "3. Close session: Call 'close_browser' with session_id to cleanup\n\n"
          <> "WORKFLOW:\n"
          <> "- start_browser → returns session_id\n"
          <> "- navigate, find_element, click_element, etc. → require session_id\n"
          <> "- close_browser → cleanup with session_id\n\n"
          <> "Multiple sessions can be active simultaneously with different session_ids."

  server <- createServer serverInfo serverCapabilities instructions

  -- Register all tools
  let allTools =
        [ startBrowserTool,
          navigateTool,
          findElementTool,
          findElementsTool,
          clickElementTool,
          sendKeysTool,
          getElementTextTool,
          getElementsTextTool,
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
          getInjectedConsoleLogsTool,
          getSourceTool,
          executeJSTool
        ]

  registerTools server allTools

  -- Create handler with the same tools instance
  let handler = createHandler tools
  debugLog "SERVER_DEBUG: Created handler with tools instance"
  registerToolCallHandler server handler

  return server

-- | Generic handler that routes tool calls to appropriate handlers
createHandler :: SeleniumTools -> ToolCallHandler
createHandler tools request = do
  debugLog $ "SERVER_DEBUG: Handler called for tool: " ++ T.unpack (callToolName request)
  case callToolName request of
    "start_browser" -> parseAndHandle handleStartBrowser
    "navigate" -> parseAndHandle handleNavigate
    "find_element" -> parseAndHandle handleFindElement
    "find_elements" -> parseAndHandle handleFindElements
    "click_element" -> parseAndHandle handleClickElement
    "send_keys" -> parseAndHandle handleSendKeys
    "get_element_text" -> parseAndHandle handleGetElementText
    "get_elements_text" -> parseAndHandle handleGetElementsText
    "hover" -> parseAndHandle handleHover
    "drag_and_drop" -> parseAndHandle handleDragAndDrop
    "double_click" -> parseAndHandle handleDoubleClick
    "right_click" -> parseAndHandle handleRightClick
    "press_key" -> parseAndHandle handlePressKey
    "upload_file" -> parseAndHandle handleUploadFile
    "take_screenshot" -> parseAndHandle handleTakeScreenshot
    "close_browser" -> parseAndHandle handleCloseBrowser
    "get_console_logs" -> parseAndHandle handleGetConsoleLogs
    "get_available_log_types" -> parseAndHandle handleGetAvailableLogTypes
    "inject_console_logger" -> parseAndHandle handleInjectConsoleLogger
    "get_injected_console_logs" -> parseAndHandle handleGetInjectedConsoleLogs
    "get_source" -> parseAndHandle handleGetSource
    "execute_js" -> parseAndHandle handleExecuteJS
    _ -> return $ CallToolResult [ToolContent TextualContent (Just "Unknown tool")] True
  where
    parseAndHandle :: (FromJSON params) => (SeleniumTools -> params -> IO CallToolResult) -> IO CallToolResult
    parseAndHandle handler = do
      let requestArgs = object (map (\(k, v) -> fromText k .= v) (Map.toList (callToolArguments request)))
      debugLog $ "SERVER_DEBUG: Parsing arguments: " ++ show requestArgs
      case parseMaybe parseJSON requestArgs of
        Nothing -> do
          debugLog "SERVER_DEBUG: Failed to parse parameters"
          return $ CallToolResult [ToolContent TextualContent (Just "Invalid parameters")] True
        Just params -> do
          debugLog "SERVER_DEBUG: Successfully parsed parameters, calling handler"
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
          "session_id": {
            "type": "string",
            "description": "Session ID returned from start_browser"
          },
          "url": {
            "type": "string",
            "description": "URL to navigate to"
          }
        },
        "required": ["session_id", "url"]
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
          "session_id": {
            "type": "string",
            "description": "Session ID returned from start_browser"
          },
          "by": {
            "type": "string",
            "enum": ["id", "css", "xpath", "name", "tag", "class"],
            "description": "Locator strategy"
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
        "required": ["session_id", "value"]
      }|]
    }

findElementsTool :: Tool
findElementsTool =
  Tool
    { toolName = "find_elements",
      toolDescription = Just "Finds multiple elements on the page",
      toolInputSchema =
        [aesonQQ|{
        "type": "object",
        "properties": {
          "session_id": {
            "type": "string",
            "description": "Session ID returned from start_browser"
          },
          "by": {
            "type": "string",
            "enum": ["id", "css", "xpath", "name", "tag", "class"],
            "description": "Locator strategy"
          },
          "value": {
            "type": "string",
            "description": "Value for the locator strategy"
          },
          "timeout": {
            "type": "number",
            "description": "Maximum time to wait for elements in milliseconds",
            "default": 10000
          }
        },
        "required": ["session_id", "value"]
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
          "session_id": {
            "type": "string",
            "description": "Session ID returned from start_browser"
          },
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
        "required": ["session_id", "by", "value"]
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
          "session_id": {
            "type": "string",
            "description": "Session ID returned from start_browser"
          },
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
        "required": ["session_id", "by", "value", "text"]
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
          "session_id": {
            "type": "string",
            "description": "Session ID returned from start_browser"
          },
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
        "required": ["session_id", "by", "value"]
      }|]
    }

getElementsTextTool :: Tool
getElementsTextTool =
  Tool
    { toolName = "get_elements_text",
      toolDescription = Just "Gets the text content of multiple elements",
      toolInputSchema =
        [aesonQQ|{
        "type": "object",
        "properties": {
          "session_id": {
            "type": "string",
            "description": "Session ID returned from start_browser"
          },
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
        "required": ["session_id", "by", "value"]
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
          "session_id": {
            "type": "string",
            "description": "Session ID returned from start_browser"
          },
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
        "required": ["session_id", "by", "value"]
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
          "session_id": {
            "type": "string",
            "description": "Session ID returned from start_browser"
          },
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
        "required": ["session_id", "by", "value", "targetBy", "targetValue"]
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
          "session_id": {
            "type": "string",
            "description": "Session ID returned from start_browser"
          },
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
        "required": ["session_id", "by", "value"]
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
          "session_id": {
            "type": "string",
            "description": "Session ID returned from start_browser"
          },
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
        "required": ["session_id", "by", "value"]
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
          "session_id": {
            "type": "string",
            "description": "Session ID returned from start_browser"
          },
          "key": {
            "type": "string",
            "description": "Key to press (e.g., 'Enter', 'Tab', 'a', etc.)"
          }
        },
        "required": ["session_id", "key"]
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
          "session_id": {
            "type": "string",
            "description": "Session ID returned from start_browser"
          },
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
        "required": ["session_id", "by", "value", "filePath"]
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
          "session_id": {
            "type": "string",
            "description": "Session ID returned from start_browser"
          }
        },
        "required": ["session_id"]
      }|]
    }

closeSessionTool :: Tool
closeSessionTool =
  Tool
    { toolName = "close_browser",
      toolDescription = Just "Closes the current browser session and cleans up resources",
      toolInputSchema =
        [aesonQQ|{
        "type": "object",
        "properties": {
          "session_id": {
            "type": "string",
            "description": "Session ID returned from start_browser"
          }
        },
        "required": ["session_id"]
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
          "session_id": {
            "type": "string",
            "description": "Session ID returned from start_browser"
          },
          "logLevel": {
            "type": "string",
            "enum": ["ALL", "SEVERE", "WARNING", "INFO", "DEBUG"],
            "description": "Filter logs by level (default: ALL)"
          },
          "maxEntries": {
            "type": "number",
            "description": "Maximum number of log entries to return"
          }
        },
        "required": ["session_id"]
      }|]
    }

getAvailableLogTypesTool :: Tool
getAvailableLogTypesTool =
  Tool
    { toolName = "get_available_log_types",
      toolDescription = Just "Retrieves the available log types supported by the current browser",
      toolInputSchema =
        [aesonQQ|{
        "type": "object",
        "properties": {
          "session_id": {
            "type": "string",
            "description": "Session ID returned from start_browser"
          }
        },
        "required": ["session_id"]
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
          "session_id": {
            "type": "string",
            "description": "Session ID returned from start_browser"
          },
          "timeout": {
            "type": "number",
            "description": "Script execution timeout in milliseconds (default: 60000)",
            "default": 60000
          }
        },
        "required": ["session_id"]
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
          "session_id": {
            "type": "string",
            "description": "Session ID returned from start_browser"
          },
          "clear": {
            "type": "boolean",
            "description": "Clear the captured logs after retrieving them (default: false)"
          }
        },
        "required": ["session_id"]
      }|]
    }

getSourceTool :: Tool
getSourceTool =
  Tool
    { toolName = "get_source",
      toolDescription = Just "Gets the current page's HTML source code",
      toolInputSchema =
        [aesonQQ|{
        "type": "object",
        "properties": {
          "session_id": {
            "type": "string",
            "description": "Session ID returned from start_browser"
          }
        },
        "required": ["session_id"]
      }|]
    }

executeJSTool :: Tool
executeJSTool =
  Tool
    { toolName = "execute_js",
      toolDescription = Just "Executes JavaScript code in the browser and returns the result",
      toolInputSchema =
        [aesonQQ|{
        "type": "object",
        "properties": {
          "session_id": {
            "type": "string",
            "description": "Session ID returned from start_browser"
          },
          "script": {
            "type": "string",
            "description": "JavaScript code to execute"
          },
          "args": {
            "type": "array",
            "items": {},
            "description": "Arguments to pass to the script (optional)"
          },
          "timeout": {
            "type": "number",
            "description": "Script execution timeout in milliseconds",
            "default": 30000
          }
        },
        "required": ["session_id", "script"]
      }|]
    }

-- | Run the Selenium MCP server using stdio transport
runSeleniumServer :: IO ()
runSeleniumServer = do
  server <- createSeleniumServer
  runServerWithSTDIO server
