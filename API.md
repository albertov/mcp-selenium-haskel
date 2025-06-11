# MCP Selenium Server API Documentation

This document provides comprehensive API documentation for all tools available in the MCP Selenium Server. The server provides browser automation capabilities through standardized MCP (Model Context Protocol) tools.

## Server Information

- **Implementation**: mcp-selenium-haskell
- **Version**: 1.0.0
- **Description**: Selenium WebDriver automation server for browser automation tasks
- **Supported Browsers**: Chrome, Firefox

## Tool Categories

The tools are organized into the following categories:

1. [Session Management](#session-management)
2. [Navigation](#navigation)
3. [Element Location](#element-location)
4. [Element Interaction](#element-interaction)
5. [Advanced Actions](#advanced-actions)
6. [File Operations](#file-operations)
7. [Utility Operations](#utility-operations)
8. [Console Logging](#console-logging)

---

## Session Management

### start_browser

Launches a browser session with configurable options.

**Parameters:**
- `browser` (required): Browser type - "chrome" or "firefox"
- `options` (optional): Browser configuration object
  - `headless` (boolean): Run browser in headless mode
  - `arguments` (array of strings): Additional browser arguments
- `enableLogging` (optional, boolean): Enable logging for debugging

**Example:**
```json
{
  "browser": "chrome",
  "options": {
    "headless": true,
    "arguments": ["--no-sandbox", "--disable-dev-shm-usage"]
  },
  "enableLogging": true
}
```

**Response:**
- Success: Browser started successfully message
- Error: Browser startup failure details

---

### close_session

Closes the current browser session and cleans up resources.

**Parameters:**
None (empty object)

**Example:**
```json
{}
```

**Response:**
- Success: Session closed confirmation
- Error: Session close failure details

---

## Navigation

### navigate

Navigates to a specified URL.

**Parameters:**
- `url` (required): URL to navigate to

**Example:**
```json
{
  "url": "https://example.com"
}
```

**Response:**
- Success: Navigation confirmation with URL
- Error: Navigation failure details

---

## Element Location

### find_element

Locates an element on the page using various strategies.

**Parameters:**
- `value` (required): Value for the locator strategy
- `by` (optional): Locator strategy - "id", "css", "xpath", "name", "tag", "class" (defaults to "id")
- `timeout` (optional): Maximum wait time in milliseconds (default: 10000)

**Supported Locator Strategies:**
- `id`: Find by element ID
- `css`: Find by CSS selector
- `xpath`: Find by XPath expression
- `name`: Find by name attribute
- `tag`: Find by HTML tag name
- `class`: Find by CSS class name

**Example:**
```json
{
  "by": "css",
  "value": "#submit-button",
  "timeout": 5000
}
```

**Response:**
- Success: JSON object with `elementId` and `found: true`
- Error: Element not found details

---

## Element Interaction

### click_element

Clicks on a specified element.

**Parameters:**
- `by` (required): Locator strategy
- `value` (required): Value for the locator strategy
- `timeout` (optional): Maximum wait time in milliseconds (default: 10000)

**Example:**
```json
{
  "by": "id",
  "value": "submit-btn",
  "timeout": 5000
}
```

**Response:**
- Success: Click confirmation message
- Error: Click operation failure details

---

### send_keys

Sends keystrokes to a specified element (typing text).

**Parameters:**
- `by` (required): Locator strategy
- `value` (required): Value for the locator strategy
- `text` (required): Text to type into the element
- `timeout` (optional): Maximum wait time in milliseconds (default: 10000)

**Example:**
```json
{
  "by": "name",
  "value": "username",
  "text": "john.doe@example.com",
  "timeout": 5000
}
```

**Response:**
- Success: Send keys confirmation message
- Error: Send keys operation failure details

---

### get_element_text

Retrieves the text content of a specified element.

**Parameters:**
- `by` (required): Locator strategy
- `value` (required): Value for the locator strategy
- `timeout` (optional): Maximum wait time in milliseconds (default: 10000)

**Example:**
```json
{
  "by": "css",
  "value": ".status-message",
  "timeout": 3000
}
```

**Response:**
- Success: The text content of the element
- Error: Get text operation failure details

---

## Advanced Actions

### hover

Moves the mouse to hover over a specified element.

**Parameters:**
- `by` (required): Locator strategy
- `value` (required): Value for the locator strategy
- `timeout` (optional): Maximum wait time in milliseconds (default: 10000)

**Example:**
```json
{
  "by": "xpath",
  "value": "//button[@class='dropdown-trigger']"
}
```

**Response:**
- Success: Hover confirmation message
- Error: Hover operation failure details

---

### double_click

Performs a double click on a specified element.

**Parameters:**
- `by` (required): Locator strategy
- `value` (required): Value for the locator strategy
- `timeout` (optional): Maximum wait time in milliseconds (default: 10000)

**Example:**
```json
{
  "by": "id",
  "value": "editable-field"
}
```

**Response:**
- Success: Double click confirmation message
- Error: Double click operation failure details

---

### right_click

Performs a right click (context click) on a specified element.

**Parameters:**
- `by` (required): Locator strategy
- `value` (required): Value for the locator strategy
- `timeout` (optional): Maximum wait time in milliseconds (default: 10000)

**Example:**
```json
{
  "by": "css",
  "value": ".context-menu-target"
}
```

**Response:**
- Success: Right click confirmation message
- Error: Right click operation failure details

---

### drag_and_drop

Drags an element and drops it onto another element.

**Parameters:**
- `by` (required): Source element locator strategy
- `value` (required): Source element value
- `targetBy` (required): Target element locator strategy
- `targetValue` (required): Target element value
- `timeout` (optional): Maximum wait time in milliseconds (default: 10000)

**Example:**
```json
{
  "by": "id",
  "value": "draggable-item",
  "targetBy": "id",
  "targetValue": "drop-zone"
}
```

**Response:**
- Success: Drag and drop confirmation message
- Error: Drag and drop operation failure details

---

### press_key

Simulates pressing a keyboard key.

**Parameters:**
- `key` (required): Key to press (e.g., "Enter", "Tab", "Escape", "a", "1")

**Common Key Values:**
- Navigation: "Enter", "Tab", "Escape", "Backspace", "Delete"
- Function keys: "F1", "F2", etc.
- Arrow keys: "ArrowUp", "ArrowDown", "ArrowLeft", "ArrowRight"
- Modifiers: "Shift", "Control", "Alt"
- Regular characters: "a", "A", "1", "!", etc.

**Example:**
```json
{
  "key": "Enter"
}
```

**Response:**
- Success: Key press confirmation message
- Error: Key press operation failure details

---

## File Operations

### upload_file

Uploads a file using a file input element.

**Parameters:**
- `by` (required): Locator strategy for the file input element
- `value` (required): Value for the locator strategy
- `filePath` (required): Absolute path to the file to upload
- `timeout` (optional): Maximum wait time in milliseconds (default: 10000)

**Example:**
```json
{
  "by": "name",
  "value": "file-upload",
  "filePath": "/home/user/documents/report.pdf"
}
```

**Response:**
- Success: File upload confirmation message
- Error: File upload operation failure details

---

## Utility Operations

### take_screenshot

Captures a screenshot of the current page.

**Parameters:**
- `session_id` (required): Session ID returned from start_browser

**Example:**
```json
{
  "session_id": "550e8400-e29b-41d4-a716-446655440000"
}
```

**Response:**
- Success: Screenshot path or base64 data
- Error: Screenshot operation failure details

---

### get_source

Gets the current page's HTML source code.

**Parameters:**
- `session_id` (required): Session ID returned from start_browser

**Example:**
```json
{
  "session_id": "550e8400-e29b-41d4-a716-446655440000"
}
```

**Response:**
- Success: The HTML source code of the current page
- Error: Get source operation failure details

---

## Console Logging

The server provides several tools for capturing and analyzing browser console logs.

### get_console_logs

Retrieves JavaScript console logs from the browser.

**Parameters:**
- `logLevel` (optional): Filter logs by level - "ALL", "SEVERE", "WARNING", "INFO", "DEBUG" (default: "ALL")
- `maxEntries` (optional): Maximum number of log entries to return

**Example:**
```json
{
  "logLevel": "SEVERE",
  "maxEntries": 50
}
```

**Response:**
- Success: JSON object with `logs` array containing log entries with `level`, `message`, and `timestamp`
- Error: Console log retrieval failure details

---

### get_available_log_types

Retrieves the available log types supported by the current browser.

**Parameters:**
None (empty object)

**Example:**
```json
{}
```

**Response:**
- Success: JSON object with `logTypes` array
- Error: Log types retrieval failure details

---

### inject_console_logger

Injects a script to capture all console messages including console.log, console.warn, etc.

**Parameters:**
- `timeout` (optional): Script execution timeout in milliseconds (default: 60000)

**Example:**
```json
{
  "timeout": 30000
}
```

**Response:**
- Success: Console logger injection confirmation
- Error: Console logger injection failure details

---

### get_injected_console_logs

Retrieves console logs captured by the injected logger script.

**Parameters:**
- `clear` (optional): Clear the captured logs after retrieving them (default: false)

**Example:**
```json
{
  "clear": true
}
```

**Response:**
- Success: JSON string with captured console logs
- Error: Injected console logs retrieval failure details

---

## Error Handling

All tools return standardized responses:

- **Success**: `isError: false` with appropriate content
- **Error**: `isError: true` with error message details

Common error scenarios:
- No active browser session
- Element not found within timeout
- Invalid parameters
- Browser driver issues
- Network connectivity problems

## Best Practices

1. **Session Management**: Always start a browser session before using other tools
2. **Timeouts**: Use appropriate timeout values based on page load times
3. **Element Location**: Prefer stable locators (ID, data attributes) over fragile ones (XPath with positions)
4. **Error Handling**: Check tool responses for errors before proceeding
5. **Resource Cleanup**: Close sessions when automation tasks are complete
6. **Console Logging**: Use injected logger for comprehensive console monitoring

## Example Workflow

```json
// 1. Start browser
{
  "tool": "start_browser",
  "arguments": {
    "browser": "chrome",
    "options": {"headless": false}
  }
}

// 2. Navigate to page
{
  "tool": "navigate",
  "arguments": {
    "session_id": "550e8400-e29b-41d4-a716-446655440000",
    "url": "https://example.com/login"
  }
}

// 3. Fill login form
{
  "tool": "send_keys",
  "arguments": {
    "session_id": "550e8400-e29b-41d4-a716-446655440000",
    "by": "id",
    "value": "username",
    "text": "user@example.com"
  }
}

// 4. Submit form
{
  "tool": "click_element",
  "arguments": {
    "session_id": "550e8400-e29b-41d4-a716-446655440000",
    "by": "css",
    "value": "button[type='submit']"
  }
}

// 5. Get page source
{
  "tool": "get_source",
  "arguments": {
    "session_id": "550e8400-e29b-41d4-a716-446655440000"
  }
}

// 6. Take screenshot
{
  "tool": "take_screenshot",
  "arguments": {
    "session_id": "550e8400-e29b-41d4-a716-446655440000"
  }
}

// 7. Clean up
{
  "tool": "close_session",
  "arguments": {
    "session_id": "550e8400-e29b-41d4-a716-446655440000"
  }
}
```
