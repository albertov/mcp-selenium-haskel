import json
from typing import Dict, Any, Optional, List

from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client


class MCPSeleniumClient:
    """Wrapper for MCP Selenium client to simplify test interactions"""

    def __init__(self, executable_path: str):
        self.executable_path = executable_path
        self.session: Optional[ClientSession] = None
        self._stdio_context_manager = None
        self._session_context_manager = None
        self.session_id: Optional[str] = None

    async def __aenter__(self):
        print(f"DEBUG: Starting MCP client with executable: {self.executable_path}")

        # Configure server parameters for Haskell executable
        server_params = StdioServerParameters(
            command=self.executable_path,
            args=[],  # Add any required args for your Haskell executable
            env=None
        )
        print("DEBUG: Configured server parameters")

        # Connect to server
        print("DEBUG: Connecting to server...")
        self._stdio_context_manager = stdio_client(server_params)
        stdio_transport = await self._stdio_context_manager.__aenter__()
        self.stdio, self.write = stdio_transport
        print("DEBUG: Connected to server")

        # Create session
        print("DEBUG: Creating session...")
        self._session_context_manager = ClientSession(self.stdio, self.write)
        self.session = await self._session_context_manager.__aenter__()
        print("DEBUG: Session created")

        # Initialize connection
        print("DEBUG: Initializing connection...")
        await self.session.initialize()
        print("DEBUG: Connection initialized successfully")
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        print("DEBUG: MCPSeleniumClient __aexit__")
        if self._session_context_manager is not None:
            try:
                await self._session_context_manager.__aexit__(exc_type, exc_val, exc_tb)
            except Exception:
                pass  # Ignore cleanup errors

        if self._stdio_context_manager is not None:
            try:
                await self._stdio_context_manager.__aexit__(exc_type, exc_val, exc_tb)
            except Exception:
                pass  # Ignore cleanup errors

    async def list_tools(self) -> List[str]:
        """Get list of available tools from server"""
        print("DEBUG: Calling list_tools...")
        response = await self.session.list_tools()
        print(f"DEBUG: Got tools response: {[tool.name for tool in response.tools]}")
        return [tool.name for tool in response.tools]

    async def call_tool(self, tool_name: str, arguments: Dict[str, Any]) -> Dict[str, Any]:
        """Call a tool and return the result"""
        # For tools that require session_id, add it automatically if not present
        session_required_tools = {
            "navigate", "find_element", "click_element", "send_keys", "get_element_text",
            "hover", "drag_and_drop", "double_click", "right_click", "press_key",
            "upload_file", "take_screenshot", "close_browser", "get_console_logs",
            "get_available_log_types", "inject_console_logger", "get_injected_console_logs",
            "get_source"
        }

        if tool_name in session_required_tools and "session_id" not in arguments:
            if not self.session_id:
                return {"error": "No active browser session. Call start_browser() first."}
            arguments = {**arguments, "session_id": self.session_id}

        result = await self.session.call_tool(tool_name, arguments)

        # Parse result content
        if result.content and result.content[0].type == "text":
            try:
                return json.loads(result.content[0].text)
            except json.JSONDecodeError:
                return {"text": result.content[0].text}

        return {"error": "No content returned"}

    async def start_browser(self, browser: str = "chrome", headless: bool = True) -> Dict[str, Any]:
        """Start a browser session"""
        # Check if browser is already running
        if self.session_id:
            return {"error": "Browser already started. Close the current session before starting a new one."}

        options = {
            "headless": headless,
            "arguments": []
        }

        result = await self.call_tool("start_browser", {
            "browser": browser,
            "options": options,
            "enableLogging": True
        })

        # Extract and store session_id from result
        if "sessionId" in result:
            self.session_id = result["sessionId"]

        return result

    async def navigate(self, url: str) -> Dict[str, Any]:
        """Navigate to URL"""
        return await self.call_tool("navigate", {"url": url})

    async def find_element(self, by: str, value: str, timeout: Optional[int] = 10) -> Dict[str, Any]:
        """Find element on page"""
        return await self.call_tool("find_element", {
            "by": by,
            "value": value,
            "timeout": timeout * 1000 if timeout else 10000  # Convert to milliseconds
        })

    async def click_element(self, by: str, value: str, timeout: Optional[int] = 10) -> Dict[str, Any]:
        """Click an element"""
        return await self.call_tool("click_element", {
            "by": by,
            "value": value,
            "timeout": timeout * 1000 if timeout else 10000
        })

    async def send_keys(self, by: str, value: str, text: str, timeout: Optional[int] = 10) -> Dict[str, Any]:
        """Send keys to element"""
        return await self.call_tool("send_keys", {
            "by": by,
            "value": value,
            "text": text,
            "timeout": timeout * 1000 if timeout else 10000
        })

    async def take_screenshot(self, element_id: Optional[str] = None) -> Dict[str, Any]:
        """Take screenshot"""
        args = {}
        if element_id:
            args["elementId"] = element_id
        return await self.call_tool("take_screenshot", args)

    async def get_source(self) -> Dict[str, Any]:
        """Get the current page's HTML source code"""
        return await self.call_tool("get_source", {})

    async def close_browser(self) -> Dict[str, Any]:
        """Close the browser session"""
        if not self.session_id:
            return {"error": "No active browser session to close."}

        result = await self.call_tool("close_browser", {})
        # Clear session_id after closing
        if "error" not in result:
            self.session_id = None
        return result

    async def hover(self, by: str, value: str, timeout: Optional[int] = 10) -> Dict[str, Any]:
        """Hover over an element"""
        return await self.call_tool("hover", {
            "by": by,
            "value": value,
            "timeout": timeout * 1000 if timeout else 10000
        })

    async def double_click(self, by: str, value: str, timeout: Optional[int] = 10) -> Dict[str, Any]:
        """Double click an element"""
        return await self.call_tool("double_click", {
            "by": by,
            "value": value,
            "timeout": timeout * 1000 if timeout else 10000
        })

    async def right_click(self, by: str, value: str, timeout: Optional[int] = 10) -> Dict[str, Any]:
        """Right click an element"""
        return await self.call_tool("right_click", {
            "by": by,
            "value": value,
            "timeout": timeout * 1000 if timeout else 10000
        })

    async def press_key(self, key: str) -> Dict[str, Any]:
        """Press a key (e.g., 'Enter', 'Tab', 'Escape')"""
        return await self.call_tool("press_key", {
            "key": key
        })

    async def drag_and_drop(self, by: str, value: str, target_by: str, target_value: str, timeout: Optional[int] = 10) -> Dict[str, Any]:
        """Drag an element and drop it on another element"""
        return await self.call_tool("drag_and_drop", {
            "by": by,
            "value": value,
            "targetBy": target_by,
            "targetValue": target_value,
            "timeout": timeout * 1000 if timeout else 10000
        })

    async def upload_file(self, by: str, value: str, file_path: str, timeout: Optional[int] = 10) -> Dict[str, Any]:
        """Upload a file to a file input element"""
        return await self.call_tool("upload_file", {
            "by": by,
            "value": value,
            "filePath": file_path,
            "timeout": timeout * 1000 if timeout else 10000
        })

    async def get_element_text(self, by: str, value: str, timeout: Optional[int] = 10) -> Dict[str, Any]:
        """Get text content of an element"""
        return await self.call_tool("get_element_text", {
            "by": by,
            "value": value,
            "timeout": timeout * 1000 if timeout else 10000
        })

    async def get_console_logs(self, log_level: Optional[str] = None, max_entries: Optional[int] = None) -> Dict[str, Any]:
        """Get console logs from the browser"""
        args = {}
        if log_level:
            args["logLevel"] = log_level
        if max_entries:
            args["maxEntries"] = max_entries
        return await self.call_tool("get_console_logs", args)

    async def inject_console_logger(self, timeout: Optional[int] = None) -> Dict[str, Any]:
        """Inject console logger into the current page"""
        args = {}
        if timeout:
            args["timeout"] = timeout
        return await self.call_tool("inject_console_logger", args)

    async def get_injected_console_logs(self, clear: bool = False) -> Dict[str, Any]:
        """Get logs from the injected console logger"""
        return await self.call_tool("get_injected_console_logs", {
            "clear": clear
        })

    async def get_available_log_types(self) -> Dict[str, Any]:
        """Get available log types from the browser"""
        return await self.call_tool("get_available_log_types", {})
