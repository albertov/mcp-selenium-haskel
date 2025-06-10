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
        options = {
            "headless": headless,
            "arguments": []
        }

        return await self.call_tool("start_browser", {
            "browser": browser,
            "options": options,
            "enableLogging": True
        })

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
