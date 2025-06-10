import asyncio
import json
import subprocess
import tempfile
from contextlib import AsyncExitStack
from typing import Dict, Any, Optional, List
from pathlib import Path

from mcp import ClientSession, StdioServerParameters
from mcp.client.stdio import stdio_client


class MCPSeleniumClient:
    """Wrapper for MCP Selenium client to simplify test interactions"""

    def __init__(self, executable_path: str):
        self.executable_path = executable_path
        self.session: Optional[ClientSession] = None
        self.exit_stack = AsyncExitStack()

    async def __aenter__(self):
        # Configure server parameters for Haskell executable
        server_params = StdioServerParameters(
            command=self.executable_path,
            args=[],  # Add any required args for your Haskell executable
            env=None
        )

        # Connect to server
        stdio_transport = await self.exit_stack.enter_async_context(
            stdio_client(server_params)
        )
        self.stdio, self.write = stdio_transport

        # Create session
        self.session = await self.exit_stack.enter_async_context(
            ClientSession(self.stdio, self.write)
        )

        # Initialize connection
        await self.session.initialize()
        return self

    async def __aexit__(self, exc_type, exc_val, exc_tb):
        await self.exit_stack.aclose()

    async def list_tools(self) -> List[str]:
        """Get list of available tools from server"""
        response = await self.session.list_tools()
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
            "arguments": ["--no-sandbox", "--disable-dev-shm-usage"]
        }

        return await self.call_tool("StartBrowser", {
            "browser": browser,
            "opts": options,
            "enableLogging": True
        })

    async def navigate(self, url: str) -> Dict[str, Any]:
        """Navigate to URL"""
        return await self.call_tool("Navigate", {"url": url})

    async def find_element(self, by: str, value: str, timeout: Optional[int] = 10) -> Dict[str, Any]:
        """Find element on page"""
        return await self.call_tool("FindElement", {
            "strategy": by,
            "value": value,
            "timeout": timeout
        })

    async def click_element(self, element_id: str) -> Dict[str, Any]:
        """Click an element"""
        return await self.call_tool("Click", {"elementId": element_id})

    async def send_keys(self, element_id: str, text: str) -> Dict[str, Any]:
        """Send keys to element"""
        return await self.call_tool("SendKeys", {
            "elementId": element_id,
            "text": text
        })

    async def take_screenshot(self, element_id: Optional[str] = None) -> Dict[str, Any]:
        """Take screenshot"""
        args = {}
        if element_id:
            args["elementId"] = element_id
        return await self.call_tool("TakeScreenshot", args)
