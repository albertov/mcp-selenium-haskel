import pytest
from utils.mcp_client import MCPSeleniumClient


class TestBrowserManagement:
    """Test browser startup, configuration, and shutdown"""

    @pytest.mark.asyncio
    async def test_list_available_tools(self, mcp_client: MCPSeleniumClient):
        """Verify all expected tools are available"""
        tools = await mcp_client.list_tools()

        expected_tools = [
            "start_browser", "navigate", "find_element", "click_element",
            "send_keys", "get_element_text", "take_screenshot"
        ]

        for tool in expected_tools:
            assert tool in tools, f"Tool {tool} not found in available tools"

    @pytest.mark.asyncio
    async def test_start_chrome_browser(self, mcp_client: MCPSeleniumClient):
        """Test starting Chrome browser with explicit cleanup"""
        result = await mcp_client.start_browser("chrome", headless=True)

        assert "error" not in result
        # Check for success indicators
        assert "sessionId" in result or "success" in result or "text" in result

        # Explicitly close the browser session
        close_result = await mcp_client.close_browser()
        assert "error" not in close_result
