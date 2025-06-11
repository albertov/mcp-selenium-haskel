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

    @pytest.mark.asyncio
    async def test_prevent_duplicate_browser_start(self, mcp_client: MCPSeleniumClient):
        """Test that starting a browser when one is already running returns an error"""
        # Start the first browser
        result1 = await mcp_client.start_browser("chrome", headless=True)
        assert "error" not in result1
        assert "sessionId" in result1 or "success" in result1 or "text" in result1

        # Try to start a second browser - should fail
        result2 = await mcp_client.start_browser("chrome", headless=True)
        assert "error" in result2
        assert "browser already started" in result2["error"].lower()

        # Clean up the first browser
        close_result = await mcp_client.close_browser()
        assert "error" not in close_result

        # Now we should be able to start a browser again
        result3 = await mcp_client.start_browser("chrome", headless=True)
        assert "error" not in result3
        assert "sessionId" in result3 or "success" in result3 or "text" in result3

        # Clean up
        close_result2 = await mcp_client.close_browser()
        assert "error" not in close_result2
