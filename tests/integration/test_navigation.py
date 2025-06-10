import pytest
from utils.mcp_client import MCPSeleniumClient


class TestNavigation:
    """Test URL navigation functionality"""

    @pytest.mark.asyncio
    async def test_navigate_to_valid_url(self, mcp_client: MCPSeleniumClient, test_server):
        """Test navigation to valid URL"""
        # Start browser first
        await mcp_client.start_browser()

        # Navigate to test page
        url = f"{test_server.base_url}/test_page.html"
        result = await mcp_client.navigate(url)

        assert "error" not in result
        # Accept various success indicators
        assert ("success" in result or
                "navigated" in result.get("text", "").lower() or
                "text" in result)

    @pytest.mark.asyncio
    async def test_navigate_without_browser(self, mcp_client: MCPSeleniumClient):
        """Test navigation without starting browser first (should error)"""
        result = await mcp_client.navigate("http://example.com")

        # Should return error since no browser session exists
        assert ("error" in result or
                "no browser" in result.get("text", "").lower() or
                "no active browser" in result.get("text", "").lower())
