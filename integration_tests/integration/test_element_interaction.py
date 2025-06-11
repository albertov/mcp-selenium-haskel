import pytest
from utils.mcp_client import MCPSeleniumClient


class TestElementInteraction:
    """Test element finding and interaction"""

    @pytest.mark.asyncio
    async def test_find_element_by_id(self, browser: MCPSeleniumClient, test_server):
        """Test finding element by ID"""
        print("lele")
        url = f"{test_server.base_url}/form_page.html"
        await browser.navigate(url)
        print("lelo")

        result = await browser.find_element("id", "username")

        assert "error" not in result
        # The server returns JSON with elementId when found
        assert "elementId" in result or "found" in result

    @pytest.mark.asyncio
    async def test_find_nonexistent_element(self, browser: MCPSeleniumClient, test_server):
        """Test finding non-existent element"""
        url = f"{test_server.base_url}/test_page.html"
        await browser.navigate(url)

        result = await browser.find_element("id", "nonexistent", 1)

        # The server should return an error for non-existent elements
        assert "error" in result or "Element not found" in result.get("text", "")

    @pytest.mark.asyncio
    async def test_send_keys_to_input(self, browser: MCPSeleniumClient, test_server):
        """Test sending keys to input element"""
        url = f"{test_server.base_url}/form_page.html"
        await browser.navigate(url)

        # Send keys directly using locator
        keys_result = await browser.send_keys("id", "username", "testuser")
        assert "error" not in keys_result
        assert "Sent keys" in keys_result.get("text", "")
