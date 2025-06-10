import pytest
from utils.mcp_client import MCPSeleniumClient


class TestElementInteraction:
    """Test element finding and interaction"""

    @pytest.mark.asyncio
    async def test_find_element_by_id(self, mcp_client: MCPSeleniumClient, test_server):
        """Test finding element by ID"""
        await mcp_client.start_browser()
        url = f"{test_server.base_url}/form_page.html"
        await mcp_client.navigate(url)

        result = await mcp_client.find_element("id", "username")

        assert "error" not in result
        assert "elementId" in result or "element" in result

    @pytest.mark.asyncio
    async def test_find_nonexistent_element(self, mcp_client: MCPSeleniumClient, test_server):
        """Test finding non-existent element"""
        await mcp_client.start_browser()
        url = f"{test_server.base_url}/test_page.html"
        await mcp_client.navigate(url)

        result = await mcp_client.find_element("id", "nonexistent")

        assert "error" in result

    @pytest.mark.asyncio
    async def test_send_keys_to_input(self, mcp_client: MCPSeleniumClient, test_server):
        """Test sending keys to input element"""
        await mcp_client.start_browser()
        url = f"{test_server.base_url}/form_page.html"
        await mcp_client.navigate(url)

        # Find input element
        find_result = await mcp_client.find_element("id", "username")
        assert "error" not in find_result

        # Extract element ID
        element_id = find_result.get("elementId") or find_result.get("element")
        assert element_id is not None

        # Send keys
        keys_result = await mcp_client.send_keys(element_id, "testuser")
        assert "error" not in keys_result
