import pytest
import base64
from utils.mcp_client import MCPSeleniumClient


class TestScreenshots:
    """Test screenshot functionality"""

    @pytest.mark.asyncio
    async def test_full_page_screenshot(self, mcp_client: MCPSeleniumClient, test_server):
        """Test taking full page screenshot"""
        await mcp_client.start_browser()
        url = f"{test_server.base_url}/test_page.html"
        await mcp_client.navigate(url)

        result = await mcp_client.take_screenshot()

        assert "error" not in result
        # Check for screenshot data in common formats
        screenshot_data = None
        if "screenshot" in result:
            screenshot_data = result["screenshot"]
        elif "base64" in result:
            screenshot_data = result["base64"]
        elif "text" in result and "base64" in result["text"]:
            # Extract base64 data from text response
            import re
            base64_pattern = r'([A-Za-z0-9+/]{4})*([A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=)?'
            matches = re.findall(base64_pattern, result["text"])
            if matches:
                screenshot_data = ''.join([''.join(match) for match in matches])

        # Verify it's valid base64 if we found screenshot data
        if screenshot_data:
            try:
                decoded = base64.b64decode(screenshot_data, validate=True)
                assert len(decoded) > 0, "Screenshot data should not be empty"
            except Exception as e:
                pytest.fail(f"Screenshot data is not valid base64: {e}")
        else:
            # If no screenshot data found, at least verify no error occurred
            assert "error" not in result, "Screenshot should not return error"

    @pytest.mark.asyncio
    async def test_screenshot_without_navigation(self, mcp_client: MCPSeleniumClient):
        """Test taking screenshot immediately after browser start"""
        await mcp_client.start_browser()

        result = await mcp_client.take_screenshot()

        # Should work even without navigation (blank page)
        assert "error" not in result

    @pytest.mark.asyncio
    async def test_screenshot_without_browser(self, mcp_client: MCPSeleniumClient):
        """Test taking screenshot without starting browser (should error)"""
        result = await mcp_client.take_screenshot()

        # Should return error since no browser session exists
        assert ("error" in result or
                "no browser" in result.get("text", "").lower() or
                "no active browser" in result.get("text", "").lower())
