import pytest
from utils.mcp_client import MCPSeleniumClient


class TestGetSource:
    """Test get_source functionality"""

    @pytest.mark.asyncio
    async def test_get_source_basic_page(self, browser: MCPSeleniumClient, test_server):
        """Test getting source from a basic HTML page"""
        url = f"{test_server.base_url}/test_page.html"
        await browser.navigate(url)

        result = await browser.get_source()

        assert "error" not in result
        assert "text" in result

        source = result["text"]
        assert isinstance(source, str)
        assert len(source) > 0

        # Check for expected HTML elements from test_page.html
        assert "<html>" in source or "<!DOCTYPE html>" in source
        assert "<title>" in source
        assert "<body>" in source

    @pytest.mark.asyncio
    async def test_get_source_complex_page(self, browser: MCPSeleniumClient, test_server):
        """Test getting source from a more complex page with forms"""
        url = f"{test_server.base_url}/form_page.html"
        await browser.navigate(url)

        result = await browser.get_source()

        assert "error" not in result
        assert "text" in result

        source = result["text"]
        assert isinstance(source, str)
        assert len(source) > 0

        # Check for form elements
        assert "<form" in source
        assert "<input" in source or "<button" in source

    @pytest.mark.asyncio
    async def test_get_source_after_interaction(self, browser: MCPSeleniumClient, test_server):
        """Test getting source after page interaction (e.g., clicking elements)"""
        url = f"{test_server.base_url}/test_page.html"
        await browser.navigate(url)

        # First get initial source
        initial_result = await browser.get_source()
        assert "error" not in initial_result

        # Try to interact with an element if it exists
        find_result = await browser.find_element("tag", "body")
        if "error" not in find_result:
            # Get source after finding element (state may have changed)
            after_result = await browser.get_source()
            assert "error" not in after_result
            after_source = after_result["text"]

            # Source should still be valid HTML
            assert isinstance(after_source, str)
            assert len(after_source) > 0
            assert "<html>" in after_source or "<!DOCTYPE html>" in after_source

    @pytest.mark.asyncio
    async def test_get_source_without_browser(self):
        """Test get_source without an active browser session"""
        # Create client without starting browser
        import os
        executable_path = os.path.join(os.getcwd(), "dist-newstyle/build/x86_64-linux/ghc-9.10.2/mcp-selenium-0.1.0/x/mcp-selenium-hs/build/mcp-selenium-hs/mcp-selenium-hs")

        async with MCPSeleniumClient(executable_path) as client:
            result = await client.get_source()

            # Should return error about no active session
            assert "error" in result
            assert "session" in result["error"].lower()

    @pytest.mark.asyncio
    async def test_get_source_returns_current_state(self, browser: MCPSeleniumClient, test_server):
        """Test that get_source returns the current DOM state"""
        url = f"{test_server.base_url}/test_page.html"
        await browser.navigate(url)

        # Get initial source
        result1 = await browser.get_source()
        assert "error" not in result1
        source1 = result1["text"]

        # Navigate to a different page
        url2 = f"{test_server.base_url}/form_page.html"
        await browser.navigate(url2)

        # Get source of new page
        result2 = await browser.get_source()
        assert "error" not in result2
        source2 = result2["text"]

        # Sources should be different (different pages)
        assert source1 != source2

        # Both should be valid HTML
        assert "<html>" in source1 or "<!DOCTYPE html>" in source1
        assert "<html>" in source2 or "<!DOCTYPE html>" in source2
