import pytest
from utils.mcp_client import MCPSeleniumClient


class TestMultipleElements:
    """Test finding multiple elements and getting their text content"""

    @pytest.mark.asyncio
    async def test_find_elements_by_class(self, browser: MCPSeleniumClient, test_server):
        """Test finding multiple elements by class name"""
        url = f"{test_server.base_url}/multi_elements_page.html"
        await browser.navigate(url)

        # Find all fruit items
        result = await browser.find_elements("class", "fruit-item")

        assert "error" not in result
        assert "elementIds" in result
        assert "count" in result
        assert result["count"] == 5  # Should find 5 fruit items
        assert len(result["elementIds"]) == 5

    @pytest.mark.asyncio
    async def test_find_elements_by_css_selector(self, browser: MCPSeleniumClient, test_server):
        """Test finding multiple elements by CSS selector"""
        url = f"{test_server.base_url}/multi_elements_page.html"
        await browser.navigate(url)

        # Find all product cards
        result = await browser.find_elements("css", ".card.product")

        assert "error" not in result
        assert "elementIds" in result
        assert "count" in result
        assert result["count"] == 3  # Should find 3 product cards
        assert len(result["elementIds"]) == 3

    @pytest.mark.asyncio
    async def test_find_elements_by_tag_name(self, browser: MCPSeleniumClient, test_server):
        """Test finding multiple elements by tag name"""
        url = f"{test_server.base_url}/multi_elements_page.html"
        await browser.navigate(url)

        # Find all buttons
        result = await browser.find_elements("tag", "button")

        assert "error" not in result
        assert "elementIds" in result
        assert "count" in result
        assert result["count"] == 4  # Should find 4 action buttons

    @pytest.mark.asyncio
    async def test_find_elements_no_matches(self, browser: MCPSeleniumClient, test_server):
        """Test finding elements when no matches exist"""
        url = f"{test_server.base_url}/multi_elements_page.html"
        await browser.navigate(url)

        # Try to find elements that don't exist - use short timeout since we expect no matches
        result = await browser.find_elements("class", "nonexistent-class", timeout=0.2)

        assert "error" not in result
        assert "elementIds" in result
        assert "count" in result
        assert result["count"] == 0  # Should find 0 elements
        assert len(result["elementIds"]) == 0

    @pytest.mark.asyncio
    async def test_get_elements_text_by_class(self, browser: MCPSeleniumClient, test_server):
        """Test getting text content from multiple elements by class"""
        url = f"{test_server.base_url}/multi_elements_page.html"
        await browser.navigate(url)

        # Get text from all fruit items
        result = await browser.get_elements_text("class", "fruit-item")

        assert "error" not in result
        assert "texts" in result
        assert "count" in result
        assert result["count"] == 5

        expected_fruits = ["Apple", "Banana", "Cherry", "Date", "Elderberry"]
        assert result["texts"] == expected_fruits

    @pytest.mark.asyncio
    async def test_get_elements_text_by_css_selector(self, browser: MCPSeleniumClient, test_server):
        """Test getting text content from multiple elements by CSS selector"""
        url = f"{test_server.base_url}/multi_elements_page.html"
        await browser.navigate(url)

        # Get text from all price spans
        result = await browser.get_elements_text("css", ".price")

        assert "error" not in result
        assert "texts" in result
        assert "count" in result
        assert result["count"] == 3

        expected_prices = ["$10.99", "$15.49", "$8.75"]
        assert result["texts"] == expected_prices

    @pytest.mark.asyncio
    async def test_get_elements_text_action_buttons(self, browser: MCPSeleniumClient, test_server):
        """Test getting text content from action buttons"""
        url = f"{test_server.base_url}/multi_elements_page.html"
        await browser.navigate(url)

        # Get text from all action buttons
        result = await browser.get_elements_text("class", "action-btn")

        assert "error" not in result
        assert "texts" in result
        assert "count" in result
        assert result["count"] == 4

        expected_buttons = ["Save", "Cancel", "Delete", "Submit"]
        assert result["texts"] == expected_buttons

    @pytest.mark.asyncio
    async def test_get_elements_text_navigation_links(self, browser: MCPSeleniumClient, test_server):
        """Test getting text content from navigation links"""
        url = f"{test_server.base_url}/multi_elements_page.html"
        await browser.navigate(url)

        # Get text from all navigation links
        result = await browser.get_elements_text("class", "nav-link")

        assert "error" not in result
        assert "texts" in result
        assert "count" in result
        assert result["count"] == 4

        expected_links = ["Home", "About", "Contact", "Services"]
        assert result["texts"] == expected_links

    @pytest.mark.asyncio
    async def test_get_elements_text_info_paragraphs(self, browser: MCPSeleniumClient, test_server):
        """Test getting text content from info paragraphs"""
        url = f"{test_server.base_url}/multi_elements_page.html"
        await browser.navigate(url)

        # Get text from all info paragraphs
        result = await browser.get_elements_text("class", "info-text")

        assert "error" not in result
        assert "texts" in result
        assert "count" in result
        assert result["count"] == 3

        expected_texts = [
            "First paragraph with some information",
            "Second paragraph with different content",
            "Third paragraph explaining something else"
        ]
        assert result["texts"] == expected_texts

    @pytest.mark.asyncio
    async def test_get_elements_text_no_matches(self, browser: MCPSeleniumClient, test_server):
        """Test getting text content when no elements match"""
        url = f"{test_server.base_url}/multi_elements_page.html"
        await browser.navigate(url)

        # Try to get text from elements that don't exist - use short timeout since we expect no matches
        result = await browser.get_elements_text("class", "nonexistent-class", timeout=0.2)

        assert "error" not in result
        assert "texts" in result
        assert "count" in result
        assert result["count"] == 0
        assert result["texts"] == []

    @pytest.mark.asyncio
    async def test_find_elements_xpath(self, browser: MCPSeleniumClient, test_server):
        """Test finding multiple elements using XPath"""
        url = f"{test_server.base_url}/multi_elements_page.html"
        await browser.navigate(url)

        # Find all divs with class 'item'
        result = await browser.find_elements("xpath", "//div[@class='item']")

        assert "error" not in result
        assert "elementIds" in result
        assert "count" in result
        assert result["count"] == 5  # Should find 5 data items

    @pytest.mark.asyncio
    async def test_get_elements_text_xpath(self, browser: MCPSeleniumClient, test_server):
        """Test getting text content using XPath"""
        url = f"{test_server.base_url}/multi_elements_page.html"
        await browser.navigate(url)

        # Get text from all divs with class 'item'
        result = await browser.get_elements_text("xpath", "//div[@class='item']")

        assert "error" not in result
        assert "texts" in result
        assert "count" in result
        assert result["count"] == 5

        expected_items = [
            "User Item 1",
            "User Item 2",
            "Admin Item 1",
            "User Item 3",
            "Guest Item 1"
        ]
        assert result["texts"] == expected_items

    @pytest.mark.asyncio
    async def test_find_elements_without_browser(self):
        """Test that find_elements fails without an active browser session"""
        # Create client without using browser fixture
        from utils.mcp_client import MCPSeleniumClient
        import os

        executable_path = os.environ.get("MCP_SELENIUM_EXECUTABLE", "/home/alberto/src/mcp-selenium-haskell/run_server.sh")

        async with MCPSeleniumClient(executable_path) as client:
            # Don't start browser - should fail
            result = await client.find_elements("class", "test-class")

            assert "error" in result
            assert "No active browser session" in result["error"]

    @pytest.mark.asyncio
    async def test_get_elements_text_without_browser(self):
        """Test that get_elements_text fails without an active browser session"""
        # Create client without using browser fixture
        from utils.mcp_client import MCPSeleniumClient
        import os

        executable_path = os.environ.get("MCP_SELENIUM_EXECUTABLE", "/home/alberto/src/mcp-selenium-haskell/run_server.sh")

        async with MCPSeleniumClient(executable_path) as client:
            # Don't start browser - should fail
            result = await client.get_elements_text("class", "test-class")

            assert "error" in result
            assert "No active browser session" in result["error"]

    @pytest.mark.asyncio
    async def test_find_elements_with_timeout(self, browser: MCPSeleniumClient, test_server):
        """Test finding elements with custom timeout"""
        url = f"{test_server.base_url}/multi_elements_page.html"
        await browser.navigate(url)

        # Find elements with a short timeout - should still work for existing elements
        result = await browser.find_elements("class", "fruit-item", timeout=1)

        assert "error" not in result
        assert "elementIds" in result
        assert "count" in result
        assert result["count"] == 5

    @pytest.mark.asyncio
    async def test_get_elements_text_with_timeout(self, browser: MCPSeleniumClient, test_server):
        """Test getting elements text with custom timeout"""
        url = f"{test_server.base_url}/multi_elements_page.html"
        await browser.navigate(url)

        # Get text with a short timeout - should still work for existing elements
        result = await browser.get_elements_text("class", "fruit-item", timeout=1)

        assert "error" not in result
        assert "texts" in result
        assert "count" in result
        assert result["count"] == 5
        expected_fruits = ["Apple", "Banana", "Cherry", "Date", "Elderberry"]
        assert result["texts"] == expected_fruits
