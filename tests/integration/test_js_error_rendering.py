import pytest
from utils.mcp_client import MCPSeleniumClient


class TestJSErrorRendering:
    """Test that pages with JavaScript errors still render correctly"""

    @pytest.mark.asyncio
    async def test_page_with_js_errors_renders_correctly(self, mcp_client: MCPSeleniumClient, test_server):
        """Test that a page with JS errors still renders and is functional"""
        await mcp_client.start_browser()

        # Navigate to the page with JS errors
        url = f"{test_server.base_url}/test_page_with_js_error.html"
        nav_result = await mcp_client.navigate(url)

        # Navigation should succeed despite JS errors
        assert "error" not in nav_result
        assert ("success" in nav_result or
                "navigated" in nav_result.get("text", "").lower() or
                "text" in nav_result)

        # Verify main page elements are rendered correctly
        title_result = await mcp_client.find_element("id", "page-title")
        assert "error" not in title_result
        assert "elementId" in title_result or "found" in title_result

        # Verify main content is present
        content_result = await mcp_client.find_element("id", "main-content")
        assert "error" not in content_result
        assert "elementId" in content_result or "found" in content_result

        # Verify the status indicator is present
        status_result = await mcp_client.find_element("id", "status")
        assert "error" not in status_result
        assert "elementId" in status_result or "found" in status_result

        # Verify the button that triggers JS errors is present and clickable
        button_result = await mcp_client.find_element("id", "trigger-error-button")
        assert "error" not in button_result
        assert "elementId" in button_result or "found" in button_result

    @pytest.mark.asyncio
    async def test_page_with_js_errors_interactive_elements_work(self, mcp_client: MCPSeleniumClient, test_server):
        """Test that interactive elements work despite JS errors"""
        await mcp_client.start_browser()

        # Navigate to the page
        url = f"{test_server.base_url}/test_page_with_js_error.html"
        await mcp_client.navigate(url)

        # Click the button that triggers additional JS errors
        click_result = await mcp_client.click_element("id", "trigger-error-button")

        # The click should succeed even though it triggers JS errors
        assert "error" not in click_result
        assert ("clicked" in click_result.get("text", "").lower() or
                "success" in click_result or
                "text" in click_result)

        # Verify that the error indicator element is still accessible after the click
        error_indicator_result = await mcp_client.find_element("id", "error-indicator")
        assert "error" not in error_indicator_result
        assert "elementId" in error_indicator_result or "found" in error_indicator_result

    @pytest.mark.asyncio
    async def test_page_heading_text_correct_despite_js_errors(self, mcp_client: MCPSeleniumClient, test_server):
        """Test that page heading text is accessible despite JS errors"""
        await mcp_client.start_browser()

        # Navigate to the page
        url = f"{test_server.base_url}/test_page_with_js_error.html"
        await mcp_client.navigate(url)

        # Get the heading text - this should work despite JS errors
        heading_result = await mcp_client.call_tool("get_element_text", {
            "by": "id",
            "value": "page-title",
            "timeout": 10000
        })

        assert "error" not in heading_result
        # The heading should contain "Test Page with JavaScript Error"
        heading_text = heading_result.get("text", "").lower()
        assert ("test page with javascript error" in heading_text or
                "javascript error" in heading_text)
