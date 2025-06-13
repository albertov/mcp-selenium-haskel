"""Tests for execute_js tool functionality."""

import json
import pytest
from utils.mcp_client import MCPSeleniumClient


class TestExecuteJS:
    """Test execute_js tool functionality."""

    @pytest.mark.asyncio
    async def test_execute_js_simple_calculation(self, browser: MCPSeleniumClient):
        """Test executing simple JavaScript calculation."""
        result = await browser.execute_js("return 2 + 3;")

        assert "error" not in result, f"JavaScript execution failed: {result}"
        assert result["text"] == "5", f"Expected '5', got '{result['text']}'"

    @pytest.mark.asyncio
    async def test_execute_js_string_manipulation(self, browser: MCPSeleniumClient):
        """Test executing JavaScript string manipulation."""
        result = await browser.execute_js("return 'Hello, ' + 'World!';")

        assert "error" not in result, f"JavaScript execution failed: {result}"
        assert result["text"] == "Hello, World!", f"Expected 'Hello, World!', got '{result['text']}'"

    @pytest.mark.asyncio
    async def test_execute_js_with_arguments(self, browser: MCPSeleniumClient):
        """Test executing JavaScript with arguments."""
        script = "var name = arguments[0]; var age = arguments[1]; return 'Hello ' + name + ', you are ' + age + ' years old';"
        result = await browser.execute_js(script, args=["John", "25"])

        assert "error" not in result, f"JavaScript execution failed: {result}"
        expected = "Hello John, you are 25 years old"
        assert result["text"] == expected, f"Expected '{expected}', got '{result['text']}'"

    @pytest.mark.asyncio
    async def test_execute_js_dom_access(self, browser: MCPSeleniumClient, test_server):
        """Test executing JavaScript that accesses the DOM."""
        url = f"{test_server.base_url}/test_page.html"
        await browser.navigate(url)

        result = await browser.execute_js("return document.title;")

        assert "error" not in result, f"JavaScript execution failed: {result}"
        # The test page should have a title
        assert len(result["text"]) > 0, "Expected non-empty title"

    @pytest.mark.asyncio
    async def test_execute_js_object_return(self, browser: MCPSeleniumClient, test_server):
        """Test executing JavaScript that returns an object."""
        url = f"{test_server.base_url}/test_page.html"
        await browser.navigate(url)

        script = "return {title: document.title, elementCount: document.querySelectorAll('*').length};"
        result = await browser.execute_js(script)

        assert "error" not in result, f"JavaScript execution failed: {result}"

        # The response should be a JSON string representation of the object
        try:
            data = json.loads(result["text"])
            assert "title" in data, f"Expected 'title' in data, got {data}"
            assert "elementCount" in data, f"Expected 'elementCount' in data, got {data}"
            assert data["elementCount"] > 0, f"Expected element count > 0, got {data.get('elementCount')}"
        except json.JSONDecodeError:
            pytest.fail(f"Response is not valid JSON: {result['text']}")

    @pytest.mark.asyncio
    async def test_execute_js_null_return(self, browser: MCPSeleniumClient):
        """Test executing JavaScript that returns null."""
        result = await browser.execute_js("return null;")

        assert "error" not in result, f"JavaScript execution failed: {result}"
        assert result["text"] == "null", f"Expected 'null', got '{result['text']}'"

    @pytest.mark.asyncio
    async def test_execute_js_undefined_return(self, browser: MCPSeleniumClient):
        """Test executing JavaScript that returns undefined."""
        result = await browser.execute_js("var x; return x;")

        assert "error" not in result, f"JavaScript execution failed: {result}"
        # Undefined typically becomes null in JSON serialization
        assert result["text"] == "null", f"Expected 'null', got '{result['text']}'"

    @pytest.mark.asyncio
    async def test_execute_js_with_custom_timeout(self, browser: MCPSeleniumClient):
        """Test executing JavaScript with custom timeout."""
        result = await browser.execute_js("return 'Success!';", timeout=5)

        assert "error" not in result, f"JavaScript execution failed: {result}"
        assert result["text"] == "Success!", f"Expected 'Success!', got '{result['text']}'"

    @pytest.mark.asyncio
    async def test_execute_js_without_browser(self, mcp_client: MCPSeleniumClient):
        """Test execute_js tool without starting a browser."""
        result = await mcp_client.call_tool("execute_js", {
            "session_id": "non-existent-session",
            "script": "return 'test';"
        })

        assert "error" in result or ("text" in result and "Session not found" in result["text"]), \
            "Expected error when using non-existent session"

    @pytest.mark.asyncio
    async def test_execute_js_array_return(self, browser: MCPSeleniumClient):
        """Test executing JavaScript that returns an array."""
        result = await browser.execute_js("return [1, 2, 3, 'hello', {key: 'value'}];")

        assert "error" not in result, f"JavaScript execution failed: {result}"

        try:
            data = json.loads(result["text"])
            assert isinstance(data, list), f"Expected array, got {type(data)}"
            assert len(data) == 5, f"Expected 5 elements, got {len(data)}"
            assert data[0] == 1
            assert data[1] == 2
            assert data[2] == 3
            assert data[3] == "hello"
            assert data[4] == {"key": "value"}
        except json.JSONDecodeError:
            pytest.fail(f"Response is not valid JSON: {result['text']}")

    @pytest.mark.asyncio
    async def test_execute_js_dom_manipulation(self, browser: MCPSeleniumClient):
        """Test executing JavaScript that manipulates the DOM."""
        # Navigate to a simple page
        html_content = "<html><body><div id='test'>Original</div></body></html>"
        url = f"data:text/html,{html_content}"
        await browser.navigate(url)

        # Change the content and return it
        script = "document.getElementById('test').textContent = 'Modified'; return document.getElementById('test').textContent;"
        result = await browser.execute_js(script)

        assert "error" not in result, f"JavaScript execution failed: {result}"
        assert result["text"] == "Modified", f"Expected 'Modified', got '{result['text']}'"

        # Verify the change persisted
        result2 = await browser.execute_js("return document.getElementById('test').textContent;")

        assert "error" not in result2, f"JavaScript execution failed: {result2}"
        assert result2["text"] == "Modified", f"Expected 'Modified', got '{result2['text']}'"

    @pytest.mark.asyncio
    async def test_execute_js_window_properties(self, browser: MCPSeleniumClient):
        """Test accessing window properties through JavaScript."""
        result = await browser.execute_js("return typeof window;")

        assert "error" not in result, f"JavaScript execution failed: {result}"
        assert result["text"] == "object", f"Expected 'object', got '{result['text']}'"

    @pytest.mark.asyncio
    async def test_execute_js_math_operations(self, browser: MCPSeleniumClient):
        """Test complex math operations."""
        script = "return Math.sqrt(16) + Math.pow(2, 3) + Math.round(4.7);"
        result = await browser.execute_js(script)

        assert "error" not in result, f"JavaScript execution failed: {result}"
        # sqrt(16) = 4, pow(2,3) = 8, round(4.7) = 5, total = 17
        assert result["text"] == "17", f"Expected '17', got '{result['text']}'"
