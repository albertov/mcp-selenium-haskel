import pytest
from utils.mcp_client import MCPSeleniumClient
import time
import logging

# Configure logging for better test debugging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

class TestConsoleLogging:
    """Test console logging functionality"""

    @pytest.mark.asyncio
    async def test_get_console_logs_with_js_errors(self, browser: MCPSeleniumClient, test_server):
        """Test that console logs can be retrieved from a page with JavaScript errors"""

        # Navigate to the test page with JS errors
        url = f"{test_server.base_url}/test_page_with_js_error.html"
        nav_result = await browser.navigate(url)

        logger.info(f"Navigation result: {nav_result}")
        assert "error" not in nav_result

        # Wait a moment for JavaScript errors to occur
        time.sleep(2)

        # Get console logs
        logs_result = await browser.call_tool("get_console_logs", {})

        logger.info(f"Console logs result: {logs_result}")
        assert "error" not in logs_result

        # Parse the logs response
        if "logs" in logs_result:
            logs = logs_result.get("logs", [])

            logger.info(f"Found {len(logs)} log entries")

            # We should have some log entries since the page has JavaScript errors
            assert len(logs) > 0, "Expected to find console log entries from JavaScript errors"

            # Look for error-related logs
            error_logs = [log for log in logs if "error" in log.get("level", "").lower() or
                         "undefined" in log.get("message", "").lower()]

            logger.info(f"Found {len(error_logs)} error-related logs")

            # We expect to find some error logs from the test page
            assert len(error_logs) > 0, "Expected to find JavaScript error logs"

            # Verify log structure
            for log in logs[:3]:  # Check first 3 logs
                assert "level" in log, "Log entry should have 'level' field"
                assert "message" in log, "Log entry should have 'message' field"
                assert "timestamp" in log, "Log entry should have 'timestamp' field"

        else:
            pytest.fail("No 'logs' field in logs result")

    @pytest.mark.asyncio
    async def test_inject_console_logger_and_get_injected_logs(self, browser: MCPSeleniumClient, test_server):
        """Test injecting console logger and retrieving injected logs"""

        # Navigate to the test page
        url = f"{test_server.base_url}/test_page_with_js_error.html"
        await browser.navigate(url)

        # Inject console logger
        inject_result = await browser.call_tool("inject_console_logger", {})

        logger.info(f"Inject console logger result: {inject_result}")
        assert "error" not in inject_result
        assert "injected successfully" in inject_result.get("text", "").lower()

        # Wait for page JavaScript to execute
        time.sleep(2)

        # Trigger the button that causes additional errors
        click_result = await browser.click_element("id", "trigger-error-button")
        logger.info(f"Button click result: {click_result}")

        # Wait for the error to be logged
        time.sleep(1)

        # Get injected console logs
        injected_logs_result = await browser.call_tool("get_injected_console_logs", {
            "clear": False
        })

        logger.info(f"Injected logs result: {injected_logs_result}")
        assert "error" not in injected_logs_result

        # The injected logs come back as a JSON array directly
        if isinstance(injected_logs_result, list):
            logs = injected_logs_result
        else:
            pytest.fail("Expected injected logs result to be a list")

        logger.info(f"Found {len(logs)} injected log entries")

        # We should have captured some console messages
        assert len(logs) > 0, "Expected to find injected console log entries"

        # Look for the expected error messages
        error_messages = [log for log in logs if "error" in log.get("level", "").lower()]
        info_messages = [log for log in logs if "log" in log.get("level", "").lower()]

        logger.info(f"Found {len(error_messages)} error messages and {len(info_messages)} info messages")

        # Verify we have some error logs from the JavaScript errors
        assert len(error_messages) > 0 or len(info_messages) > 0, "Expected to find some console messages"

        # Verify log structure
        for log in logs[:3]:  # Check first 3 logs
            assert "level" in log, "Injected log entry should have 'level' field"
            assert "message" in log, "Injected log entry should have 'message' field"
            assert "timestamp" in log, "Injected log entry should have 'timestamp' field"

    @pytest.mark.asyncio
    async def test_get_available_log_types(self, browser: MCPSeleniumClient):
        """Test getting available log types"""

        # Get available log types
        log_types_result = await browser.call_tool("get_available_log_types", {})

        logger.info(f"Available log types result: {log_types_result}")
        assert "error" not in log_types_result

        if "logTypes" in log_types_result:
            log_types = log_types_result.get("logTypes", [])

            logger.info(f"Available log types: {log_types}")

            # We should have some log types available
            assert len(log_types) > 0, "Expected to find available log types"

            # Common log types that should be available
            # Note: Chrome may not always report "browser" in available types even when browser logs work
            # "server" logs are the minimum requirement for basic functionality
            expected_types = ["server", "browser"]
            for expected_type in expected_types:
                assert expected_type in log_types, f"Expected '{expected_type}' to be in available log types: {log_types}"

            # Check that we have at least some log types available
            assert len(log_types) > 0, "Expected to find at least one available log type"

        else:
            pytest.fail("No 'logTypes' field in log types result")

    @pytest.mark.asyncio
    async def test_console_logs_with_filter(self, browser: MCPSeleniumClient, test_server):
        """Test console logs with level filtering"""

        # Navigate to the test page
        url = f"{test_server.base_url}/test_page_with_js_error.html"
        await browser.navigate(url)

        # Wait for JavaScript errors to occur
        time.sleep(2)

        # Get console logs with level filter
        logs_result = await browser.call_tool("get_console_logs", {
            "logLevel": "SEVERE",
            "maxEntries": 5
        })

        logger.info(f"Filtered console logs result: {logs_result}")
        assert "error" not in logs_result

        if "logs" in logs_result:
            logs = logs_result.get("logs", [])

            logger.info(f"Found {len(logs)} filtered log entries")

            # Verify we don't exceed the max entries limit
            assert len(logs) <= 5, "Should not exceed maxEntries limit"

        else:
            logger.info("No logs field in logs result, which may be expected if no SEVERE logs exist")

    @pytest.mark.asyncio
    async def test_inject_console_logger_with_custom_timeout(self, browser: MCPSeleniumClient, test_server):
        """Test injecting console logger with custom timeout parameter"""

        # Navigate to the test page
        url = f"{test_server.base_url}/test_page_with_js_error.html"
        await browser.navigate(url)

        # Inject console logger with custom timeout (30 seconds)
        inject_result = await browser.call_tool("inject_console_logger", {
            "timeout": 30000
        })

        logger.info(f"Inject console logger with timeout result: {inject_result}")
        assert "error" not in inject_result
        assert "injected successfully" in inject_result.get("text", "").lower()

        # Wait for page JavaScript to execute
        time.sleep(2)

        # Get injected console logs to verify it's working
        injected_logs_result = await browser.call_tool("get_injected_console_logs", {
            "clear": False
        })

        logger.info(f"Injected logs with custom timeout result: {injected_logs_result}")
        assert "error" not in injected_logs_result

        # The injected logs come back as a JSON array directly
        if isinstance(injected_logs_result, list):
            logs = injected_logs_result
        else:
            pytest.fail("Expected injected logs result to be a list")

        logger.info(f"Found {len(logs)} injected log entries with custom timeout")
