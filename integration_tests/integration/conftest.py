import os
from utils.mcp_client import MCPSeleniumClient
from utils.html_server import TestHTMLServer
import pytest
import pytest_asyncio
import tempfile
import shutil
from pathlib import Path
from typing import AsyncGenerator




@pytest_asyncio.fixture(scope="function")
async def mcp_client() -> AsyncGenerator[MCPSeleniumClient, None]:
    """Create MCP client connected to Haskell server"""
    # IMPORTANT: Do not change this!
    # Get the absolute path to run_server.sh relative to the project root
    project_root = Path(__file__).parent.parent.parent
    executable_path = os.environ.get('MCP_SELENIUM_EXE',str(project_root /
                                                            "run_server.sh"))
    print(f"DEBUG: Using executable: {executable_path}")

    async with MCPSeleniumClient(executable_path) as client:
        print("DEBUG: Created client")
        yield client


@pytest_asyncio.fixture(scope="session")
async def test_server():
    """Start local HTTP server for test HTML files"""
    server = TestHTMLServer()
    await server.start()
    print("DEBUG: Started HTTP server")
    yield server
    await server.stop()


@pytest.fixture
def temp_dir():
    """Create temporary directory for test files"""
    temp_dir = tempfile.mkdtemp()
    yield Path(temp_dir)
    shutil.rmtree(temp_dir)


@pytest.fixture
def sample_file(temp_dir):
    """Create sample file for upload tests"""
    file_path = temp_dir / "sample.txt"
    file_path.write_text("This is a test file for upload")
    return file_path


@pytest_asyncio.fixture(scope="function")
async def browser(mcp_client: MCPSeleniumClient) -> AsyncGenerator[MCPSeleniumClient, None]:
    """Provide a headless chrome browser session to integration tests"""
    result = await mcp_client.start_browser("chrome", headless=True)
    if "error" in result:
        raise RuntimeError(f"Failed to start browser: {result}")

    yield mcp_client

    # Clean up browser session
    await mcp_client.close_browser()
