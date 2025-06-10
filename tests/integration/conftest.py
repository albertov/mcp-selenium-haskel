import pytest
import pytest_asyncio
import asyncio
import tempfile
import shutil
import sys
from pathlib import Path
from typing import AsyncGenerator

# Add the tests directory to Python path so we can import utils
tests_dir = Path(__file__).parent.parent
sys.path.insert(0, str(tests_dir))

from utils.mcp_client import MCPSeleniumClient
from utils.html_server import TestHTMLServer



@pytest_asyncio.fixture(scope="session")
async def mcp_client() -> AsyncGenerator[MCPSeleniumClient, None]:
    """Create MCP client connected to Haskell server"""
    async with MCPSeleniumClient("./run_client.sh") as client:
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
