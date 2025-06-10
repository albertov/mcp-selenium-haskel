import pytest
import asyncio
import tempfile
import shutil
from pathlib import Path
from typing import AsyncGenerator

from utils.mcp_client import MCPSeleniumClient
from utils.html_server import TestHTMLServer


@pytest.fixture(scope="session")
def event_loop():
    """Create an instance of the default event loop for the test session."""
    loop = asyncio.get_event_loop_policy().new_event_loop()
    yield loop
    loop.close()


@pytest.fixture(scope="session")
def mcp_executable_path():
    """Path to the mcp-selenium-hs executable"""
    # Look for the executable in the cabal build directory
    possible_paths = [
        Path("dist-newstyle/build/x86_64-linux/ghc-9.10.2/mcp-selenium-2.2.0/x/mcp-selenium-hs/build/mcp-selenium-hs/mcp-selenium-hs"),
        Path("dist-newstyle/build/x86_64-linux/ghc-9.6.7/mcp-selenium-2.2.0/x/mcp-selenium-hs/build/mcp-selenium-hs/mcp-selenium-hs"),
        # Add more potential paths as needed
    ]

    for path in possible_paths:
        if path.exists():
            return str(path.resolve())

    # Fallback to assuming it's in PATH
    return "mcp-selenium-hs"


@pytest.fixture
async def mcp_client(mcp_executable_path) -> AsyncGenerator[MCPSeleniumClient, None]:
    """Create MCP client connected to Haskell server"""
    async with MCPSeleniumClient(str(mcp_executable_path)) as client:
        yield client


@pytest.fixture(scope="session")
async def test_server():
    """Start local HTTP server for test HTML files"""
    server = TestHTMLServer()
    await server.start()
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
