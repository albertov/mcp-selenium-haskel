#!/usr/bin/env python3
"""
Debug script to test MCP client connection without pytest overhead
"""

import asyncio
import sys
from pathlib import Path

# Add the tests directory to Python path
tests_dir = Path(__file__).parent / "tests"
sys.path.insert(0, str(tests_dir))

from utils.mcp_client import MCPSeleniumClient


async def debug_mcp_connection():
    """Test MCP connection with detailed debugging"""
    executable_path = "./run_server.sh"
    print(f"DEBUG: Testing MCP connection with executable: {executable_path}")

    try:
        async with MCPSeleniumClient(executable_path) as client:
            print("DEBUG: MCP client connected successfully")

            # Test list_tools - this is where it hangs
            print("DEBUG: About to call list_tools...")
            tools = await asyncio.wait_for(client.list_tools(), timeout=10.0)
            print(f"DEBUG: Got tools: {tools}")

    except asyncio.TimeoutError:
        print("ERROR: Timeout while calling list_tools")
    except Exception as e:
        print(f"ERROR: {e}")
        import traceback
        traceback.print_exc()


if __name__ == "__main__":
    asyncio.run(debug_mcp_connection())
