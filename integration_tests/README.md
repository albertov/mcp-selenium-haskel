# Integration Testing for mcp-selenium-haskell

This directory contains comprehensive black-box integration tests for the `mcp-selenium` MCP server.

## Architecture

The testing setup follows this architecture:

```
[Python Test Client] --stdio--> [mcp-selenium-hs executable] --webdriver--> [Browser]
```

- **Python Test Client**: Uses MCP Python SDK to communicate with the server
- **mcp-selenium-hs**: Your Haskell executable acting as MCP server
- **Browser**: Chrome/Firefox controlled by the Haskell server

## Test Organization

```
integration_tests/
├── orchestrate_integration_tests.py   # Test orchestration script
├── integration/
│   ├── conftest.py                     # pytest configuration and fixtures
│   ├── test_browser_management.py      # Browser startup/shutdown tests
│   ├── test_navigation.py              # URL navigation tests
│   └── test_element_interaction.py     # Element finding and interaction
├── fixtures/
│   ├── html/                           # Test HTML pages
│   │   ├── test_page.html
│   │   ├── form_page.html
│   │   └── upload_page.html
│   └── test_files/
│       └── sample.txt
└── utils/
    ├── mcp_client.py                   # MCP client wrapper
    └── html_server.py                  # Local test server
```

## Running Tests

### Using the Orchestration Script

The easiest way to run tests is using the provided orchestration script:

```bash
# Run all integration tests
./run_integration_tests.sh

# Or directly with the orchestration script
python3 integration_tests/orchestrate_integration_tests.py
```

This script automatically:
1. Starts selenium-server daemon
2. Starts HTTP server for test fixtures
3. Builds the mcp-selenium-hs executable
4. Runs the integration test suite
5. Stops all services (always, regardless of test outcome)

### Nix-Packaged Tests

You can also run the tests using the modern Nix package:

```bash
nix run .#integration-tests
```

This uses a proper Python package built with `buildPythonApplication` and automatic dependency management.

### Manual Testing

For development and debugging, you can run components manually:

1. **Start services:**
   ```bash
   # Start selenium server (in one terminal)
   java -jar selenium-server-standalone-*.jar -port 4444

   # Start HTTP server for fixtures (in another terminal)
   cd integration_tests/fixtures/html
   python3 -m http.server 8080
   ```

2. **Build the executable:**
   ```bash
   cabal build mcp-selenium-hs
   ```

3. **Run tests:**
   ```bash
   # Set up Python path and run tests
   export PYTHONPATH="$(pwd)/integration_tests"
   cd integration_tests

   # Run all integration tests
   python -m pytest integration/ -v

   # Run specific test file
   python -m pytest integration/test_browser_management.py -v

   # Run with more verbose output
   python -m pytest integration/ -v -s
   ```

## Test Configuration

The tests use pytest and support the following markers:

- `@pytest.mark.asyncio` - For async tests
- `@pytest.mark.slow` - For slow-running tests
- `@pytest.mark.browser` - For tests requiring browser
- `@pytest.mark.network` - For tests requiring network access

## Fixtures

### Key Fixtures

- `mcp_client` - Provides an MCP client connected to the Haskell server
- `test_server` - HTTP server serving test HTML files
- `temp_dir` - Temporary directory for test files
- `sample_file` - Sample file for upload tests

### HTML Test Pages

- `test_page.html` - Basic test page with simple elements
- `form_page.html` - Form with input fields and buttons
- `upload_page.html` - File upload form

## Dependencies

All required Python packages are provided by the Nix environment or managed automatically:

### Development Environment
- `python3` - Python interpreter
- `pytest` - Test framework
- `pytest-asyncio` - Async test support
- `mcp` - MCP Python SDK
- `selenium-server-standalone` - Selenium server
- `chromium` - Chrome browser

### Nix Package (Automatic)
The modern `pyproject.toml` build system automatically manages dependencies:
- Uses `buildPythonApplication` for proper packaging
- `hatchling` build backend for modern Python packaging
- Automatic transitive dependency resolution
- Integration with Nix dependency management

## Troubleshooting

### Common Issues

1. **Selenium server fails to start**: Check if port 4444 is already in use
2. **Browser fails to start**: Ensure chromium is installed and accessible
3. **MCP client connection fails**: Verify the executable path and that it's built
4. **HTTP server fails**: Check if port 8080 is available
5. **Import errors**: Ensure `PYTHONPATH` includes `integration_tests/` directory

### Debug Output

Enable verbose logging by setting environment variables:

```bash
export PYTEST_CURRENT_TEST=1
cd integration_tests
python -m pytest integration/ -v -s --tb=long
```

### Manual MCP Testing

You can manually test the MCP server:

```bash
# Build the executable
cabal build mcp-selenium-hs

# Find the executable path
find dist-newstyle -name "mcp-selenium-hs" -type f

# Test MCP communication
echo '{"jsonrpc": "2.0", "method": "tools/list", "id": 1}' | ./path/to/mcp-selenium-hs
```

## Test Development

### Adding New Tests

1. Create test files in `integration_tests/integration/` following the pattern `test_*.py`
2. Use the existing fixtures and utilities
3. Follow the async/await pattern for MCP client calls
4. Add appropriate markers for test categorization

### Test Structure

```python
import pytest
from utils.mcp_client import MCPSeleniumClient

class TestFeature:
    @pytest.mark.asyncio
    async def test_specific_functionality(self, mcp_client: MCPSeleniumClient):
        # Test implementation
        result = await mcp_client.call_tool("ToolName", {"param": "value"})
        assert "error" not in result
```

### Best Practices

- Test both success and failure scenarios
- Use descriptive test names
- Clean up resources properly
- Handle timeouts and async operations correctly
- Verify error messages are meaningful
