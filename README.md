# mcp-selenium-haskell

A Haskell implementation of MCP Selenium Server using WebDriver, enabling browser automation through standardized MCP clients like Claude.

## Credits

This implementation is inspired by the original concept developed by [Angie Jones](https://github.com/angiejones/mcp-selenium). We thank the original contributors for their foundational work in MCP Selenium automation.

## Features

- Start browser sessions with customizable options
- Navigate to URLs and interact with page elements
- Find elements using various locator strategies
- Click, type, and interact with elements
- Perform mouse actions (hover, drag and drop)
- Take screenshots
- Support for headless mode
- Console logs functionality
- Support for Chrome and Firefox browsers

## Building

```bash
cabal build
```

## Running

```bash
nix run github:albertov/mcp-selenium-haskell
```

## Integration Testing

The project includes comprehensive black-box integration tests that validate the MCP server functionality through its external interface.

### Quick Start

Run all integration tests:

```bash
./run_integration_tests.sh
```

This automatically:
1. Starts selenium-server daemon
2. Starts HTTP server for test fixtures
3. Builds the mcp-selenium-hs executable
4. Runs the integration test suite
5. Stops all services

### Documentation

For detailed information about the integration testing setup, see [tests/README.md](tests/README.md).

## Dependencies

The project uses the following key dependencies:

- `webdriver` - Selenium WebDriver bindings for Haskell
- `hs-mcp` - Model Context Protocol implementation
- `aeson` - JSON parsing and encoding

## Development

### Prerequisites

- GHC 9.10.2 or compatible
- Cabal 3.0+
- For integration tests: Python 3, pytest, selenium-server-standalone, chromium

### Nix Environment

The project provides a Nix environment with all dependencies:

```bash
nix-shell
```

Or with flakes:

```bash
nix develop
```

### Testing

Unit tests:
```bash
cabal test
```

Integration tests:
```bash
./run_integration_tests.sh
```

### Code Quality

Format code:
```bash
nix fmt
```

Check code quality and run linting:
```bash
nix check
```

## License

BSD-3-Clause - see [LICENSE](LICENSE) file for details.

## Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Add tests if applicable
5. Run the test suite
6. Submit a pull request

Please ensure all tests pass and code follows the project style guidelines.
