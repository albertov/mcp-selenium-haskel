# mcp-selenium-haskell

A Haskell implementation of MCP Selenium Server using WebDriver, enabling browser automation through standardized MCP clients like Claude.

## Credits

This implementation is inspired by the [mcp-selenium project developed by Angie Jones](https://github.com/angiejones/mcp-selenium). We thank the original contributors for their foundational work in MCP Selenium automation.

## Comparison with mcp-selenium (Node.js)

This project is a Haskell reimplementation of the original JavaScript [mcp-selenium](https://github.com/angiejones/mcp-selenium) project. Here's an honest comparison between the two implementations:

### Target Audience & Architecture
- **Node.js**: Direct WebDriver integration, single-session model
- **Haskell**: Multi-session support with UUID-based session management, connects to Selenium Grid

### Installation & Dependencies
- **Node.js**: Published to npm with `npx` support, includes browser management
- **Haskell**: Statically-linked executable, requires separate Selenium server

### Features & API
Both implementations provide identical MCP tool interfaces with these differences:
- **Node.js**: Implicit session management (one session per server instance)
- **Haskell**: Explicit session management with concurrent sessions

**Additional Tools (Haskell-only):**
- **Console Logging Suite**: `get_console_logs`, `inject_console_logger`, `get_injected_console_logs`, `get_available_log_types` - comprehensive JavaScript console monitoring
- **Page Source**: `get_source` - retrieve current page HTML
- **Session Management**: UUID-based session IDs with proper multi-session isolation

### Testing & Reliability
- **Node.js**: Basic test coverage
- **Haskell**: 90+ integration tests with comprehensive validation

### When to Choose Which
**Choose Node.js mcp-selenium if you:**
- Want the quickest setup with built-in browser management
- Prefer the JavaScript ecosystem
- Need a single browser session

**Choose Haskell mcp-selenium if you:**
- Need multiple concurrent browser sessions
- Want type safety and compile-time error checking
- Have existing Selenium Grid infrastructure
- Prefer zero-dependency executables

### Current Status
Both implementations are production-ready with different strengths:

- **Node.js**: Faster setup, mature ecosystem, simpler deployment
- **Haskell**: Multi-session support, type safety, extensive test coverage

Choose based on your technical requirements and ecosystem preferences.

## Features

- Multi-session browser management with UUID-based session IDs
- Element interaction: click, type, hover, drag & drop, double-click, right-click
- Element location using CSS selectors, XPath, ID, name, class, tag
- JavaScript console logging with injection and monitoring capabilities
- Screenshot capture and page source retrieval
- File upload support
- Keyboard input simulation
- Chrome and Firefox browser support
- Headless mode operation
- Statically-linked executable with zero runtime dependencies

## API Documentation

For comprehensive documentation of all available tools and their parameters, see [API.md](API.md).

## Building

```bash
nix develop
# inside the shell that will open...
cabal build
```

## Running

### From the flake

```bash
nix run github:albertov/mcp-selenium-haskell
```

### From the release
```bash
mcp-selenium-hs
```

## Claude Desktop Configuration

To use this MCP server with Claude Desktop, add the following configuration to your `claude_desktop_config.json` file:

```json
{
  "mcpServers": {
    "selenium": {
      "command": "mcp-selenium-hs",
      "args": [],
      "env": {
         "SELENIUM_HOST": "some.tailscale.host",
         "SELENIUM_PORT": "1234",
      }
    }
  }
}
```

Or from a flake

```json
{
  "mcpServers": {
    "selenium": {
      "command": "nix",
      "args": ["run", "github:albertov/mcp-selenium-haskell"],
      "env": {
         "SELENIUM_HOST": "some.tailscale.host",
         "SELENIUM_PORT": "1234",
      }
    }
  }
}
```

The configuration file is typically located at:
- **macOS**: `~/Library/Application Support/Claude/claude_desktop_config.json`
- **Windows**: `%APPDATA%\Claude\claude_desktop_config.json`

After adding this configuration, restart Claude Desktop to enable the Selenium MCP server.

## Using with mcp-proxy

[mcp-proxy](https://github.com/sparfenyuk/mcp-proxy) allows you to expose the mcp-selenium server over HTTP/SSE for remote access or to connect to remote MCP servers. This is useful when you want to make the mcp-selenium proxy accessible through the network to MCP clients running on different machines. The underlying Selenium server can be located anywhere as long as the proper environment variables (`SELENIUM_HOST`, `SELENIUM_PORT`, etc.) are configured to reach it.

> **⚠️ SECURITY WARNING**
>
> **NEVER bind mcp-proxy to `0.0.0.0` on production systems or networks you don't fully control!** This exposes your MCP server to the entire internet, potentially allowing unauthorized access to browser automation capabilities.
>
> **Safe alternatives:**
> - **Tailscale/VPN**: Bind to your Tailscale interface IP (e.g., `--host=100.x.x.x`)
> - **Local + SSH**: Use `--host=127.0.0.1` and SSH port forwarding (`ssh -L 8080:localhost:8080 user@server`)
> - **Private networks**: Only bind to private network interfaces you control
> - **Firewall**: If you must use `0.0.0.0`, ensure proper firewall rules restrict access

### Exposing mcp-selenium over SSE (Local to Remote)

To run mcp-selenium behind mcp-proxy and expose it over SSE for remote access:

```bash
# Install mcp-proxy
uv tool install mcp-proxy

# Run mcp-selenium behind the proxy on localhost (safe for local development)
mcp-proxy --port=8080 mcp-selenium-hs

# For network access, bind to a specific interface (replace with your Tailscale IP)
mcp-proxy --host=100.64.1.100 --port=8080 mcp-selenium-hs

# Or use SSH port forwarding for remote access
# On the server:
mcp-proxy --host=127.0.0.1 --port=8080 mcp-selenium-hs
# On the client:
# ssh -L 8080:localhost:8080 user@your-server
```

The server will be accessible at:
- SSE endpoint: `http://localhost:8080/sse`
- Status endpoint: `http://localhost:8080/status`

You can then configure Claude Desktop to connect to the proxy instead of directly to the server:

```json
{
  "mcpServers": {
    "selenium-proxy": {
      "command": "mcp-proxy",
      "args": ["http://your-server:8080/sse"],
      "env": {}
    }
  }
}
```

### Multiple Named Servers

You can run multiple instances of mcp-selenium with different configurations:

```bash
# Run multiple selenium instances with different headless settings
mcp-proxy --port=8080 \
  --named-server selenium-headless 'mcp-selenium-hs' \
  --named-server selenium-desktop 'mcp-selenium-hs'
```

This exposes:
- Headless instance: `http://localhost:8080/servers/selenium-headless/sse`
- Desktop instance: `http://localhost:8080/servers/selenium-desktop/sse`

Note: Browser type (Chrome/Firefox) and other options are specified through the MCP tool parameters, not environment variables.

### Using Configuration Files

For complex setups, use a JSON configuration file:

```json
{
  "mcpServers": {
    "selenium-default": {
      "command": "mcp-selenium-hs",
      "args": [],
      "transportType": "stdio"
    },
    "selenium-remote": {
      "command": "mcp-selenium-hs",
      "args": [],
      "env": {
        "SELENIUM_HOST": "selenium.example.com",
        "SELENIUM_PORT": "4444"
      },
      "transportType": "stdio"
    }
  }
}
```

Then run:

```bash
mcp-proxy --port=8080 --named-server-config ./selenium-servers.json
```

### Docker Deployment

For containerized deployments, you can use mcp-proxy with Docker. **Always use specific interface binding for security:**

```dockerfile
FROM ghcr.io/sparfenyuk/mcp-proxy:latest

# Install dependencies and copy mcp-selenium-hs binary
RUN wget -O /usr/local/bin/mcp-selenium-hs https://github.com/albertov/mcp-selenium-haskell/releases/latest/download/mcp-selenium-hs
RUN chmod +x /usr/local/bin/mcp-selenium-hs

EXPOSE 8080
# Use a specific interface IP instead of 0.0.0.0
ENTRYPOINT ["mcp-proxy", "--host=127.0.0.1", "--port=8080", "mcp-selenium-hs"]
```

For Tailscale integration in Docker:

```bash
# Run with Tailscale network access
docker run -d \
  --name mcp-selenium-proxy \
  --network=host \
  -e TAILSCALE_HOSTNAME=$(tailscale ip -4) \
  your-mcp-proxy-image \
  mcp-proxy --host=$(tailscale ip -4) --port=8080 mcp-selenium-hs
```

### Secure Network Access with Tailscale

For the safest remote access, use Tailscale to create a secure network:

1. Install Tailscale on both server and client machines
2. Get your server's Tailscale IP: `tailscale ip -4`
3. Run mcp-proxy bound to the Tailscale interface:

```bash
# Replace 100.64.1.100 with your actual Tailscale IP
mcp-proxy --host=100.64.1.100 --port=8080 mcp-selenium-hs
```

4. Configure Claude Desktop to connect using the Tailscale IP:

```json
{
  "mcpServers": {
    "selenium-proxy": {
      "command": "mcp-proxy",
      "args": ["http://100.64.1.100:8080/sse"],
      "env": {}
    }
  }
}
```

### Environment Variables

When using mcp-proxy, you can pass environment variables to configure the Selenium connection:

```bash
# Pass custom Selenium server configuration
mcp-proxy --port=8080 \
  --env SELENIUM_HOST selenium.example.com \
  --env SELENIUM_PORT 4444 \
  mcp-selenium-hs
```

Supported environment variables:
- `SELENIUM_HOST`: Selenium server hostname (default: `127.0.0.1`)
- `SELENIUM_PORT`: Selenium server port (default: `4444`)

### CORS Configuration

To allow browser-based clients to connect, you can enable CORS. **Be careful with wildcard origins:**

```bash
# ⚠️  Only use '*' for development/testing - NEVER in production!
mcp-proxy --port=8080 --allow-origin='*' mcp-selenium-hs
```

For production, always specify exact origins:

```bash
# Safe: specify exact origins you trust
mcp-proxy --port=8080 \
  --allow-origin='https://claude.ai' \
  --allow-origin='https://your-app.com' \
  mcp-selenium-hs
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

For detailed information about the integration testing setup, see [integration_tests/README.md](integration_tests/README.md).

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
