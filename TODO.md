# TODO: MCP Selenium Haskell - Areas for Improvement

This document lists identified areas for improvement, missing features, incomplete implementations, and technical debt in the mcp-selenium-haskell project.

## 🚨 Critical Issues

### Version Inconsistencies
- [x] **Fix version mismatch**: `mcp-selenium.cabal` shows version `0.1.0`, but `Server.hs` declares version `1.0.0`. It should be 0.1.0
- [ ] **Update CHANGELOG**: Version is marked as `UNRELEASED`. Do this when we're ready to release
- [ ] **Synchronize all version references** across cabal file, server implementation, and documentation

### Missing GHC Version Support
- [x] **Fix GHC version mismatch**: `mcp-selenium.cabal` declares `tested-with: GHC == 9.12.2` but project uses GHC 9.10.2
- [x] **Update tested-with field** to reflect actual supported GHC versions

## 🔧 Incomplete/Mock Implementations

### WebDriver Functionality
- [x] **Complete right-click implementation**: Currently only moves to element, doesn't perform actual right-click
  ```haskell
  -- In WebDriver.hs line 233: contextClick is not available, only moveToCenter
  ```
- [x] Add integration test to verify right-click functionality works. Should create a new html fixture with needed JS code to verify correctness
- [x] Add integration test to verify hover functinality worksa. Should create a new html fixture with needed JS code to verify correctness
- [x] Add integration test to verify drag and drop functionality works. Should create a new html fixture with needed JS code to verify correctness
- [ ] **Implement actual log type detection**: `getAvailableLogTypes` returns hard-coded list instead of querying WebDriver
- [ ] **Add missing browser arguments support**: Firefox browser options are not implemented (only Chrome has full options support)
- [ ] **Implement element highlighting**: Add functionality to highlight elements before interaction for debugging
- [ ] **Add viewport/window management**: Functions to resize, maximize, minimize browser windows

#### Missing WebDriver Commands (Not Yet Implemented as Tools)
The following WebDriver commands are available in `Test.WebDriver.Commands` but not yet implemented as MCP tools:
- [ ] **Alert handling**: `acceptAlert`, `dismissAlert` - Handle JavaScript alert/confirm dialogs
- [ ] **Window management**: `maximize`, `setWindowSize`, `getWindowSize` - Window sizing and state control
- [ ] **Navigation**: `refresh`, `getCurrentURL`, `getTitle` - Additional page navigation and info retrieval
- [ ] **Element state inspection**: `isSelected`, `isEnabled`, `isDisplayed` - Check element status/visibility
- [ ] **Element attributes**: `attr` - Get element attributes and properties
- [ ] **Form operations**: `submit` - Submit forms directly
- [ ] **JavaScript execution**: `executeJS` tool - Execute arbitrary JavaScript (currently only used internally)
- [ ] **Element clearing**: Clear text from input elements
- [ ] **Browser navigation**: Back/forward navigation commands
- [ ] **Cookie management**: Get/set/delete cookies
- [ ] **Local storage operations**: Access browser local/session storage
- [ ] **Frame/window switching**: Switch between frames, tabs, and windows
- [ ] **Element selection**: Select options in dropdown elements

**Integration tests should be implemented for all new tools with appropriate HTML fixtures to verify functionality**

### Error Handling and Validation
- [ ] **Implement specific error types**: Replace generic `SomeException` with domain-specific error types
- [ ] **Add input validation**: Validate URLs, file paths, timeouts, and other parameters before WebDriver calls
- [ ] **Improve error messages**: Provide more descriptive error messages with suggested fixes
- [ ] **Add retry mechanisms**: Implement configurable retry logic for transient failures

### Session Management
- [ ] **Add session persistence**: Option to save/restore session state
- [ ] **Implement session metadata**: Track creation time, last activity, browser capabilities per session
- [ ] **Add session cleanup on crash**: Automatic cleanup of orphaned sessions
- [ ] **Implement session limits**: Configurable maximum number of concurrent sessions

## 🧪 Testing Issues

### Disabled Integration Tests
Multiple integration test files are disabled and need investigation:
- [x] **Re-enable `test_actions.py.disabled`**: Mouse and keyboard actions tests
- [ ] **Re-enable `test_advanced_elements.py.disabled`**: Advanced element interaction tests
- [ ] **Re-enable `test_browser_config.py.disabled`**: Browser configuration tests
- [ ] **Re-enable `test_error_handling.py.disabled`**: Error handling validation tests (needs timeout fixes)
- [ ] **Re-enable `test_file_upload.py.disabled`**: File upload functionality tests
- [ ] **Re-enable `test_javascript.py.disabled`**: JavaScript execution tests
- [ ] **Re-enable `test_logging.py.disabled`**: Logging functionality tests
- [ ] **Re-enable `test_performance.py.disabled`**: Performance and timeout tests

### Missing Test Coverage
- [ ] **Add actual WebDriver tests**: Current unit tests only test JSON serialization
- [ ] **Add session management tests**: Test concurrent sessions, cleanup, edge cases
- [ ] **Add error condition tests**: Test network failures, invalid selectors, timeouts
- [ ] **Add browser compatibility tests**: Ensure features work across Chrome and Firefox
- [ ] **Add load testing**: Test with multiple concurrent sessions

## 📚 Documentation Issues

### API Documentation
- [ ] **Fix API documentation inconsistencies**: Some examples don't match actual implementation
- [ ] **Add missing error codes**: Document all possible error responses
- [ ] **Update browser argument examples**: Provide comprehensive browser configuration examples
- [ ] **Add session management workflow examples**: Document multi-session usage patterns
- [ ] **Add troubleshooting guide**: Common issues and solutions

### Code Documentation
- [ ] **Add module-level documentation**: Comprehensive module descriptions
- [ ] **Document internal types**: Better documentation for SessionData, SeleniumSession, etc.
- [ ] **Add inline examples**: Code examples in function documentation
- [ ] **Document environment variables**: Complete list of supported environment variables

## 🔄 Refactoring Opportunities

### Code Organization
- [ ] **Extract WebDriver utilities**: Move common WebDriver patterns to utility functions
- [ ] **Separate tool definitions**: Move tool JSON schemas to separate module
- [ ] **Implement tool registration system**: More flexible tool registration mechanism
- [ ] **Add configuration module**: Centralize all configuration logic

### Type Safety Improvements
- [ ] **Replace String with Text**: Eliminate remaining String usage in favor of Text
- [ ] **Add newtype wrappers**: For timeout values, element selectors, URLs
- [ ] **Implement refined types**: Use refined library for validated inputs
- [ ] **Add phantom types**: For different element states (found/not found)

### Performance Improvements
- [ ] **Implement connection pooling**: Reuse WebDriver connections where possible
- [ ] **Add caching mechanisms**: Cache element lookups, page metadata
- [ ] **Optimize JSON encoding**: Use more efficient JSON encoding for large responses
- [ ] **Implement lazy evaluation**: For expensive operations like screenshot capture

## 🚀 Missing Features

### Browser Features
- [ ] **Add Safari support**: Extend browser support beyond Chrome and Firefox
- [ ] **Implement mobile browser support**: iOS Safari, Chrome Mobile
- [ ] **Add browser profile management**: Custom profiles, extensions, settings
- [ ] **Implement incognito/private mode**: Dedicated private browsing sessions

### Element Interaction
- [ ] **Add element visibility checks**: Verify elements are visible before interaction
- [ ] **Implement element waiting strategies**: Wait for elements to be clickable, present, visible
- [ ] **Add multi-element operations**: Bulk operations on element collections
- [ ] **Implement element attribute manipulation**: Get/set element attributes, styles

### Advanced Automation
- [ ] **Add cookie management**: Tools for reading, setting, deleting cookies
- [ ] **Implement local storage operations**: Access to browser local/session storage
- [ ] **Add network monitoring**: Capture network requests and responses
- [ ] **Implement page performance metrics**: Loading times, resource sizes

### Console and Debugging
- [ ] **Add console command execution**: Execute arbitrary JavaScript
- [ ] **Implement breakpoint support**: Debugging capabilities
- [ ] **Add network request interception**: Modify requests/responses
- [ ] **Implement coverage reporting**: Code coverage for tested pages

## 🛠️ Infrastructure Improvements

### Build System
- [ ] **Add continuous integration**: GitHub Actions for testing across GHC versions
- [ ] **Implement automated releases**: Automatic version bumping and releasing
- [ ] **Add benchmark suite**: Performance regression testing
- [ ] **Implement static analysis**: HLint, code quality checks

### Development Experience
- [ ] **Add development shell**: Nix shell with all development dependencies
- [ ] **Implement hot reloading**: Development server with automatic recompilation
- [ ] **Add debugging tools**: Better logging, profiling capabilities
- [ ] **Create development documentation**: Getting started guide for contributors

### Deployment
- [ ] **Add container support**: Docker images for easy deployment
- [ ] **Implement configuration management**: Support for config files, environment-based configs
- [ ] **Add monitoring capabilities**: Health checks, metrics, observability
- [ ] **Create deployment guides**: Various deployment scenarios and best practices

## 🐛 Potential Bugs and Edge Cases

### Session Management
- [ ] **Handle concurrent session access**: Race conditions in session map updates
- [ ] **Memory leak prevention**: Ensure proper cleanup of session resources
- [ ] **Handle browser crashes**: Recovery when underlying browser process dies
- [ ] **Validate session UUIDs**: Prevent injection attacks through malformed UUIDs

### WebDriver Integration
- [ ] **Handle network timeouts**: Graceful handling of Selenium Grid unavailability
- [ ] **Browser compatibility checks**: Validate browser capabilities before session creation
- [ ] **Handle stale element references**: Automatic retry for stale element errors
- [ ] **Resource cleanup on errors**: Ensure browser processes are cleaned up on failures

### MCP Protocol
- [ ] **Validate MCP message format**: Ensure compliance with MCP specification
- [ ] **Handle protocol version mismatches**: Graceful degradation for different MCP versions
- [ ] **Implement proper streaming**: For large responses like screenshots, page source
- [ ] **Add rate limiting**: Prevent abuse of automation capabilities

## 📊 Monitoring and Observability

### Logging
- [ ] **Implement structured logging**: JSON logs with consistent fields
- [ ] **Add log levels**: Configurable verbosity levels
- [ ] **Performance logging**: Track operation timing and performance metrics
- [ ] **Add request tracing**: Track requests through the entire pipeline

### Metrics
- [ ] **Add performance metrics**: Response times, error rates, session counts
- [ ] **Implement health checks**: Service health endpoints
- [ ] **Add resource monitoring**: Memory usage, CPU usage, connection counts
- [ ] **Create dashboards**: Visualization of key metrics

## 🔒 Security Considerations

### Input Validation
- [ ] **Sanitize file paths**: Prevent directory traversal attacks in file upload
- [ ] **Validate URLs**: Prevent SSRF attacks through URL validation
- [ ] **Limit resource usage**: Prevent DoS through resource exhaustion
- [ ] **Add rate limiting**: Prevent abuse of automation endpoints

### Configuration Security
- [ ] **Secure default configurations**: Ensure secure defaults for all options
- [ ] **Add authentication**: Optional authentication for MCP server
- [ ] **Implement access controls**: Per-tool permissions and restrictions
- [ ] **Add audit logging**: Track all automation activities

---

## Priority Classification

### 🚨 High Priority (Critical for stability)
- Version inconsistencies
- GHC version mismatches
- Incomplete right-click implementation
- Disabled integration tests

### 🔧 Medium Priority (Improves functionality)
- Missing browser features
- Advanced automation capabilities
- Better error handling
- Documentation improvements

### 🚀 Low Priority (Nice to have)
- Performance optimizations
- Additional browser support
- Advanced debugging features
- Monitoring and observability

---

This TODO list should be regularly updated as issues are resolved and new requirements emerge. Consider creating GitHub issues for major items to track progress and enable community contributions.
