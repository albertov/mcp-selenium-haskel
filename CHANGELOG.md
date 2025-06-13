# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased] - 2025-6-13

### Added
- `execute_js` tool for executing JavaScript code in the browser with support for:
  - JavaScript code execution in browser context
  - Optional arguments passing to JavaScript functions
  - Configurable execution timeout (default: 30 seconds)
  - Proper handling of different return types (strings, numbers, objects, arrays, null, undefined)
  - Structured JSON response format with `{"text": "result"}` format
  - DOM manipulation and page interaction capabilities
  - Comprehensive security documentation and usage examples
- `find_elements` tool for finding multiple elements on a page using locator strategies
- `get_elements_text` tool for retrieving text content from multiple elements
- Support for finding multiple elements with all existing locator strategies (id, css, xpath, name, tag, class)
- JSON response format for multiple elements with element count and element IDs/texts
- Comprehensive integration test suite for multiple element functionality
- Comprehensive integration test suite for JavaScript execution functionality with 13 test cases
- New HTML test fixture (`multi_elements_page.html`) for testing multiple element scenarios
- Enhanced MCP client with `find_elements`, `get_elements_text`, and `execute_js` methods
- **PR automation infrastructure**:
  - `run_create_pr.sh` script for automated GitHub PR creation
  - GitHub CLI (`gh`) added to nix development shell
  - `PR_DESCRIPTION.md` template for consistent PR descriptions
  - Intelligent branch detection and validation
  - Auto-generation of PR titles and bodies from git history

### Changed
- **Enhanced `take_screenshot` tool** to return base64 image data directly instead of descriptive message
  - Now returns raw base64 PNG data for immediate use by clients
  - Documented limitation with current hs-mcp library's simplified ImageContent support
  - Improved usability for screenshot functionality
- Updated MCP server to register and handle the new multiple element tools and execute_js tool
- Extended WebDriver module with `findElementsByLocator`, `getElementsText`, and `executeJavaScript` functions
- Enhanced tool parameter types with `FindElementsParams`, `GetElementsTextParams`, and `ExecuteJSParams`
- Updated API documentation with comprehensive JavaScript execution section including security considerations
- **Updated API.md to match current tool schema**:
  - Added documentation for `find_elements` tool with examples and error codes
  - Added documentation for `get_elements_text` tool with response format details
  - Corrected version number from 1.0.0 to 0.1.0 to match implementation
  - Updated table of contents to include JavaScript Execution section
  - All tool schemas now accurately reflect the current implementation

### Fixed
- **Corrected `execute_js` tool schema** to match actual implementation:
  - Fixed schema constraint in Server.hs that incorrectly limited `args` parameter to strings only
  - Implementation actually accepts any JSON values (strings, numbers, objects, arrays, booleans, null)
  - Updated API.md documentation to reflect correct parameter types with examples
  - Schema now properly matches the `[Value]` type used in Tools.hs and WebDriver.hs
  - Added comprehensive integration test `test_execute_js_with_diverse_argument_types` validating all JSON types
- Fixed integration test infrastructure bug (`sys.args` -> `sys.argv`) in orchestrate_integration_tests.py

### Improved
- Reduced timeout to 0.2 seconds for integration tests that expect no element matches, improving test performance by ~20 seconds

## [0.1.0] - 2025-6-12

### Added
- Initial release of MCP Selenium server for browser automation
- Support for Chrome and Firefox browsers with session management
- Element interaction tools (click, send keys, hover, etc.)
- Screenshot and page source retrieval capabilities
- Console logging injection and retrieval
- Comprehensive integration test suite
- Release tarball generation script
