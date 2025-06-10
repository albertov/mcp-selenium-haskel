# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [2.2.2] - 2025-06-10

### Changed
- Made WebDriver host and port configurable via environment variables (`SELENIUM_HOST` and `SELENIUM_PORT`)
- Improved code readability by using raw-strings-qq for JavaScript injection code
- Replaced escaped JavaScript strings with cleaner raw string literals

### Fixed
- Resolved FIXMEs related to hardcoded WebDriver configuration and string formatting
- Fixed name shadowing warning in console logging functionality

## [2.2.1] - 2025-06-10

### Fixed
- Fixed console logging integration tests by correcting parameter parsing for empty JSON objects
- Resolved parameter structure mismatch issues in browser start functionality
- Fixed test expectations to work with properly parsed JSON responses instead of raw JSON strings
- Improved error handling and debugging for console logging tools

## [2.2.0] - 2025-06-10

### Changed
- Refactored tool input schemas to use aeson-qq quasi-quoters for improved readability
- Replaced manual JSON object construction with inline JSON syntax using `[aesonQQ|...|]`
- Added aeson-qq dependency for cleaner schema definitions

### Technical Details
- All MCP tool input schemas now use aeson-qq quasi-quoters instead of Data.Aeson object builders
- Removed redundant type annotations and improved schema maintainability
- Schemas are now defined using native JSON syntax within Haskell code

## [2.1.0] - 2025-06-10

### Added
- Added `mcp-selenium-hs` executable for running the MCP server directly
- Created standalone application entry point in `app/Main.hs`

## [2.0.0] - 2025-06-10

### Changed
- **BREAKING**: Refactored WebDriver session management to use explicit session handling
- Replaced `runSession` with `runWD` for better session control
- Renamed `closeSession` function to `closeSeleniumSession` to avoid naming conflicts
- Updated `SeleniumSession` data type to hold `WDSession` directly instead of config
- Changed `initializeSession` to create actual WebDriver sessions using `mkSession`

### Technical Details
- Sessions are now created explicitly using `mkSession` from `Test.WebDriver.Config`
- All WebDriver operations now use `runWD` with the explicit session
- Session cleanup is handled through `closeSeleniumSession` which calls `WDC.closeSession`
- This provides better resource management and clearer session lifecycle control

## [1.0.0] - 2025-06-10

### Added
- Initial release of MCP Selenium Haskell server
- WebDriver operations wrapper for Selenium automation
- Support for Chrome and Firefox browsers
- Browser session management with customizable options
- Element interaction (click, send keys, hover, etc.)
- Screenshot capabilities
- MCP server implementation for browser automation
