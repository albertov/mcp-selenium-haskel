# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.1.0] - UNRELEASED

### Added
- Initial release of MCP Selenium Haskell server
- WebDriver operations wrapper for Selenium automation
- Support for Chrome and Firefox browsers
- Browser session management with customizable options
- Element interaction (click, send keys, hover, etc.)
- Screenshot capabilities
- MCP server implementation for browser automation
- Comprehensive integration test suite with 25+ tests
- Modern Python packaging with pyproject.toml and hatchling
- Configurable timeout parameter for `inject_console_logger` tool (default: 60000ms)
- Additional integration test demonstrating custom timeout configuration
- Increased HTTP retry count for improved reliability in CI environments

### Fixed
- Removed unused outputPath parameter from TakeScreenshotParams
- Python dependency management in flake.nix by migrating integration tests to writeShellApplication with proper pythonEnv
- HTML fixtures directory path to work in both development and packaged environments
- Import issues by creating proper Python package structure
- Integration test orchestration to use correct paths
- Console logger injection timeout issues in CI environments by adding configurable script timeout parameter

### Changed
- **BREAKING**: Moved integration tests from `tests/` to `integration_tests/` directory
- **BREAKING**: Restructured Python package to use proper module naming (underscores)
- **BREAKING**: Removed `strategy` parameter from `find_element` tool, keeping only `by` parameter for locator strategy
- Modernized build system from `writeShellApplication` to `buildPythonApplication`
- Updated from manual dependency management to automatic transitive dependency resolution
- Converted to modern `format = "pyproject"` with `hatchling` build backend
- Updated integration test scripts to work with new directory structure

### Refactored
- Abstracted stderr logging pattern `hPutStrLn stderr "..." >> hFlush stderr` into shared utility function
- Created new `MCP.Selenium.Utils` module with `debugLog` helper function
- Updated all modules to use shared logging utility for improved maintainability
- Removed code duplication across Server and Tools modules

### Technical Details
- Replaced custom shell scripts with proper Python console script entry points
- Added pyproject.toml with modern Python packaging standards
- Improved Nix integration with buildPythonApplication for better dependency management
- Enhanced test infrastructure with automatic fixture path resolution
