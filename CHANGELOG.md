# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased] - 2025-6-13

### Added
- `find_elements` tool for finding multiple elements on a page using locator strategies
- `get_elements_text` tool for retrieving text content from multiple elements
- Support for finding multiple elements with all existing locator strategies (id, css, xpath, name, tag, class)
- JSON response format for multiple elements with element count and element IDs/texts
- Comprehensive integration test suite for multiple element functionality
- New HTML test fixture (`multi_elements_page.html`) for testing multiple element scenarios
- Enhanced MCP client with `find_elements` and `get_elements_text` methods

### Changed
- Updated MCP server to register and handle the new multiple element tools
- Extended WebDriver module with `findElementsByLocator` and `getElementsText` functions
- Enhanced tool parameter types with `FindElementsParams` and `GetElementsTextParams`

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
