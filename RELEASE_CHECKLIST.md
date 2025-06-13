# Release Checklist

This document outlines the steps to follow when releasing a new version of mcp-selenium-haskell.

## Pre-Release Steps

### 1. Documentation Review
- [ ] Ensure README.md is up to date with latest features
- [ ] Ensure API.md is up to date with current tool schemas and version
- [ ] Review CHANGELOG.md for completeness and accuracy

### 2. Version Updates
Update the version number in the following files:
- [ ] `mcp-selenium.cabal` - Update the `version` field
- [ ] `CHANGELOG.md` - Move "Unreleased" section to new version with release date
- [ ] `API.md` - Update version in "Server Information" section

### 3. Testing
- [ ] Run all tests: `cabal test`
- [ ] Run integration tests: `./run_integration_tests.sh`
- [ ] Run lint checks: `hlint .`
- [ ] Verify builds cleanly: `cabal build`

### 4. Final Code Review
- [ ] Review all changes since last release
- [ ] Ensure code quality and documentation standards
- [ ] Verify no sensitive information is exposed

## Release Steps

### 5. Git Operations
- [ ] Commit all version updates
- [ ] Create and push git tag following semantic versioning: `git tag v<version>`
- [ ] Push the tag: `git push origin v<version>`
- [ ] Push the branch: `git push`

### 6. Pull Request
- [ ] Update PR_DESCRIPTION.md if needed
- [ ] Create pull request: `./run_create_pr.sh` or use `create_pr` command
- [ ] Ensure PR includes all changes and proper description

## Post-Release Steps

### 7. GitHub Release
- [ ] Create GitHub release from the tag
- [ ] Add release notes from CHANGELOG.md
- [ ] Upload any release artifacts if applicable

### 8. Documentation Updates
- [ ] Update any external documentation
- [ ] Notify relevant stakeholders of the release

### 9. Verification
- [ ] Verify the release works as expected
- [ ] Test installation/usage instructions
- [ ] Monitor for any immediate issues

## Version Numbering Guidelines

Follow [Semantic Versioning](https://semver.org/):

- **MAJOR** version (X.0.0): Incompatible API changes
- **MINOR** version (0.X.0): New functionality in backwards compatible manner
- **PATCH** version (0.0.X): Backwards compatible bug fixes

## Notes

- Always test thoroughly before releasing
- Keep CHANGELOG.md updated with each change
- Ensure documentation reflects the current state
- Follow the git tag naming convention: `v<semver>` (e.g., `v1.2.3`)
- Use the `create_pr` command for consistent PR creation
