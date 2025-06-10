#!/usr/bin/env python3
"""
Validation script to check if the integration test setup is working.
This script performs basic validation without running the full test suite.
"""

import sys
import subprocess
from pathlib import Path


def check_python_dependencies():
    """Check if required Python modules are available"""
    print("Checking Python dependencies...")

    required_modules = ["pytest", "mcp", "asyncio"]
    missing_modules = []

    for module in required_modules:
        try:
            __import__(module)
            print(f"  ✓ {module}")
        except ImportError:
            missing_modules.append(module)
            print(f"  ✗ {module}")

    if missing_modules:
        print(f"Missing modules: {missing_modules}")
        return False
    return True


def check_test_structure():
    """Check if test directory structure is correct"""
    print("Checking test directory structure...")

    required_paths = [
        "tests/__init__.py",
        "tests/integration/__init__.py",
        "tests/integration/conftest.py",
        "tests/integration/test_browser_management.py",
        "tests/utils/__init__.py",
        "tests/utils/mcp_client.py",
        "tests/utils/html_server.py",
        "tests/fixtures/html/test_page.html",
        "tests/fixtures/html/form_page.html",
        "pytest.ini"
    ]

    missing_paths = []

    for path_str in required_paths:
        path = Path(path_str)
        if path.exists():
            print(f"  ✓ {path}")
        else:
            missing_paths.append(path_str)
            print(f"  ✗ {path}")

    if missing_paths:
        print(f"Missing paths: {missing_paths}")
        return False
    return True


def check_executable_build():
    """Check if the mcp-selenium-hs executable can be built"""
    print("Checking if mcp-selenium-hs can be built...")

    try:
        result = subprocess.run(
            ["cabal", "build", "mcp-selenium-hs"],
            capture_output=True,
            text=True,
            timeout=120
        )

        if result.returncode == 0:
            print("  ✓ mcp-selenium-hs builds successfully")
            return True
        else:
            print("  ✗ mcp-selenium-hs build failed:")
            print(f"    stdout: {result.stdout}")
            print(f"    stderr: {result.stderr}")
            return False
    except subprocess.TimeoutExpired:
        print("  ✗ mcp-selenium-hs build timed out")
        return False
    except Exception as e:
        print(f"  ✗ Error building mcp-selenium-hs: {e}")
        return False


def check_test_imports():
    """Check if test modules can be imported without errors"""
    print("Checking test module imports...")

    test_modules = [
        "tests.utils.mcp_client",
        "tests.utils.html_server",
        "tests.integration.conftest"
    ]

    import_errors = []

    for module in test_modules:
        try:
            __import__(module)
            print(f"  ✓ {module}")
        except Exception as e:
            import_errors.append(f"{module}: {e}")
            print(f"  ✗ {module}: {e}")

    if import_errors:
        print(f"Import errors: {import_errors}")
        return False
    return True


def main():
    """Run all validation checks"""
    print("Validating integration test setup...\n")

    checks = [
        ("Python dependencies", check_python_dependencies),
        ("Test directory structure", check_test_structure),
        ("Executable build", check_executable_build),
        ("Test module imports", check_test_imports)
    ]

    results = []

    for check_name, check_func in checks:
        print(f"\n--- {check_name} ---")
        try:
            result = check_func()
            results.append((check_name, result))
        except Exception as e:
            print(f"  ✗ Error during {check_name}: {e}")
            results.append((check_name, False))

    print("\n" + "="*50)
    print("VALIDATION SUMMARY")
    print("="*50)

    all_passed = True
    for check_name, passed in results:
        status = "PASS" if passed else "FAIL"
        print(f"{check_name}: {status}")
        if not passed:
            all_passed = False

    if all_passed:
        print("\n✓ All validation checks passed!")
        print("Integration test setup is ready.")
        return 0
    else:
        print("\n✗ Some validation checks failed.")
        print("Please fix the issues before running integration tests.")
        return 1


if __name__ == "__main__":
    sys.exit(main())
