#!/usr/bin/env python3
"""
Integration test orchestrator for mcp-selenium-haskell.

This script manages:
1. Starting selenium-server daemon
2. Starting SimpleHTTPServer for test fixtures
3. Running the integration test suite
4. Stopping both servers (always, regardless of test outcome)
"""

import asyncio
import atexit
import signal
import subprocess
import sys
import time
from pathlib import Path
from typing import Optional


class ServiceManager:
    """Manages external services needed for integration tests"""

    def __init__(self):
        self.selenium_process: Optional[subprocess.Popen] = None
        self.http_process: Optional[subprocess.Popen] = None
        self.selenium_port = 4444
        self.http_port = 8080

        # Register cleanup handlers
        atexit.register(self.cleanup)
        signal.signal(signal.SIGINT, self._signal_handler)
        signal.signal(signal.SIGTERM, self._signal_handler)

    def _signal_handler(self, signum, frame):
        """Handle shutdown signals"""
        print(f"\nReceived signal {signum}, cleaning up...")
        self.cleanup()
        sys.exit(1)

    def start_selenium_server(self):
        """Start selenium-server-standalone daemon"""
        print("Starting selenium-server daemon...")

        try:
            # Try different methods to start selenium server
            selenium_commands = [
                # Method 1: Direct java command if selenium jar is in nix store
                lambda: self._try_java_selenium(),
                # Method 2: Use selenium-server-standalone if in PATH
                lambda: self._try_selenium_standalone(),
                # Method 3: Skip selenium server (assume external)
                lambda: self._skip_selenium_server()
            ]

            for method in selenium_commands:
                try:
                    if method():
                        break
                except Exception as e:
                    print(f"Selenium startup method failed: {e}, trying next...")
                    continue
            else:
                print("Warning: Could not start selenium server, assuming external server")

            # Wait for selenium server to be available
            self._wait_for_service("localhost", self.selenium_port, "Selenium server")
            print(f"Selenium server available on port {self.selenium_port}")

        except TimeoutError:
            print("Warning: Selenium server not detected, tests may fail if selenium is required")
        except Exception as e:
            print(f"Failed to start selenium server: {e}")
            raise

    def _try_java_selenium(self):
        """Try to start selenium using java -jar"""
        import glob
        jar_files = glob.glob("/nix/store/*/share/selenium-server-standalone-*.jar")
        if not jar_files:
            jar_files = glob.glob("/nix/store/*/selenium-server-standalone-*.jar")

        if jar_files:
            cmd = [
                "java", "-jar", jar_files[0],
                "-port", str(self.selenium_port)
            ]

            self.selenium_process = subprocess.Popen(
                cmd,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL
            )
            return True
        return False

    def _try_selenium_standalone(self):
        """Try to use selenium-server-standalone command"""
        cmd = ["selenium-server-standalone", "-port", str(self.selenium_port)]

        self.selenium_process = subprocess.Popen(
            cmd,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL
        )
        return True

    def _skip_selenium_server(self):
        """Skip starting selenium server (assume external)"""
        print("Skipping selenium server startup, assuming external server")
        return True

    def start_http_server(self):
        """Start SimpleHTTPServer for test fixtures"""
        print("Starting HTTP server for test fixtures...")

        try:
            fixtures_dir = Path("tests/fixtures/html").resolve()
            if not fixtures_dir.exists():
                raise FileNotFoundError(f"Fixtures directory not found: {fixtures_dir}")

            # Use Python's built-in HTTP server
            cmd = [
                sys.executable, "-m", "http.server", str(self.http_port),
                "--directory", str(fixtures_dir)
            ]

            self.http_process = subprocess.Popen(
                cmd,
                stdout=subprocess.DEVNULL,
                stderr=subprocess.DEVNULL
            )

            # Wait for HTTP server to start
            self._wait_for_service("localhost", self.http_port, "HTTP server")
            print(f"HTTP server started on port {self.http_port}")

        except Exception as e:
            print(f"Failed to start HTTP server: {e}")
            raise

    def _wait_for_service(self, host: str, port: int, service_name: str, timeout: int = 30):
        """Wait for a service to become available"""
        import socket

        start_time = time.time()
        while time.time() - start_time < timeout:
            try:
                with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as sock:
                    sock.settimeout(1)
                    result = sock.connect_ex((host, port))
                    if result == 0:
                        return
            except Exception:
                pass
            time.sleep(0.5)

        raise TimeoutError(f"{service_name} did not start within {timeout} seconds")

    def run_tests(self) -> int:
        """Run the integration test suite"""
        print("Running integration tests...")

        try:
            # Build the Haskell executable first
            print("Building mcp-selenium-hs executable...")
            build_result = subprocess.run(["cabal", "build", "mcp-selenium-hs"],
                                        capture_output=True, text=True)

            if build_result.returncode != 0:
                print("Failed to build mcp-selenium-hs:")
                print(build_result.stderr)
                return 1

            # Run pytest on the integration tests
            cmd = [
                "python", "-m", "pytest",
                "tests/integration/",
                "-v",
                "--tb=short"
            ]

            result = subprocess.run(cmd)
            return result.returncode

        except Exception as e:
            print(f"Failed to run tests: {e}")
            return 1

    def cleanup(self):
        """Stop all services"""
        print("Cleaning up services...")

        if self.selenium_process:
            print("Stopping selenium server...")
            self.selenium_process.terminate()
            try:
                self.selenium_process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                self.selenium_process.kill()
            self.selenium_process = None

        if self.http_process:
            print("Stopping HTTP server...")
            self.http_process.terminate()
            try:
                self.http_process.wait(timeout=5)
            except subprocess.TimeoutExpired:
                self.http_process.kill()
            self.http_process = None

        print("Cleanup complete")


def main():
    """Main orchestrator function"""
    service_manager = ServiceManager()

    try:
        # Start services
        service_manager.start_selenium_server()
        service_manager.start_http_server()

        # Run tests
        exit_code = service_manager.run_tests()

        print(f"Tests completed with exit code: {exit_code}")
        return exit_code

    except Exception as e:
        print(f"Error during test orchestration: {e}")
        return 1

    finally:
        # Cleanup is handled by atexit and signal handlers
        service_manager.cleanup()


if __name__ == "__main__":
    sys.exit(main())
