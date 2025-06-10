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
import sys


class ServiceManager:
    """Manages external services needed for integration tests"""

    def __init__(self):
        self.selenium_process: Optional[subprocess.Popen] = None
        self.selenium_port = 4444

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
            self._try_selenium_standalone()
        except Exception as e:
            print(f"Selenium startup method failed: {e}, trying next...")
        except TimeoutError:
            print("Warning: Selenium server not detected, tests may fail if selenium is required")
        except Exception as e:
            print(f"Failed to start selenium server: {e}")
            raise

        # Wait for selenium server to be available
        self._wait_for_service("localhost", self.selenium_port, "Selenium server")
        print(f"Selenium server available on port {self.selenium_port}")


    def _try_selenium_standalone(self):
        """Try to use selenium-server-standalone command"""
        cmd = ["selenium-server", "-port", str(self.selenium_port)]

        self.selenium_process = subprocess.Popen(
            cmd,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.DEVNULL
        )
        return True

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

        print("Cleanup complete")


def main():
    """Main orchestrator function"""
    service_manager = ServiceManager()

    try:
        # Start services
        service_manager.start_selenium_server()

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
