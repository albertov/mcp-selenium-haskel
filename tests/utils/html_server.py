import asyncio
import threading
import socket
from http.server import HTTPServer, SimpleHTTPRequestHandler
from pathlib import Path


class TestHTMLServer:
    """Simple HTTP server for serving test HTML fixtures"""

    def __init__(self, port=None, fixtures_dir="tests/fixtures/html"):
        self.port = port or self._find_free_port()
        self.fixtures_dir = Path(fixtures_dir).resolve()
        self.base_url = f"http://localhost:{self.port}"
        self.server = None
        self.server_thread = None

    def _find_free_port(self):
        """Find a free port to bind to"""
        with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
            s.bind(('', 0))
            s.listen(1)
            port = s.getsockname()[1]
        return port

    async def start(self):
        """Start the HTTP server in a separate thread"""
        # Change to fixtures directory to serve files
        original_cwd = Path.cwd()

        def run_server():
            try:
                # Change to fixtures directory
                import os
                os.chdir(self.fixtures_dir)

                # Create and start server with retry logic for port conflicts
                max_retries = 5
                for attempt in range(max_retries):
                    try:
                        self.server = HTTPServer(('localhost', self.port), SimpleHTTPRequestHandler)
                        break
                    except OSError as e:
                        if e.errno == 98 and attempt < max_retries - 1:  # Address already in use
                            self.port = self._find_free_port()
                            self.base_url = f"http://localhost:{self.port}"
                            continue
                        raise

                if self.server:
                    self.server.serve_forever()
            except Exception as e:
                print(f"Server error: {e}")
            finally:
                # Restore original directory
                os.chdir(original_cwd)

        self.server_thread = threading.Thread(target=run_server, daemon=True)
        self.server_thread.start()

        # Wait a bit for server to start
        await asyncio.sleep(0.5)

    async def stop(self):
        """Stop the HTTP server"""
        if self.server:
            self.server.shutdown()
            self.server.server_close()

        if self.server_thread:
            self.server_thread.join(timeout=1.0)
