import asyncio
import threading
import time
from http.server import HTTPServer, SimpleHTTPRequestHandler
from pathlib import Path


class TestHTMLServer:
    """Simple HTTP server for serving test HTML fixtures"""

    def __init__(self, port=8080, fixtures_dir="tests/fixtures/html"):
        self.port = port
        self.fixtures_dir = Path(fixtures_dir).resolve()
        self.base_url = f"http://localhost:{port}"
        self.server = None
        self.server_thread = None

    async def start(self):
        """Start the HTTP server in a separate thread"""
        # Change to fixtures directory to serve files
        original_cwd = Path.cwd()

        def run_server():
            try:
                # Change to fixtures directory
                import os
                os.chdir(self.fixtures_dir)

                # Create and start server
                self.server = HTTPServer(('localhost', self.port), SimpleHTTPRequestHandler)
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
