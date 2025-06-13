#!/usr/bin/env bash
set -eu -o pipefail

REPO_ROOT="$(git rev-parse --show-toplevel)"

exec mcp-language-server \
  --workspace "${REPO_ROOT}" \
  --lsp haskell-language-server -- --lsp
