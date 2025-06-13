#!/usr/bin/env bash
set -eu -o pipefail
REPO_ROOT="$(git rev-parse --show-toplevel)"
trap "git config --global --unset core.hooksPath" EXIT
# Disable pre-commit hooks because they format the source whenever the agent
# commits stuff and confuses it. Me no like getting throttled because of
# confused robots.
git config --global core.hooksPath /dev/null
# Always bind to an interface that is NOT, NOT, NOT exposed to the internet.
cd $REPO_ROOT
mcp-proxy \
  --port 8000 \
  --host localhost \
  --allow-origin=https://claude.ai \
  --named-server codemcp \
    codemcp \
  --named-server haskell-lsp ./mcp_language_haskell.sh
