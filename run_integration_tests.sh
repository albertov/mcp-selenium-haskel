#!/usr/bin/env bash

# Run the Python orchestration script for integration tests
# IMPORTANT: Do not change the timeout!
set -eu -o pipefail
cabal build
NEWPYTHONPATH="$(pwd)/tests:$PYTHONPATH"
export PYTHONPATH="${NEWPYTHONPATH}"
exec timeout 30s python3 orchestrate_integration_tests.py
