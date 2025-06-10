#!/usr/bin/env bash

# Run the Python orchestration script for integration tests
# IMPORTANT: Do not change the timeout!
exec timeout 20s python3 orchestrate_integration_tests.py
