#!/usr/bin/env bash

# Run the Python orchestration script for integration tests
exec timeout 120s python3 orchestrate_integration_tests.py
