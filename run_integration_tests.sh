#!/usr/bin/env bash

# Run the Python orchestration script for integration tests
exec timeout 20s python3 orchestrate_integration_tests.py
