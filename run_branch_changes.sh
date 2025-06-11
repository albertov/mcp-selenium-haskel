#!/usr/bin/env bash
set -eu -o pipefail

FILES="$(git diff --name-only origin/main...HEAD | grep -v 'nix/materialized')"
git diff origin/main...HEAD -- $FILES
