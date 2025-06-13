#!/usr/bin/env bash
set -eu -o pipefail

git diff --name-only "$(git merge-base HEAD origin/main)...HEAD" ':(exclude)nix/materialized'

