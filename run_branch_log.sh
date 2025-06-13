#!/usr/bin/env bash
set -eu -o pipefail

git log "$(git merge-base HEAD origin/main)...HEAD" ':(exclude)nix/materialized'
