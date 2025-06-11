#!/usr/bin/env bash
set -eu -o pipefail

git log "$(git merge-base HEAD main)...HEAD" ':(exclude)nix/materialized'

