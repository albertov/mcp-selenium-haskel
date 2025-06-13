#!/usr/bin/env bash
set -eu -o pipefail

git diff "$(git merge-base HEAD origin/main)...HEAD" ':(exclude)nix/materialized'
