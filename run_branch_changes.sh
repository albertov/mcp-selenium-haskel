#!/usr/bin/env bash
set -eu -o pipefail

git diff "$(git merge-base HEAD main)...HEAD" ':(exclude)nix/materialized'
