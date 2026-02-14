# shellcheck shell=bash

set unstable := true

# List available recipes
default:
    @just --list

# Serve docs locally
docs-serve:
    mkdocs serve

# Build docs (strict mode)
docs-build:
    mkdocs build --strict

# Format all source files
format:
    #!/usr/bin/env bash
    set -euo pipefail
    for i in {1..3}; do
        fourmolu -i lib test
    done
    cabal-fmt -i *.cabal
    nixfmt flake.nix nix/*.nix

# Run hlint
hlint:
    #!/usr/bin/env bash
    hlint lib test

# Build all components
build:
    #!/usr/bin/env bash
    cabal build all -O0 --enable-tests

# Run unit tests with optional match pattern
unit match="":
    #!/usr/bin/env bash
    if [[ '{{ match }}' == "" ]]; then
        cabal test unit -O0 --enable-tests --test-show-details=direct
    else
        cabal test unit -O0 --enable-tests \
            --test-show-details=direct \
            --test-option=--match \
            --test-option="{{ match }}"
    fi

# Full CI pipeline
CI:
    #!/usr/bin/env bash
    set -euo pipefail
    just build
    just unit
    fourmolu -m check lib test
    hlint lib test
    nixfmt --check flake.nix nix/*.nix
