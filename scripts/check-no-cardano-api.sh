#!/usr/bin/env bash
set -euo pipefail

# T002: Fail if any Cardano.Api imports remain in lib/ or test/spec/.
# Excludes comments and strings — matches actual import statements.

hits=$(grep -rn '^import.*Cardano\.Api' lib/ test/spec/ 2>/dev/null || true)

if [ -n "$hits" ]; then
    echo "ERROR: Cardano.Api imports found:"
    echo "$hits"
    exit 1
fi

# Also check cabal build-depends
if grep -q '^\s*,\s*cardano-api' cardano-balance-tx.cabal; then
    echo "ERROR: cardano-api in cabal build-depends"
    exit 1
fi

echo "OK: zero Cardano.Api references"
