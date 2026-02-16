{- |
Inlined from @cardano-wallet-test-utils@ 'Test.Utils.Paths'.
-}
module Test.Utils.Paths
    ( getTestData
    )
where

import Language.Haskell.TH
    ( Exp (..)
    , Lit (..)
    , Q
    )
import Prelude

{- | Get the path to the test/data directory.
Hardcoded so that @nix run@ works from the repo root.
-}
getTestData :: Q Exp
getTestData = pure $ LitE (StringL "test/data")
