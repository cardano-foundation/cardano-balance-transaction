{- |
Inlined from @cardano-wallet-test-utils@ 'Test.Hspec.Extra'.
-}
module Test.Hspec.Extra
    ( hspecMain
    , parallel
    )
where

import Test.Hspec
    ( Spec
    , hspec
    )
import Prelude

import qualified Test.Hspec as Hspec

-- | Run a test suite.
hspecMain :: Spec -> IO ()
hspecMain = hspec

-- | Mark specs for parallel execution.
parallel :: Spec -> Spec
parallel = Hspec.parallel
