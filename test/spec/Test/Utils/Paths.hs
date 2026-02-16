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
import System.FilePath
    ( (</>)
    )
import Prelude

import qualified Data.FileEmbed as FE

-- | Get the path to the test/data directory at compile time.
getTestData :: Q Exp
getTestData = do
    path <- FE.makeRelativeToProject ("test" </> "data")
    pure $ LitE (StringL path)
