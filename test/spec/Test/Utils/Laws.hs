{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

{- |
Inlined from @cardano-wallet-test-utils@ 'Test.Utils.Laws'.
-}
module Test.Utils.Laws
    ( testLawsMany
    )
where

import Data.Proxy
    ( Proxy (..)
    )
import Data.Typeable
    ( Typeable
    , typeRep
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    )
import Test.QuickCheck
    ( property
    )
import Test.QuickCheck.Classes
    ( Laws (..)
    )
import Prelude

-- | Test multiple sets of laws for a type.
testLawsMany
    :: forall a
     . (Typeable a)
    => [Proxy a -> Laws]
    -> Spec
testLawsMany getLawsMany =
    describe (show $ typeRep (Proxy @a)) $ do
        mapM_ testLaws getLawsMany
  where
    testLaws getLaws = do
        let Laws name pairs = getLaws (Proxy @a)
        describe name $ do
            mapM_ (uncurry testProp) pairs
    testProp name prop = it name $ property prop
