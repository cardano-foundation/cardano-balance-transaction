{- |
Inlined from @cardano-wallet-test-utils@ 'Test.Utils.Pretty'.
-}
module Test.Utils.Pretty
    ( Pretty (..)
    , (====)
    )
where

import Test.QuickCheck
    ( Property
    , (===)
    )
import Prelude

import qualified Text.Show.Pretty as P

-- | Newtype for pretty-printing in test failures.
newtype Pretty a = Pretty {unPretty :: a}
    deriving (Eq, Ord)

instance (Show a) => Show (Pretty a) where
    show (Pretty a) = P.ppShow a

-- | Like '(===)' but with pretty-printed output.
(====) :: (Eq a, Show a) => a -> a -> Property
a ==== b = Pretty a === Pretty b

infix 4 ====
