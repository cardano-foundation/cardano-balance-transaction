{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

{- |
Copyright: © 2021-2022 IOHK
License: Apache-2.0

Inlined from @std-gen-seed@ 'System.Random.StdGenSeed'.
-}
module System.Random.StdGenSeed
    ( StdGenSeed (..)
    , stdGenSeed
    , stdGenFromSeed
    , stdGenToSeed
    ) where

import Prelude

import Control.Monad.Random.Class
    ( MonadRandom (..)
    )
import Data.Bits
    ( (.|.)
    )
import Data.Word
    ( Word64
    )
import Data.Word.Odd
    ( Lit
    , OddWord
    )
import GHC.Generics
    ( Generic
    )
import Quiet
    ( Quiet (..)
    )
import System.Random.Internal
    ( StdGen (..)
    )
import System.Random.SplitMix
    ( seedSMGen'
    , unseedSMGen
    )

import qualified Data.Bits as Bits

type Word127 = OddWord Integer (Lit 127)

newtype StdGenSeed = StdGenSeed
    { unStdGenSeed :: Word127
    }
    deriving (Eq, Bounded, Generic, Ord)
    deriving (Show) via (Quiet StdGenSeed)

stdGenSeed :: (MonadRandom m) => m StdGenSeed
stdGenSeed = do
    hi <- getRandom
    lo <- getRandom
    pure $
        StdGenSeed $
            (.|.)
                (fromIntegral @Word64 @Word127 hi `Bits.shiftL` 63)
                (fromIntegral @Word64 @Word127 lo)

stdGenFromSeed :: StdGenSeed -> StdGen
stdGenFromSeed =
    StdGen
        . seedSMGen'
        . ( \s ->
                (,)
                    (fromIntegral @Word127 @Word64 (s `Bits.shiftR` 63))
                    (fromIntegral @Word127 @Word64 (s `Bits.shiftL` 1))
          )
        . unStdGenSeed

stdGenToSeed :: StdGen -> StdGenSeed
stdGenToSeed =
    StdGenSeed
        . ( \(a, b) ->
                (.|.)
                    (fromIntegral @Word64 @Word127 a `Bits.shiftL` 63)
                    (fromIntegral @Word64 @Word127 b `Bits.shiftR` 1)
          )
        . unseedSMGen
        . unStdGen
