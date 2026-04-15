{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Cardano.Balance.Tx.TxSpec where

import Cardano.Balance.Tx.Eras
    ( AnyRecentEra (..)
    , Conway
    , Dijkstra
    , RecentEra (..)
    )
import Cardano.Balance.Tx.Tx
    ( Coin (..)
    , DatumHash
    , TxOut
    , computeMinimumCoinForTxOut
    , datumHashFromBytes
    , datumHashToBytes
    , isBelowMinimumCoinForTxOut
    )
import Cardano.Ledger.Address
    ( Addr (..)
    )
import Cardano.Ledger.Api
    ( PParams
    , coinTxOutL
    , mkBasicTxOut
    , ppCoinsPerUTxOByteL
    )
import Cardano.Ledger.BaseTypes
    ( Network (..)
    )
import Cardano.Ledger.Credential
    ( Credential (..)
    , StakeReference (..)
    )
import Cardano.Ledger.Hashes
    ( KeyHash (..)
    )
import Control.Lens
    ( (&)
    , (.~)
    )
import Data.ByteString
    ( ByteString
    )
import Data.Default
    ( Default (..)
    )
import Data.Maybe
    ( fromMaybe
    )
import Data.Word
    ( Word16
    , Word64
    )
import Test.Cardano.Ledger.Alonzo.Arbitrary
    (
    )
import Test.Cardano.Ledger.Babbage.Arbitrary
    (
    )
import Test.Cardano.Ledger.Conway.Arbitrary
    (
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , pendingWith
    , shouldBe
    , shouldNotBe
    )
import Test.QuickCheck
    ( Arbitrary (..)
    , Gen
    , Property
    , choose
    , conjoin
    , counterexample
    , elements
    , property
    , suchThat
    , vectorOf
    , (===)
    )
import Test.QuickCheck.Classes
    ( eqLaws
    , showLaws
    )
import Test.Utils.Laws
    ( testLawsMany
    )
import Prelude

import qualified Cardano.Crypto.Hash.Class as Crypto
import qualified Cardano.Ledger.Coin as Ledger
import qualified Data.ByteString as BS

spec :: Spec
spec = do
    describe "AnyRecentEra" $ do
        -- boundedEnumLaws blocked on cardano-api DijkstraEra
        -- runtime support (see #12)
        it "boundedEnumLaws" $
            pendingWith
                "Blocked on cardano-api DijkstraEra \
                \runtime support (see #12)"
        describe "Class instances obey laws" $ do
            testLawsMany @AnyRecentEra
                [ eqLaws
                , showLaws
                ]

    describe "DatumHash" $ do
        it "datumHashFromBytes . datumHashToBytes == Just" $
            property $
                \(ValidDatumHash h) -> do
                    let f = datumHashFromBytes . datumHashToBytes
                    f h === Just h

        describe "datumHashFromBytes goldens" $ do
            it "32 bytes -> Just" $ do
                datumHashFromBytes (BS.replicate 32 0)
                    `shouldNotBe` Nothing
            it "28 bytes -> Nothing" $ do
                datumHashFromBytes (BS.replicate 28 0)
                    `shouldBe` Nothing
            it "33 bytes -> Nothing" $ do
                datumHashFromBytes (BS.replicate 28 0)
                    `shouldBe` Nothing

    describe "TxOut" $ do
        describe "computeMinimumCoinForTxOut" $ do
            it
                "isBelowMinimumCoinForTxOut (setCoin (result <> delta)) \
                \ == False (Dijkstra)"
                $ property
                $ \(DijkstraTxOut out)
                   (AdditionalCoin delta)
                   (TestCoinPerByte perByte) -> do
                        let pp =
                                (def :: PParams Dijkstra)
                                    & ppCoinsPerUTxOByteL .~ perByte
                        let c = delta <> computeMinimumCoinForTxOut pp out
                        isBelowMinimumCoinForTxOut
                            pp
                            (out & coinTxOutL .~ c)
                            === False

            it
                "isBelowMinimumCoinForTxOut (setCoin (result <> delta)) \
                \ == False (Conway)"
                $ property
                $ \(ConwayTxOut out)
                   (AdditionalCoin delta)
                   (TestCoinPerByte perByte) -> do
                        let pp =
                                (def :: PParams Conway)
                                    & ppCoinsPerUTxOByteL .~ perByte
                        let c = delta <> computeMinimumCoinForTxOut pp out
                        isBelowMinimumCoinForTxOut
                            pp
                            (out & coinTxOutL .~ c)
                            === False

--------------------------------------------------------------------------------
-- Arbitrary instances
--------------------------------------------------------------------------------

{- | Uses explicit constructors instead of 'arbitraryBoundedEnum'
because cardano-api's 'toEnum' doesn't support DijkstraEra
yet (see #12).
-}
instance Arbitrary AnyRecentEra where
    arbitrary =
        elements
            [ AnyRecentEra RecentEraConway
            , AnyRecentEra RecentEraDijkstra
            ]
    shrink _ = []

newtype ValidDatumHash = ValidDatumHash DatumHash
    deriving stock (Show)

newtype ConwayTxOut = ConwayTxOut (TxOut Conway)
    deriving stock (Show)

newtype DijkstraTxOut = DijkstraTxOut (TxOut Dijkstra)
    deriving stock (Show)

newtype AdditionalCoin = AdditionalCoin Coin
    deriving stock (Show)

newtype TestCoinPerByte = TestCoinPerByte Ledger.CoinPerByte
    deriving stock (Show)

instance Arbitrary ValidDatumHash where
    arbitrary =
        ValidDatumHash
            . fromMaybe err
            . datumHashFromBytes
            <$> genHashBytes
      where
        err = error "ValidDatumHash: failed to construct datum hash"
    shrink _ = []

instance Arbitrary ConwayTxOut where
    arbitrary =
        ConwayTxOut <$> (arbitrary `suchThat` isReasonableConwayTxOut)
    shrink _ = []

instance Arbitrary DijkstraTxOut where
    arbitrary =
        DijkstraTxOut
            <$> (genDijkstraTxOut `suchThat` isReasonableDijkstraTxOut)
    shrink _ = []

instance Arbitrary AdditionalCoin where
    arbitrary =
        AdditionalCoin . Coin . fromIntegral
            <$> choose @(Word16) (0, 10000)
    shrink _ = []

instance Arbitrary TestCoinPerByte where
    arbitrary =
        TestCoinPerByte
            . Ledger.CoinPerByte
            . Ledger.CompactCoin
            . fromIntegral
            <$> choose @(Word16) (1, 1000)
    shrink _ = []

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

genDijkstraTxOut :: Gen (TxOut Dijkstra)
genDijkstraTxOut =
    mkBasicTxOut <$> genAddr <*> arbitrary

genAddr :: Gen Addr
genAddr =
    Addr
        <$> elements [Mainnet, Testnet]
        <*> (KeyHashObj <$> genKeyHash)
        <*> pure StakeRefNull

genKeyHash :: Gen (KeyHash kr)
genKeyHash =
    KeyHash
        . fromMaybe err
        . Crypto.hashFromBytes
        <$> genHashBytes28
  where
    err = error "genKeyHash: invalid 28-byte hash"

genHashBytes :: Gen ByteString
genHashBytes = BS.pack <$> vectorOf 32 arbitrary

genHashBytes28 :: Gen ByteString
genHashBytes28 = BS.pack <$> vectorOf 28 arbitrary

isReasonableConwayTxOut :: TxOut Conway -> Bool
isReasonableConwayTxOut out =
    coinFitsTxOut $ computeMinimumCoinForTxOut minPParams out
  where
    minPParams =
        (def :: PParams Conway)
            & ppCoinsPerUTxOByteL
                .~ Ledger.CoinPerByte (Ledger.CompactCoin 1)

isReasonableDijkstraTxOut :: TxOut Dijkstra -> Bool
isReasonableDijkstraTxOut out =
    coinFitsTxOut $ computeMinimumCoinForTxOut minPParams out
  where
    minPParams =
        (def :: PParams Dijkstra)
            & ppCoinsPerUTxOByteL
                .~ Ledger.CoinPerByte (Ledger.CompactCoin 1)

coinFitsTxOut :: Coin -> Bool
coinFitsTxOut (Coin c) =
    c >= 0 && c <= toInteger (maxBound :: Word64)

{- | Allows 'testIsomorphism' to be called more conveniently when short on
horizontal space, compared to a multiline "(a -> b, String)".

@@
     (NamedFun
         fun
         "fun")
@@

vs

@@
     ( fun
     , "fun"
     )
@@
-}
data NamedFun a b = NamedFun (a -> b) String

{- | Tests @f . g == id@ and @g . f == id@

@@
                f
     ┌───┐ ─────────▶ ┌───┐
     │ a │            │ b │
     │   │            │   │
     └───┘ ◀───────── └───┘
                g
@@
-}
testIsomorphism
    :: (Arbitrary a, Arbitrary b, Show a, Show b, Eq a, Eq b)
    => NamedFun a b
    -> NamedFun b a
    -> (b -> b)
    -- ^ Optional normalization, otherwise use @id@.
    -> Property
testIsomorphism (NamedFun f fName) (NamedFun g gName) normalize =
    conjoin
        [ counterexample
            (fName <> " . " <> gName <> " == id")
            (property $ \x -> f (g (normalize x)) === normalize x)
        , counterexample
            (gName <> " . " <> fName <> " == id")
            (property $ \x -> g (f x) === x)
        ]
