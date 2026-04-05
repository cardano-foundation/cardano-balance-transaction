{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

{- |
Module      : Cardano.Balance.Tx.SerializationSpec
Description : CBOR serialization round-trip golden tests

Verifies that 'serializeTx' and 'deserializeTx' produce
byte-identical output for known transaction CBOR. This
guards against regressions when changing the serialization
backend (e.g. migrating from cardano-api to direct ledger
serialization).
-}
module Cardano.Balance.Tx.SerializationSpec
    ( spec
    ) where

import Cardano.Balance.Tx.Eras
    ( Conway
    , IsRecentEra
    )
import Cardano.Balance.Tx.Tx
    ( deserializeTx
    , serializeTx
    )
import Data.List
    ( isSuffixOf
    )
import System.Directory
    ( listDirectory
    )
import System.FilePath
    ( (</>)
    )
import Test.Hspec
    ( Spec
    , describe
    , it
    , pendingWith
    , shouldBe
    )
import Test.Utils.Paths
    ( getTestData
    )
import Prelude

import qualified Data.ByteString as BS

spec :: Spec
spec = do
    describe "serializeTx . deserializeTx round-trip" $ do
        describe "Conway" $ do
            it "round-trips signedTx golden files" $ do
                let dir = $(getTestData) </> "signedTxs"
                files <- listCborFiles dir
                mapM_ (assertRoundTrip @Conway) files

        describe "Dijkstra" $ do
            it "round-trips signedTx golden files" $ do
                -- Golden files are Conway-era transactions.
                -- Dijkstra deserialization via cardano-api is
                -- blocked on runtime support (see #12).
                -- Once cardano-api is removed, this test
                -- should work with Dijkstra-era test data.
                pendingWith
                    "No Dijkstra golden data yet; \
                    \cardano-api DijkstraEra not supported \
                    \at runtime (see #12)"

{- | Verify that deserializing and re-serializing a CBOR
file produces identical bytes.
-}
assertRoundTrip
    :: forall era
     . (IsRecentEra era)
    => (FilePath, BS.ByteString)
    -> IO ()
assertRoundTrip (_path, original) =
    let roundTripped =
            serializeTx (deserializeTx @era original)
    in  roundTripped `shouldBe` original

listCborFiles
    :: FilePath
    -> IO [(FilePath, BS.ByteString)]
listCborFiles dir = do
    entries <- listDirectory dir
    fmap concat $ mapM load entries
  where
    load name
        | ".cbor" `isSuffixOf` name = do
            bs <- BS.readFile (dir </> name)
            pure [(name, bs)]
        | otherwise = pure []
