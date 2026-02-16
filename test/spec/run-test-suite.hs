module Main where

import Test.Hspec.Extra
    ( hspecMain
    )
import Prelude

import qualified Spec

main :: IO ()
main = hspecMain Spec.spec
