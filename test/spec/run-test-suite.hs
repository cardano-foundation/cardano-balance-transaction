module Main where

import Prelude
import Test.Hspec.Extra
    ( hspecMain
    )

import qualified Spec

main :: IO ()
main = hspecMain Spec.spec
