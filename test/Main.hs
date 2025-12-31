module Main where

import Test.Hspec

import qualified ParserSpec
import qualified TypesSpec
import qualified IntegrationSpec

main :: IO ()
main = hspec $ do
    describe "Parser" ParserSpec.spec
    describe "Types" TypesSpec.spec
    describe "Integration" IntegrationSpec.spec
