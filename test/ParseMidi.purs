module Test.ParseMidi where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import ParseMidi (parseFileHeader, parseVarLenNum)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

individualParserTests :: Effect Unit
individualParserTests = do
    runTest do
        suite "parse variable length" do
            test "parseVarLenNum: read single number" do
                let
                    expected = Right $ Tuple 1 []
                    input = [ 1 ]
                Assert.equal expected (parseVarLenNum input)
            test "parseVarLenNum: read longer number" do
                let
                    expected = Right $ Tuple 0x3FFF []
                    input = [ 0xff, 0x7F ]
                Assert.equal expected (parseVarLenNum input)
            test "parseVarLenNum: read longest number" do
                let
                    expected = Right $ Tuple 0xFFFFFFF []
                    input = [ 0xff, 0xff, 0xff, 0x7F ]
                Assert.equal expected (parseVarLenNum input)
            test "parseVarLenNum: should fail if illegal length" do
                let
                    expected = Left "variable length exceeded allowed length"
                    input = [ 0xff, 0xff, 0xff, 0xff, 0xff ]
                Assert.equal expected (parseVarLenNum input)
            test "parseFileHeader: read valid file header chunk" do
                let
                    expected = Just $ Tuple { format: 1, nTracks: 332, division: 480 } []
                    input = [77, 84, 104, 100, 0, 0, 0, 6, 0, 1, 1, 76, 1, 224 ]
                Assert.equal expected (parseFileHeader input)
            test "parseFileHeader: read invalid header chunk" do
                let
                    expected = Nothing
                    input = [ 77, 84, 104, 100, 0, 0, 0, 6, 0, 1, 1, 76, 129, 224 ]
                Assert.equal expected (parseFileHeader input)

