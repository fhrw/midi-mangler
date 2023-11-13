module Test.ParseMidi where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import ParseMidi (Event(..), HeaderEvent(..), parseFile, parseFileHeader, parseHeader, parseTrackHeader, parseVarLenNum)
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
                    expected = Just $ Tuple (HeaderEvent $ File {format: 1, nTracks: 332, division: 480}) []
                    input = [ 0, 0, 0, 6, 0, 1, 1, 76, 1, 224 ] 
                Assert.equal expected (parseFileHeader input)
            test "parseFileHeader: read invalid header chunk" do
                let
                    expected = Nothing
                    input = [ 0, 0, 0, 6, 0, 1, 1, 76, 129, 224 ] 
                Assert.equal expected (parseFileHeader input)
            test "parseTrackHeader: read valid track header chunk" do
                let
                    expected = Just $ Tuple (HeaderEvent $ Track 2250) [0, 255]
                    input = [0, 0, 8, 202, 0, 255] 
                Assert.equal expected (parseTrackHeader input)
            test "parseHeader: read entire valid header chunk using generic parseHeader" do
                let
                    expected = Right $ Tuple (HeaderEvent $ Track 2250) [0, 255]
                    input = [ 114, 107, 0, 0, 8, 202, 0, 255] 
                Assert.equal expected (parseHeader input)
            test "parseHeader: read entire valid file header chunk using generic parseHeader" do
                let
                    expected = Right $ Tuple (HeaderEvent $ File {format: 1, nTracks: 332, division: 480}) [77, 84]
                    input = [ 104, 100, 0, 0, 0, 6, 0, 1, 1, 76, 1, 224, 77, 84] 
                Assert.equal expected (parseHeader input)
            test "parseHeader: should fail if header invalid" do
                let
                    expected = Left "Failed to reader Header Chunk"
                    input = [ 104, 100, 0, 0, 0, 6, 1, 1, 76, 1, 224, 77, 84] 
                Assert.equal expected (parseHeader input)
            test "parseFile: read opening of valid test file" do
                let
                    expected = Left "ldkfj"
                    input = [ 77, 84, 104, 100, 0, 0, 0, 6, 1, 1, 76, 1, 224, 77, 84, 114, 107, 0,0,8,202] 
                Assert.equal expected (parseFile input)

