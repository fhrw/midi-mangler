module Test.Parser where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.State (evalStateT)
import Data.Either (Either(..))
import Data.Newtype (unwrap)
import Effect (Effect)
import StateParser (ParseError(..), parseHeader, parseVarLen, readUint16)
import Test.Unit (test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

individualParserTests :: Effect Unit
individualParserTests = do
    runTest do
            test "parseFileHeader: read valid header chunk" do
                let
                    expected = Right { format: 1, nTracks: 332, division: 480 }
                    input = [ 77, 84, 104, 100, 0, 0, 0, 6, 0, 1, 1, 76, 1, 224 ]
                Assert.equal expected
                    ( unwrap
                          $ runExceptT
                          $ evalStateT parseHeader { file: input, pos: 0 }
                    )
            test "parseFileHeader: read invalid header chunk" do
                let
                    expected = Left MissingMThd
                    input = [ 78, 84, 104, 100, 0, 1, 0, 6, 0, 1, 1, 76, 129, 224 ]
                Assert.equal expected
                    ( unwrap
                          $ runExceptT
                          $ evalStateT parseHeader { file: input, pos: 0 }
                    )
            test "parseUint16: read uint correctly" do
                let
                    expected = Right 1
                    input = [ 0, 1 ]
                Assert.equal expected (unwrap $ runExceptT $ evalStateT readUint16 { file: input, pos: 0 })
            test "parseUint16: read non-uint should be error" do
                let
                    expected = Left $ GenericError "didn't find unsigned uint16"
                    input = [ 128, 1 ]
                Assert.equal expected
                    ( unwrap
                          $ runExceptT
                          $ evalStateT readUint16 { file: input, pos: 0 }
                    )
            test "parseUint16: empty arr is err" do
                let
                    expected = Left $ GenericError "failed to read byte!"
                    input = [ 1 ]
                Assert.equal expected
                    ( unwrap
                          $ runExceptT
                          $ evalStateT readUint16 { file: input, pos: 0 }
                    )
            test "parseVarLen: read single digit" do
                let
                    expected = Right 1
                    input = [ 1 ]
                Assert.equal expected
                    ( unwrap
                          $ runExceptT
                          $ evalStateT parseVarLen { file: input, pos: 0 }
                    )
            test "parseVarLen: read long num" do
                let
                    expected = Right 0xFFFFFFF 
                    input = [ 0xff, 0xff, 0xff, 0x7F ]
                Assert.equal expected
                    ( unwrap
                          $ runExceptT
                          $ evalStateT parseVarLen { file: input, pos: 0 }
                    )
            test "parseVarLen: read diff num" do
                let
                    expected = Right 129 
                    input = [129, 1]
                Assert.equal expected
                    ( unwrap
                          $ runExceptT
                          $ evalStateT parseVarLen { file: input, pos: 0 }
                    )
