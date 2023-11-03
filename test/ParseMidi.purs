module Test.ParseMidi where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import ParseMidi (Event(..), MidiEvent(..), parseMidi, parseNoteOff)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

midiParserTests :: Effect Unit
midiParserTests = do
    runTest do
        suite "parseNoteOff - MIDI event parser" do
            test "correctly read valid input" do
                let
                    expected = Just $ Tuple (MidiEvent $ NoteOff {key: 1, vel: 1, chan: 1}) [0,0,0]
                    input = [129, 1, 1, 0,0,0]
                Assert.equal expected (parseNoteOff input)
            test "Nothing if input has insufficient bytes" do
                let
                    expected = Nothing
                    input = [129, 1]
                Assert.equal expected (parseNoteOff input)

majorMidiParseTests :: Effect Unit
majorMidiParseTests = do
    runTest do
        suite "MidiEvent parser tests" do
            test "correctly ident note off" do
                let
                    expected = Just $ Tuple (MidiEvent $ NoteOff {key: 1, vel: 1, chan: 1}) [0,0,0]
                    input = [129, 1, 1, 0,0,0]
                Assert.equal expected (parseMidi input)
            test "correctly ident note on" do
                let
                    expected = Just $ Tuple (MidiEvent $ NoteOn {key: 1, vel: 1, chan: 1}) [0,0,0]
                    input = [145, 1, 1, 0,0,0]
                Assert.equal expected (parseMidi input)
            test "correctly ident polykeypress" do
                let
                    expected = Just $ Tuple (MidiEvent $ PolyKeyPress) [0,0,0]
                    input = [160, 1, 1, 0,0,0]
                Assert.equal expected (parseMidi input)
