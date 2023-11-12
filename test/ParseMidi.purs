module Test.ParseMidi where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import ParseMidi (Event(..), HeaderEvent(..), MetaEvent(..), MidiEvent(..), parseEvent, parseHeader, parseLenBytes, parseMidi, parseNoteOff)
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
            test "test out parseLenBytes" do
                let
                    expected = Just $ Tuple 259 [0,0,0]
                    input = [129, 129, 1, 0,0,0]
                Assert.equal expected (parseLenBytes 0 input)
            test "test parse text event" do
                let
                    expected = Just $ Tuple (MetaEvent $ Text "MMM") []
                    input = [0, 255, 1, 3, 77, 77,77]
                Assert.equal expected (parseEvent input)

eventParserTests :: Effect Unit
eventParserTests = do
    runTest do
        suite "Event parser Tests" do
            test "correctly ident track header" do
                let
                    expected = Just $ Tuple (HeaderEvent $ File) []
                    input = [77, 84, 104, 100,0,0, 0, 0]
                Assert.equal expected (parseEvent input)
