module Test.ParseMidi where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import ParseMidi (Event(..), HeaderEvent(..), MetaEvent(..), MidiEvent(..), SmpteOffset, parseEvent, parseFile, parseHeader, parseLenBytes, parseMidi, parseNoteOff)
import Test.Unit (suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

midiParserTests :: Effect Unit
midiParserTests = do
    runTest do
        suite "parseNoteOff - MIDI event parser" do
            test "correctly read valid input" do
                let
                    expected = Right $ Tuple (MidiEvent $ NoteOff {key: 1, vel: 1, chan: 1}) [0,0,0]
                    input = [129, 1, 1, 0,0,0]
                Assert.equal expected (parseNoteOff input)
            test "Nothing if input has insufficient bytes" do
                let
                    expected = Left "failed at noteOff"
                    input = [129, 1]
                Assert.equal expected (parseNoteOff input)

majorMidiParseTests :: Effect Unit
majorMidiParseTests = do
    runTest do
        suite "MidiEvent parser tests" do
            test "correctly ident note off" do
                let
                    expected = Right $ Tuple (MidiEvent $ NoteOff {key: 1, vel: 1, chan: 1}) [0,0,0]
                    input = [129, 1, 1, 0,0,0]
                Assert.equal expected (parseMidi input)
            test "correctly ident note on" do
                let
                    expected = Right $ Tuple (MidiEvent $ NoteOn {key: 1, vel: 1, chan: 1}) [0,0,0]
                    input = [145, 1, 1, 0,0,0]
                Assert.equal expected (parseMidi input)
            test "correctly ident polykeypress" do
                let
                    expected = Right $ Tuple (MidiEvent $ PolyKeyPress) [0,0,0]
                    input = [160, 1, 1, 0,0,0]
                Assert.equal expected (parseMidi input)
            test "correctly ident progChange" do
                let
                    expected = Right $ Tuple (MidiEvent $ ProgChange) []
                    input = [193, 1]
                Assert.equal expected (parseMidi input)
            test "test out parseLenBytes" do
                let
                    expected = Just $ Tuple 259 [0,0,0]
                    input = [129, 129, 1, 0,0,0]
                Assert.equal expected (parseLenBytes 0 input)

eventParserTests :: Effect Unit
eventParserTests = do
    runTest do
        suite "Event parser Tests" do
            test "correctly ident track header" do
                let
                    expected = Right $ Tuple (HeaderEvent $ File) []
                    input = [77, 84, 104, 100,0,0, 0, 0]
                Assert.equal expected (parseEvent input)
            test "test parse text event" do
                let
                    expected = Right $ Tuple (MetaEvent $ Text "MMM") []
                    input = [0, 255, 1, 3, 77, 77,77]
                Assert.equal expected (parseEvent input)
            test "correctly ident progChange" do
                let
                    expected = Right $ Tuple (MidiEvent $ ProgChange) [0, 255, 0, 0]
                    input = [193, 1, 0, 255, 0, 0]
                Assert.equal expected (parseEvent input)
            test "correctly ident chanPress" do
                let
                    expected = Right $ Tuple (MidiEvent $ AfterTouch) [0, 255, 0, 0]
                    input = [208, 1, 0, 255, 0, 0]
                Assert.equal expected (parseEvent input)
            test "correctly do SMTPE offset" do
                let
                    expected = Right $ Tuple (MetaEvent $ SmpteOffset {hr: 65, mn: 0, sec: 30, fr: 2, fFr: 55}) [] 
                    input = [0, 255, 84, 5,65, 0,30,2,55]
                Assert.equal expected (parseEvent input)
            test "correctly do tempo" do
                let
                    expected = Right $ Tuple (MetaEvent $ Tempo 0) [] 
                    input = [0, 255, 81, 3,15, 0,15,200,242, 158]
                Assert.equal expected (parseEvent input)

parseFileTests :: Effect Unit
parseFileTests = do
    runTest do
        suite "file parse tests" do
            test "tiny file test" do
                let
                    expected = Right [(HeaderEvent File),(MetaEvent (Text "MMM")),(MidiEvent ProgChange)]
                    input = [77, 84, 104, 100, 0, 0, 0, 6, 0, 1, 1, 75,1, 224, 0, 255, 1, 3, 77, 77, 77, 193, 1]
                Assert.equal expected (parseFile input)
