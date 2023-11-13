module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.ParseMidi (eventParserTests, majorMidiParseTests, midiParserTests, parseFileTests)

main :: Effect Unit
main = do
    log "-- Individual Parser Tests --"
    midiParserTests
    majorMidiParseTests
    eventParserTests
    parseFileTests
