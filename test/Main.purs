module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (log)
import Test.ParseMidi (majorMidiParseTests, midiParserTests)

main :: Effect Unit
main = do
    log "-- Individual Parser Tests --"
    midiParserTests
    majorMidiParseTests
