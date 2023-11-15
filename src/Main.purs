module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import ParseMidi (many, parseTrack)

main :: Effect Unit
main = do
    let
        trackHeader = [77, 84, 114, 107, 0,0,0,7]
        events = [0, 0xFF, 0x02, 3, 77, 77, 77 ]
        mock = trackHeader <> events <> trackHeader <> events
    log $ show $ many parseTrack mock

