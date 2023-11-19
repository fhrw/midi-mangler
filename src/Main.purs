module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import ParseMidi (many, parseTrack)

main :: Effect Unit
main = do
    let
        trackHeader = [ 77, 84, 114, 107, 0, 0, 0, 0 ]
        someText = [ 0, 0xFF, 0x02, 3, 77, 77, 77 ]
        seqNum = [0, 0xFF, 0x00, 0x02, 0, 1 ]
        chanPre = [0, 0xFF, 0x20, 0x01, 12]
        eot = [0, 0xFF, 0x2F, 0x00]
        tempo = [0, 0xFF, 0x51, 0x03, 0, 0, 64]
        mock = trackHeader <> seqNum <> tempo <> eot <> trackHeader <> chanPre
    log $ show $ many parseTrack mock

