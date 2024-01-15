module Main where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Node.Buffer (toArray)
import Node.FS.Sync (readFile)
import ParseMidi (many, parseFile, parseMidi, parseTrack)

main :: Effect Unit
main = do
    buf <- readFile "./1m1.mid"
    arr <- toArray buf
    let
        trackHeader = [ 77, 84, 114, 107, 0, 0, 0, 0 ]
        someText = [ 0, 0xFF, 0x01, 3, 77, 77, 77 ]
        seqNum = [ 0, 0xFF, 0x00, 0x02, 0, 1 ]
        chanPre = [ 0, 0xFF, 0x20, 0x01, 12 ]
        eot = [ 0, 0xFF, 0x2F, 0x00 ]
        tempo = [ 0, 0xFF, 0x51, 0x03, 0, 0, 64 ]
        soff = [ 0, 0xFF, 0x54, 0x05, 1, 1, 1, 0, 0 ]
        timeSig = [ 0, 0xFF, 0x58, 0x04, 4, 4, 0, 0 ]
        new = [ 0, 0xFF, 0x90, 0x05, 0, 0, 0, 0, 0 ]
        seqspec = [ 0, 0xFF, 0x7F, 1, 0 ]
        sys = [ 0, 0xF0, 3, 43, 12, 0 ]
        noteOff = [ 0, 129, 127, 127 ]
        noteOn = [ 0, 145, 0, 64 ]
        cc = [ 0, 176, 1, 64, 64 ]
        mock = trackHeader <> cc
    log $ show $ parseFile arr
--    log $ show $ parseMidi mock
--    log $ show $ many parseTrack mock

