module ParseMidi where

import Prelude

import Bits (intToBits, padEight, unsafeBitsToInt)
import Data.Array (drop, head, mapMaybe, take)
import Data.Char (fromCharCode)
import Data.Int.Bits (and)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..), fst, snd)

data Event
  = MidiEvent
  | SysexEvent
  | MetaEvent

data MetaEvent
  = SeqNum
  | Text
  | Copyright
  | TrackName
  | InstName
  | Lyric
  | Marker
  | CuePoint
  | ChannelPrefix
  | EndOfTrack
  | Tempo
  | SmtpeOffset
  | TimeSig
  | KeySig
  | SeqSpec

type TimeSig =
  { num :: Int
  , denom :: Int
  , cpc :: Int
  , bb :: Int
  }

parseSeqNum :: Array Int -> Maybe (Tuple Int (Array Int))
parseSeqNum bytes = do
  byte1 <- head bytes
  byte2 <- drop 1 bytes # head
  let
    val = unsafeBitsToInt
      $ (intToBits byte1 # padEight)
          <> (intToBits byte2 # padEight)
  Just $ Tuple val bytes

parseLenBytes :: Int -> Array Int -> Maybe (Tuple Int (Array Int))
parseLenBytes t bytes = do
  byte <- head bytes
  let
    masked = and 127 byte
  case masked of
    128 -> parseLenBytes (t + byte) (drop 1 bytes)
    _ -> Just $ Tuple (t + byte) (drop 1 bytes)

-- this can be used for all text style events
parseText :: Array Int -> Maybe (Tuple String (Array Int))
parseText bytes = do
  lenTuple <- parseLenBytes 0 bytes
  let
    lengthOf = fst lenTuple
    newBytes = snd lenTuple
    text = take lengthOf newBytes
      # mapMaybe fromCharCode
      # fromCharArray
  Just $ Tuple text (drop lengthOf newBytes)

parseMidiChanPrefix :: Array Int -> Maybe (Tuple Int (Array Int))
parseMidiChanPrefix bytes = do
  num <- drop 2 bytes # head
  if num >= 15 && num <= 0 then Nothing
  else Just $ Tuple num (drop 1 bytes)

parseTempo :: Array Int -> Maybe (Tuple Int (Array Int))
parseTempo bytes = do
  byte1 <- drop 1 bytes # head
  byte2 <- drop 2 bytes # head
  byte3 <- drop 3 bytes # head
  let
    msPerQ = unsafeBitsToInt $
      (intToBits byte1 # padEight)
        <> (intToBits byte2 # padEight)
        <> (intToBits byte3 # padEight)
  Just $ Tuple msPerQ (drop 3 bytes)

parseTimeSig :: Array Int -> Maybe (Tuple TimeSig (Array Int))
parseTimeSig bytes = do
  byte1 <- drop 1 bytes # head
  byte2 <- drop 2 bytes # head
  byte3 <- drop 3 bytes # head
  byte4 <- drop 4 bytes # head
  let
    sig = { num: byte1, denom: byte2, cpc: byte3, bb: byte4 }
  Just $ Tuple sig (drop 4 bytes)

