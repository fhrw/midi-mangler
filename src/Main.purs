module Main where

import Prelude

import Bits (intToBits, unsafeBitsToInt)
import Data.Array (drop, head, slice)
import Data.Int.Bits (shl)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Node.Buffer (toArray)
import Node.FS.Sync (readFile)

main :: Effect Unit
main = do
  let
    b = intToBits 233
  log $ show (intToBits 233)
  log $ show (unsafeBitsToInt b)

type MidiFile = { header :: MidiHeader, file :: MidiData }

type MidiData = Array Int

type MidiHeader = { format :: MidiFormat, numTracks :: Int, divisionForm :: DivisionForm }

data MidiFormat = Type0 | Type1 | Type2

data DivisionForm
  = QuarterTicks Int
  | SmtpeTicks Int Int

openMidiFile :: Effect MidiFile
openMidiFile = do
  buf <- readFile "./1m1.mid"
  arr <- toArray buf
  let
    format = slice 9 12 arr
      # parseMidiFormat
    numTracks = slice 13 14 arr
    division = slice 15 16 arr
  pure
    { header: { format: Type1, numTracks: 12, divisionForm: QuarterTicks 480 }
    , file: []
    }

parseMidiDivision :: Array Int -> Maybe DivisionForm
parseMidiDivision bytes = do
  byte1 <- head bytes
  byte2 <- drop 1 bytes # head
  -- this is wrong
  if (shl 1 byte1) <= 0 then
    -- this is quarter ticks
    Nothing
  else
    -- this is smtpe ticks
    Nothing

parseMidiFormat :: Array Int -> Maybe MidiFormat
parseMidiFormat bytes = do
  byte1 <- head bytes
  byte2 <- drop 1 bytes # head
  case byte1 of
    0 -> case byte2 of
      0 -> Just Type0
      1 -> Just Type1
      2 -> Just Type2
      _ -> Nothing
    _ -> Nothing
