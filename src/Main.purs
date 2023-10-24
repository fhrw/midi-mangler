module Main where

import Prelude

import Bits (intToBits, padEight, unsafeBitsToInt)
import Data.Array (drop, head, slice)
import Data.Generic.Rep (class Generic)
import Data.Int.Bits (shl, shr)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Node.Buffer (toArray)
import Node.FS.Sync (readFile)

main :: Effect Unit
main = do
  file <- openMidiFile
  log $ show $ (shr 2 1)
  log $ show $ shr (shl 1 1) 1
  log $ show file

newtype MidiFile = MidiFile { header :: MidiHeader, file :: MidiData }

derive newtype instance Show MidiFile

type MidiData = Array Int

type MidiHeader = { format :: MidiFormat, numTracks :: Int, divisionForm :: DivisionForm }

data MidiFormat = Type0 | Type1 | Type2

derive instance Generic MidiFormat _

instance Show MidiFormat where
  show = genericShow

data DivisionForm
  = QuarterTicks Int
  | SmtpeTicks Int Int

instance Show DivisionForm where
  show (QuarterTicks x) = "QuarterTicks" <> show x
  show (SmtpeTicks x y) = "SMPTE ticks" <> show x <> show y

openMidiFile :: Effect (Maybe MidiFile)
openMidiFile = do
  buf <- readFile "./1m1.mid"
  arr <- toArray buf
  pure do
    format <- parseMidiFormat $ slice 8 10 arr
    numTracks <- parseNumTracks $ slice 10 12 arr
    divisionForm <- parseMidiDivision $ slice 12 14 arr
    pure $ MidiFile
      { header:
          { format
          , numTracks
          , divisionForm
          }
      , file: []
      }

parseNumTracks :: Array Int -> Maybe Int
parseNumTracks bytes = do
  byte1 <- head bytes
  byte2 <- drop 1 bytes # head
  let combined = (padEight $ intToBits byte1) <> (padEight $ intToBits byte2)
  pure $ unsafeBitsToInt combined

parseMidiDivision :: Array Int -> Maybe DivisionForm
parseMidiDivision bytes = do
  byte1 <- head bytes
  byte2 <- drop 1 bytes # head
  let
    combined = (padEight $ intToBits byte1) <> (padEight $ intToBits byte2) # unsafeBitsToInt
    combinedShifted = shl combined 1
    backShift = shr combinedShifted 1
  if (combined - backShift) == 0 then
    -- this is quarter ticks
    -- 01111111 -> 11111110 -> 01111111
    Just $ QuarterTicks backShift
  else
    -- this is smtpe ticks
    Just $ QuarterTicks 1

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
