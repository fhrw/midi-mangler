module Main where

import Prelude

import Bits (intToBits, padEight, unsafeBitsToInt)
import Data.Array (drop, head, slice)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Int.Bits (shl, shr)
import Data.List (List, fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Node.Buffer (toArray)
import Node.FS.Sync (readFile)
import ParseMidi (Event, parseFile)

-----------
-- TYPES --
-----------

type File = List Chunk

data Chunk
  = Header
  | Track

newtype MidiFile = MidiFile
  { header :: MidiHeader
  , metaTrack :: Track
  , midiTracks :: List Track
  }

type MidiHeader =
  { format :: MidiFormat
  , len :: Int
  , numTracks :: Int
  , divisionForm :: DivisionForm
  }

data MidiFormat = Type0 | Type1 | Type2

data DivisionForm
  = QuarterTicks Int
  | SmtpeTicks Int Int

type Track =
  { name :: String
  , eventList :: List TrackEvent
  }

type TrackEvent =
  { deltaTime :: Int
  , event :: MidiEvent
  }

data MidiEvent
  = MetaEvent MetaEventType
  | ChannelEvent ChannelEventType

data ChannelEventType
  = NoteOff Int Int
  | NoteOn Int Int
  | IgnoredChan

data MetaEventType
  = TimeSig
      { num :: Int
      , den :: Int
      , clocksPerClick :: Int
      , bbParam :: Int -- don't really understand this
      }
  | Tempo Int
  | SmtpeOffset
      { hr :: Int
      , min :: Int
      , sec :: Int
      , frm :: Int
      , fFrm :: Int
      }
  | SeqNum Int
  | InstrumentName String
  | CuePoint String
  | ChannelPrefix Int
  | TrackEnd
  | IgnoredMeta

-----------
-- FUNCS --
-----------

main :: Effect Unit
main = do
  file <- fooOpen
  log $ show file

fooOpen :: Effect (Either String (Array Event))
fooOpen = do
    buf <- readFile "./1m1.mid"
    arr <- toArray buf
    pure $ parseFile arr

openMidiFile :: Effect (Maybe MidiFile)
openMidiFile = do
  buf <- readFile "./1m1.mid"
  arr <- toArray buf
  pure do
    header <- parseHeaderChunk $ slice 0 14 arr
    pure $ MidiFile
      { header: header
      , metaTrack:
          { name: "metatrack"
          , eventList: fromFoldable []
          }
      , midiTracks: fromFoldable []
      }

idChunk :: Array Int -> Maybe Chunk
idChunk typeBytes = do
  byte1 <- head typeBytes
  byte2 <- drop 1 typeBytes # head
  byte3 <- drop 2 typeBytes # head
  byte4 <- drop 3 typeBytes # head
  case byte1, byte2, byte3, byte4 of
    77, 84, 104, 100 -> Just Header
    77, 84, 114, 107 -> Just Track
    _, _, _, _ -> Nothing

parseHeaderChunk :: Array Int -> Maybe MidiHeader
parseHeaderChunk bytes = do
  len <- parseLenByteSpec 6 $ slice 4 8 bytes
  format <- parseMidiFormat $ slice 8 10 bytes
  numTracks <- parseNumTracks $ slice 10 12 bytes
  divisionForm <- parseMidiDivision $ slice 12 14 bytes
  pure
    { format
    , len
    , numTracks
    , divisionForm
    }

parseLenByteSpec :: Int -> Array Int -> Maybe Int
parseLenByteSpec spec bytes = do
  byte1 <- head bytes
  byte2 <- drop 1 bytes # head
  byte3 <- drop 2 bytes # head
  byte4 <- drop 3 bytes # head
  let
    combined = (padEight $ intToBits byte1)
      <> (padEight $ intToBits byte2)
      <> (padEight $ intToBits byte3)
      <> (padEight $ intToBits byte4)
    num = unsafeBitsToInt combined
  if num == spec then (Just num) else Nothing

parseLenByte :: Array Int -> Maybe Int
parseLenByte bytes = do
  byte1 <- head bytes
  byte2 <- drop 1 bytes # head
  byte3 <- drop 2 bytes # head
  byte4 <- drop 3 bytes # head
  let
    combined = (padEight $ intToBits byte1)
      <> (padEight $ intToBits byte2)
      <> (padEight $ intToBits byte3)
      <> (padEight $ intToBits byte4)
  pure $ unsafeBitsToInt combined

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
    shifted = shr (shl combined 1) 1
  if (combined - shifted) == 0 then
    -- this is quarter ticks
    -- 01111111 -> 11111110 -> 01111111
    Just $ QuarterTicks shifted
  else
    -- this is smtpe ticks
    -- incomplete behaviour
    Just $ SmtpeTicks (negate 1) 1

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

-- INSTANCES

derive newtype instance Show MidiFile

derive instance Generic MidiEvent _
instance Show MidiEvent where
  show = genericShow

derive instance Generic MetaEventType _
instance Show MetaEventType where
  show = genericShow

derive instance Generic ChannelEventType _
instance Show ChannelEventType where
  show = genericShow

derive instance Generic MidiFormat _

instance Show MidiFormat where
  show = genericShow

derive instance Generic DivisionForm _
instance Show DivisionForm where
  show = genericShow

