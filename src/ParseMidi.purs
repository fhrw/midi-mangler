module ParseMidi where

import Prelude

import Bits (combine2, intToBits, padEight, unsafeBitsToInt)
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
  = SeqNum Int
  | Text String
  | Copyright
  | TrackName
  | InstName
  | Lyric
  | Marker
  | CuePoint
  | ChannelPrefix Int
  | EndOfTrack
  | Tempo
  | SmtpeOffset
  | TimeSigEv
  | KeySigEv
  | SeqSpec

data MidiEvent
  = NoteOff NoteInfo
  | NoteOn NoteInfo
  | PolyKeyPress
  | CC
  | ProgChange
  | AfterTouch
  | PitchWheel
  | ChanMode

type NoteInfo =
  { key :: Int
  , vel :: Int
  , chan :: Int
  }

type CChange =
  { ctrl :: Int
  , val :: Int
  , chan :: Int
  }

type TimeSig =
  { num :: Int
  , denom :: Int
  , cpc :: Int
  , bb :: Int
  }

type KeySig =
  { accidentalType :: Accidental
  , numAcc :: Int
  , keyType :: Key
  }

data Key = Major | Minor

data Accidental = Flats | Sharps

type SmpteOffset =
  { hr :: Int
  , mn :: Int
  , sec :: Int
  , fr :: Int
  , fFr :: Int
  }

parseEvent :: Array Int -> Maybe (Tuple Event (Array Int))
parseEvent bytes = do
  byte1 <- head bytes
  byte2 <- drop 1 bytes # head
  let
    combined =
      ( (intToBits byte1 # padEight)
          <>
            ( intToBits byte2
                # padEight
            )
      )
        # unsafeBitsToInt
    next = drop 2 bytes
  case combined of
    255 -> parseMeta next
    _ -> Nothing

parseMeta :: Array Int -> Maybe (Tuple MetaEvent (Array Int))
parseMeta bytes = do
  byte1 <- head bytes
  byte2 <- drop 1 bytes # head
  let
    combined = combine2 byte1 byte2
    next = drop 2 bytes
  case combined of
    0 -> parseSeqNum next
    1 -> do
      t <- parseText next
      Just $ Tuple (fst t # Text) (snd t)
    2 -> do
      t <- parseText next
      Just $ Tuple (fst t # Text) (snd t)
    3 -> do
      t <- parseText next
      Just $ Tuple (fst t # Text) (snd t)
    4 -> do
      t <- parseText next
      Just $ Tuple (fst t # Text) (snd t)
    5 -> do
      t <- parseText next
      Just $ Tuple (fst t # Text) (snd t)
    6 -> do
      t <- parseText next
      Just $ Tuple (fst t # Text) (snd t)
    7 -> do
      t <- parseText next
      Just $ Tuple (fst t # Text) (snd t)
    20 -> parseMidiChanPrefix next
    47 -> Nothing

parseSeqNum :: Array Int -> Maybe (Tuple MetaEvent (Array Int))
parseSeqNum bytes = do
  byte1 <- head bytes
  byte2 <- drop 1 bytes # head
  let
    val = unsafeBitsToInt
      $ (intToBits byte1 # padEight)
          <> (intToBits byte2 # padEight)
  Just $ Tuple (SeqNum val) bytes

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

parseMidiChanPrefix :: Array Int -> Maybe (Tuple MetaEvent (Array Int))
parseMidiChanPrefix bytes = do
  num <- drop 2 bytes # head
  if num >= 15 && num <= 0 then Nothing
  else Just $ Tuple (ChannelPrefix num) (drop 1 bytes)

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

parseSmpteOffset :: Array Int -> Maybe (Tuple SmpteOffset (Array Int))
parseSmpteOffset bytes = do
  byte1 <- drop 1 bytes # head
  byte2 <- drop 2 bytes # head
  byte3 <- drop 3 bytes # head
  byte4 <- drop 4 bytes # head
  byte5 <- drop 5 bytes # head
  let
    offset =
      { hr: byte1
      , mn: byte2
      , sec: byte3
      , fr: byte4
      , fFr: byte5
      }
  Just $ Tuple offset (drop 5 bytes)

parseKeySig :: Array Int -> Maybe (Tuple KeySig (Array Int))
parseKeySig bytes = do
  byte1 <- drop 1 bytes # head
  byte2 <- drop 2 bytes # head
  let
    k = case byte1 of
      -7 -> Just $ Tuple 7 Flats
      (-6) -> Just $ Tuple 6 Flats
      (-5) -> Just $ Tuple 5 Flats
      (-4) -> Just $ Tuple 4 Flats
      (-3) -> Just $ Tuple 3 Flats
      (-2) -> Just $ Tuple 2 Flats
      (-1) -> Just $ Tuple 1 Flats
      0 -> Just $ Tuple 0 Sharps
      1 -> Just $ Tuple 1 Sharps
      2 -> Just $ Tuple 2 Sharps
      3 -> Just $ Tuple 3 Sharps
      4 -> Just $ Tuple 4 Sharps
      5 -> Just $ Tuple 5 Sharps
      6 -> Just $ Tuple 6 Sharps
      7 -> Just $ Tuple 7 Sharps
      _ -> Nothing
    t = case byte2 of
      0 -> Just Major
      1 -> Just Minor
      _ -> Nothing
  case k, t of
    Nothing, _ -> Nothing
    _, Nothing -> Nothing
    Just x, Just y -> Just $ Tuple
      { accidentalType: snd x
      , numAcc: fst x
      , keyType: y
      }
      (drop 2 bytes)

parseSeqSpec :: Array Int -> Maybe (Array Int)
parseSeqSpec bytes = do
  lengthTup <- parseLenBytes 0 bytes
  let
    b = snd lengthTup
  Just b

parseNoteOff :: Array Int -> Maybe (Tuple MidiEvent (Array Int))
parseNoteOff bytes = do
  byte1 <- head bytes
  byte2 <- drop 1 bytes # head
  byte3 <- drop 2 bytes # head
  let chan = and 15 byte1
  Just $ Tuple
    ( NoteOff
        { key: byte2
        , vel: byte3
        , chan: chan
        }
    )
    (drop 3 bytes)

parseNoteOn :: Array Int -> Maybe (Tuple MidiEvent (Array Int))
parseNoteOn bytes = do
  byte1 <- head bytes
  byte2 <- drop 1 bytes # head
  byte3 <- drop 2 bytes # head
  let chan = and 15 byte1
  Just $ Tuple
    ( NoteOn
        { key: byte2
        , vel: byte3
        , chan: chan
        }
    )
    (drop 3 bytes)

-- This is stuff we aren't handling.
-- We just skip those bytes and return
-- a new array after!
parsePolyKeyPress :: Array Int -> Tuple MidiEvent (Array Int)
parsePolyKeyPress bytes =
  Tuple PolyKeyPress (drop 3 bytes)

parseControlChange :: Array Int -> Tuple MidiEvent (Array Int)
parseControlChange bytes =
  Tuple CC (drop 3 bytes)

parseProgChange :: Array Int -> Tuple MidiEvent (Array Int)
parseProgChange bytes = Tuple ProgChange (drop 2 bytes)

parseChanPress :: Array Int -> Tuple MidiEvent (Array Int)
parseChanPress bytes = Tuple AfterTouch (drop 2 bytes)

parsePitchWheel :: Array Int -> Tuple MidiEvent (Array Int)
parsePitchWheel bytes = Tuple PitchWheel (drop 3 bytes)

parseChanMode :: Array Int -> Tuple MidiEvent (Array Int)
parseChanMode bytes = Tuple ChanMode (drop 3 bytes)
