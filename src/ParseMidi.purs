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
  = MidiEvent MidiEvent
  | SysexEvent 
  | MetaEvent MetaEvent

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
  | Tempo Int
  | SmpteOffset SmpteOffsetR
  | TimeSigEv TimeSig
  | KeySigEv KeySig
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

type SmpteOffsetR =
  { hr :: Int
  , mn :: Int
  , sec :: Int
  , fr :: Int
  , fFr :: Int
  }

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
  case combined of
    255 -> parseMeta (drop 2 bytes)
    _ -> parseMidi (drop 1 bytes)

parseMidi :: Array Int -> Maybe (Tuple Event (Array Int))
parseMidi bytes = do
    idByte <- head bytes
    let
        sig = and 240 idByte
    case sig of
        128 -> parseNoteOff bytes
        144 -> parseNoteOn bytes
        160 -> parsePolyKeyPress bytes # Just
        192 -> parseProgChange bytes # Just
        208 -> parseChanPress bytes # Just
        224 -> parsePitchWheel bytes # Just
        176 -> parseChanMode bytes # Just
        _ -> Nothing

parseMeta :: Array Int -> Maybe (Tuple Event (Array Int))
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
      Just $ Tuple (fst t # Text # MetaEvent) (snd t)
    2 -> do
      t <- parseText next
      Just $ Tuple (fst t # Text # MetaEvent ) (snd t)
    3 -> do
      t <- parseText next
      Just $ Tuple (fst t # Text # MetaEvent ) (snd t)
    4 -> do
      t <- parseText next
      Just $ Tuple (fst t # Text # MetaEvent ) (snd t)
    5 -> do
      t <- parseText next
      Just $ Tuple (fst t # Text # MetaEvent ) (snd t)
    6 -> do
      t <- parseText next
      Just $ Tuple (fst t # Text # MetaEvent ) (snd t)
    7 -> do
      t <- parseText next
      Just $ Tuple (fst t # Text # MetaEvent ) (snd t)
    20 -> parseMidiChanPrefix next
    47 -> Just $ Tuple (MetaEvent EndOfTrack) next
    51 -> parseTempo next
    54 -> parseSmpteOffset next
    58 -> parseTimeSig next
    59 -> parseKeySig next
    127 -> parseSeqSpec next
    _ -> Nothing

parseSeqNum :: Array Int -> Maybe (Tuple Event (Array Int))
parseSeqNum bytes = do
  byte1 <- head bytes
  byte2 <- drop 1 bytes # head
  let
    val = unsafeBitsToInt
      $ (intToBits byte1 # padEight)
          <> (intToBits byte2 # padEight)
  Just $ Tuple (MetaEvent $ SeqNum val) bytes

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

parseMidiChanPrefix :: Array Int -> Maybe (Tuple Event (Array Int))
parseMidiChanPrefix bytes = do
  num <- drop 2 bytes # head
  if num >= 15 && num <= 0 then Nothing
  else Just $ Tuple (MetaEvent $ ChannelPrefix num) (drop 1 bytes)

parseTempo :: Array Int -> Maybe (Tuple Event (Array Int))
parseTempo bytes = do
  byte1 <- drop 1 bytes # head
  byte2 <- drop 2 bytes # head
  byte3 <- drop 3 bytes # head
  let
    msPerQ = unsafeBitsToInt $
      (intToBits byte1 # padEight)
        <> (intToBits byte2 # padEight)
        <> (intToBits byte3 # padEight)
  Just $ Tuple (MetaEvent $ Tempo msPerQ) (drop 3 bytes)

parseTimeSig :: Array Int -> Maybe (Tuple Event (Array Int))
parseTimeSig bytes = do
  byte1 <- drop 1 bytes # head
  byte2 <- drop 2 bytes # head
  byte3 <- drop 3 bytes # head
  byte4 <- drop 4 bytes # head
  let
    sig = { num: byte1, denom: byte2, cpc: byte3, bb: byte4 }
  Just $ Tuple (MetaEvent $ TimeSigEv sig) (drop 4 bytes)

parseSmpteOffset :: Array Int -> Maybe (Tuple Event (Array Int))
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
  Just $ Tuple (MetaEvent $ SmpteOffset offset) (drop 5 bytes)

parseKeySig :: Array Int -> Maybe (Tuple Event (Array Int))
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
      ( MetaEvent $ KeySigEv
          { accidentalType: snd x
          , numAcc: fst x
          , keyType: y
          }
      )
      (drop 2 bytes)

parseSeqSpec :: Array Int -> Maybe (Tuple Event (Array Int))
parseSeqSpec bytes = do
  lengthTup <- parseLenBytes 0 bytes
  let
    b = snd lengthTup
  Just $ Tuple (MetaEvent SeqSpec) b

parseNoteOff :: Array Int -> Maybe (Tuple Event (Array Int))
parseNoteOff bytes = do
  byte1 <- head bytes
  byte2 <- drop 1 bytes # head
  byte3 <- drop 2 bytes # head
  let chan = and 15 byte1
  Just $ Tuple
    ( MidiEvent $ NoteOff
        { key: byte2
        , vel: byte3
        , chan: chan
        }
    )
    (drop 3 bytes)

parseNoteOn :: Array Int -> Maybe (Tuple Event (Array Int))
parseNoteOn bytes = do
  byte1 <- head bytes
  byte2 <- drop 1 bytes # head
  byte3 <- drop 2 bytes # head
  let chan = and 15 byte1
  Just $ Tuple
    ( MidiEvent $ NoteOn
        { key: byte2
        , vel: byte3
        , chan: chan
        }
    )
    (drop 3 bytes)

-- This is stuff we aren't handling.
-- We just skip those bytes and return
-- a new array after!
parsePolyKeyPress :: Array Int -> Tuple Event (Array Int)
parsePolyKeyPress bytes =
  Tuple (MidiEvent PolyKeyPress) (drop 3 bytes)

parseControlChange :: Array Int -> Tuple Event (Array Int)
parseControlChange bytes =
  Tuple (MidiEvent CC) (drop 3 bytes)

parseProgChange :: Array Int -> Tuple Event (Array Int)
parseProgChange bytes = Tuple (MidiEvent ProgChange) (drop 2 bytes)

parseChanPress :: Array Int -> Tuple Event (Array Int)
parseChanPress bytes = Tuple (MidiEvent AfterTouch) (drop 2 bytes)

parsePitchWheel :: Array Int -> Tuple Event (Array Int)
parsePitchWheel bytes = Tuple (MidiEvent PitchWheel) (drop 3 bytes)

parseChanMode :: Array Int -> Tuple Event (Array Int)
parseChanMode bytes = Tuple (MidiEvent ChanMode) (drop 3 bytes)
