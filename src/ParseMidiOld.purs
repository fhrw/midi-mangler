module ParseMidiOld where

import Prelude

import Bits (combine2, intToBits, padEight, unsafeBitsToInt)
import Data.Array (drop, head, length, mapMaybe, snoc, take)
import Data.Char (fromCharCode)
import Data.Either (Either(..), note)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Int.Bits (and)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..), fst, snd)

data HeaderEvent
    = File
    | Track

data Event
    = MidiEvent MidiEvent
    | SysexEvent
    | MetaEvent MetaEvent
    | HeaderEvent HeaderEvent

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

parseFile :: Array Int -> Either String (Array Event)
parseFile bytes = go { accum: [], remainingBytes: bytes }
    where
    go { accum: acc, remainingBytes: [] } = Right acc
    go { accum: acc, remainingBytes: rem } = do
        nextTup <- parseEvent rem
        go { accum: snoc acc (fst nextTup), remainingBytes: snd nextTup }

parseEvent :: Array Int -> Either String (Tuple Event (Array Int))
parseEvent bytes = do
    byte1 <- head bytes # note "event byte fail"
    byte2 <- drop 1 bytes # head # note "event byte fail"
    let
        combined = combine2 byte1 byte2
    case combined of
        255 -> parseMeta (drop 2 bytes)
        19796 -> parseHeader bytes
        _ -> parseMidi bytes

parseMidi :: Array Int -> Either String (Tuple Event (Array Int))
parseMidi bytes = do
    idByte <- head bytes # note "failed reading parseMidi IdByte"
    let
        sig = and 240 idByte
    case sig of
        128 -> parseNoteOff bytes
        144 -> parseNoteOn bytes
        160 -> parsePolyKeyPress bytes # Right
        192 -> parseProgChange bytes # Right
        208 -> parseChanPress bytes # Right
        224 -> parsePitchWheel bytes # Right
        176 -> parseControlChange bytes # Right
        _ -> Left $ "unknown midi event message" <> show idByte

parseHeader :: Array Int -> Either String (Tuple Event (Array Int))
parseHeader bytes = note "failed to parse header" do
    byte1 <- head bytes
    byte2 <- drop 1 bytes # head
    byte3 <- drop 2 bytes # head
    byte4 <- drop 3 bytes # head
    mM <- fromCharCode byte1
    mT <- fromCharCode byte2
    m3 <- fromCharCode byte3
    m4 <- fromCharCode byte4
    let
        str = [ mM, mT, m3, m4 ] # fromCharArray
    case str of
        "MThd" -> Just $ Tuple (HeaderEvent $ File) (drop 14 bytes)
        "MTrk" -> Just $ Tuple (HeaderEvent $ Track) (drop 8 bytes)
        _ -> Nothing

parseMeta :: Array Int -> Either String (Tuple Event (Array Int))
parseMeta bytes = do
    byte1 <- head bytes # note "failed reading byte1"
    let
        next = drop 1 bytes
    case byte1 of
        0 -> parseSeqNum next
        1 -> do
            t <- parseText next
            Right $ Tuple (fst t # Text # MetaEvent) (snd t)
        2 -> do
            t <- parseText next
            Right $ Tuple (fst t # Text # MetaEvent) (snd t)
        3 -> do
            t <- parseText next
            Right $ Tuple (fst t # Text # MetaEvent) (snd t)
        4 -> do
            t <- parseText next
            Right $ Tuple (fst t # Text # MetaEvent) (snd t)
        5 -> do
            t <- parseText next
            Right $ Tuple (fst t # Text # MetaEvent) (snd t)
        6 -> do
            t <- parseText next
            Right $ Tuple (fst t # Text # MetaEvent) (snd t)
        7 -> do
            t <- parseText next
            Right $ Tuple (fst t # Text # MetaEvent) (snd t)
        32 -> parseMidiChanPrefix next
        71 -> Right $ Tuple (MetaEvent EndOfTrack) (drop 2 next)
        81 -> parseTempo next
        84 -> parseSmpteOffset next
        88 -> parseTimeSig next
        89 -> parseKeySig next
        127 -> parseSeqSpec next
        _ -> Left $ "failed to parse meta" <> show bytes

parseSeqNum :: Array Int -> Either String (Tuple Event (Array Int))
parseSeqNum bytes = note "failed at seq number" do
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
        masked = and 128 byte
    case masked of
        128 -> parseLenBytes (t + byte) (drop 1 bytes)
        _ -> Just $ Tuple (t + byte) (drop 1 bytes)

-- this can be used for all text style events
parseText :: Array Int -> Either String (Tuple String (Array Int))
parseText bytes = note "failed to parse text" do
    lenTuple <- parseLenBytes 0 bytes
    let
        lengthOf = fst lenTuple
        newBytes = snd lenTuple
        text = take lengthOf newBytes
            # mapMaybe fromCharCode
            # fromCharArray
    Just $ Tuple text (drop lengthOf newBytes)

parseMidiChanPrefix :: Array Int -> Either String (Tuple Event (Array Int))
parseMidiChanPrefix bytes = note "failed at midichan prefix" do
    num <- drop 2 bytes # head
    if num >= 15 && num <= 0 then Nothing
    else Just $ Tuple (MetaEvent $ ChannelPrefix num) (drop 1 bytes)

parseTempo :: Array Int -> Either String (Tuple Event (Array Int))
parseTempo bytes = note "failed at tempo" do
    byte1 <- drop 2 bytes # head
    byte2 <- drop 3 bytes # head
    byte3 <- drop 4 bytes # head
    byte4 <- drop 5 bytes # head
    let
        msPerQ = unsafeBitsToInt $
            (intToBits byte1 # padEight)
                <> (intToBits byte2 # padEight)
                <> (intToBits byte3 # padEight)
                <> (intToBits byte4 # padEight)
    Just $ Tuple (MetaEvent $ Tempo msPerQ) (drop 6 bytes)

parseTimeSig :: Array Int -> Either String (Tuple Event (Array Int))
parseTimeSig bytes = note "failed at time sig" do
    byte1 <- head bytes
    byte2 <- drop 1 bytes # head
    byte3 <- drop 2 bytes # head
    byte4 <- drop 3 bytes # head
    let
        sig = { num: byte1, denom: byte2, cpc: byte3, bb: byte4 }
    Just $ Tuple (MetaEvent $ TimeSigEv sig) (drop 5 bytes)

parseSmpteOffset :: Array Int -> Either String (Tuple Event (Array Int))
parseSmpteOffset bytes = note "failed smtpe offset" do
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
    Just $ Tuple (MetaEvent $ SmpteOffset offset) (drop 6 bytes)

parseKeySig :: Array Int -> Either String (Tuple Event (Array Int))
parseKeySig bytes = note "failed at keysig" do
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

parseSeqSpec :: Array Int -> Either String (Tuple Event (Array Int))
parseSeqSpec bytes = note "failed at seqSpec" do
    lengthTup <- parseLenBytes 0 bytes
    let
        b = snd lengthTup
    Just $ Tuple (MetaEvent SeqSpec) b

parseNoteOff :: Array Int -> Either String (Tuple Event (Array Int))
parseNoteOff bytes = note "failed at noteOff" do
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

parseNoteOn :: Array Int -> Either String (Tuple Event (Array Int))
parseNoteOn bytes = note "failed at noteOn message" do
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

---------------
-- INSTANCES --
---------------

derive instance Generic Event _
instance Eq Event where
    eq = genericEq

instance Show Event where
    show = genericShow

derive instance Generic HeaderEvent _
instance Eq HeaderEvent where
    eq = genericEq

instance Show HeaderEvent where
    show = genericShow

derive instance Generic MidiEvent _
instance Eq MidiEvent where
    eq = genericEq

instance Show MidiEvent where
    show = genericShow

derive instance Generic MetaEvent _
instance Eq MetaEvent where
    eq = genericEq

instance Show MetaEvent where
    show = genericShow

derive instance Generic Key _
instance Eq Key where
    eq = genericEq

instance Show Key where
    show = genericShow

derive instance Generic Accidental _
instance Eq Accidental where
    eq = genericEq

instance Show Accidental where
    show = genericShow
