module ParseMidi where

import Prelude

import Bits (combine2)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Control.MonadPlus (guard)
import Data.Array (cons, drop, head, mapMaybe, take, (!!))
import Data.Array as A
import Data.Char (fromCharCode)
import Data.Either (Either(..), note)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Int.Bits (and, or, shl)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))
import Debug (traceM)

type MidiFile = { header :: FileHeader }

type FileHeader = { format :: Int, nTracks :: Int, division :: Int }
type Track = { events :: Array Event }

data Event
    = MidiEvent MidiEvent DeltaTime
    | SysexEvent
    | MetaEvent MetaEvent DeltaTime

data MetaEvent
    = SeqNum Int
    | Text String
    | Copyright String
    | TrackName String
    | InstName String
    | Lyric String
    | Marker String
    | CuePoint String
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

type DeltaTime = Int

data Key = Major | Minor

data Accidental = Flats | Sharps

type Parser a = Array Int -> Either String (Tuple a (Array Int))
type MParser m = Array Int -> Maybe (Tuple m (Array Int))

-- parseFile :: Parser MidiFile
-- parseFile ints = do
--    header <- parseFileHeader ints

parseTrack :: Parser Track
parseTrack ints = do
    mByte <- ints !! 0 # note "couldn't read track"
    mByte2 <- ints !! 1 # note "couldn't read track"
    mByte3 <- ints !! 2 # note "couldn't read track"
    mByte4 <- ints !! 3 # note "couldn't read track"
    if mByte == 77 && mByte2 == 84 && mByte3 == 114 && mByte4 == 107 then
        pure unit
    else Left "invalid track header text"
    let rest = drop 8 ints
    Tuple events rem <- many parseTrackEvent rest
    pure $ Tuple { events } rem

many :: forall a. Parser a -> Parser (Array a)
many p = \ints -> tailRec go { acc: [], rem: ints }
    where
    go r =
        case p r.rem of
            Right (Tuple result rest) ->
                Loop { acc: cons result r.acc, rem: rest }
            Left _ -> Done $ Right $ Tuple r.acc r.rem

parseTrackEvent :: Parser Event
parseTrackEvent ints = do
    Tuple deltaT rest <- parseVarLenNum ints 
    { head, tail } <- A.uncons rest # note "sldkjfs"
    let deltaTail = cons deltaT tail
    case head of
        255 -> parseMeta deltaTail
        240 -> parseSysEx tail
        oth ->
            let
                masked = and 240 oth
            in
                case masked of
                    128 -> parseNoteOff tail
                    144 -> parseNoteOn tail
                    160 -> parseAfterTouch tail
                    176 -> parseCC tail
                    224 -> parsePitchwheel tail
                    _ -> Left "parseTrackEvent fail"

parseMeta :: Parser Event
parseMeta ints = do
    delta <- head ints # note "parseMeta error"
    { head, tail } <- drop 1 ints
        # A.uncons
        # note "parseMeta error"
    let deltaTail = cons delta tail
    case head of
        0x00 -> parseSeqNum deltaTail # note "failed to parse sequence number"
        0x01 -> do 
            Tuple text rest <- parseTextEvent tail
            pure $ Tuple (MetaEvent (Text text) delta) rest
        0x02 -> do
            Tuple text rest <- parseTextEvent tail
            pure $ Tuple (MetaEvent (Copyright text) delta) rest
        0x03 -> do
            Tuple text rest <- parseTextEvent tail
            pure $ Tuple (MetaEvent (TrackName text) delta) rest
        0x04 -> do
            Tuple text rest <- parseTextEvent tail
            pure $ Tuple (MetaEvent (InstName text) delta) rest
        0x05 -> do
            Tuple text rest <- parseTextEvent tail
            pure $ Tuple (MetaEvent (Lyric text) delta) rest
        0x06 -> do
            Tuple text rest <- parseTextEvent tail
            pure $ Tuple (MetaEvent (Marker text) delta) rest
        0x07 -> do
            Tuple text rest <- parseTextEvent tail
            pure $ Tuple (MetaEvent (CuePoint text) delta) rest
        0x20 -> parseChanPrefix deltaTail
        _ -> Left "dsflkj"

parseAfterTouch :: Parser Event
parseAfterTouch ints = Left "TODO"

parseCC :: Parser Event
parseCC ints = Left "TODO"

parsePitchwheel :: Parser Event
parsePitchwheel ints = Left "TODO"

parseNoteOn :: Parser Event
parseNoteOn ints = Left "TODO"

parseNoteOff :: Parser Event
parseNoteOff ints = Left "TODO"

parseSysEx :: Parser Event
parseSysEx ints = Left "TODO"

parseChanPrefix :: Parser Event
parseChanPrefix ints = Left "TODO"

parseCuePoint :: Parser Event
parseCuePoint ints = do
    { head: delta, tail: rest } <- A.uncons ints # note "parseCuePoint error"
    Tuple len rest1 <- parseVarLenNum rest
    let
        text = take len rest
            # mapMaybe fromCharCode
            # fromCharArray
    pure $ Tuple (MetaEvent (CuePoint text) delta) rest1

parseMarker :: Parser Event
parseMarker ints = do 
    { head: delta, tail: rest } <- A.uncons ints # note "parseMarker error"
    Tuple len rest1 <- parseVarLenNum rest
    let
        text = take len rest
            # mapMaybe fromCharCode
            # fromCharArray
    pure $ Tuple (MetaEvent (Marker text) delta) rest1

parseLyric :: Parser Event
parseLyric ints = do 
    { head: delta, tail: rest } <- A.uncons ints # note "parseLyric error"
    Tuple len rest1 <- parseVarLenNum rest
    let
        text = take len rest
            # mapMaybe fromCharCode
            # fromCharArray
    pure $ Tuple (MetaEvent (Lyric text) delta) rest1

parseInstName :: Parser Event
parseInstName ints = do
    { head: delta, tail: rest } <- A.uncons ints # note "parseInstName error"
    Tuple len rest1 <- parseVarLenNum rest
    let
        text = take len rest
            # mapMaybe fromCharCode
            # fromCharArray
    pure $ Tuple (MetaEvent (InstName text) delta) rest1

parseTrackName :: Parser Event
parseTrackName ints = do
    { head: delta, tail: rest } <- A.uncons ints # note "parseTrackName error"
    Tuple len rest1 <- parseVarLenNum rest
    let
        text = take len rest
            # mapMaybe fromCharCode
            # fromCharArray
    pure $ Tuple (MetaEvent (Copyright text) delta) rest1

parseCopyright :: Parser Event
parseCopyright ints = do
    { head: delta, tail: rest } <- A.uncons ints # note "parseCopyright error"
    Tuple len rest1 <- parseVarLenNum rest
    let
        text = take len rest
            # mapMaybe fromCharCode
            # fromCharArray
    pure $ Tuple (MetaEvent (Copyright text) delta) rest1

parseTextEvent :: Parser String
parseTextEvent ints = do
    Tuple len rest <- parseVarLenNum ints
    let
        text = take len rest
            # mapMaybe fromCharCode
            # fromCharArray
    pure $ Tuple (text) (drop len rest)

parseSeqNum :: MParser Event
parseSeqNum ints = do
    { head: delta, tail: rest } <- A.uncons ints
    { head, tail } <- A.uncons rest
    guard $ head == 0x02
    b1 <- tail !! 0
    b2 <- tail !! 1
    pure $ Tuple (MetaEvent (SeqNum $ combine2 b1 b2) delta) (drop 2 tail)

parseFileHeader :: MParser FileHeader
parseFileHeader ints = do
    mByte <- ints !! 0
    mByte2 <- ints !! 1
    mByte3 <- ints !! 2
    mByte4 <- ints !! 3
    guard $ mByte == 77 && mByte2 == 84 && mByte3 == 104 && mByte4 == 100
    lByte <- ints !! 4
    lByte2 <- ints !! 5
    lByte3 <- ints !! 6
    lByte4 <- ints !! 7
    guard $ lByte == 0 && lByte2 == 0 && lByte3 == 0 && lByte4 == 6
    ftByte1 <- ints !! 8
    ftByte2 <- ints !! 9
    nTracks1 <- ints !! 10
    nTracks2 <- ints !! 11
    div1 <- ints !! 12
    guard $ (and 128 div1) < 128
    div2 <- ints !! 13
    let
        format = combine2 ftByte1 ftByte2
        nTracks = combine2 nTracks1 nTracks2
        division = combine2 div1 div2
    pure $ Tuple { format, nTracks, division } (drop 14 ints)

parseVarLenNum :: Array Int -> Either String (Tuple Int (Array Int))
parseVarLenNum ints = do
    let
        f rem z depth = do
            { head, tail } <- A.uncons rem
                # note "parseVarLenNum: failed to extract head and tail"
            let
                mask = and 128 head
                val = and 127 head
                next = or (shl z 7) val
            case mask of
                128 -> do
                    if depth > 3 then Left "variable length exceeded allowed length"
                    else f tail next (depth + 1)
                _ -> Right $ Tuple next tail
    f ints 0 0

---------------
-- INSTANCES --
---------------

derive instance Generic Event _
instance Eq Event where
    eq = genericEq

instance Show Event where
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

