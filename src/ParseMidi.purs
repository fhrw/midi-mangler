module ParseMidi where

import Prelude

import Bits (combine2, combine4)
import Control.MonadPlus (guard)
import Data.Array (drop, head, (!!))
import Data.Array as A
import Data.Either (Either(..), note)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Int.Bits (and, or, shl)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Debug (traceM)

data HeaderEvent
    = File { format :: Int, nTracks :: Int, division :: Int }
    | Track Int

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

type Parser a = Array Int -> Either String (Tuple a (Array Int))
type MParser m = Array Int -> Maybe (Tuple m (Array Int))

parseHeader :: Parser Event
parseHeader ints = note "Failed to reader Header Chunk" do
    byte1 <- ints !! 0
    byte2 <- ints !! 1
    let next = drop 2 ints
    case byte1, byte2 of
        104, 100 -> parseFileHeader next
        114, 107 -> parseTrackHeader next
        _, _ -> Nothing

parseFileHeader :: MParser Event
parseFileHeader ints = do
    ftByte1 <- ints !! 4
    ftByte2 <- ints !! 5
    nTracks1 <- ints !! 6
    nTracks2 <- ints !! 7
    div1 <- ints !! 8
    guard $ (and 128 div1) < 128
    div2 <- ints !! 9
    let
        format = combine2 ftByte1 ftByte2
        nTracks = combine2 nTracks1 nTracks2
        division = combine2 div1 div2
    pure $ Tuple (HeaderEvent $ File { format, nTracks, division }) (drop 10 ints)

parseTrackHeader :: MParser Event
parseTrackHeader ints = do
    l1 <- ints !! 0
    l2 <- ints !! 1
    l3 <- ints !! 2
    l4 <- ints !! 3
    pure $ Tuple (HeaderEvent $ Track (combine4 l1 l2 l3 l4)) (drop 4 ints)

parseVarLenNum :: Array Int -> Either String (Tuple Int (Array Int))
parseVarLenNum ints = do
    let 
        f rem z depth = do 
            {head, tail} <- A.uncons rem 
                # note "parseVarLenNum: failed to extract head and tail"
            let
                mask = and 128 head 
                val = and 127 head
                next = or (shl z 7) val
            case mask of
                128 -> do
                    if depth > 3 then Left "variable length exceeded allowed length" 
                        else f tail next (depth+1)
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

