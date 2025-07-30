module MidiTypes where

import Prelude

import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

type MidiFile = { header :: FileHeader, tracks :: Array Track }

type FileHeader = { format :: Int, nTracks :: Int, division :: Int }

type Track = { events :: Array Event }

data Event
    = MidiEvent MidiEvent TimeVal
    | MetaEvent MetaEvent TimeVal

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
    | UnknownMeta

data MidiEvent
    = NoteOff NoteInfo
    | NoteOn NoteInfo
    | PolyKeyPress
    | CC CChange
    | ProgChange ProgChange
    | AfterTouch AfterTouchVal
    | PitchWheel PitchWheelChange
    | ChanMode

type TimeVal = Int

data Key = Major | Minor

data Accidental = Flats | Sharps

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

type ProgChange =
    { chan :: Int
    , progNum :: Int
    }

type PitchWheelChange =
    { chan :: Int
    , pos :: Int
    }

type AfterTouchVal =
    { chan :: Int
    , val :: Int
    }

type TimeSig =
    { nn :: Int
    , dd :: Int
    , cc :: Int
    , bb :: Int
    }

type KeySig =
    { sf :: Int
    , mi :: Int
    }

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
