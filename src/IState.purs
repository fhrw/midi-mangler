module IState where

import Prelude

import Data.Map (Map)
import ParseMidi (Event, Note)

type IState =
    { timeM :: Map BarNum TimeSig
    , musicTracks :: Array MusicTrack
    }

type BarNum = Int

type TimeSig =
    { num :: Int
    , denom :: Int
    }

type MusicTrack =
    { noteEvents :: Array Note
    , otherEvents :: Array Event
    }

type PassageR = {bars :: Array BarNum, tNum :: Int}
