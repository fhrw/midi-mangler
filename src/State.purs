module State where

import Data.Maybe (Maybe)
import MidiTypes (MidiFile)

type State =
    { mMidiFile :: Maybe MidiFile
    }
