module DisplayState where

import Prelude

import Data.Array ((!!))
import Data.Array as A
import Data.Map as M
import Data.Maybe (Maybe)
import ParseMidi (IState, Note, BarNum)

getBarNotes :: IState -> { bar :: BarNum, track :: Int } -> Maybe (Array Note)
getBarNotes state r = do
    track <- state.musicTracks !! r.track
    span <- M.lookup r.bar state.timeM
    pure $ A.filter
        ( \x ->
              if span.start <= x.on && span.end >= x.off then true
              else false
        )
        track.noteEvents
