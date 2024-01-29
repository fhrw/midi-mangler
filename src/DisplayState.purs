module DisplayState where

import Prelude

import Data.Array ((!!))
import Data.Maybe (Maybe(..))
import ParseMidi (IState, Note)

f :: IState -> {bar :: Int, track :: Int} -> Maybe (Array Note)
f state r = do 
        track <- state.musicTracks !! r.track
        Nothing
