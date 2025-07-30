module Components.Display where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import MidiTypes (MidiFile)

renderGrid :: forall m. MidiFile -> H.ComponentHTML Unit () m
renderGrid _ = do
  HH.canvas []
