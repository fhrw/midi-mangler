module Main where

import Prelude

import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.VDom.Driver (runUI)
import Node.Buffer (toArray)
import Node.FS.Sync (readFile)
import ParseMidi (parseFile)

type State = Maybe String

data Action = GodSpeed

main :: Effect Unit
main = runHalogenAff do
  body <- awaitBody
  runUI component unit body

component :: forall query input output m. MonadEffect m => H.Component query input output m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }

initialState :: forall input. input -> State
initialState _ = Nothing

render :: forall m. State -> H.ComponentHTML Action () m
render state = do
        let val = maybe "file not opened yet" show state
        HH.div_
                [ HH.h1_
                        [HH.text "MIDI Mangler"]        
                , HH.p_
                        [ HH.text ("file date: " <> val)]
                , HH.button
                        [HE.onClick \_ -> GodSpeed ]
                        [ HH.text "Open file"]
                ]

handleAction :: forall output m. MonadEffect m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
        GodSpeed -> do
                file <- H.liftEffect openMidi
                H.modify_ \_ -> Just file

openMidi :: Effect String
openMidi = do
        buf <- readFile "./1m1.mid"
        arr <- toArray buf
        pure $ show $ parseFile arr

