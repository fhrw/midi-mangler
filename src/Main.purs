module Main where

import Prelude

import Control.Monad.Except (runExceptT)
import Control.Monad.State (evalStateT)
import DOM.HTML.Indexed.InputAcceptType (InputAcceptType(..), InputAcceptTypeAtom(..))
import Data.Array as A
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Halogen as H
import Halogen.Aff (awaitBody, runHalogenAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import MidiTypes (MidiFile)
import StateParser (parseFile)
import Web.Event.Event (Event)

type State =
    { mMidiFile :: Maybe MidiFile
    }

data Action = GodSpeed Event

main :: Effect Unit
main = runHalogenAff do
    body <- awaitBody
    runUI component unit body

component :: forall query input output m. MonadAff m => H.Component query input output m
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }

initialState :: forall input. input -> State
initialState _ = { mMidiFile: Nothing }

----------
-- VIEW --
----------

render :: forall m. State -> H.ComponentHTML Action () m
render state = do
    HH.div_
        [ HH.h1_
              [ HH.text "QuickClean foo" ]
        , foo state
        , HH.form_
              [ HH.input
                    [ HP.type_ HP.InputFile
                    , HP.accept $ InputAcceptType [ AcceptFileExtension ".mid" ]
                    , HE.onChange \event -> GodSpeed event
                    ]
              ]
        ]

foo :: forall m. State -> H.ComponentHTML Action () m
foo st = do
    case st.mMidiFile of
        Nothing -> HH.p_ [ HH.text "" ]
        Just file -> HH.div_
            [ HH.p_ [ HH.text $ show $ A.length file.tracks ]
            , HH.p_ [ HH.text $ show file ]
            ]

----------
-- EVAL --
----------

handleAction :: forall output m. MonadAff m => Action -> H.HalogenM State Action () output m Unit
handleAction = case _ of
    GodSpeed event -> do
        mFile <- liftAff
            $ fromEffectFnAff
            $ readFileFromFilePickEvent
                  { just: Just
                  , nothing: Nothing
                  , event
                  }
        let parsed = unwrap $ runExceptT $ evalStateT parseFile { file: (fromMaybe [] mFile), pos: 0 }
        case parsed of
            Left _ -> pure unit
            Right midi -> do
                H.modify_ \st -> st { mMidiFile = Just (midi) }

foreign import readFileFromFilePickEvent
    :: { just :: forall a. a -> Maybe a
       , nothing :: forall a. Maybe a
       , event :: Event
       }
    -> EffectFnAff (Maybe (Array Int))
