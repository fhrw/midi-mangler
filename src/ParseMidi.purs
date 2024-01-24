module ParseMidi where

import Prelude

import Bits (combine2, combine3)
import Control.Monad.Rec.Class (Step(..), tailRec)
import Control.MonadPlus (guard)
import Data.Array (cons, drop, head, index, mapMaybe, snoc, take, (!!))
import Data.Array as A
import Data.Char (fromCharCode)
import Data.Either (Either(..), note)
import Data.Eq.Generic (genericEq)
import Data.Generic.Rep (class Generic)
import Data.Int (fromNumber, toNumber)
import Data.Int.Bits (and, or, shl)
import Data.Map as M
import Data.Maybe (Maybe(..))
import Data.Number (pow)
import Data.Show.Generic (genericShow)
import Data.String.CodeUnits (fromCharArray)
import Data.Tuple (Tuple(..))

----------------
-- EVALUATION --
----------------

trackName :: Track -> Maybe String
trackName track = do
    let
        f :: Event -> Maybe String
        f (MetaEvent (TrackName str) _) = pure str
        f _ = Nothing
        arr = A.filter
            ( \e ->
                  case e of
                      MetaEvent (TrackName _) _ -> true
                      _ -> false
            )
            track.events
    name <- A.index arr 0
    str <- f name
    pure str

type Note = { on :: Int, off :: Int, key :: Int, vel :: Int, chan :: Int }

notesInTrack :: Track -> Array Note
notesInTrack track =
    let
        notes = A.foldl
            ( \z e ->
                  let
                      newZ = z { curTime = z.curTime + eventDelta e }
                  in
                      case e of
                          MidiEvent (NoteOn r) _ ->
                              let
                                  key = { key: r.key, chan: r.chan }
                              in
                                  if M.member key z.q then newZ
                                  else newZ { q = M.insert key { on: newZ.curTime, vel: r.vel } newZ.q }
                          MidiEvent (NoteOff r) _ ->
                              let
                                  key = { key: r.key, chan: r.chan }
                                  on = M.lookup key newZ.q
                              in
                                  case on of
                                      Nothing -> newZ
                                      Just val -> newZ
                                          { q = M.delete key newZ.q
                                          , notes = A.snoc newZ.notes { on: val.on, off: newZ.curTime, key: key.key, chan: key.chan, vel: val.vel }
                                          }
                          _ -> z { curTime = z.curTime + (eventDelta e) }
            )
            { q: M.empty
            , curTime: 0
            , notes: []
            }
            track.events
    in
        notes.notes
    where
    eventDelta :: Event -> TimeVal
    eventDelta (MidiEvent (_) d) = d
    eventDelta (MetaEvent (_) d) = d

-- this sucks
toAbsolute :: Track -> Track
toAbsolute track =
    let
        e = A.foldl
            ( \z ele ->
                  let
                      newTime d = z.curTime + d
                  in
                      case ele of
                          MidiEvent (NoteOn r) d -> z { events = A.snoc z.events (MidiEvent (NoteOn r) (newTime d)), curTime = newTime d }
                          MidiEvent (NoteOff r) d -> z { events = A.snoc z.events (MidiEvent (NoteOff r) (newTime d)), curTime = newTime d }
                          MidiEvent PolyKeyPress d -> z { events = A.snoc z.events (MidiEvent PolyKeyPress (newTime d)), curTime = newTime d }
                          MidiEvent (CC r) d -> z { events = A.snoc z.events (MidiEvent (CC r) (newTime d)), curTime = newTime d }
                          MidiEvent ProgChange d -> z { events = A.snoc z.events (MidiEvent ProgChange (newTime d)), curTime = newTime d }
                          MidiEvent (AfterTouch r) d -> z { events = A.snoc z.events (MidiEvent (AfterTouch r) (newTime d)), curTime = newTime d }
                          MidiEvent (PitchWheel r) d -> z { events = A.snoc z.events (MidiEvent (PitchWheel r) (newTime d)), curTime = newTime d }
                          MidiEvent ChanMode d -> z { events = A.snoc z.events (MidiEvent ChanMode (newTime d)), curTime = newTime d }
                          MetaEvent (SeqNum n) d -> z { events = A.snoc z.events (MetaEvent (SeqNum n) (newTime d)), curTime = newTime d }
                          MetaEvent (Text str) d -> z { events = A.snoc z.events (MetaEvent (Text str) (newTime d)), curTime = newTime d }
                          MetaEvent (Copyright str) d -> z { events = A.snoc z.events (MetaEvent (Copyright str) (newTime d)), curTime = newTime d }
                          MetaEvent (TrackName str) d -> z { events = A.snoc z.events (MetaEvent (TrackName str) (newTime d)), curTime = newTime d }
                          MetaEvent (InstName str) d -> z { events = A.snoc z.events (MetaEvent (InstName str) (newTime d)), curTime = newTime d }
                          MetaEvent (Lyric str) d -> z { events = A.snoc z.events (MetaEvent (Lyric str) (newTime d)), curTime = newTime d }
                          MetaEvent (Marker str) d -> z { events = A.snoc z.events (MetaEvent (Marker str) (newTime d)), curTime = newTime d }
                          MetaEvent (CuePoint str) d -> z { events = A.snoc z.events (MetaEvent (CuePoint str) (newTime d)), curTime = newTime d }
                          MetaEvent (ChannelPrefix n) d -> z { events = A.snoc z.events (MetaEvent (ChannelPrefix n) (newTime d)), curTime = newTime d }
                          MetaEvent EndOfTrack d -> z { events = A.snoc z.events (MetaEvent EndOfTrack (newTime d)), curTime = newTime d }
                          MetaEvent (Tempo n) d -> z { events = A.snoc z.events (MetaEvent (Tempo n) (newTime d)), curTime = newTime d }
                          MetaEvent (SmpteOffset r) d -> z { events = A.snoc z.events (MetaEvent (SmpteOffset r) (newTime d)), curTime = newTime d }
                          MetaEvent (TimeSigEv r) d -> z { events = A.snoc z.events (MetaEvent (TimeSigEv r) (newTime d)), curTime = newTime d }
                          MetaEvent (KeySigEv r) d -> z { events = A.snoc z.events (MetaEvent (KeySigEv r) (newTime d)), curTime = newTime d }
                          MetaEvent SeqSpec d -> z { events = A.snoc z.events (MetaEvent SeqSpec (newTime d)), curTime = newTime d }
                          MetaEvent UnknownMeta d -> z { events = A.snoc z.events (MetaEvent UnknownMeta (newTime d)), curTime = newTime d }
            )
            { events: [], curTime: 0 }
            track.events
    in
        track { events = e.events }

-------------
-- PARSING --
-------------

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
    | ProgChange
    | AfterTouch AfterTouchVal
    | PitchWheel PitchWheelChange
    | ChanMode

type TimeVal = Int

data Key = Major | Minor

data Accidental = Flats | Sharps

type Parser a = Array Int -> Either String (Tuple a (Array Int))
type MParser m = Array Int -> Maybe (Tuple m (Array Int))

parseFile :: Parser MidiFile
parseFile ints = do
    Tuple header rest <- parseFileHeader ints # note "failed to parse file header"
    Tuple tracks rest2 <- many parseTrack rest
    pure $ Tuple { header, tracks } rest2

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
                Loop { acc: snoc r.acc result, rem: rest }
            Left _ -> Done $ Right $ Tuple r.acc r.rem

parseTrackEvent :: Parser Event
parseTrackEvent ints = do
    Tuple deltaT rest <- parseVarLenNum ints
    { head, tail } <- A.uncons rest # note "sldkjfs"
    let deltaTail = cons deltaT tail
    case head of
        255 -> parseMeta deltaTail
        -- 240 -> parseSysEx tail
        _ -> parseMidi ints

parseMidi :: Parser Event
parseMidi ints = do
    Tuple delta rest <- parseVarLenNum ints
    lead <- head rest # note "parseMidi error getting leader byte"
    let mask = and 240 lead
    case mask of
        128 -> do
            Tuple off rest1 <- parseNoteOff rest
            pure $ Tuple (MidiEvent off delta) rest1
        144 -> do
            Tuple on rest1 <- parseNoteOn rest
            pure $ Tuple (MidiEvent on delta) rest1
        176 -> do
            Tuple cc rest1 <- parseCC rest
            pure $ Tuple (MidiEvent cc delta) rest1
        208 -> do
            Tuple afterT rest1 <- parseAfterTouch rest
            pure $ Tuple (MidiEvent afterT delta) rest1
        224 -> do
            Tuple pitchWheel rest1 <- parsePitchwheel rest
            pure $ Tuple (MidiEvent pitchWheel delta) rest1
        _ -> Left "illegal unspecified midi event"

parseMeta :: Parser Event
parseMeta ints = do
    delta <- head ints # note "parseMeta error"
    { head, tail } <- drop 1 ints
        # A.uncons
        # note "parseMeta error"
    case head of
        0x00 -> do
            Tuple seqN rest <- parseSeqNum tail
            pure $ Tuple (MetaEvent seqN delta) rest
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
        0x20 -> do
            Tuple chan rest <- parseChanPrefix tail
            pure $ Tuple (MetaEvent chan delta) rest
        0x2F -> do
            Tuple end rest <- parseEndOfTrack tail
            pure $ Tuple (MetaEvent end delta) rest
        0x51 -> do
            Tuple tempo rest <- parseTempo tail
            pure $ Tuple (MetaEvent tempo delta) rest
        0x54 -> do
            Tuple offset rest <- parseSMTPEOffset tail
            pure $ Tuple (MetaEvent offset delta) rest
        0x058 -> do
            Tuple sig rest <- parseTimeSig tail
            pure $ Tuple (MetaEvent sig delta) rest
        0x058 -> do
            Tuple sig rest <- parseKeySig tail
            pure $ Tuple (MetaEvent sig delta) rest
        0x7F -> do
            Tuple seqspec rest <- parseSeqSpec tail
            pure $ Tuple (MetaEvent seqspec delta) rest
        _ -> do
            Tuple unknown rest <- parseUnknown tail
            pure $ Tuple (MetaEvent unknown delta) rest

parseUnknown :: Parser MetaEvent
parseUnknown ints = do
    Tuple len rest <- parseVarLenNum ints
    pure $ Tuple (UnknownMeta) (drop len rest)

parseAfterTouch :: Parser MidiEvent
parseAfterTouch ints = note "parseAfterTouch failed" do
    b1 <- index ints 0
    let chan = and 15 b1
    val <- index ints 1
    pure $ Tuple (AfterTouch { chan, val }) (drop 2 ints)

parseCC :: Parser MidiEvent
parseCC ints = note "parseCC failed" do
    b1 <- index ints 0
    let chan = and 15 b1
    ctrl <- index ints 1
    val <- index ints 2
    pure $ Tuple (CC { chan, ctrl, val }) (drop 3 ints)

parsePitchwheel :: Parser MidiEvent
parsePitchwheel ints = note "parsePitch failed" do
    b1 <- index ints 0
    let chan = and 15 b1
    least <- index ints 1
    most <- index ints 2
    pure $ Tuple (PitchWheel { chan, pos: (combine2 most least) }) (drop 3 ints)

parseNoteOn :: Parser MidiEvent
parseNoteOn ints = note "parseNoteOn failed" do
    b1 <- index ints 0
    let chan = and 15 b1
    key <- index ints 1
    vel <- index ints 2
    pure $ Tuple (NoteOn { chan, key, vel }) (drop 3 ints)

parseNoteOff :: Parser MidiEvent
parseNoteOff ints = note "parseNoteOff failed" do
    b1 <- index ints 0
    let chan = and 15 b1
    key <- index ints 1
    vel <- index ints 2
    pure $ Tuple (NoteOff { chan, key, vel }) (drop 3 ints)

parseSeqSpec :: Parser MetaEvent
parseSeqSpec ints = do
    Tuple len rest <- parseVarLenNum ints
    pure $ Tuple (SeqSpec) (drop len rest)

parseKeySig :: Parser MetaEvent
parseKeySig ints = note "parseKeySig failed" do
    { head, tail } <- A.uncons ints
    guard $ head == 0x02
    sf <- index tail 0
    mi <- index tail 1
    guard $ (sf >= -7 && sf <= 7)
    guard $ (mi == 1 || mi == 0)
    pure $ Tuple (KeySigEv { sf, mi }) tail

parseTimeSig :: Parser MetaEvent
parseTimeSig ints = note "parseTimeSigFailed" do
    { head, tail } <- A.uncons ints
    guard $ head == 0x04
    nn <- index tail 0
    dd <- index tail 1
    cc <- index tail 2
    bb <- index tail 3
    denom <- pow (toNumber dd) (-2.0) # div 1.0 # fromNumber
    pure $ Tuple
        ( TimeSigEv
              { nn
              , dd: denom
              , cc
              , bb
              }
        )
        (drop 4 tail)

parseSMTPEOffset :: Parser MetaEvent
parseSMTPEOffset ints = note "parseSMTPEOffset failed" do
    { head, tail } <- A.uncons ints
    guard $ head == 0x05
    hr <- index tail 0
    mn <- index tail 1
    sec <- index tail 2
    fr <- index tail 3
    fFr <- index tail 4
    pure $ Tuple (SmpteOffset { hr, mn, sec, fr, fFr }) (drop 5 tail)

parseTempo :: Parser MetaEvent
parseTempo ints = note "parse tempo failed" do
    { head, tail } <- A.uncons ints
    guard $ head == 0x03
    b1 <- index tail 0
    b2 <- index tail 1
    b3 <- index tail 2
    pure $ Tuple (Tempo $ combine3 b1 b2 b3) (drop 3 tail)

parseEndOfTrack :: Parser MetaEvent
parseEndOfTrack ints = note "parseEOT failed" do
    { head, tail } <- A.uncons ints
    guard $ head == 0x00
    pure $ Tuple (EndOfTrack) tail

parseChanPrefix :: Parser MetaEvent
parseChanPrefix ints = note "parseChanPrefix failed" do
    leadByte <- head ints
    guard $ leadByte == 0x01
    { head, tail } <- drop 1 ints # A.uncons
    pure $ Tuple (ChannelPrefix $ head) tail

parseTextEvent :: Parser String
parseTextEvent ints = do
    Tuple len rest <- parseVarLenNum ints
    let
        text = take len rest
            # mapMaybe fromCharCode
            # fromCharArray
    pure $ Tuple (text) (drop len rest)

parseSeqNum :: Parser MetaEvent
parseSeqNum ints = note "parseSeqNum failed" do
    { head, tail } <- A.uncons ints
    guard $ head == 0x02
    b1 <- tail !! 0
    b2 <- tail !! 1
    pure $ Tuple (SeqNum $ combine2 b1 b2) (drop 2 tail)

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

