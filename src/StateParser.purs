module StateParser where

import Prelude

import Bits (combine2, combine3, combine4)
import Control.Monad.Cont (lift)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Except (ExceptT)
import Control.Monad.State (StateT, get, put)
import Control.MonadPlus (alt)
import Data.Array (fromFoldable, index, length, mapMaybe, replicate)
import Data.Char (fromCharCode)
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Int (fromNumber, toNumber)
import Data.Int.Bits (and, or, shl)
import Data.List (List, manyRec)
import Data.Maybe (Maybe(..))
import Data.Number (pow)
import Data.String.CodeUnits (fromCharArray)
import Data.Traversable (sequence)
import MidiTypes (Event(..), FileHeader, MetaEvent(..), MidiEvent(..), MidiFile, Track)

type ParserState =
    { file :: Array Int
    , pos :: Int
    }

data ParseError
    = GenericError String
    | MissingMThd
    | MissingMTrk
    | VarLenExccededMaxDepth
    | InvalidHeaderLengthVal
    | UnspecifiedMidiEvent
    | UnspecifiedMetaEvent
    | ManyErrors (Array ParseError)
    | NoError

type Parser = StateT ParserState (ExceptT ParseError Identity)

parseFile :: Parser MidiFile
parseFile = do
    header <- parseHeader
    tracks <- fromFoldable <$> many parseTrack
    pure $ { header, tracks }

parseHeader :: Parser FileHeader
parseHeader = do
    arr <- readManyInts 4
    unless (arr == [ 77, 84, 104, 100 ]) $ do
        throwError MissingMThd
    lenVal <- readInt32
    unless (lenVal == 6) $ do
        throwError InvalidHeaderLengthVal
    format <- readInt16
    nTracks <- readInt16
    division <- readInt16
    pure { format, nTracks, division }

parseTrack :: Parser Track
parseTrack = do
    mtrk <- readManyInts 4
    unless (mtrk == [ 77, 84, 114, 107 ]) $ do
        throwError MissingMTrk
    _ <- readInt32
    events <- fromFoldable <$> many parseTrackEvent
    pure { events }

parseTrackEvent :: Parser Event
parseTrackEvent = do
    delta <- parseVarLen
    event <- alt
        (flip MetaEvent delta <$> parseMetaEvent)
        (flip MidiEvent delta <$> parseMidiEvent)
    pure event

parseMidiEvent :: Parser MidiEvent
parseMidiEvent = do
    leader <- and 240 <$> peekInt
    case leader of
        128 -> do
            noteOff <- parseNoteOff
            pure noteOff
        144 -> do
            noteOn <- parseNoteOn
            pure noteOn
        176 -> do
            cc <- parseCC
            pure cc
        192 -> do
            prog <- parseProgChange
            pure prog
        208 -> do
            afterTouch <- parseAfterTouch
            pure afterTouch
        224 -> do
            pitchWheel <- parsePitchWheel
            pure pitchWheel
        _ -> throwError $ UnspecifiedMidiEvent

parseMetaEvent :: Parser MetaEvent
parseMetaEvent = do
    assertNext 0xFF
    leader <- readInt
    case leader of
        0x00 -> do
            seqNum <- parseSeqNum
            pure seqNum
        0x01 -> do
            text <- parseTextEvent
            pure $ Text text
        0x02 -> do
            text <- parseTextEvent
            pure $ Copyright text
        0x03 -> do
            text <- parseTextEvent
            pure $ TrackName text
        0x04 -> do
            text <- parseTextEvent
            pure $ InstName text
        0x05 -> do
            text <- parseTextEvent
            pure $ Lyric text
        0x06 -> do
            text <- parseTextEvent
            pure $ Marker text
        0x07 -> do
            text <- parseTextEvent
            pure $ CuePoint text
        0x20 -> do
            chanPrefix <- parseChanPrefix
            pure chanPrefix
        0x2F -> do
            eot <- parseEndOfTrack
            pure eot
        0x51 -> do
            tempo <- parseTempo
            pure tempo
        0x54 -> do
            offset <- parseSMTPEOffset
            pure offset
        0x58 -> do
            sig <- parseTimeSig
            pure sig
        0x59 -> do
            sig <- parseKeySig
            pure sig
        0x7F -> do
            spec <- parseSeqSpec
            pure spec
        _ -> do
            throwError UnspecifiedMetaEvent

parseSeqSpec :: Parser MetaEvent
parseSeqSpec = do
    _ <- parseVarLen
    pure SeqSpec

parseKeySig :: Parser MetaEvent
parseKeySig = do
    assertNext 0x02
    sf <- readInt
    unless (sf >= -1 && sf <= 7) $ do
        throwError $ GenericError "sharps or flats byte out of valid range"
    mi <- readInt
    unless (mi == 1 || mi == 0) $ do
        throwError $ GenericError "key byte not 1 or 0"
    pure $ KeySigEv { sf, mi }

parseTimeSig :: Parser MetaEvent
parseTimeSig = do
    assertNext 0x04
    nn <- readInt
    dd <- readInt
    cc <- readInt
    bb <- readInt
    let denom = pow (toNumber dd) (-2.0) # div 1.0 # fromNumber
    case denom of
        Just x ->
            pure $ TimeSigEv { nn, dd: x, cc, bb }
        _ -> throwError $ GenericError "couldn't convert timesig denom"

parseSMTPEOffset :: Parser MetaEvent
parseSMTPEOffset = do
    assertNext 0x05
    hr <- readInt
    mn <- readInt
    sec <- readInt
    fr <- readInt
    fFr <- readInt
    pure $ SmpteOffset { hr, mn, sec, fr, fFr }

parseTempo :: Parser MetaEvent
parseTempo = do
    assertNext 0x03
    a <- readInt
    b <- readInt
    c <- readInt
    pure $ Tempo $ combine3 a b c

parseEndOfTrack :: Parser MetaEvent
parseEndOfTrack = do
    assertNext 0x00
    pure EndOfTrack

parseChanPrefix :: Parser MetaEvent
parseChanPrefix = do
    assertNext 0x01
    prefix <- readInt
    pure $ ChannelPrefix prefix

parseTextEvent :: Parser String
parseTextEvent = do
    len <- parseVarLen
    text <- readManyInts len
        <#> mapMaybe fromCharCode
    pure $ fromCharArray text

parseSeqNum :: Parser MetaEvent
parseSeqNum = do
    assertNext 0x02
    num <- readInt16
    pure $ SeqNum num

parseCC :: Parser MidiEvent
parseCC = do
    chan <- and 15 <$> readInt
    ctrl <- readInt
    val <- readInt
    pure $ CC { chan, ctrl, val }

parseProgChange :: Parser MidiEvent
parseProgChange = do
    chan <- and 15 <$> readInt
    progNum <- readInt
    pure $ ProgChange { chan, progNum }

parseAfterTouch :: Parser MidiEvent
parseAfterTouch = do
    chan <- and 15 <$> readInt
    val <- readInt
    pure $ AfterTouch { chan, val }

parsePitchWheel :: Parser MidiEvent
parsePitchWheel = do
    chan <- and 15 <$> readInt
    pos <- readInt16
    pure $ PitchWheel { chan, pos }

parseNoteOff :: Parser MidiEvent
parseNoteOff = do
    chan <- and 15 <$> readInt
    key <- readInt
    vel <- readInt
    pure $ NoteOff { key, vel, chan }

parseNoteOn :: Parser MidiEvent
parseNoteOn = do
    chan <- and 15 <$> readInt
    key <- readInt
    vel <- readInt
    pure $ NoteOn { key, vel, chan }

parseVarLen :: Parser Int
parseVarLen = do
    f 0 0
    where
    f z depth = do
        val <- readInt
        let
            mask = and 128 val
            val' = and 127 val
            next = or (shl z 7) val'
        case mask of
            128 -> do
                if depth > 3 then throwError VarLenExccededMaxDepth
                else f next (depth + 1)
            _ -> pure next

-------------
-- HELPERS --
-------------

many :: forall a. Parser a -> Parser (List a)
many = manyRec

assertNext :: Int -> Parser Unit
assertNext n = do
    byte <- readInt
    when (byte /= n) $ do
        throwError $ GenericError $ "next byte was not equal to " <> show n

assertNextBytes :: Array Int -> Parser Unit
assertNextBytes arr = do
    got <- readManyInts $ length arr
    when (arr /= got) $ do
        throwError $ GenericError $ "Expected " <> show arr <> "; Got " <> show got

peekInt :: Parser Int
peekInt = do
    { file, pos } <- get
    case index file pos of
        Just n -> do
            pure n
        _ -> lift $ throwError $ GenericError "failed to peek byte"

readInt :: Parser Int
readInt = do
    st@{ file, pos } <- get
    case index file pos of
        Just n -> do
            put st { pos = pos + 1 }
            pure n
        _ -> lift $ throwError $ GenericError "failed to read byte!"

readManyInts :: Int -> Parser (Array Int)
readManyInts n = sequence $ replicate n readInt

readInt16 :: Parser Int
readInt16 = do
    bytes <- readManyInts 2
    case bytes of
        [ a, b ] -> pure $ combine2 a b
        _ -> lift $ throwError $ GenericError "failed to read two bits"

readInt32 :: Parser Int
readInt32 = do
    bytes <- readManyInts 4
    case bytes of
        [ a, b, c, d ] -> pure $ combine4 a b c d
        _ -> lift $ throwError $ GenericError "failed to read four bits"

readUint16 :: Parser Int
readUint16 = do
    bytes <- readManyInts 2
    case bytes of
        [ a, b ] ->
            if (and 128 a < 128) then
                pure $ combine2 a b
            else throwError $ GenericError "didn't find unsigned uint16"
        _ -> lift $ throwError $ GenericError "failed to read two bits"

readUint32 :: Parser Int
readUint32 = do
    bytes <- readManyInts 4
    case bytes of
        [ a, b, c, d ] ->
            if (and 128 a < 128) then
                pure $ combine4 a b c d
            else throwError $ GenericError "didn't find unsigned uint32"
        _ -> lift $ throwError $ GenericError "failed to read 4 bits"

---------------
-- INSTANCES --
---------------

derive instance Generic ParseError _
instance Eq ParseError where
    eq (ManyErrors _) _ = true
    eq _ _ = false

instance Monoid ParseError where
    mempty = NoError

instance Show ParseError where
  show (GenericError s) = "generic error: " <> s
  show MissingMThd = "missing midi file header"
  show MissingMTrk = "missing midi track header"
  show VarLenExccededMaxDepth = "variable length exceeded depth of 3"
  show InvalidHeaderLengthVal = "header length val was invalid"
  show UnspecifiedMidiEvent = "found a midi event that could not be parsed"
  show UnspecifiedMetaEvent = "found a meta event that could not be parsed"
  show NoError = "no error"
  show (ManyErrors arr) = "many errors: " <> show arr

instance Semigroup ParseError where
    append (GenericError a) (GenericError b) = GenericError (a <> " ;" <> b)
    append NoError err = err
    append err NoError = err
    append err _ = err

