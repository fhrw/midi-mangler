module Bits (Bits(..), Bit(..), intToBits, unsafeBitsToInt, padEight, combine2) where

import Prelude hiding (zero)

import Control.Plus (empty)
import Data.Array (length, replicate)
import Data.Array as A
import Data.Int as Int
import Data.Newtype (class Newtype, unwrap)
import Data.Tuple (Tuple(..), fst)

newtype Bit = Bit Boolean

newtype Bits = Bits (Array Bit)

derive instance newtypeBits :: Newtype Bits _
derive newtype instance Show Bits
derive newtype instance Semigroup Bits
instance Show Bit where
  show (Bit false) = "0"
  show (Bit true) = "1"

_0 :: Bit
_0 = Bit false

_1 :: Bit
_1 = Bit true

zero :: Bits
zero = Bits [ _0 ]

intToBits :: Int -> Bits
intToBits 0 = zero
intToBits i = Bits (f i)
  where
  f 0 = empty
  f n
    | Int.odd n = A.snoc (f (n `div` 2)) _1
    | otherwise = A.snoc (f (n `div` 2)) _0

bitToInt :: Bit -> Int
bitToInt (Bit true) = 1
bitToInt (Bit false) = 0

unsafeBitsToInt :: Bits -> Int
unsafeBitsToInt (Bits bits) = fst $ A.foldr f (Tuple 0 1) bits
  where
  f b (Tuple r p) = Tuple (p * bitToInt b + r) (p * 2)

padEight :: Bits -> Bits
padEight bits =
  let
    unwrapped = unwrap bits
    pad = replicate (8 - length unwrapped) _0

  in
    Bits pad <> bits

combine2 :: Int -> Int -> Int
combine2 byte1 byte2 =
  ((intToBits byte1 # padEight) <> (intToBits byte2 # padEight)) # unsafeBitsToInt
