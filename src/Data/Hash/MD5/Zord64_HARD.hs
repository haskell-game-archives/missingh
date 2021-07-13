{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK hide #-}

-- | Obsolete legacy module
module Data.Hash.MD5.Zord64_HARD (Zord64) where

import Data.Bits
import Data.Word

data Zord64 = W64
  { lo :: Word32,
    hi :: Word32
  }
  deriving (Eq, Ord, Bounded)

w64ToInteger :: Zord64 -> Integer
w64ToInteger W64 {..} = toInteger lo + 0x100000000 * toInteger hi

integerToW64 :: Integer -> Zord64
integerToW64 x =
  case x `quotRem` 0x100000000 of
    (h, l) -> W64 {lo = fromInteger l, hi = fromInteger h}

instance Show Zord64 where
  show = undefined

instance Read Zord64 where
  readsPrec = undefined

instance Num Zord64 where
  W64 {lo = lo_a, hi = hi_a} + W64 {lo = lo_b, hi = hi_b} = W64 {lo = lo', hi = hi'}
    where
      lo' = lo_a + lo_b
      hi' = hi_a + hi_b + if lo' < lo_a then 1 else 0
  W64 {lo = lo_a, hi = hi_a} - W64 {lo = lo_b, hi = hi_b} = W64 {lo = lo', hi = hi'}
    where
      lo' = lo_a - lo_b
      hi' = hi_a - hi_b + if lo' > lo_a then 1 else 0
  fromInteger = integerToW64

  (*) = undefined
  abs = undefined
  signum = undefined

instance Bits Zord64 where
  W64 {lo = lo_a, hi = hi_a} .&. W64 {lo = lo_b, hi = hi_b} = W64 {lo = lo', hi = hi'}
    where
      lo' = lo_a .&. lo_b
      hi' = hi_a .&. hi_b
  W64 {lo = lo_a, hi = hi_a} .|. W64 {lo = lo_b, hi = hi_b} = W64 {lo = lo', hi = hi'}
    where
      lo' = lo_a .|. lo_b
      hi' = hi_a .|. hi_b
  shift w 0 = w
  shift W64 {..} x
    | x > 63 = W64 {lo = 0, hi = 0}
    | x > 31 = W64 {lo = 0, hi = shift lo (x -32)}
    | x > 0 = W64 {lo = shift lo x, hi = shift hi x .|. shift lo (x -32)}
    | x < -63 = W64 {lo = 0, hi = 0}
    | x < -31 = W64 {lo = shift hi (x + 32), hi = 0}
    | x < 0 = W64 {lo = shift lo x .|. shift hi (x + 32), hi = shift hi x}
  shift _ _ = error "shift"
  complement W64 {..} = W64 {lo = complement lo, hi = complement hi}

  xor = undefined
  rotate = undefined
  bitSize = undefined
  bitSizeMaybe = undefined
  isSigned = undefined
  testBit = undefined
  bit = undefined
  popCount = undefined

instance Integral Zord64 where
  toInteger = w64ToInteger
  quotRem = undefined

instance Real Zord64 where
  toRational = undefined

instance Enum Zord64 where
  toEnum = undefined
  fromEnum = undefined
