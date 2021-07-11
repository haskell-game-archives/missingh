{-# LANGUAGE Safe #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGuAGE TupleSections #-}

-- arch-tag: Inflate implementation for Haskell

{-
Inflate implementation for Haskell

Copyright 2004 Ian Lynagh <igloo@earth.li>
Licence: 3 clause BSD.

\section{Inflate}

This module provides a Haskell implementation of the inflate function,
as described by RFC 1951.

-}

-- |
--   Module     : Data.Compression.Inflate
--   Copyright  : Copyright (C) 2004 Ian Lynagh
--   SPDX-License-Identifier: BSD-3-Clause
--
--   Stability  : provisional
--   Portability: portable
--
-- Inflate algorithm implementation
--
-- Copyright (C) 2004 Ian Lynagh
module Data.Compression.Inflate
  ( inflateString,
    inflateStringRemainder,
    inflate,
    Output,
    Bit,
    bits_to_word32,
  )
where

import Control.Monad
import Data.Array
import Data.Bits
import qualified Data.Char
import Data.List
import Data.Word

inflateString :: String -> String
inflateString = fst . inflateStringRemainder

-- | Returns (Data, Remainder)
inflateStringRemainder :: String -> (String, String)
inflateStringRemainder s =
  let res = inflate $ map Data.Char.ord s
      convw32l l = map (Data.Char.chr . fromIntegral) l
      output = convw32l $ fst res
      b2w32 [] = []
      b2w32 b =
        let (this, next) = splitAt 8 b
         in bits_to_word32 this : b2w32 next
      remainder = convw32l $ b2w32 $ snd res
   in (output, remainder)

{-
\section{Types}

Type synonyms are your friend.

-}
type Output = [Word32] -- The final output

type Code = Word32 -- A generic code

type Dist = Code -- A distance code

type LitLen = Code -- A literal/length code

type Length = Word32 -- Number of bits needed to identify a code

type Table = InfM Code -- A Huffman table

type Tables = (Table, Table) -- lit/len and dist Huffman tables

{-

The \verb!Bit! datatype is used for the input. We can show values and
convert from the input we are given and to \verb!Word32!s which we us to
represent most values.

-}
newtype Bit = Bit Bool
  deriving (Eq)

instance Show Bit where
  show = (:[]) . showB
  showList bs = showString $ "'" ++ map showB bs ++ "'"

showB :: Bit -> Char
showB (Bit True) = '1'
showB (Bit False) = '0'

intToBits :: Int -> [Bit]
intToBits = word8_to_bits . fromIntegral

word8_to_bits :: Word8 -> [Bit]
word8_to_bits n = map (Bit . testBit n) [0 .. 7]

bits_to_word32 :: [Bit] -> Word32
bits_to_word32 = foldr (\(Bit b) i -> 2 * i + (if b then 1 else 0)) 0

{-

\section{Monad}

offset is rarely used, so make it strict to avoid building huge closures.

-}
data State = State
  { bits :: [Bit], -- remaining input bits
    offset :: !Word32, -- num bits consumed mod 8
    history :: Array Word32 Word32, -- last 32768 output words
    loc :: Word32 -- where in history we are
  }

data InfM a = InfM (State -> (a, State))

instance Monad InfM where
  (>>=)  :: InfM a -> (a -> InfM b) -> InfM b
  InfM v >>= f = InfM $ \s ->
    let (x, s') = v s
        InfM y = f x
     in y s'

  return :: a -> InfM a
  return x = InfM (x,)

instance Applicative InfM where
  pure = return
  (<*>) = ap

instance Functor InfM where
  fmap f (InfM g) = InfM $ \s ->
    case g s of ~(a, s') -> (f a, s')

setBits :: [Bit] -> InfM ()
setBits bs = InfM $ const ((), State bs 0 (array (0, 32767) []) 0)

align_8_bits :: InfM ()
align_8_bits =
  InfM $ \s ->
    ( (),
      s
        { bits = genericDrop ((8 - offset s) `mod` 8) (bits s),
          offset = 0
        }
    )

getBits :: Word32 -> InfM [Bit]
getBits n = InfM $ \s -> case need n (bits s) of
  (ys, zs) ->
    ( ys,
      s
        { bits = zs,
          offset = (n + offset s) `mod` 8
        }
    )
  where
    need 0 xs = ([], xs)
    need _ [] = error "getBits: Don't have enough!"
    need i (x : xs) = let (ys, zs) = need (i -1) xs in (x : ys, zs)

extractInfM :: InfM a -> (a, [Bit])
extractInfM (InfM f) = let (x, s) = f undefined in (x, bits s)

output_w32 :: Word32 -> InfM ()
output_w32 w = InfM $ \s ->
  let l = loc s
   in ( (),
        s
          { history = history s // [(l, w)],
            loc = l + 1
          }
      )

repeat_w32s :: Word32 -> Word32 -> InfM [Word32]
repeat_w32s len dist =
  InfM $ \s ->
    let l = loc s
        h = history s
        new = map (h !) $ genericTake dist ([(l - dist) `mod` 32768 .. 32767] ++ [0 ..])
        new_bit = genericTake len (cycle new)
        h' = h // zip (map (`mod` 32768) [l ..]) new_bit
     in (new_bit, s {history = h', loc = (l + len) `mod` 32768})

-----------------------------------

get_word32s :: Word32 -> Word32 -> InfM [Word32]
get_word32s _ 0 = return []
get_word32s b n = do
  w <- get_w32 b
  ws <- get_word32s b (n -1)
  return (w : ws)

get_w32 :: Word32 -> InfM Word32
get_w32 i = do
  bs <- getBits i
  return (bits_to_word32 bs)

getBit :: InfM Bit
getBit = do
  res <- getBits 1
  case res of
    [x] -> return x
    _ -> error "getBit: expected exactly one bit"

{-
\section{Inflate itself}

The hardcore stuff!

-}
inflate :: [Int] -> (Output, [Bit])
inflate is = extractInfM $ do
  setBits $ concatMap intToBits is
  x <- inflateBlocks False
  align_8_bits
  return x

-- Bool is true if we have seen the "last" block
inflateBlocks :: Bool -> InfM Output
inflateBlocks True = return []
inflateBlocks False =
  do
    res <- getBits 3
    case res of
      [Bit is_last, Bit t1, Bit t2] ->
        case (t1, t2) of
          (False, False) ->
            do
              align_8_bits
              len <- get_w32 16
              nlen <- get_w32 16
              unless (len + nlen == 2 ^ (32 :: Int) - 1) $
                error "inflateBlocks: Mismatched lengths"
              ws <- get_word32s 8 len
              mapM_ output_w32 ws
              return ws
          (True, False) ->
            inflateCodes is_last inflateTreesFixed
          (False, True) ->
            do
              tables <- inflateTables
              inflateCodes is_last tables
          (True, True) ->
            error "inflateBlocks: case 11 reserved"
      _ -> error "inflateBlocks: expected 3 bits"

inflateTables :: InfM Tables
inflateTables =
  do
    hlit <- get_w32 5
    hdist <- get_w32 5
    hclen <- get_w32 4
    llc_bs <- getBits ((hclen + 4) * 3)
    let llc_bs' =
          zip
            (map bits_to_word32 $ triple llc_bs)
            [16, 17, 18, 0, 8, 7, 9, 6, 10, 5, 11, 4, 12, 3, 13, 2, 14, 1, 15]
        tab = makeTable llc_bs'
    lit_dist_lengths <-
      makeLitDistLengths
        tab
        (258 + hlit + hdist)
        (error "inflateTables dummy")
    let (lit_lengths, dist_lengths) =
          genericSplitAt
            (257 + hlit)
            lit_dist_lengths
        lit_table = makeTable (zip lit_lengths [0 ..])
        dist_table = makeTable (zip dist_lengths [0 ..])
    return (lit_table, dist_table)

triple :: [a] -> [[a]]
triple (a : b : c : xs) = [a, b, c] : triple xs
triple [] = []
triple _ = error "triple: can't happen"

makeLitDistLengths :: Table -> Word32 -> Word32 -> InfM [Word32]
makeLitDistLengths _ i _ | i < 0 = error "makeLitDistLengths i < 0"
makeLitDistLengths _ 0 _ = return []
makeLitDistLengths tab i last_thing = do
  c <- tab
  (ls, i', last_thing') <- metaCode i c last_thing
  ws <- makeLitDistLengths tab i' last_thing'
  return (ls ++ ws)

metaCode :: Word32 -> Code -> Word32 -> InfM ([Word32], Word32, Word32)
metaCode c i _ | i < 16 = return ([i], c - 1, i)
metaCode c 16 last_thing = do
  xs <- getBits 2
  let l = 3 + bits_to_word32 xs
  return (genericReplicate l last_thing, c - l, last_thing)
metaCode c 17 _ = do
  xs <- getBits 3
  let l = 3 + bits_to_word32 xs
  return (genericReplicate l 0, c - l, 0)
metaCode c 18 _ = do
  xs <- getBits 7
  let l = 11 + bits_to_word32 xs
  return (genericReplicate l 0, c - l, 0)
metaCode _ i _ = error $ "metaCode: " ++ show i

inflateCodes :: Bool -> Tables -> InfM Output
inflateCodes seen_last tabs@(tab_litlen, tab_dist) = do
  i <- tab_litlen
  if i == 256
    then inflateBlocks seen_last
    else do
      pref <-
        if i < 256
          then do
            output_w32 i
            return [i]
          else case lookup i litlens of
            Nothing -> error "do_code_litlen"
            Just (base, num_bits) ->
              do
                extra <- get_w32 num_bits
                let l = base + extra
                dist <- distCode tab_dist
                repeat_w32s l dist
      o <- inflateCodes seen_last tabs
      return (pref ++ o)

litlens :: [(Code, (LitLen, Word32))]
litlens = zip [257 .. 285] $ mkBases 3 litlen_counts ++ [(258, 0)]
  where
    litlen_counts = [(8, 0), (4, 1), (4, 2), (4, 3), (4, 4), (4, 5)]

distCode :: Table -> InfM Dist
distCode tab = do
  code <- tab
  case lookup code dists of
    Nothing -> error "distCode"
    Just (base, num_bits) -> do
      extra <- get_w32 num_bits
      return (base + extra)

dists :: [(Code, (Dist, Word32))]
dists = zip [0 .. 29] $ mkBases 1 dist_counts
  where
    dist_counts = (4, 0) : map (2,) [1 .. 13]

mkBases :: Word32 -> [(Int, Word32)] -> [(Word32, Word32)]
mkBases base counts = snd $ mapAccumL next_base base incs
  where
    next_base current bs = (current + 2 ^ bs, (current, bs))
    incs = concatMap (uncurry replicate) counts

{-
\section{Fixed tables}

The fixed tables. Not much to say really.

-}
inflateTreesFixed :: Tables
inflateTreesFixed =
  ( makeTable $
      [(8, c) | c <- [0 .. 143]]
        ++ [(9, c) | c <- [144 .. 255]]
        ++ [(7, c) | c <- [256 .. 279]]
        ++ [(8, c) | c <- [280 .. 287]],
    makeTable [(5, c) | c <- [0 .. 29]]
  )

{-
\section{The Huffman Tree}

As the name suggests, the obvious way to store Huffman trees is in a
tree datastructure. Externally we want to view them as functions though,
so we wrap the tree with \verb!getCode! which takes a list of bits and
returns the corresponding code and the remaining bits. To make a tree
from a list of length code pairs is a simple recursive process.

-}
data Tree = Branch Tree Tree | Leaf Word32 | Null

makeTable :: [(Length, Code)] -> Table
makeTable lcs = case makeTree 0 $ sort $ filter ((/= 0) . fst) lcs of
  (tree, []) -> getCode tree
  _ -> error "makeTable: Left-over lcs from"

getCode :: Tree -> InfM Code
getCode (Branch zero_tree one_tree) = do
  Bit b <- getBit
  if b then getCode one_tree else getCode zero_tree
getCode (Leaf w) = return w
getCode Null = error "getCode Null"

makeTree :: Word32 -> [(Length, Code)] -> (Tree, [(Length, Code)])
makeTree _ [] = (Null, [])
makeTree i lcs@((l, c) : lcs')
  | i == l = (Leaf c, lcs')
  | i < l =
    let (zero_tree, lcs_z) = makeTree (i + 1) lcs
        (one_tree, lcs_o) = makeTree (i + 1) lcs_z
     in (Branch zero_tree one_tree, lcs_o)
  | otherwise = error "makeTree: can't happen"
