{-# LANGUAGE LambdaCase #-}

{- arch-tag: GZip file support in Haskell
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

-- |
--   Module     : System.FileArchive.GZip
--   Copyright  : Copyright (C) 2004-2011 John Goerzen
--   SPDX-License-Identifier: BSD-3-Clause
--
--   Stability  : provisional
--   Portability: portable
--
-- GZip file decompression
--
-- Copyright (c) 2004 John Goerzen, jgoerzen\@complete.org
--
-- The GZip format is described in RFC1952.
module System.FileArchive.GZip
  ( -- * GZip Files
    -- $gzipfiles

    -- * Types
    Header (..),
    Section,
    GZipError (..),
    Footer (..),

    -- * Whole-File Processing
    decompress,
    hDecompress,
    readSections,

    -- * Section Processing
    readHeader,
    readSection,
  )
where

import Control.Monad.Except
import Data.Bifunctor
import Data.Bits
import Data.Bits.Utils
import Data.Char
import Data.Compression.Inflate
import Data.Hash.CRC32.GZip
import Data.Word
import System.IO

data GZipError
  = -- | CRC-32 check failed
    CRCError
  | -- | Couldn't find a GZip header
    NotGZIPFile
  | -- | Compressed with something other than method 8 (deflate)
    UnknownMethod
  | -- | Other problem arose
    UnknownError String
  deriving (Eq, Show)

-- | First two bytes of file
magic :: String
magic = "\x1f\x8b"

-- | Flags
fFHCRC, fFEXTRA, fFNAME, fFCOMMENT :: Int
fFHCRC = 2
fFEXTRA = 4
fFNAME = 8
fFCOMMENT = 16

-- | The data structure representing the GZip header.  This occurs
-- at the beginning of each 'Section' on disk.
data Header = Header
  { -- | Compression method.  Only 8 is defined at present.
    method :: Int,
    flags :: Int,
    extra :: Maybe String,
    filename :: Maybe String,
    comment :: Maybe String,
    -- | Modification time of the original file
    mtime :: Word32,
    -- | Extra flags
    xfl :: Int,
    -- | Creating operating system
    os :: Int
  }
  deriving (Eq, Show)

-- | Stored on-disk at the end of each section.
data Footer = Footer
  { -- | The size of the original, decompressed data
    size :: Word32,
    -- | The stored GZip CRC-32 of the original, decompressed data
    crc32 :: Word32,
    -- | Whether or not the stored CRC-32 matches the calculated CRC-32 of the data
    crc32valid :: Bool
  }

-- | A section represents a compressed component in a GZip file.
-- Every GZip file has at least one.
type Section = (Header, String, Footer)

split1 :: String -> (Char, String)
split1 s = (head s, tail s)

-- | Read a GZip file, decompressing all sections found.
--
-- Writes the decompressed data stream to the given output handle.
--
-- Returns Nothing if the action was successful, or Just GZipError if there
-- was a problem.  If there was a problem, the data written to the output
-- handle should be discarded.
hDecompress ::
  -- | Input handle
  Handle ->
  -- | Output handle
  Handle ->
  IO (Maybe GZipError)
hDecompress infd outfd = do
  inc <- hGetContents infd
  let (outstr, err) = decompress inc
  hPutStr outfd outstr
  return err

-- | Read a GZip file, decompressing all sections that are found.
--
-- Returns a decompresed data stream and Nothing, or an unreliable string
-- and Just (error).  If you get anything other than Nothing, the String
-- returned should be discarded.
decompress :: String -> (String, Maybe GZipError)
decompress s =
  let procs :: [Section] -> (String, Bool)
      procs [] = ([], True)
      procs ((_, content, foot) : xs) =
        let (nexth, nextb) = procs xs
         in (content ++ nexth, crc32valid foot && nextb)
   in case readSections s of
        Left x -> ("", Just x)
        Right x ->
          let (decomp, iscrcok) = procs x
           in (decomp, if iscrcok then Nothing else Just CRCError)

-- | Read all sections.
readSections :: String -> Either GZipError [Section]
readSections [] = Right []
readSections s = do
  readSection s >>= \case
    (sect, remain) -> do
      next <- readSections remain
      return $ sect : next

parseword :: String -> Word32
parseword = fromBytes . map (fromIntegral . ord) . reverse

-- | Read one section, returning (ThisSection, Remainder)
readSection :: String -> Either GZipError (Section, String)
readSection s = do
  x <- readHeader s
  let headerrem = snd x
  let (decompressed, crc, remainder) = readData headerrem
  let (crc32str, rm) = splitAt 4 remainder
  let (sizestr, rem2) = splitAt 4 rm
  let filecrc32 = parseword crc32str
  let filesize = parseword sizestr
  let footer =
        Footer
          { size = filesize,
            crc32 = filecrc32,
            crc32valid = filecrc32 == crc
          }
  return ((fst x, decompressed, footer), rem2)

-- | Read the file's compressed data, returning
-- (Decompressed, Calculated CRC32, Remainder)
readData :: String -> (String, Word32, String)
readData x =
  let (decompressed1, remainder) = inflateStringRemainder x
      (decompressed, crc32') = readDataInternal decompressed1 0
   in (decompressed, crc32', remainder)
  where
    readDataInternal [] ck = ([], ck)
    readDataInternal (y : ys) ck =
      let newcrc = updateCrc ck y
          n = newcrc `seq` readDataInternal ys newcrc
       in first (y :) n

-- | Read the GZip header.  Return (Header, Remainder).
readHeader :: String -> Either GZipError (Header, String)
readHeader s =
  let ok = Right "ok"
   in do
        let (mag, rem') = splitAt 2 s
        _ <-
          if mag /= magic
            then throwError NotGZIPFile
            else ok
        let (method', rem2) = split1 rem'
        _ <-
          if ord method' /= 8
            then throwError UnknownMethod
            else ok
        let (flag_S, rem3) = split1 rem2
        let flag = ord flag_S
        let (mtimea, rem3a) = splitAt 4 rem3
        let mtime' = parseword mtimea
        let (xfla, rem3b) = split1 rem3a
        let xfl' = ord xfla
        let (osa, _) = split1 rem3b
        let os' = ord osa
        -- skip modtime (4), extraflag (1), and os (1)
        let rem4 = drop 6 rem3

        let (extra', rem5) =
              if flag .&. fFEXTRA /= 0
                then -- Skip past the extra field if we have it.

                  let (xlen_S, _) = split1 rem4
                      (xlen2_S, rem4b) = split1 rem4
                      xlen = ord xlen_S + 256 * ord xlen2_S
                      (ex, rrem) = splitAt xlen rem4b
                   in (Just ex, rrem)
                else (Nothing, rem4)

        let (filename', rem6) =
              if flag .&. fFNAME /= 0
                then -- Skip past the null-terminated filename

                  let fn = takeWhile (/= '\x00') rem5
                   in (Just fn, drop (length fn + 1) rem5)
                else (Nothing, rem5)

        let (comment', rem7) =
              if flag .&. fFCOMMENT /= 0
                then -- Skip past the null-terminated comment

                  let cm = takeWhile (/= '\x00') rem6
                   in (Just cm, drop (length cm + 1) rem6)
                else (Nothing, rem6)

        rem8 <-
          if flag .&. fFHCRC /= 0
            then -- Skip past the header CRC
              return $ drop 2 rem7
            else return rem7

        return
          ( Header
              { method = ord method',
                flags = flag,
                extra = extra',
                filename = filename',
                comment = comment',
                mtime = mtime',
                xfl = xfl',
                os = os'
              },
            rem8
          )

----------------------------------------------------------------------
-- Documentation
----------------------------------------------------------------------

-- $gzipfiles
--
-- GZip files contain one or more 'Section's.  Each 'Section', on disk, begins
-- with a GZip 'Header', then stores the compressed data itself, and finally
-- stores a GZip 'Footer'.
--
-- The 'Header' identifies the file as a GZip file, records the original
-- modification date and time, and, in some cases, also records the original
-- filename and comments.
--
-- The 'Footer' contains a GZip CRC32 checksum over the decompressed data as
-- well as a 32-bit length of the decompressed data.  The module
-- 'Data.Hash.CRC32.GZip' is used to validate stored CRC32 values.
--
-- The vast majority of GZip files contain only one 'Section'.  Standard tools
-- that work with GZip files create single-section files by default.
--
-- Multi-section files can be created by simply concatenating two existing
-- GZip files together.  The standard gunzip and zcat tools will simply
-- concatenate the decompressed data when reading these files back.  The
-- 'decompress' function in this module will do the same.
--
-- When reading data from this module, please use caution regarding how you access
-- it.  For instance, if you are wanting to write the decompressed stream
-- to disk and validate its CRC32 value, you could use the 'decompress'
-- function.  However, you should process the entire stream before you check
-- the value of the Bool it returns.  Otherwise, you will force Haskell to buffer
-- the entire file in memory just so it can check the CRC32.
