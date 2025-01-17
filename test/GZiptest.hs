{- arch-tag: Tests for GZip module
Copyright (C) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

module GZiptest (tests) where

import Data.Compression.Inflate
import Data.Either.Utils
import System.FileArchive.GZip
import System.FilePath
import System.IO.Binary
import Test.HUnit

mf :: (Eq a, Show a) => String -> a -> (String -> a) -> Test
mf fn exp' conf = TestLabel fn $
  TestCase $
    do
      c <-
        readBinaryFile $
          joinPath ["testsrc", "gzfiles", fn]
      assertEqual "" exp' (conf c)

test_inflate :: [Test]
test_inflate =
  let f fn exp' conv = mf fn exp' (conv . snd . forceEither . readHeader)
   in [ f "t1.gz" "Test 1" inflateString,
        f "t1.gz" 6 (length . inflateString),
        f
          "t1.gz"
          ( "Test 1",
            "\x19\xf8\x27\x99\x06\x00\x00\x00"
          )
          inflateStringRemainder,
        f "empty.gz" "" inflateString
      ]

test_header :: [Test]
test_header =
  let f fn exp' = mf fn exp' (fst . forceEither . readHeader)
   in [ f
          "t1.gz"
          Header
            { method = 8,
              flags = 0,
              extra = Nothing,
              filename = Nothing,
              comment = Nothing,
              mtime = 1102111446,
              xfl = 2,
              os = 3
            },
        f
          "empty.gz"
          Header
            { method = 8,
              flags = 8,
              extra = Nothing,
              filename = Just "empty",
              comment = Nothing,
              mtime = 1102127257,
              xfl = 0,
              os = 3
            }
      ]

test_gunzip :: [Test]
test_gunzip =
  let f fn exp' = mf fn exp' decompress
   in [ f "t1.gz" ("Test 1", Nothing),
        f "t1bad.gz" ("Test 1", Just CRCError),
        f "t2.gz" ("Test 1Test 2", Nothing)
      ]

tests :: Test
tests =
  TestList
    [ TestLabel "inflate" (TestList test_inflate),
      TestLabel "header" (TestList test_header),
      TestLabel "gunzip" (TestList test_gunzip)
    ]
