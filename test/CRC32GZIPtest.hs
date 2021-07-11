{-
Copyright (C) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

module CRC32GZIPtest (tests) where

import Data.Hash.CRC32.GZip
import Test.HUnit

test_crcgzip :: [Test]
test_crcgzip =
  let f msg inp exp' = TestLabel msg $ TestCase $ assertEqual "" exp' (calc_crc32 inp)
   in [ f "Simple" "Test 1" 0x9927f819,
        f "Empty" "" 0x0
      ]

tests :: Test
tests =
  TestList
    [ TestLabel "crcgzip" (TestList test_crcgzip)
    ]
