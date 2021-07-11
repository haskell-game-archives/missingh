{- arch-tag: Tests main file
Copyright (C) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

module Tests (tests) where

import qualified Bitstest
import qualified CRC32GZIPtest
import qualified Eithertest
import qualified GZiptest
import qualified Globtest
import qualified HVFStest
import qualified HVIOtest
import qualified Listtest
import qualified MIMETypestest
import qualified Maptest
import qualified Pathtest
import qualified ProgressTrackertest
import qualified Str.CSVtest
import qualified Strtest
import Test.HUnit
import qualified Timetest
import qualified WildMatchtest

test1 :: Test
test1 = TestCase ("x" @=? "x")

tests :: Test
tests =
  TestList
    [ TestLabel "test1" test1,
      TestLabel "List" Listtest.tests,
      TestLabel "Str" Strtest.tests,
      TestLabel "CSV" Str.CSVtest.tests,
      TestLabel "Time" Timetest.tests,
      TestLabel "Map" Maptest.tests,
      TestLabel "ProgressTracker" ProgressTrackertest.tests,
      TestLabel "Path" Pathtest.tests,
      TestLabel "WildMatch" WildMatchtest.tests,
      TestLabel "HVIO" HVIOtest.tests,
      TestLabel "HVFS" HVFStest.tests,
      TestLabel "Glob" Globtest.tests,
      TestLabel "MIMETypes" MIMETypestest.tests,
      TestLabel "Bitstest" Bitstest.tests,
      TestLabel "Eithertest" Eithertest.tests,
      TestLabel "CRC32GZIPtest" CRC32GZIPtest.tests,
      TestLabel "GZiptest" GZiptest.tests
    ]
