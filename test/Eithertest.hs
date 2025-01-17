{- arch-tag: Data.Either.Utils tests
Copyright (C) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE

-}

module Eithertest (tests) where

import Data.Either.Utils
import Test.HUnit
import TestUtils

test_maybeToEither :: [Test]
test_maybeToEither =
  let f msg inp exp' = TestLabel msg $ TestCase $ assertEqual "" exp' inp
   in [ f
          "Nothing"
          (maybeToEither "error" (Nothing :: Maybe String))
          (Left "error"),
        f
          "Nothing diff types"
          (maybeToEither "error" (Nothing :: Maybe Int))
          (Left "error"),
        f "Just" (maybeToEither "error" (Just "good")) (Right "good"),
        f
          "Diff types"
          (maybeToEither "error" (Just (5 :: Int)))
          (Right (5 :: Int))
      ]

test_forceEither :: [Test]
test_forceEither =
  let f msg inp exp' = TestLabel msg $ TestCase $ assertEqual "" exp' inp
   in [ f "Right" (forceEither (Right "foo" :: Either Int String)) "foo",
        TestLabel "Left" $ TestCase $ assertRaises ((== "\"wrong\"") . errorCallMsg) ("" @=? forceEither (Left "wrong"))
      ]

tests :: Test
tests =
  TestList
    [ TestLabel "test_maybeToEither" (TestList test_maybeToEither),
      TestLabel "test_forceEither" (TestList test_forceEither)
    ]
