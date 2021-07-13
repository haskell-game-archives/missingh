{- arch-tag: Str tests main file
Copyright (C) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

module Strtest (tests) where

import Data.Char
import Data.String.Utils
import Test.HUnit
import TestUtils
import Text.Regex

test_lstrip :: [Test]
test_lstrip =
  mapassertEqual
    "lstrip"
    lstrip
    [ ("", ""),
      ("a", "a"),
      (" a ", "a "),
      ("  abas", "abas"),
      ("\n\t fdsa", "fdsa"),
      ("abc def", "abc def")
    ]

test_rstrip :: [Test]
test_rstrip =
  mapassertEqual
    "rstrip"
    rstrip
    [ ("", ""),
      ("a", "a"),
      (" a ", " a"),
      ("abas  ", "abas"),
      ("fdsa \n\t", "fdsa"),
      ("abc def", "abc def")
    ]

test_strip :: [Test]
test_strip =
  mapassertEqual
    "strip"
    strip
    [ ("", ""),
      ("a", "a"),
      (" a ", "a"),
      ("abas  ", "abas"),
      ("  abas", "abas"),
      ("asdf\n\t ", "asdf"),
      ("\nbas", "bas"),
      ("abc def", "abc def")
    ]

test_splitWs :: [Test]
test_splitWs =
  let f exp' inp = TestCase $ exp' @=? splitWs inp
   in [ f [] "    ",
        f [] "",
        f ["asdf"] " asdf\n",
        f ["one", "two", "three"] "  one\ntwo \tthree \n"
      ]

test_escapeRe :: [Test]
test_escapeRe =
  map
    ( \i ->
        TestLabel (show $ chr i) $
          TestCase $
            assertEqual
              [chr i]
              (Just [])
              (matchRegex (mkRegex $ escapeRe [chr i]) [chr i])
    )
    [1 .. 255]
    ++ [ TestCase $
           assertEqual
             "big string"
             (Just ([], teststr, [], []))
             (matchRegexAll (mkRegex $ escapeRe teststr) teststr)
       ]
  where
    teststr = map chr [1 .. 255]

tests :: Test
tests =
  TestList
    [ TestLabel "lstrip" (TestList test_lstrip),
      TestLabel "rstrip" $ TestList test_rstrip,
      TestLabel "strip" $ TestList test_strip,
      TestLabel "splitWs" $ TestList test_splitWs,
      TestLabel "escapeRe" $ TestList test_escapeRe
    ]
