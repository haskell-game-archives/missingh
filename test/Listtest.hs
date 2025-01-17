{-# LANGUAGE TypeApplications #-}
{- arch-tag: List tests main file
Copyright (C) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

module Listtest (tests) where

import Data.List
import Data.List.Utils
import Test.HUnit

test_delFromAL :: [Test]
test_delFromAL =
  let f :: [(String, Int)] -> [(String, Int)] -> Test
      f inp exp' = TestCase $ exp' @=? delFromAL inp "testkey"
   in [ f [] [],
        f [("one", 1)] [("one", 1)],
        f [("1", 1), ("2", 2), ("testkey", 3)] [("1", 1), ("2", 2)],
        f [("testkey", 1)] [],
        f [("testkey", 1), ("testkey", 2)] [],
        f [("testkey", 1), ("2", 2), ("3", 3)] [("2", 2), ("3", 3)],
        f
          [("testkey", 1), ("2", 2), ("testkey", 3), ("4", 4)]
          [("2", 2), ("4", 4)]
      ]

test_addToAL :: [Test]
test_addToAL =
  let f :: [(String, Int)] -> [(String, Int)] -> Test
      f inp exp' = TestCase $ exp' @=? addToAL inp "testkey" 101
   in [ f [] [("testkey", 101)],
        f [("testkey", 5)] [("testkey", 101)],
        f [("testkey", 5), ("testkey", 6)] [("testkey", 101)]
      ]

test_split :: [Test]
test_split =
  let f delim inp exp' = TestCase $ exp' @=? split delim inp
   in [ f "," "foo,bar,,baz," ["foo", "bar", "", "baz", ""],
        f "ba" ",foo,bar,,baz," [",foo,", "r,,", "z,"],
        f "," "" [],
        f "," "," ["", ""]
      ]

test_join :: [Test]
test_join =
  let f :: (Eq a, Show a) => [a] -> [[a]] -> [a] -> Test
      f delim inp exp' = TestCase $ exp' @=? join delim inp
   in [ f "|" ["foo", "bar", "baz"] "foo|bar|baz",
        f "|" [] "",
        f "|" ["foo"] "foo"
      ]

test_replace :: [Test]
test_replace =
  let f old new inp exp' = TestCase $ exp' @=? replace old new inp
   in [ f "" "" "" "",
        f "foo" "bar" "" "",
        f "foo" "bar" "foo" "bar",
        f "foo" "bar" "footestfoothisisabarfoo" "bartestbarthisisabarbar",
        f "," ", " "1,2,3,4" "1, 2, 3, 4",
        f "," "." "1,2,3,4" "1.2.3.4"
      ]

test_genericJoin :: [Test]
test_genericJoin =
  let
    f :: (Eq a, Show a) => String -> [a] -> String -> Test
    f delim inp exp' = TestCase $ exp' @=? genericJoin delim inp
  in [ f @Int ", " [1, 2, 3, 4] "1, 2, 3, 4",
       f @Int ", " [] "",
       f "|" ["foo", "bar", "baz"] "\"foo\"|\"bar\"|\"baz\"",
       f @Int ", " [5] "5"
     ]

test_flipAL :: [Test]
test_flipAL =
  let f inp exp' = TestCase $ exp' @=? flipAL inp
   in [ f ([] :: [(Int, Int)]) ([] :: [(Int, [Int])]),
        f [("a", "b")] [("b", ["a"])],
        f
          [ ("a", "b"),
            ("c", "b"),
            ("d", "e"),
            ("b", "b")
          ]
          [ ("b", ["b", "c", "a"]),
            ("e", ["d"])
          ]
      ]

test_uniq :: [Test]
test_uniq =
  let f inp exp' = TestCase $ exp' @=? uniq inp
   in [ f ([] :: [Int]) [],
        f "asdf" "asdf",
        f "aabbcc" "abc",
        f "abcabc" "abc",
        f "aaaaaa" "a",
        f "aaaaaab" "ab",
        f "111111111111111" "1",
        f "baaaaaaaaa" "ba",
        f "baaaaaaaaab" "ba",
        f "aaacccdbbbefff" "acdbef",
        f "foo" "fo",
        f "15553344409" "153409",
        f "Mississippi" "Misp"
      ]

test_trunc :: [Test]
test_trunc =
  let
    f :: (Eq a, Show a) => Int -> [a] -> [a] -> Test
    f len inp exp' = TestCase $ exp' @=? take len inp
   in [ f 2 "Hello" "He",
        f 1 "Hello" "H",
        f 0 "Hello" "",
        f 2 "H" "H",
        f 2 "" "",
        f @Int 2 [1, 2, 3, 4, 5] [1, 2],
        f 10 "Hello" "Hello",
        f 0 "" ""
      ]

test_contains :: [Test]
test_contains =
  let f msg sub testlist exp' = TestCase $ assertEqual msg exp' (sub `isInfixOf` testlist)
   in [ f "t1" "Haskell" "I really like Haskell." True,
        f "t2" "" "Foo" True,
        f "t3" "" "" True,
        f "t4" "Hello" "" False,
        f "t5" "Haskell" "Haskell" True,
        f "t6" "Haskell" "1Haskell" True,
        f "t7" "Haskell" "Haskell1" True,
        f "t8" "Haskell" "Ocaml" False,
        f "t9" "Haskell" "OCamlasfasfasdfasfd" False,
        f "t10" "a" "Hello" False,
        f "t11" "e" "Hello" True
      ]

test_elemRIndex :: [Test]
test_elemRIndex =
  let f item inp exp' = TestCase $ exp' @=? elemRIndex item inp
   in [ f "foo" [] Nothing,
        f "foo" ["bar", "baz"] Nothing,
        f "foo" ["foo"] (Just 0),
        f "foo" ["foo", "bar"] (Just 0),
        f "foo" ["bar", "foo"] (Just 1),
        f "foo" ["foo", "bar", "foo", "bar", "foo"] (Just 4),
        f 'f' ['f', 'b', 'f', 'f', 'b'] (Just 3),
        f 'f' ['b', 'b', 'f'] (Just 2)
      ]

test_alwaysElemRIndex :: [Test]
test_alwaysElemRIndex =
  let f item inp exp' = TestCase $ exp' @=? alwaysElemRIndex item inp
   in [ f "foo" [] (-1),
        f 'f' ['b', 'q'] (-1),
        f 'f' ['f', 'b', 'f', 'f', 'b'] 3
      ]

test_subIndex :: [Test]
test_subIndex =
  let f item inp exp' = TestCase $ exp' @=? subIndex item inp
   in [ f "foo" "asdfoobar" (Just 3),
        f "foo" [] Nothing,
        f "" [] (Just 0),
        f "" "asdf" (Just 0),
        f "test" "asdftestbartest" (Just 4),
        f [1 :: Int, 2] [0, 5, 3, 2, 1, 2, 4] (Just 4)
      ]

test_fixedWidth :: [Test]
test_fixedWidth =
  let 
    f :: (Eq a, Show a) => [Int] -> [a] -> [[a]] -> Test
    f inplen inplist exp' =
        TestLabel (show inplen ++ ", " ++ show inplist) $
          TestCase $
            wholeMap (fixedWidth inplen) inplist @=? exp'
   in [ f @Int [] [] [],
        f @Int [1] [5] [[5]],
        f @Int [1] [3, 4, 5, 6] [[3], [4, 5, 6]],
        f @Int [1] [] [],
        f @Int [2] [3] [[3]],
        f @Int [2] [3, 4, 5, 6] [[3, 4], [5, 6]],
        f @Int [2] [3, 4, 5] [[3, 4], [5]],
        f [1, 2, 3] "1234567890" ["1", "23", "456", "7890"],
        f (repeat 2) "123456789" ["12", "34", "56", "78", "9"],
        f [] "123456789" ["123456789"],
        f
          [5, 3, 6, 1]
          "Hello, This is a test."
          ["Hello", ", T", "his is", " ", "a test."]
      ]

test_strToAL :: [Test]
test_strToAL =
  let f inp exp' = TestLabel (show inp) $
        TestCase $ do
          let r = strFromAL inp
          exp' @=? r
          inp @=? strToAL r
   in [ f ([] :: [(String, String)]) "",
        f [("foo", "bar")] "\"foo\",\"bar\"\n",
        f [("foo", "bar"), ("baz", "quux")] "\"foo\",\"bar\"\n\"baz\",\"quux\"\n",
        f [(1 :: Int, 2 :: Int), (3, 4)] "1,2\n3,4\n",
        f [(1 :: Int, "one"), (2, "two")] "1,\"one\"\n2,\"two\"\n",
        f
          [("one", 1 :: Double), ("n\nl", 2 :: Double)]
          "\"one\",1.0\n\"n\\nl\",2.0\n"
      ]

test_spanList :: [Test]
test_spanList =
  let f func inp exp' = TestLabel (show inp) $ TestCase $ exp' @=? spanList func inp
   in [ f (isInfixOf "foo") "Testfoobar" ("Testf", "oobar"),
        f (const True) "Testasdf" ("Testasdf", ""),
        f (const False) "Testasdf" ("", "Testasdf"),
        f (isInfixOf "foo") "" ("", ""),
        f (isInfixOf "foo") "foo" ("f", "oo")
      ]

tests :: Test
tests =
  TestList
    [ TestLabel "delFromAL" (TestList test_delFromAL),
      TestLabel "uniq" (TestList test_uniq),
      TestLabel "addToAL" (TestList test_addToAL),
      TestLabel "split" (TestList test_split),
      TestLabel "join" (TestList test_join),
      TestLabel "genericJoin" (TestList test_genericJoin),
      TestLabel "trunc" (TestList test_trunc),
      TestLabel "flipAL" (TestList test_flipAL),
      TestLabel "elemRIndex" (TestList test_elemRIndex),
      TestLabel "alwaysElemRIndex" (TestList test_alwaysElemRIndex),
      TestLabel "replace" (TestList test_replace),
      TestLabel "contains" (TestList test_contains),
      TestLabel "strFromAL & strToAL" (TestList test_strToAL),
      TestLabel "fixedWidth" (TestList test_fixedWidth),
      TestLabel "subIndex" (TestList test_subIndex),
      TestLabel "spanList" (TestList test_spanList)
    ]
