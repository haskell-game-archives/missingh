{-# LANGUAGE Safe #-}

{-
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

-- |
--   Module     : Data.Map.Utils
--   Copyright  : Copyright (C) 2004-2011 John Goerzen
--   SPDX-License-Identifier: BSD-3-Clause
--
--   Stability  : provisional
--   Portability: portable
--
-- This module provides various helpful utilities for dealing with Data.Maps.
--
-- Written by John Goerzen, jgoerzen\@complete.org
module Data.Map.Utils
  ( -- * Basic Utilities
    flipM,
    flippedLookupM,
    forceLookupM,

    -- * Conversions
    strToM,
    strFromM,
  )
where

import Data.List.Utils
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

-- | Converts a String, String Map into a string representation.
-- See 'Data.List.Utils.strFromAL' for more on the similar function for
-- association lists.  This implementation is simple:
--
-- >strFromM = strFromAL . Map.toList
--
-- This function is designed to work with Map String String objects,
-- but may also work with other objects with simple representations.
strFromM :: (Show a, Show b, Ord a) => Map a b -> String
strFromM = strFromAL . Map.toList

-- | Converts a String into a String, String Map.  See
-- 'Data.List.Utils.strToAL' for more on the similar function for association
-- lists.
--
-- This implementation is simple:
--
-- >strToM = Map.fromList . strToAL
--
-- This function is designed to work with Map String String objects,
-- but may work with other key\/value combinations if they have simple
-- representations.
strToM :: (Read a, Read b, Ord a) => String -> Map a b
strToM = Map.fromList . strToAL

-- | Flips a Map.  See 'Data.List.Utils.flipAL' for more on the similar
-- function for lists.
flipM :: (Ord key, Ord val) => Map key val -> Map val [key]
flipM = Map.fromList . flipAL . Map.toList

-- | Returns a list of all keys in the Map whose value matches the
-- parameter. If the value does not occur in the Map, the empty
-- list is returned.
flippedLookupM :: (Ord val, Ord key) => val -> Map key val -> [key]
flippedLookupM v fm = fromMaybe [] (Map.lookup v (flipM fm))

-- | Performs a lookup, and raises an exception (with an error message
-- prepended with the given string) if the key could not be found.
forceLookupM ::
  (Show key, Ord key) =>
  String ->
  key ->
  Map key elt ->
  elt
forceLookupM msg k fm =
  case Map.lookup k fm of
    Just x -> x
    Nothing -> error $ msg ++ ": could not find key " ++ show k
