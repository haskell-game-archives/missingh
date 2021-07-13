{-# LANGUAGE LambdaCase #-}

{- arch-tag: HVFS instance helpers
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

-- |
--   Module     : System.IO.HVFS.InstanceHelpers
--   Copyright  : Copyright (C) 2004-2011 John Goerzen
--   SPDX-License-Identifier: BSD-3-Clause
--
--   Stability  : provisional
--   Portability: portable
--
-- Utilities for creating instances of the items defined in
-- "System.IO.HVFS".
module System.IO.HVFS.InstanceHelpers
  ( -- * HVFSStat objects
    SimpleStat (..),

    -- * HVFS objects & types

    -- ** MemoryVFS
    MemoryVFS,
    newMemoryVFS,
    newMemoryVFSRef,
    MemoryNode,
    MemoryEntry (..),

    -- * Utilities
    niceSlice,
    getFullPath,
    getFullSlice,
  )
where

import Data.Functor
import Data.IORef
import Data.List
import System.FilePath
import System.IO
import System.IO.Error
import System.IO.HVFS
import System.IO.HVIO
import System.Path
import System.Path.NameManip

-- | A simple "System.IO.HVFS.HVFSStat"
-- class that assumes that everything is either a file
-- or a directory.
data SimpleStat = SimpleStat
  { -- | True if file, False if directory
    isFile :: Bool,
    -- | Set to 0 if unknown or a directory
    fileSize :: FileOffset
  }
  deriving (Show, Eq)

instance HVFSStat SimpleStat where
  vIsRegularFile x = isFile x
  vIsDirectory x = not (isFile x)
  vFileSize x = fileSize x

----------------------------------------------------------------------
-- In-Memory Tree Types
----------------------------------------------------------------------

-- | The basic node of a 'MemoryVFS'.  The String corresponds to the filename,
-- and the entry to the contents.
type MemoryNode = (String, MemoryEntry)

-- | The content of a file or directory in a 'MemoryVFS'.
data MemoryEntry
  = MemoryDirectory [MemoryNode]
  | MemoryFile String
  deriving (Eq, Show)

-- | An in-memory read\/write filesystem.  Think of it as a dynamically
-- resizable ramdisk written in Haskell.
data MemoryVFS = MemoryVFS
  { content :: IORef [MemoryNode],
    cwd :: IORef FilePath
  }

instance Show MemoryVFS where
  show _ = "<MemoryVFS>"

-- | Create a new 'MemoryVFS' object from an existing tree.
-- An empty filesystem may be created by using @[]@ for the parameter.
newMemoryVFS :: [MemoryNode] -> IO MemoryVFS
newMemoryVFS s = do
  r <- newIORef s
  newMemoryVFSRef r

-- | Create a new 'MemoryVFS' object using an IORef to an
-- existing tree.
newMemoryVFSRef :: IORef [MemoryNode] -> IO MemoryVFS
newMemoryVFSRef r = do
  c <- newIORef [pathSeparator]
  return (MemoryVFS {content = r, cwd = c})

-- | Similar to 'System.Path.NameManip' but the first element
-- won't be @\/@.
--
-- >niceSlice "/" -> []
-- >niceSlice "/foo/bar" -> ["foo", "bar"]
niceSlice :: String -> [String]
niceSlice path
  | path == [pathSeparator] = []
  | otherwise =
    let sliced1 = slicePath path
        h = head sliced1
        t = tail sliced1
        newh = if isPathSeparator (head h) then tail h else h
        sliced2 = newh : t
     in sliced2

-- | Gets a full path, after investigating the cwd.
getFullPath :: HVFS a => a -> String -> IO String
getFullPath fs path = do
  cwd' <- vGetCurrentDirectory fs
  case absNormPath cwd' path of
    Nothing ->
      vRaiseError
        fs
        doesNotExistErrorType
        ("Trouble normalizing path " ++ path)
        (Just (cwd' </> path))
    Just newpath -> return newpath

-- | Gets the full path via 'getFullPath', then splits it via 'niceSlice'.
getFullSlice :: HVFS a => a -> String -> IO [String]
getFullSlice fs fp = do
  newpath <- getFullPath fs fp
  return (niceSlice newpath)

-- | Find an element on the tree, assuming a normalized path
findMelem :: MemoryVFS -> String -> IO MemoryEntry
findMelem x path
  | path == [pathSeparator] = readIORef (content x) <&> MemoryDirectory
  | otherwise =
    let sliced1 = slicePath path
        h = head sliced1
        t = tail sliced1
        newh = if (h /= [pathSeparator]) && isPathSeparator (head h) then tail h else h
        sliced2 = newh : t

        -- Walk the tree
        walk :: MemoryEntry -> [String] -> Either String MemoryEntry
        -- Empty list -- return the item we have
        walk y zs
          | null zs = Right y
          | zs == [[pathSeparator]] = Right y
          | otherwise = case y of
            MemoryFile _ -> Left $ "Attempt to look up name " ++ head zs ++ " in file"
            MemoryDirectory y' ->
              let newentry = case lookup (head zs) y' of
                    Nothing -> Left $ "Couldn't find entry " ++ head zs
                    Just a -> Right a
               in do
                    newobj <- newentry
                    walk newobj (tail zs)
     in do
          c <- readIORef $ content x
          case walk (MemoryDirectory c) sliced2 of
            Left err -> vRaiseError x doesNotExistErrorType err Nothing
            Right result -> return result

-- | Find an element on the tree, normalizing the path first
getMelem :: MemoryVFS -> String -> IO MemoryEntry
getMelem x s = do
  base <- readIORef $ cwd x
  case absNormPath base s of
    Nothing ->
      vRaiseError
        x
        doesNotExistErrorType
        ("Trouble normalizing path " ++ s)
        (Just s)
    Just newpath -> findMelem x newpath

instance HVFS MemoryVFS where
  vGetCurrentDirectory x = readIORef $ cwd x

  vSetCurrentDirectory x fp = do
    curpath <- vGetCurrentDirectory x
    -- Make sure new dir is valid
    getMelem x fp >>= \case
      MemoryFile _ ->
        vRaiseError
          x
          doesNotExistErrorType
          ("Attempt to cwd to non-directory " ++ fp)
          (Just fp)
      MemoryDirectory _ ->
        case absNormPath curpath fp of
          Nothing ->
            -- should never happen due to above getMelem call
            vRaiseError x illegalOperationErrorType "Bad internal error" (Just fp)
          Just y -> writeIORef (cwd x) y

  vGetFileStatus x fp = do
    getMelem x fp >>= \case
      MemoryFile y ->
        return $
          HVFSStatEncap $
            SimpleStat {isFile = True, fileSize = genericLength y}
      MemoryDirectory _ ->
        return $
          HVFSStatEncap $
            SimpleStat {isFile = False, fileSize = 0}

  vGetDirectoryContents x fp = do
    getMelem x fp >>= \case
      MemoryFile _ -> vRaiseError x doesNotExistErrorType "Can't list contents of a file" (Just fp)
      MemoryDirectory c -> return $ map fst c

instance HVFSOpenable MemoryVFS where
  vOpen x fp ReadMode = do
    getMelem x fp >>= \case
      MemoryDirectory _ -> vRaiseError x doesNotExistErrorType "Can't open a directory" (Just fp)
      MemoryFile y -> (newStreamReader y <&> HVFSOpenEncap)
  vOpen x fp _ = vRaiseError x permissionErrorType "Only ReadMode is supported with MemoryVFS files" (Just fp)
