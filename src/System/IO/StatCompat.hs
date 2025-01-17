{-# LANGUAGE CPP #-}

{-
Copyright (c) 2005-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

-- |
--   Module     : System.IO.StatCompat
--   Copyright  : Copyright (C) 2005-2011 John Goerzen
--   SPDX-License-Identifier: BSD-3-Clause
--
--   Stability  : provisional
--   Portability: portable
--
-- Provide a stat-like structure for use in MissingH.  Especially
-- useful with HVFS and on Windows.  See also "System.IO.WindowsCompat".
module System.IO.StatCompat where

import System.Posix.Consts
import System.Posix.Types

#if !(defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
import           System.Posix.Files  (intersectFileModes)
#endif

#if (defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
type LinkCount = Int
type UserID = Int
type GroupID = Int
#endif

data FileStatusCompat = FileStatusCompat
  { deviceID :: DeviceID,
    fileID :: FileID,
    fileMode :: FileMode,
    linkCount :: LinkCount,
    fileOwner :: UserID,
    fileGroup :: GroupID,
    specialDeviceID :: DeviceID,
    fileSize :: FileOffset,
    accessTime :: EpochTime,
    modificationTime :: EpochTime,
    statusChangeTime :: EpochTime
  }

scHelper :: FileMode -> FileStatusCompat -> Bool
scHelper comp stat = (fileMode stat `intersectFileModes` fileTypeModes) == comp

isBlockDevice, isCharacterDevice, isNamedPipe, isRegularFile, isDirectory, isSymbolicLink, isSocket :: FileStatusCompat -> Bool
isBlockDevice = scHelper blockSpecialMode
isCharacterDevice = scHelper characterSpecialMode
isNamedPipe = scHelper namedPipeMode
isRegularFile = scHelper regularFileMode
isDirectory = scHelper directoryMode
isSymbolicLink = scHelper symbolicLinkMode
isSocket = scHelper socketMode

#if (defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
intersectFileModes :: FileMode -> FileMode -> FileMode
intersectFileModes m1 m2 = m1 .&. m2
#endif
