{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}

{- arch-tag: Sendmail utility
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

-- |
--   Module     : Network.Email.Sendmail
--   Copyright  : Copyright (C) 2004-2011 John Goerzen
--   SPDX-License-Identifier: BSD-3-Clause
--
--   Stability  : provisional
--   Portability: portable
--
-- This Haskell module provides an interface to transmitting a mail message.
--
-- This is not compatible with Windows at this time.
--
-- Written by John Goerzen, jgoerzen\@complete.org

#if (defined(mingw32_HOST_OS) || defined(mingw32_TARGET_OS) || defined(__MINGW32__))
module Network.Email.Sendmail where
#else
module Network.Email.Sendmail (sendmail) where

import qualified Control.Exception
import System.Cmd.Utils
import System.Directory
import System.IO

sendmails :: [String]
sendmails =
  [ "/usr/sbin/sendmail",
    "/usr/local/sbin/sendmail",
    "/usr/local/bin/sendmail",
    "/usr/bin/sendmail",
    "/etc/sendmail",
    "/usr/etc/sendmail"
  ]

findsendmail :: IO String
findsendmail =
  let worker [] = return "sendmail"
      worker (this : next) = do
        e <- doesFileExist this
        if e
          then do
            p <- getPermissions this
            if executable p
              then return this
              else worker next
          else worker next
   in worker sendmails

-- | Transmits an e-mail message using the system's mail transport agent.
--
-- This function takes a message, a list of recipients, and an optional sender,
-- and transmits it using the system's MTA, sendmail.
--
-- If @sendmail@ is on the @PATH@, it will be used; otherwise, a list of system
-- default locations will be searched.
--
-- A failure will be logged, since this function uses 'System.Cmd.Utils.safeSystem'
-- internally.
--
-- This function will first try @sendmail@.  If it does not exist, an error is
-- logged under @System.Cmd.Utils.pOpen3@ and various default @sendmail@ locations
-- are tried.  If that still fails, an error is logged and an exception raised.
sendmail ::
  -- | The envelope from address.  If not specified, takes the system's default, which is usually based on the effective userid of the current process.  This is not necessarily what you want, so I recommend specifying it.
  Maybe String ->
  -- | A list of recipients for your message.  An empty list is an error.
  [String] ->
  -- | The message itself.
  String ->
  IO ()
sendmail _ [] _ = fail "sendmail: no recipients specified"
sendmail Nothing recipients msg = sendmailWorker recipients msg
sendmail (Just from) recipients msg = sendmailWorker (("-f" ++ from) : recipients) msg

sendmailWorker :: [String] -> String -> IO ()
sendmailWorker args msg =
  let func h = hPutStr h msg
   in do
        --pOpen WriteToPipe "/usr/sbin/sendmail" args func
        rv <- Control.Exception.try (pOpen WriteToPipe "sendmail" args func)
        case rv of
          Right x -> return x
          Left (_ :: Control.Exception.IOException) -> do
            sn <- findsendmail
            r <- pOpen WriteToPipe sn args func
            return $! r

#endif
