{- arch-tag: Generic Server Support
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

-- |
--   Module     : Network.SocketServer
--   Copyright  : Copyright (C) 2004-2011 John Goerzen
--   SPDX-License-Identifier: BSD-3-Clause
--
--   Stability  : experimental
--   Portability: systems with networking
--
-- This module provides an infrastructure to simplify server design.
--
-- Written by John Goerzen, jgoerzen\@complete.org
--
-- Please note: this module is designed to work with TCP, UDP, and Unix domain
-- sockets, but only TCP sockets have been tested to date.
--
-- This module is presently under-documented.  For an example of usage, please
-- see the description of "Network.FTP.Server".
module Network.SocketServer
  ( -- * Generic Options and Types
    InetServerOptions (..),
    simpleTCPOptions,
    SocketServer (..),
    HandlerT,

    -- * TCP server convenient setup
    serveTCPforever,

    -- * Lower-Level Processing
    setupSocketServer,
    handleOne,
    serveForever,
    closeSocketServer,

    -- * Combinators
    loggingHandler,
    threadedHandler,
    handleHandler,
  )
where

import Control.Concurrent
import Network.BSD
import Network.Socket
import Network.Utils
import System.IO
import qualified System.Log.Logger

-- | Options for your server.
data InetServerOptions = InetServerOptions
  { listenQueueSize :: Int,
    portNumber :: PortNumber,
    interface :: HostAddress,
    reuse :: Bool,
    family :: Family,
    sockType :: SocketType,
    protoStr :: String
  }
  deriving (Eq, Show)

-- | The main handler type.
--
-- The first parameter is the socket itself.
--
-- The second is the address of the remote endpoint.
--
-- The third is the address of the local endpoint.
type HandlerT = Socket -> SockAddr -> SockAddr -> IO ()

-- | Get Default options.  You can always modify it later.
simpleTCPOptions ::
  -- | Port Number
  Int ->
  InetServerOptions
simpleTCPOptions p =
  InetServerOptions
    { listenQueueSize = 5,
      portNumber = fromIntegral p,
      interface = 0,
      reuse = False,
      family = AF_INET,
      sockType = Stream,
      protoStr = "tcp"
    }

data SocketServer = SocketServer
  { optionsSS :: InetServerOptions,
    sockSS :: Socket
  }
  deriving (Eq, Show)

-- | Takes some options and sets up the 'SocketServer'.  I will bind
-- and begin listening, but will not accept any connections itself.
setupSocketServer :: InetServerOptions -> IO SocketServer
setupSocketServer opts = do
  proto <- getProtocolNumber (protoStr opts)
  s <- socket (family opts) (sockType opts) proto
  setSocketOption
    s
    ReuseAddr
    (if reuse opts then 1 else 0)
  bind
    s
    ( SockAddrInet
        (portNumber opts)
        (interface opts)
    )
  listen s (listenQueueSize opts)
  return $ SocketServer {optionsSS = opts, sockSS = s}

-- | Close the socket server.  Does not terminate active
-- handlers, if any.
closeSocketServer :: SocketServer -> IO ()
closeSocketServer = close . sockSS

-- | Handle one incoming request from the given 'SocketServer'.
handleOne :: SocketServer -> HandlerT -> IO ()
handleOne ss func = do
  a <- accept (sockSS ss)
  localaddr <- getSocketName (fst a)
  uncurry func a localaddr

-- | Handle all incoming requests from the given 'SocketServer'.
serveForever :: SocketServer -> HandlerT -> IO ()
serveForever ss = sequence_ . repeat . handleOne ss

-- | Convenience function to completely set up a TCP
-- 'SocketServer' and handle all incoming requests.
--
-- This function is literally this:
--
-- >serveTCPforever options func =
-- >    do sockserv <- setupSocketServer options
-- >       serveForever sockserv func
serveTCPforever ::
  -- | Server options
  InetServerOptions ->
  -- | Handler function
  HandlerT ->
  IO ()
serveTCPforever options func = do
  sockserv <- setupSocketServer options
  serveForever sockserv func

----------------------------------------------------------------------
-- Combinators
----------------------------------------------------------------------

-- | Log each incoming connection using the interface in
-- "System.Log.Logger".
--
-- Log when the incoming connection disconnects.
--
-- Also, log any failures that may occur in the child handler.
loggingHandler ::
  -- | Name of logger to use
  String ->
  -- | Priority of logged messages
  System.Log.Logger.Priority ->
  -- | Handler to call after logging
  HandlerT ->
  -- | Resulting handler
  HandlerT
loggingHandler hname prio nexth socket' r_sockaddr l_sockaddr = do
  sockStr <- showSockAddr r_sockaddr
  System.Log.Logger.logM
    hname
    prio
    ("Received connection from " ++ sockStr)
  System.Log.Logger.traplogging
    hname
    System.Log.Logger.WARNING
    ""
    ( nexth
        socket'
        r_sockaddr
        l_sockaddr
    )
  System.Log.Logger.logM
    hname
    prio
    ("Connection " ++ sockStr ++ " disconnected")

-- | Handle each incoming connection in its own thread to
-- make the server multi-tasking.
threadedHandler ::
  -- | Handler to call in the new thread
  HandlerT ->
  -- | Resulting handler
  HandlerT
threadedHandler nexth socket' r_sockaddr l_sockaddr = do
  _ <- forkIO (nexth socket' r_sockaddr l_sockaddr)
  return ()

-- | Give your handler function a Handle instead of a Socket.
--
-- The Handle will be opened with ReadWriteMode (you use one handle for both
-- directions of the Socket).  Also, it will be initialized with LineBuffering.
--
-- Unlike other handlers, the handle will be closed when the function returns.
-- Therefore, if you are doing threading, you should to it before you call this
-- handler.
handleHandler ::
  -- | Handler to call
  (Handle -> SockAddr -> SockAddr -> IO ()) ->
  HandlerT
handleHandler func socket' r_sockaddr l_sockaddr = do
  h <- socketToHandle socket' ReadWriteMode
  hSetBuffering h LineBuffering
  func h r_sockaddr l_sockaddr
  hClose h
