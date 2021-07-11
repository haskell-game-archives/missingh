{-# LANGUAGE LambdaCase #-}

{- arch-tag: HVIO main file
Copyright (c) 2004-2011 John Goerzen <jgoerzen@complete.org>

All rights reserved.

For license and copyright information, see the file LICENSE
-}

{- |
   Module     : System.IO.HVIO
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   SPDX-License-Identifier: BSD-3-Clause

   Stability  : provisional
   Portability: portable

Haskell Virtual I\/O -- a system to increase the flexibility of input and
output in Haskell

Copyright (c) 2004-2005 John Goerzen, jgoerzen\@complete.org

HVIO provides the following general features:

 * The ability to use a single set of functions on various different
   types of objects, including standard Handles, in-memory buffers,
   compressed files, network data streams, etc.

 * The ability to transparently add filters to the I\/O process.
   These filters could include things such as character set conversions,
   compression or decompression of a data stream, and more.

 * The ability to define new objects that have the properties
   of I\/O objects and can be used interchangably with them.

 * Specification compatibility with, and complete support for,
   existing I\/O on Handles.

 * Provide easier unit testing capabilities for I\/O actions

HVIO defines several basic type classes that you can use.  You will mostly
be interested in 'HVIO'.

It's trivial to adapt old code to work with HVIO.  For instance, consider
this example of old and new code:

>printMsg :: Handle -> String -> IO ()
>printMsg h msg = hPutStr h ("msg: " ++ msg)

And now, the new way:

>printMsg :: HVIO h => h -> String -> IO ()
>printMsg h msg = vPutStr h ("msg: " ++ msg)

There are several points to note about this conversion:

 * The new method can still accept a Handle in exactly the same way as
   the old method.  Changing your functions to use HVIO will require no
   changes from functions that call them with Handles.

 * Most \"h\" functions have equivolent \"v\" functions that operate
   on HVIO classes instead of the more specific Handle.  The \"v\" functions
   behave identically to the \"h\" functions whenever possible.

 * There is no equivolent of \"openFile\" in any HVIO class.  You must
   create your Handle (or other HVIO object) using normal means.
   This is because the creation is so different that it cannot be standardized.

In addition to Handle, there are several pre-defined classes for your use.
'StreamReader' is a particularly interesting one.  At creation time, you pass
it a String.  Its contents are read lazily whenever a read call is made.  It
can be used, therefore, to implement filters (simply initialize it with the
result from, say, a map over hGetContents from another HVIO object), codecs,
and simple I\/O testing.  Because it is lazy, it need not hold the entire
string in memory.  You can create a 'StreamReader' with a call to
'newStreamReader'.

'MemoryBuffer' is a similar class, but with a different purpose.  It provides
a full interface like Handle (it implements 'HVIOReader', 'HVIOWriter',
and 'HVIOSeeker').  However, it maintains an in-memory buffer with the
contents of the file, rather than an actual on-disk file.  You can access
the entire contents of this buffer at any time.  This can be quite useful
for testing I\/O code, or for cases where existing APIs use I\/O, but you
prefer a String representation.  You can create a 'MemoryBuffer' with a call
to 'newMemoryBuffer'.

Finally, there are pipes.  These pipes are analogous to the Unix
pipes that are available from System.Posix, but don't require Unix and work
only in Haskell.  When you create a pipe, you actually get two HVIO objects:
a 'PipeReader' and a 'PipeWriter'.  You must use the 'PipeWriter' in one
thread and the 'PipeReader' in another thread.  Data that's written to the
'PipeWriter' will then be available for reading with the 'PipeReader'.  The
pipes are implemented completely with existing Haskell threading primitives,
and require no special operating system support.  Unlike Unix pipes, these
pipes cannot be used across a fork().  Also unlike Unix pipes, these pipes
are portable and interact well with Haskell threads.  A new pipe can be created
with a call to 'newHVIOPipe'.

Together with "System.IO.HVFS", this module is part of a complete
virtual filesystem solution.
-}

module System.IO.HVIO(-- * Implementation Classes
                     HVIO(..),
                     -- * Standard HVIO Implementations

                     -- ** Handle
                     -- | Handle is a member of 'HVIO'.

                     -- ** Stream Reader
                     StreamReader, newStreamReader,

                     -- ** Memory Buffer
                     MemoryBuffer, newMemoryBuffer,
                     mbDefaultCloseFunc, getMemoryBuffer,

                     -- ** Haskell Pipe
                     PipeReader, PipeWriter, newHVIOPipe
                    )
where

import           Control.Concurrent.MVar
import Control.Exception
import           Data.IORef
import           Foreign.C
import           Foreign.Ptr
import           Foreign.Storable
import           System.IO
import           System.IO.Error
import  Data.Functor
import Control.Monad
import Data.Bifunctor

{- | This is the generic I\/O support class.  All objects that are to be used
in the HVIO system must provide an instance of 'HVIO'.

Functions in this class provide an interface with the same specification as
the similar functions in System.IO.  Please refer to that documentation
for a more complete specification than is provided here.

Instances of 'HVIO' must provide 'vClose', 'vIsEOF', and either
'vIsOpen' or 'vIsClosed'.

Implementators of readable objects must provide at least 'vGetChar'
and 'vIsReadable'.
An implementation of 'vGetContents' is also highly suggested, since
the default cannot implement proper partial closing semantics.

Implementators of writable objects must provide at least 'vPutChar' and
'vIsWritable'.

Implementators of seekable objects must provide at least
'vIsSeekable', 'vTell', and 'vSeek'.
-}
class (Show a) => HVIO a where
    -- | Close a file
    vClose :: a -> IO ()

    -- | Test if a file is open
    vIsOpen :: a -> IO Bool
    vIsOpen h = vIsClosed h <&> not

    -- | Test if a file is closed
    vIsClosed :: a -> IO Bool
    vIsClosed h = vIsOpen h <&> not

    -- | Raise an error if the file is not open.
    -- This is a new HVIO function and is implemented in terms of
    -- 'vIsOpen'.
    vTestOpen :: a -> IO ()
    vTestOpen h = do
      e <- vIsClosed h
      when e $ vThrow h illegalOperationErrorType

    -- | Whether or not we're at EOF.  This may raise on exception
    -- on some items, most notably write-only Handles such as stdout.
    -- In general, this is most reliable on items opened for reading.
    -- vIsEOF implementations must implicitly call vTestOpen.
    vIsEOF :: a -> IO Bool

    -- | Detailed show output.
    vShow :: a -> IO String
    vShow = return . show

    -- | Make an IOError.
    vMkIOError :: a -> IOErrorType -> String -> Maybe FilePath -> IOError
    vMkIOError _ et desc = mkIOError et desc Nothing

    -- | Throw an IOError.
    vThrow :: a -> IOErrorType -> IO b
    vThrow h et = do
      fp <- vGetFP h
      ioError (vMkIOError h et "" fp)

    -- | Get the filename\/object\/whatever that this corresponds to.
    -- May be Nothing.
    vGetFP :: a -> IO (Maybe FilePath)
    vGetFP _ = return Nothing

    -- | Throw an isEOFError if we're at EOF; returns nothing otherwise.
    -- If an implementation overrides the default, make sure that it
    -- calls vTestOpen at some point.  The default implementation is
    -- a wrapper around a call to 'vIsEOF'.
    vTestEOF :: a -> IO ()
    vTestEOF h = do
      e <- vIsEOF h
      when e $ vThrow h eofErrorType

    -- | Read one character
    vGetChar :: a -> IO Char
    vGetChar h = vThrow h illegalOperationErrorType

    -- | Read one line
    vGetLine :: a -> IO String
    vGetLine h =
      let loop accum =
            let func = vGetChar h >>= \case
                          '\n' -> return accum
                          x    -> accum `seq` loop (accum ++ [x])
                handler e = if isEOFError e then return accum else ioError e
             in catch func handler
       in vGetChar h >>= \case
            '\n' -> return []
            x    -> loop [x]

    {- | Get the remaining contents.  Please note that as a user of this
       function, the same partial-closing semantics as are used in the
       standard 'hGetContents' are /encouraged/ from implementators,
       but are not /required/.  That means that, for instance,
       a 'vGetChar' after a 'vGetContents' may return some undefined
       result instead of the error you would normally get.  You should
       use caution to make sure your code doesn't fall into that trap,
       or make sure to test your code with Handle or one of the
       default instances defined in this module.  Also, some implementations
       may essentially provide a complete close after a call to 'vGetContents'.
       The bottom line: after a call to 'vGetContents', you should do nothing
       else with the object save closing it with 'vClose'.

       For implementators, you are highly encouraged to provide a correct
       implementation. -}
    vGetContents :: a -> IO String
    vGetContents h =
      let loop =
            let func = do
                  c <- vGetChar h
                  next <- loop
                  c `seq` return (c : next)
                handler e = if isEOFError e then return [] else ioError e
             in catch func handler
       in loop

    -- | Indicate whether at least one item is ready for reading.
    -- This will always be True for a great many implementations.
    vReady :: a -> IO Bool
    vReady h = do
      vTestEOF h
      return True

    -- | Indicate whether a particular item is available for reading.
    vIsReadable :: a -> IO Bool
    vIsReadable _ = return False

    -- | Write one character
    vPutChar :: a -> Char -> IO ()
    vPutChar h _ = vThrow h illegalOperationErrorType

    -- | Write a string
    vPutStr :: a -> String -> IO ()
    vPutStr _ [] = return ()
    vPutStr h (x:xs) = do
      vPutChar h x
      vPutStr h xs

    -- | Write a string with newline character after it
    vPutStrLn :: a -> String -> IO ()
    vPutStrLn h s = vPutStr h (s ++ "\n")

    -- | Write a string representation of the argument, plus a newline.
    vPrint :: Show b => a -> b -> IO ()
    vPrint h = vPutStrLn h . show

    -- | Flush any output buffers.
    -- Note: implementations should assure that a vFlush is automatically
    -- performed
    -- on file close, if necessary to ensure all data sent is written.
    vFlush :: a -> IO ()
    vFlush = vTestOpen

    -- | Indicate whether or not this particular object supports writing.
    vIsWritable :: a -> IO Bool
    vIsWritable _ = return False

    -- | Seek to a specific location.
    vSeek :: a -> SeekMode -> Integer -> IO ()
    vSeek h _ _ = vThrow h illegalOperationErrorType

    -- | Get the current position.
    vTell :: a -> IO Integer
    vTell h = vThrow h illegalOperationErrorType

    -- | Convenience function to reset the file pointer to the beginning
    -- of the file.  A call to @vRewind h@ is the
    -- same as @'vSeek' h AbsoluteSeek 0@.
    vRewind :: a -> IO ()
    vRewind h = vSeek h AbsoluteSeek 0

    -- | Indicate whether this instance supports seeking.
    vIsSeekable :: a -> IO Bool
    vIsSeekable _ = return False

    -- | Set buffering; the default action is a no-op.
    vSetBuffering :: a -> BufferMode -> IO ()
    vSetBuffering _ _ = return ()

    -- | Get buffering; the default action always returns NoBuffering.
    vGetBuffering :: a -> IO BufferMode
    vGetBuffering _ = return NoBuffering

    -- | Binary output: write the specified number of octets from the specified
    -- buffer location.
    vPutBuf :: a -> Ptr b -> Int -> IO ()
    vPutBuf h buf len = do
      str <- peekCStringLen (castPtr buf, len)
      vPutStr h str

    -- | Binary input: read the specified number of octets from the
    -- specified buffer location, continuing to read
    -- until it either consumes that much data or EOF is encountered.
    -- Returns the number of octets actually read.  EOF errors are never
    -- raised; fewer bytes than requested are returned on EOF.
    vGetBuf :: a -> Ptr b -> Int -> IO Int
    vGetBuf h b l = worker b l 0
      where
        worker _ 0 accum = return accum
        worker buf len accum = do
          iseof <- vIsEOF h
          if iseof
            then return accum
            else do
              c <- vGetChar h
              let cc = castCharToCChar c
              poke (castPtr buf) cc
              let newptr = plusPtr buf 1
              worker newptr (len - 1) (accum + 1)

----------------------------------------------------------------------
-- Handle instances
----------------------------------------------------------------------
instance HVIO Handle where
    vClose = hClose
    vIsEOF = hIsEOF
    vShow = hShow
    vMkIOError h et desc mfp =
        mkIOError et desc (Just h) mfp
    vGetChar = hGetChar
    vGetLine = hGetLine
    vGetContents = hGetContents
    vReady = hReady
    vIsReadable = hIsReadable
    vPutChar = hPutChar
    vPutStr = hPutStr
    vPutStrLn = hPutStrLn
    vPrint = hPrint
    vFlush = hFlush
    vIsWritable = hIsWritable
    vSeek = hSeek
    vTell = hTell
    vIsSeekable = hIsSeekable
    vSetBuffering = hSetBuffering
    vGetBuffering = hGetBuffering
    vGetBuf = hGetBuf
    vPutBuf = hPutBuf
    vIsOpen = hIsOpen
    vIsClosed = hIsClosed

----------------------------------------------------------------------
-- VIO Support
----------------------------------------------------------------------
type VIOCloseSupport a = IORef (Bool, a)

viocIsopen :: VIOCloseSupport a -> IO Bool
viocIsopen x = readIORef x <&> fst

viocGet :: VIOCloseSupport a -> IO a
viocGet x = readIORef x <&> snd

viocClose :: VIOCloseSupport a -> IO ()
viocClose x = modifyIORef x (\ (_, dat) -> (False, dat))

viocSet :: VIOCloseSupport a -> a -> IO ()
viocSet x newdat = modifyIORef x (\ (stat, _) -> (stat, newdat))

----------------------------------------------------------------------
-- Stream Readers
----------------------------------------------------------------------
{- | Simulate I\/O based on a string buffer.

When a 'StreamReader' is created, it is initialized based on the contents of
a 'String'.  Its contents are read lazily whenever a request is made to read
something from the 'StreamReader'.    It
can be used, therefore, to implement filters (simply initialize it with the
result from, say, a map over hGetContents from another HVIO object), codecs,
and simple I\/O testing.  Because it is lazy, it need not hold the entire
string in memory.  You can create a 'StreamReader' with a call to
'newStreamReader'.
 -}
newtype StreamReader = StreamReader (VIOCloseSupport String)

{- | Create a new 'StreamReader' object. -}
newStreamReader :: String            -- ^ Initial contents of the 'StreamReader'
                -> IO StreamReader
newStreamReader s = do
  ref <- newIORef (True, s)
  return (StreamReader ref)

srv :: StreamReader -> VIOCloseSupport String
srv (StreamReader x) = x

instance Show StreamReader where
    show _ = "<StreamReader>"

instance HVIO StreamReader where
    vClose = viocClose . srv
    vIsEOF h = do vTestOpen h
                  d <- viocGet (srv h)
                  return $ case d of
                                  [] -> True
                                  _  -> False
    vIsOpen = viocIsopen . srv
    vGetChar h = do vTestEOF h
                    c <- viocGet (srv h)
                    let retval = head c
                    viocSet (srv h) (tail c)
                    return retval

    vGetContents h = do vTestEOF h
                        c <- viocGet (srv h)
                        vClose h
                        return c
    vIsReadable _ = return True

----------------------------------------------------------------------
-- Buffers
----------------------------------------------------------------------
{- | A 'MemoryBuffer' simulates true I\/O, but uses an in-memory buffer instead
of on-disk storage.

It provides
a full interface like Handle (it implements 'HVIOReader', 'HVIOWriter',
and 'HVIOSeeker').  However, it maintains an in-memory buffer with the
contents of the file, rather than an actual on-disk file.  You can access
the entire contents of this buffer at any time.  This can be quite useful
for testing I\/O code, or for cases where existing APIs use I\/O, but you
prefer a String representation.  You can create a 'MemoryBuffer' with a call
to 'newMemoryBuffer'.

The present 'MemoryBuffer' implementation is rather inefficient, particularly
when reading towards the end of large files.  It's best used for smallish
data storage.  This problem will be fixed eventually.
-}
data MemoryBuffer = MemoryBuffer (String -> IO ()) (VIOCloseSupport (Int, String))

{- | Create a new 'MemoryBuffer' instance.  The buffer is initialized
to the value passed, and the pointer is placed at the beginning of the file.

You can put things in it by using the normal 'vPutStr' calls, and reset to
the beginning by using the normal 'vRewind' call.

The function is called when 'vClose' is called, and is passed the contents of
the buffer at close time.  You can use 'mbDefaultCloseFunc' if you don't want to
do anything.

To create an empty buffer, pass the initial value @\"\"@. -}
newMemoryBuffer :: String               -- ^ Initial Contents
                -> (String -> IO ())    -- ^ close func
                -> IO MemoryBuffer
newMemoryBuffer initval closefunc = do ref <- newIORef (True, (0, initval))
                                       return (MemoryBuffer closefunc ref)

{- | Default (no-op) memory buf close function. -}
mbDefaultCloseFunc :: String -> IO ()
mbDefaultCloseFunc _ = return ()

vrv :: MemoryBuffer -> VIOCloseSupport (Int, String)
vrv (MemoryBuffer _ x) = x

{- | Grab the entire contents of the buffer as a string.
Unlike 'vGetContents', this has no effect on the open status of the
item, the EOF status, or the current position of the file pointer. -}
getMemoryBuffer :: MemoryBuffer -> IO String
getMemoryBuffer h = do c <- viocGet (vrv h)
                       return (snd c)

instance Show MemoryBuffer where
  show _ = "<MemoryBuffer>"

instance HVIO MemoryBuffer where
  vClose x = do
    wasopen <- vIsOpen x
    viocClose (vrv x)
    when wasopen $ do
      c <- getMemoryBuffer x
      case x of
        MemoryBuffer cf _ -> cf c

  vIsEOF h = do
    vTestOpen h
    c <- viocGet (vrv h)
    return (length (snd c) == fst c)

  vIsOpen = viocIsopen . vrv

  vGetChar h = do
    vTestEOF h
    c <- viocGet (vrv h)
    let retval = snd c !! fst c
    viocSet (vrv h) (first succ c)
    return retval

  vGetContents h = do
    vTestEOF h
    v <- viocGet (vrv h)
    let retval = uncurry drop v
    viocSet (vrv h) (-1, "")
    vClose h
    return retval

  vIsReadable _ = return True

  vPutStr h s = do
    (pos, buf) <- viocGet (vrv h)
    let (pre, post) = splitAt pos buf
    let newbuf = pre ++ s ++ drop (length s) post
    viocSet (vrv h) (pos + length s, newbuf)

  vPutChar h c = vPutStr h [c]
  vIsWritable _ = return True

  vTell h = do
    v <- viocGet (vrv h)
    return . fromIntegral $ fst v

  vSeek h seekmode seekposp = do
    (pos, buf) <- viocGet (vrv h)
    let seekpos = fromInteger seekposp
    let newpos = case seekmode of
                      AbsoluteSeek -> seekpos
                      RelativeSeek -> pos + seekpos
                      SeekFromEnd  -> length buf + seekpos
    let buf2 = buf ++ if newpos > length buf
                        then replicate (newpos - length buf) '\0'
                        else []
    viocSet (vrv h) (newpos, buf2)

  vIsSeekable _ = return True

----------------------------------------------------------------------
-- Pipes
----------------------------------------------------------------------
{- | Create a Haskell pipe.

These pipes are analogous to the Unix
pipes that are available from System.Posix, but don't require Unix and work
only in Haskell.  When you create a pipe, you actually get two HVIO objects:
a 'PipeReader' and a 'PipeWriter'.  You must use the 'PipeWriter' in one
thread and the 'PipeReader' in another thread.  Data that's written to the
'PipeWriter' will then be available for reading with the 'PipeReader'.  The
pipes are implemented completely with existing Haskell threading primitives,
and require no special operating system support.  Unlike Unix pipes, these
pipes cannot be used across a fork().  Also unlike Unix pipes, these pipes
are portable and interact well with Haskell threads. -}
newHVIOPipe :: IO (PipeReader, PipeWriter)
newHVIOPipe = do
  mv <- newEmptyMVar
  readerref <- newIORef (True, mv)
  let reader = PipeReader readerref
  writerref <- newIORef (True, reader)
  return (reader, PipeWriter writerref)

data PipeBit
  = PipeBit Char
  | PipeEOF
  deriving (Eq, Show)

{- | The reading side of a Haskell pipe.  Please see 'newHVIOPipe' for more
details. -}
newtype PipeReader = PipeReader (VIOCloseSupport (MVar PipeBit))

{- | The writing side of a Haskell pipe.  Please see 'newHVIOPipe' for more
details. -}
newtype PipeWriter = PipeWriter (VIOCloseSupport PipeReader)

------------------------------
-- Pipe Reader
------------------------------
prv :: PipeReader -> VIOCloseSupport (MVar PipeBit)
prv (PipeReader x) = x

instance Show PipeReader where
  show _ = "<PipeReader>"

dprGetc :: PipeReader -> IO PipeBit
dprGetc h = do
  mv <- viocGet (prv h)
  takeMVar mv

instance HVIO PipeReader where
  vClose = viocClose . prv
  vIsOpen = viocIsopen . prv
  vIsEOF h = do
    vTestOpen h
    mv <- viocGet (prv h)
    dat <- readMVar mv
    return (dat == PipeEOF)

  vGetChar h = do
    vTestEOF h
    dprGetc h >>= \case
      PipeBit x -> return x
      -- vTestEOF should eliminate this case
      _ -> fail "Internal error in HVIOReader vGetChar"

  vGetContents h =
      let loop = do
            dprGetc h >>= \case
              PipeEOF -> return []
              PipeBit x -> do
                next <- loop
                return (x : next)
      in do
        vTestEOF h
        loop

  vIsReadable _ = return True

------------------------------
-- Pipe Writer
------------------------------
pwv :: PipeWriter -> VIOCloseSupport PipeReader
pwv (PipeWriter x) = x

pwmv :: PipeWriter -> IO (MVar PipeBit)
pwmv (PipeWriter x) = do mv1 <- viocGet x
                         viocGet (prv mv1)

instance Show PipeWriter where
    show _ = "<PipeWriter>"

instance HVIO PipeWriter where
    vClose h = do
      o <- vIsOpen h
      when o $ do
        mv <- pwmv h
        putMVar mv PipeEOF
        viocClose (pwv h)

    vIsOpen = viocIsopen . pwv
    
    vIsEOF h = do
      vTestOpen h
      return False

    -- FIXME: race condition below (could be closed after testing)
    vPutChar h c = do
      vTestOpen h
      child <- viocGet (pwv h)
      copen <- vIsOpen child
      if copen
      then do
        mv <- pwmv h
        putMVar mv (PipeBit c)
      else fail "PipeWriter: Couldn't write to pipe because child end is closed"
      
    vIsWritable _ = return True
