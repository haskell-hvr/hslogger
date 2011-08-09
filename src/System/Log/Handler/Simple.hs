{- |
   Module     : System.Log.Handler.Simple
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Maintainer : John Goerzen <jgoerzen@complete.org> 
   Stability  : provisional
   Portability: portable

Simple log handlers

Written by John Goerzen, jgoerzen\@complete.org
-}

module System.Log.Handler.Simple(streamHandler, fileHandler,
                                      GenericHandler (..),
                                      verboseStreamHandler)
    where

import Prelude hiding (catch)
import Control.Exception (SomeException, catch)
import Data.Char (ord)

import System.Log
import System.Log.Handler
import System.Log.Formatter
import System.IO
import Control.Concurrent.MVar

{- | A helper data type. -}

data GenericHandler a = GenericHandler {priority :: Priority,
                                        formatter :: LogFormatter (GenericHandler a),
                                        privData :: a,
                                        writeFunc :: a -> String -> IO (),
                                        closeFunc :: a -> IO () }

instance LogHandler (GenericHandler a) where
    setLevel sh p = sh{priority = p}
    getLevel sh = priority sh
    setFormatter sh f = sh{formatter = f}
    getFormatter sh = formatter sh
    emit sh (_,msg) _ = (writeFunc sh) (privData sh) msg
    close sh = (closeFunc sh) (privData sh)


{- | Create a stream log handler.  Log messages sent to this handler will
   be sent to the stream used initially.  Note that the 'close' method
   will have no effect on stream handlers; it does not actually close
   the underlying stream.  -}

streamHandler :: Handle -> Priority -> IO (GenericHandler Handle)
streamHandler h pri =
    do lock <- newMVar ()
       let mywritefunc hdl msg =
               withMVar lock (\_ -> do writeToHandle hdl msg
                                       hFlush hdl
                             )
       return (GenericHandler {priority = pri,
                               formatter = nullFormatter,
                               privData = h,
                               writeFunc = mywritefunc,
                               closeFunc = \x -> return ()})
    where
      writeToHandle hdl msg =
          hPutStrLn hdl msg `catch` (handleWriteException hdl msg)
      handleWriteException :: Handle -> String -> SomeException -> IO ()
      handleWriteException hdl msg e =
          let msg' = "Error writing log message: " ++ show e ++
                     " (original message: " ++ msg ++ ")"
          in hPutStrLn hdl (encodingSave msg')
      encodingSave = concatMap (\c -> if ord c > 127
                                         then "\\" ++ show (ord c)
                                         else [c])

{- | Create a file log handler.  Log messages sent to this handler
   will be sent to the filename specified, which will be opened
   in Append mode.  Calling 'close' on the handler will close the file.
   -}

fileHandler :: FilePath -> Priority -> IO (GenericHandler Handle)
fileHandler fp pri = do
                     h <- openFile fp AppendMode
                     sh <- streamHandler h pri
                     return (sh{closeFunc = hClose})

{- | Like 'streamHandler', but note the priority and logger name along
with each message. -}
verboseStreamHandler :: Handle -> Priority -> IO (GenericHandler Handle)
verboseStreamHandler h pri = let fmt = simpleLogFormatter "[$loggername/$prio] $msg"
                             in do hndlr <- streamHandler h pri
                                   return $ setFormatter hndlr fmt
