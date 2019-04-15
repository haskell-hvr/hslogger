{- |
   Module     : System.Log.Handler
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Portability: portable

Definition of log handler support

For some handlers, check out "System.Log.Handler.Simple" and
"System.Log.Handler.Syslog".

Please see "System.Log.Logger" for extensive documentation on the
logging system.

Written by John Goerzen, jgoerzen\@complete.org
-}

module System.Log.Handler(-- * Basic Types
                                LogHandler(..)
                               ) where
import System.Log
import System.Log.Formatter
import System.IO

{- | All log handlers should adhere to this. -}

{- | This is the base class for the various log handlers.  They should
all adhere to this class. -}

class LogHandler a where
                   -- | Sets the log level.  'handle' will drop
                   -- items beneath this level.
                   setLevel :: a -> Priority -> a
                   -- | Gets the current level.
                   getLevel :: a -> Priority
                   -- | Set a log formatter to customize the log format for this Handler
                   setFormatter :: a -> LogFormatter a -> a
                   getFormatter :: a -> LogFormatter a
                   getFormatter h = nullFormatter
                   -- | Logs an event if it meets the requirements
                   -- given by the most recent call to 'setLevel'.
                   handle :: a -> LogRecord -> String-> IO ()

                   handle h (pri, msg) logname = 
                       if pri >= (getLevel h)
                          then do formattedMsg <- (getFormatter h) h (pri,msg) logname
                                  emit h (pri, formattedMsg) logname
                          else return ()
                   -- | Forces an event to be logged regardless of
                   -- the configured level.
                   emit :: a -> LogRecord -> String -> IO ()
                   -- | Closes the logging system, causing it to close
                   -- any open files, etc.
                   close :: a -> IO ()




