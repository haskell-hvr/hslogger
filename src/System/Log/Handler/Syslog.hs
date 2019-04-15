{-# LANGUAGE CPP #-}

{- |
   Module     : System.Log.Handler.Syslog
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Portability: portable

Syslog handler for the Haskell Logging Framework

Written by John Goerzen, jgoerzen\@complete.org

This module implements an interface to the Syslog service commonly
found in Unix\/Linux systems.  This interface is primarily of interest to
developers of servers, as Syslog does not typically display messages in
an interactive fashion.

This module is written in pure Haskell and is capable of logging to a local
or remote machine using the Syslog protocol.

You can create a new Syslog 'LogHandler' by calling 'openlog'.

More information on the Haskell Logging Framework can be found at
"System.Log.Logger".  This module can also be used outside
of the rest of that framework for those interested in that.
-}

module System.Log.Handler.Syslog(
                                       SyslogHandler, -- No constructors.
                                       -- * Handler Initialization
                                       openlog,
                                       -- * Advanced handler initialization
#ifndef mingw32_HOST_OS
                                       openlog_local,
#endif
                                       openlog_remote,
                                       openlog_generic,
                                       -- * Data Types
                                       Facility(..),
                                       Option(..)
                                       ) where

import qualified Control.Exception as E
import System.Log
import System.Log.Formatter
import System.Log.Handler
import Data.Bits
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SBS
import qualified Network.BSD as S
import Data.List (genericDrop)
#ifndef mingw32_HOST_OS
import System.Posix.Process(getProcessID)
#endif
import System.IO
import Control.Monad (void, when)

import UTF8

send :: S.Socket -> String -> IO Int
send s = SBS.send s . toUTF8BS

sendTo :: S.Socket -> String -> S.SockAddr -> IO Int
sendTo s str = SBS.sendTo s (toUTF8BS str)

code_of_pri :: Priority -> Int
code_of_pri p = case p of
                       EMERGENCY -> 0
                       ALERT -> 1
                       CRITICAL -> 2
                       ERROR -> 3
                       WARNING -> 4
                       NOTICE -> 5
                       INFO -> 6
                       DEBUG -> 7

{- | Facilities are used by the system to determine where messages
are sent. -}

data Facility =
              KERN                      -- ^ Kernel messages; you should likely never use this in your programs
              | USER                    -- ^ General userland messages.  Use this if nothing else is appropriate
              | MAIL                    -- ^ E-Mail system
              | DAEMON                  -- ^ Daemon (server process) messages
              | AUTH                    -- ^ Authentication or security messages
              | SYSLOG                  -- ^ Internal syslog messages; you should likely never use this in your programs
              | LPR                     -- ^ Printer messages
              | NEWS                    -- ^ Usenet news
              | UUCP                    -- ^ UUCP messages
              | CRON                    -- ^ Cron messages
              | AUTHPRIV                -- ^ Private authentication messages
              | FTP                     -- ^ FTP messages
              | LOCAL0                  -- ^ LOCAL0 through LOCAL7 are reserved for you to customize as you wish
              | LOCAL1
              | LOCAL2
              | LOCAL3
              | LOCAL4
              | LOCAL5
              | LOCAL6
              | LOCAL7
                deriving (Eq, Show, Read)

code_of_fac :: Facility -> Int
code_of_fac f = case f of
                       KERN -> 0
                       USER -> 1
                       MAIL -> 2
                       DAEMON -> 3
                       AUTH -> 4
                       SYSLOG -> 5
                       LPR -> 6
                       NEWS -> 7
                       UUCP -> 8
                       CRON -> 9
                       AUTHPRIV -> 10
                       FTP -> 11
                       LOCAL0 -> 16
                       LOCAL1 -> 17
                       LOCAL2 -> 18
                       LOCAL3 -> 19
                       LOCAL4 -> 20
                       LOCAL5 -> 21
                       LOCAL6 -> 22
                       LOCAL7 -> 23

makeCode :: Facility -> Priority -> Int
makeCode fac pri =
    let faccode = code_of_fac fac
        pricode = code_of_pri pri in
        (faccode `shiftL` 3) .|. pricode

{- | Options for 'openlog'. -}

data Option = PID                       -- ^ Automatically log process ID (PID) with each message
            | PERROR                    -- ^ Send a copy of each message to stderr
            deriving (Eq,Show,Read)

data SyslogHandler = SyslogHandler {options :: [Option],
                                    facility :: Facility,
                                    identity :: String,
                                    logsocket :: S.Socket,
                                    address :: S.SockAddr,
                                    sock_type :: S.SocketType,
                                    priority :: Priority,
                                    formatter :: LogFormatter SyslogHandler
                                   }

{- | Initialize the Syslog system using the local system's default interface,
\/dev\/log.  Will return a new 'System.Log.Handler.LogHandler'.

On Windows, instead of using \/dev\/log, this will attempt to send
UDP messages to something listening on the syslog port (514) on localhost.

Use 'openlog_remote' if you need more control.
-}

openlog :: String                       -- ^ The name of this program -- will be prepended to every log message
        -> [Option]                     -- ^ A list of 'Option's.  The list [] is perfectly valid.  ['PID'] is probably most common here.
        -> Facility                     -- ^ The 'Facility' value to pass to the syslog system for every message logged
        -> Priority                     -- ^ Messages logged below this priority will be ignored.  To include every message, set this to 'DEBUG'.
        -> IO SyslogHandler             -- ^ Returns the new handler

#ifdef mingw32_HOST_OS
openlog = openlog_remote S.AF_INET "localhost" 514
#elif darwin_HOST_OS
openlog = openlog_local "/var/run/syslog"
#else
openlog = openlog_local "/dev/log"
#endif

{- | Initialize the Syslog system using an arbitrary Unix socket (FIFO).

Not supported under Windows.
-}

#ifndef mingw32_HOST_OS
openlog_local :: String                 -- ^ Path to FIFO
              -> String                 -- ^ Program name
              -> [Option]               -- ^ 'Option's
              -> Facility               -- ^ Facility value
              -> Priority               -- ^ Priority limit
              -> IO SyslogHandler
openlog_local fifopath ident options' fac pri =
    do (s, t) <- do -- "/dev/log" is usually Datagram,
                    -- but most of syslog loggers allow it to be
                    -- of Stream type. glibc's" openlog()"
                    -- does roughly the similar thing:
                    --     http://www.gnu.org/software/libc/manual/html_node/openlog.html

                    s <- S.socket S.AF_UNIX S.Stream 0
                    tryStream s `E.catch` (onIOException (fallbackToDgram s))
       openlog_generic s (S.SockAddrUnix fifopath) t ident options' fac pri

  where onIOException :: IO a -> E.IOException -> IO a
        onIOException a _ = a

        tryStream :: S.Socket -> IO (S.Socket, S.SocketType)
        tryStream s =
            do S.connect s (S.SockAddrUnix fifopath)
               return (s, S.Stream)

        fallbackToDgram :: S.Socket -> IO (S.Socket, S.SocketType)
        fallbackToDgram s =
            do S.close s -- close Stream variant
               d <- S.socket S.AF_UNIX S.Datagram 0
               return (d, S.Datagram)
#endif

{- | Log to a remote server via UDP. -}
openlog_remote :: S.Family              -- ^ Usually AF_INET or AF_INET6; see Network.Socket
               -> S.HostName            -- ^ Remote hostname.  Some use @localhost@
               -> S.PortNumber          -- ^ 514 is the default for syslog
               -> String                -- ^ Program name
               -> [Option]              -- ^ 'Option's
               -> Facility              -- ^ Facility value
               -> Priority              -- ^ Priority limit
               -> IO SyslogHandler
openlog_remote fam hostname port ident options' fac pri =
    do
    he <- S.getHostByName hostname
    s <- S.socket fam S.Datagram 0
    let addr = S.SockAddrInet port (head (S.hostAddresses he))
    openlog_generic s addr S.Datagram ident options' fac pri

{- | The most powerful initialization mechanism.  Takes an open datagram
socket. -}
openlog_generic :: S.Socket             -- ^ A datagram socket
                -> S.SockAddr           -- ^ Address for transmissions
                -> S.SocketType         -- ^ socket connection mode (stream / datagram)
                -> String               -- ^ Program name
                -> [Option]             -- ^ 'Option's
                -> Facility             -- ^ Facility value
                -> Priority             -- ^ Priority limit
                -> IO SyslogHandler
openlog_generic sock addr sock_t ident opt fac pri =
    return (SyslogHandler {options = opt,
                            facility = fac,
                            identity = ident,
                            logsocket = sock,
                            address = addr,
                            sock_type = sock_t,
                            priority = pri,
                            formatter = syslogFormatter
                          })

syslogFormatter :: LogFormatter SyslogHandler
syslogFormatter sh (p,msg) logname =
    let format = "[$loggername/$prio] $msg"
    in varFormatter [] format sh (p,msg) logname


instance LogHandler SyslogHandler where
    setLevel sh p = sh{priority = p}
    getLevel sh = priority sh
    setFormatter sh f = sh{formatter = f}
    getFormatter sh = formatter sh
    emit sh (prio, msg) _ = do
      when (elem PERROR (options sh)) (hPutStrLn stderr msg)
      pidPart <- getPidPart
      void $ sendstr (toSyslogFormat msg pidPart)
      where
        sendstr :: String -> IO String
        sendstr [] = return []
        sendstr omsg = do
          sent <- case sock_type sh of
                    S.Datagram -> sendTo (logsocket sh) omsg (address sh)
                    S.Stream   -> send   (logsocket sh) omsg
          sendstr (genericDrop sent omsg)
        toSyslogFormat msg' pidPart =
            "<" ++ code ++ ">" ++ identity' ++ pidPart ++ ": " ++ msg' ++ "\0"
        code = show $ makeCode (facility sh) prio
        identity' = identity sh
        getPidPart = if elem PID (options sh)
                     then getPid >>= \pid -> return ("[" ++ pid ++ "]")
                     else return ""
        getPid :: IO String
        getPid =
#ifndef mingw32_HOST_OS
          getProcessID >>= return . show
#else
          return "windows"
#endif

    close sh = S.close (logsocket sh)
