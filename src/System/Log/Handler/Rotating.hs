{- arch-tag: rotating log handlers
Copyright (C) 2010-2012 ye yan <maskisland@gmail.com>

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU Lesser General Public License as published by
the Free Software Foundation; either version 2.1 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU Lesser General Public License for more details.

You should have received a copy of the GNU Lesser General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
-}

{- |
   Module     : System.Log.Handler.Rotating
   Copyright  : Copyright (C) 2010-2012 ye yan
   License    : GNU LGPL, version 2.1 or above
   Maintainer : ye yan <maskisland@gmail.com>
   Stability  : beta test
   Portability: portable

rotating log handlers

Written by ye yan

-}

module System.Log.Handler.Rotating (rotatingFileHandler, rotatingTimestampedFileHandler) where

import Prelude hiding (catch)
import Control.Exception (SomeException, catch)
import Data.Char (ord)

import System.Log
import System.Log.Handler
import System.Log.Formatter
import System.IO
import Control.Concurrent.MVar

import Data.Time.Clock
import Data.Time.Format
import System.Locale
import System.FilePath.Windows
import System.Directory

data XLogger = XLogger {
    priority :: Priority,
    formatter :: LogFormatter XLogger,
    writeFunc :: LogRecord -> String -> IO (),
    closeFunc :: IO ()
}

instance LogHandler XLogger where
    setLevel sh p = sh {priority = p}
    getLevel = priority
    setFormatter sh f = sh{formatter = f}
    getFormatter sh = formatter sh
    emit sh lr loggerName = (writeFunc sh) lr loggerName
    close sh = (closeFunc sh)

class Sizeable a where
    bytes :: (Integral b) => a -> b

instance Sizeable Char where
    bytes c 
        | value < 256 = 1
        | value < 65536 = 2
        | value < 4294967296 = 4
        where value = ord c

instance Sizeable [Char] where
    bytes str = sum $ map bytes str

getFileSize file = withFile file AppendMode hFileSize

writeToLimitedFile :: (Integral a) => FilePath -> a -> String -> (FilePath -> IO ()) -> IO ()
writeToLimitedFile file limit msg backup =
    do fileSize <- getFileSize file
       let msgSize = bytes msg
       if fileSize + msgSize > fromIntegral limit
          then do backup file 
                  writeToFile file WriteMode msg
          else writeToFile file AppendMode msg
    where writeToFile file mode msg = withFile file mode $ \handle -> do writeToHandle handle msg
                                                                         hFlush handle
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

timestampedBackup file =
    do time <- getCurrentTime
       let timeStamp = formatTime defaultTimeLocale "%Y%m%d%H%M%S" time
           tsFile = replaceBaseName file $ takeBaseName file ++ "_" ++ timeStamp
       renameFile file tsFile

whenFileExists :: FilePath -> (FilePath -> IO ()) -> IO ()
whenFileExists file func =
    do exists <- doesFileExist file
       if exists
          then func file
          else return ()

suffixedBackup count file =
    do helper $ reverse $ files
    where files = file : map (\x -> replaceBaseName file $ takeBaseName file ++ "_" ++ show x) [1..count]
          helper (f:[]) = return ()
          helper (f1:f2:fs) = do whenFileExists f2 (\_ -> renameFile f2 f1)
                                 helper $ f2:fs

-- params:
-- file -> file name of the log file
-- limit -> size limit
-- bkcnt -> backup count which will be in format <path>\\<basename>_<num>.<extend>
-- pri -> priority
rotatingFileHandler :: (Integral a) => FilePath -> a -> a -> Priority -> IO XLogger
rotatingFileHandler file limit bkcnt pri =
    do lock <- newMVar ()
       let myWriteFunc (_, msg) _ = withMVar lock $ \_ -> writeToLimitedFile file limit msg (suffixedBackup bkcnt)
       return XLogger {
           priority = pri,
           formatter = nullFormatter,
           writeFunc = myWriteFunc,
           closeFunc = return ()
       }

-- params:
-- file -> file name of the log file
-- limit -> size limit
-- pri -> priority
rotatingTimestampedFileHandler :: (Integral a) => FilePath -> a -> Priority -> IO XLogger
rotatingTimestampedFileHandler file limit pri = 
    do lock <- newMVar ()
       let myWriteFunc (_, msg) _ = withMVar lock $ \_ -> writeToLimitedFile file limit msg timestampedBackup
       return XLogger {
           priority = pri,
           formatter = nullFormatter,
           writeFunc = myWriteFunc,
           closeFunc = return ()
       }
