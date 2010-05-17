module System.Log.Formatter( LogFormatter
                           , nullFormatter
                           , simpleLogFormatter
                           , tfLogFormatter
                           , varFormatter
                           ) where
import Data.List
import Control.Applicative ((<$>))
import Control.Concurrent (myThreadId)


import System.Locale (defaultTimeLocale)
import Data.Time.LocalTime (getZonedTime)
import Data.Time.Format (formatTime)

import System.Log

-- | A LogFormatter is used to format log messages.  Note that it is paramterized on the
-- 'Handler'
type LogFormatter a = a -> LogRecord -> String -> IO String

-- | Replace some '$' variables in a string with supplied values
replaceVarM :: [(String, IO String)] -- ^ A list of (variableName, action to get the value)
           -> String   -- ^ String to perform substitution on
           -> IO String   -- ^ Resulting string
replaceVarM _ [] = return []
replaceVarM keyVals (s:ss) | s=='$' = do (f,rest) <- replaceStart keyVals ss
                                         repRest <- replaceVarM keyVals rest
                                         return $ f ++ repRest
                           | otherwise = replaceVarM keyVals ss >>= return . (s:)
    where
      replaceStart [] str = return ("$",str)
      replaceStart ((k,v):kvs) str | k `isPrefixOf` str = do vs <- v
                                                             return (vs, drop (length k) str)
                                   | otherwise = replaceStart kvs str
                

-- | Returns the passed message as is, ie. no formatting is done.
nullFormatter :: LogFormatter a
nullFormatter _ (_,msg) _ = return msg

-- | Takes a format string, and returns a function that may be used to
-- format log messages.  The format string may contain '$' variables that
-- will be replaced at runtime with corresponding values.  The currently
-- supported variables are:
--    $msg  - The actual log message
--    $loggername - The name of the logger
--    $prio - The priority level of the message
--    $time - The current time 
--    $tid  - The thread ID
simpleLogFormatter :: String -> LogFormatter a
simpleLogFormatter format h (prio, msg) loggername = 
    tfLogFormatter "%F %X" format h (prio,msg) loggername

-- | Like 'simpleLogFormatter' but allow the time format to be specified in the first
-- parameter (this is passed to 'Date.Time.Format.formatTime')
tfLogFormatter :: String -> String -> LogFormatter a
tfLogFormatter timeFormat format = do
  varFormatter [("tid", show <$> myThreadId)
               ,("time", formatTime defaultTimeLocale timeFormat <$> getZonedTime)
               ]
      format

-- | A formatter that allows extra /variables/ to be specified for substitution in
-- string
varFormatter :: [(String, IO String)] -> String -> LogFormatter a
varFormatter vars format h (prio,msg) loggername = do
    outmsg <- replaceVarM (vars++[("msg", return msg)
                                 ,("prio", return $ show prio)
                                 ,("loggername", return loggername)]
                          ) 
                  format
    return outmsg
