{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}

#if __GLASGOW_HASKELL__ >= 702
{-# LANGUAGE DeriveGeneric #-}
#endif

{- |
   Module     : System.Log
   Copyright  : Copyright (C) 2004-2011 John Goerzen
   License    : BSD3

   Portability: portable

Haskell Logging Framework

Written by John Goerzen, jgoerzen\@complete.org

This module defines basic types used for logging.

Extensive documentation is available in
"System.Log.Logger".

-}



module System.Log(-- * Types
                        Priority(..),
                        LogRecord
)

    where

import Control.DeepSeq (NFData(rnf))
import Data.Data (Data, Typeable)
#if __GLASGOW_HASKELL__ >= 702
import  GHC.Generics (Generic)
#endif

{- | Priorities are used to define how important a log message is.
Users can filter log messages based on priorities.

These have their roots on the traditional syslog system.  The standard
definitions are given below, but you are free to interpret them however you
like.  They are listed here in ascending importance order.
-}

data Priority =
            DEBUG                   -- ^ Debug messages
          | INFO                    -- ^ Information
          | NOTICE                  -- ^ Normal runtime conditions
          | WARNING                 -- ^ General Warnings
          | ERROR                   -- ^ General Errors
          | CRITICAL                -- ^ Severe situations
          | ALERT                   -- ^ Take immediate action
          | EMERGENCY               -- ^ System is unusable
#if __GLASGOW_HASKELL__ >= 702
          deriving (Eq, Ord, Enum, Bounded, Show, Read, Data, Typeable, Generic)
#else
          deriving (Eq, Ord, Enum, Bounded, Show, Read, Data, Typeable)
#endif

-- | @since 1.3.1.0
instance NFData Priority where rnf = (`seq` ())

{- | Internal type of log records -}

type LogRecord = (Priority, String)
