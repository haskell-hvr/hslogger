{- arch-tag: Test runner
Copyright (C) 2004-2011 John Goerzen <jgoerzen@complete.org>
License: BSD3
-}

module Main where 

import Test.HUnit
import Tests

main = runTestTT tests

