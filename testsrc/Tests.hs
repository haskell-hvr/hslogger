{- arch-tag: Tests main file
Copyright (C) 2004 John Goerzen <jgoerzen@complete.org>
License: BSD3
-}
module Tests(tests) where

import Test.HUnit

import System.Log

tests = TestList [TestLabel "priority levels" priorityLevels]

priorityLevels :: Test
priorityLevels = TestList [
  TestCase ( DEBUG <= DEBUG @=? True),
  TestCase ( DEBUG <= INFO @=? True),
  TestCase ( INFO <= WARNING @=? True),
  TestCase ( WARNING <= ERROR @=? True),
  TestCase ( INFO <= ERROR @=? True),
  TestCase ( ERROR > INFO @=? True)
  ]

