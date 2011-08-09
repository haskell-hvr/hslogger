{- arch-tag: Tests main file
Copyright (C) 2004 John Goerzen <jgoerzen@complete.org>
License: BSD3

-}

module Tests(tests) where
import Test.HUnit

test1 = TestCase ("x" @=? "x")

tests = TestList [TestLabel "test1" test1]


