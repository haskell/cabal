HUnitTest98.lhs  --  test for HUnit, using Haskell language system "98"

$Id: HUnitTest98.lhs,v 1.1 2002/02/19 17:12:14 heringto Exp $

> module Main (main) where

> import HUnit
> import HUnitTestBase


> main = runTestTT (test [baseTests])
