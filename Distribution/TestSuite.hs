{-# LANGUAGE CPP, ExistentialQuantification #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.TestSuite
-- Copyright   :  Thomas Tuegel 2010
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module defines the detailed test suite interface which makes it
-- possible to expose individual tests to Cabal or other test agents.

{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

#if !(defined(__HUGS__) || (defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ < 610))
#define NEW_EXCEPTION
#endif

module Distribution.TestSuite
    ( -- * Example
      -- $example
      -- * Options
      Options(..)
    , lookupOption
    , TestOptions(..)
      -- * Tests
    , Test
    , pure, impure
    , Result(..)
    , ImpureTestable(..)
    , PureTestable(..)
    ) where

#ifdef NEW_EXCEPTION
import Control.Exception ( evaluate, catch, throw, SomeException, fromException )
#else
import Control.Exception ( evaluate, catch, throw, Exception(IOException) )
#endif

--TODO: it is totally unreasonable that we have to import things from GHC.* here.
--      see ghc ticket #3517
#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ >= 612
import GHC.IO.Exception  ( IOErrorType(Interrupted) )
#else
import GHC.IOBase        ( IOErrorType(Interrupted) )
#endif
import System.IO.Error   ( ioeGetErrorType )
#endif

import Data.List ( unionBy )
import Data.Monoid ( Monoid(..) )
import Data.Typeable ( TypeRep )
import Prelude hiding ( catch )

-- | 'Options' are provided to pass options to test runners, making tests
-- reproducable.  Each option is a @('String', 'String')@ of the form
-- @(Name, Value)@.  Use 'mappend' to combine sets of 'Options'; if the same
-- option is given different values, the value from the left argument of
-- 'mappend' will be used.
newtype Options = Options [(String, String)]
    deriving (Read, Show, Eq)

instance Monoid Options where
    mempty = Options []
    mappend (Options a) (Options b) = Options $ unionBy (equating fst) a b
      where
        equating p x y = p x == p y


class TestOptions t where
    -- | The name of the test.
    name :: t -> String

    -- | A list of the options a test recognizes.  The name and 'TypeRep' are
    -- provided so that test agents can ensure that user-specified options are
    -- correctly typed.
    options :: t -> [(String, TypeRep)]

    -- | The default options for a test.  Test frameworks should provide a new
    -- random seed, if appropriate.
    defaultOptions :: t -> IO Options

    -- | Try to parse the provided options.  Return the names of unparsable
    -- options.  This allows test agents to detect bad user-specified options.
    check :: t -> Options -> [String]

-- | Read an option from the specified set of 'Options'.  It is an error to
-- lookup an option that has not been specified.  For this reason, test agents
-- should 'mappend' any 'Options' against the 'defaultOptions' for a test, so
-- the default value specified by the test framework will be used for any
-- otherwise-unspecified options.
lookupOption :: Read r => String -> Options -> r
lookupOption n (Options opts) =
    case lookup n opts of
        Just str -> read str
        Nothing -> error $ "test option not specified: " ++ n

data Result
    = Pass          -- ^ indicates a successful test
    | Fail String   -- ^ indicates a test completed unsuccessfully;
                    -- the 'String' value should be a human-readable message
                    -- indicating how the test failed.
    | Error String  -- ^ indicates a test that could not be
                    -- completed due to some error; the test framework
                    -- should provide a message indicating the
                    -- nature of the error.
    deriving (Read, Show, Eq)

-- | Class abstracting impure tests.  Test frameworks should implement this
-- class only as a last resort for test types which actually require 'IO'.
-- In particular, tests that simply require pseudo-random number generation can
-- be implemented as pure tests.
class TestOptions t => ImpureTestable t where
    -- | Runs an impure test and returns the result.  Test frameworks
    -- implementing this class are responsible for converting any exceptions to
    -- the correct 'Result' value.
    runM :: t -> Options -> IO Result

-- | Class abstracting pure tests.  Test frameworks should prefer to implement
-- this class over 'ImpureTestable'.  A default instance exists so that any pure
-- test can be lifted into an impure test; when lifted, any exceptions are
-- automatically caught.  Test agents that lift pure tests themselves must
-- handle exceptions.
class TestOptions t => PureTestable t where
    -- | The result of a pure test.
    run :: t -> Options -> Result

-- | 'Test' is a wrapper for pure and impure tests so that lists containing
-- arbitrary test types can be constructed.
data Test
    = forall p. PureTestable p => PureTest p
    | forall i. ImpureTestable i => ImpureTest i

-- | A convenient function for wrapping pure tests into 'Test's.
pure :: PureTestable p => p -> Test
pure = PureTest

-- | A convenient function for wrapping impure tests into 'Test's.
impure :: ImpureTestable i => i -> Test
impure = ImpureTest

instance TestOptions Test where
    name (PureTest p) = name p
    name (ImpureTest i) = name i

    options (PureTest p) = options p
    options (ImpureTest i) = options i

    defaultOptions (PureTest p) = defaultOptions p
    defaultOptions (ImpureTest p) = defaultOptions p

    check (PureTest p) = check p
    check (ImpureTest p) = check p

instance ImpureTestable Test where
    runM (PureTest p) o = catch (evaluate $ run p o) handler

    -- Because we have to handle old and new style exceptions, GHC and non-GHC
    -- this code is totally horrible and really fragile. Has to be tested with
    -- lots of ghc versions to check it is right, and with non-ghc too. :-(
#ifdef NEW_EXCEPTION
      where
        handler :: SomeException -> IO Result
        handler e = case fromException e of
          Just ioe | isInterruptedError ioe -> throw e
          _                                 -> return (Error (show e))
#else
      where
        handler :: Exception -> IO Result
        handler e = case e of
          IOException ioe | isInterruptedError ioe -> throw e
          _                                        -> return (Error (show e))
#endif

        -- We do not want to catch control-C here, but only GHC
        -- defines the Interrupted exception type! (ticket #3517)
        isInterruptedError ioe =
#ifdef __GLASGOW_HASKELL__
          ioeGetErrorType ioe == Interrupted
#else
          False
#endif

    runM (ImpureTest i) o = runM i o

-- $example
-- The following terms are used carefully throughout this file:
--
--  [test interface]    The interface provided by this module.
--
--  [test agent]    A program used by package users to coordinates the running
--                  of tests and the reporting of their results.
--
--  [test framework]    A package used by software authors to specify tests,
--                      such as QuickCheck or HUnit.
--
-- Test frameworks are obligated to supply, at least, instances of the
-- 'TestOptions' and 'ImpureTestable' classes.  It is preferred that test
-- frameworks implement 'PureTestable' whenever possible, so that test agents
-- have an assurance that tests can be safely run in parallel.
--
-- Test agents that allow the user to specify options should avoid setting
-- options not listed by the 'options' method.  Test agents should use 'check'
-- before running tests with non-default options.  Test frameworks must
-- implement a 'check' function that attempts to parse the given options safely.
--
-- The packages cabal-test-hunit, cabal-test-quickcheck1, and
-- cabal-test-quickcheck2 provide simple interfaces to these popular test
-- frameworks.  An example from cabal-test-quickcheck2 is shown below.  A
-- better implementation would eliminate the console output from QuickCheck\'s
-- built-in runner and provide an instance of 'PureTestable' instead of
-- 'ImpureTestable'.
--
-- > import Control.Monad (liftM)
-- > import Data.Maybe (catMaybes, fromJust, maybe)
-- > import Data.Typeable (Typeable(..))
-- > import qualified Distribution.TestSuite as Cabal
-- > import System.Random (newStdGen, next, StdGen)
-- > import qualified Test.QuickCheck as QC
-- >
-- > data QCTest = forall prop. QC.Testable prop => QCTest String prop
-- >
-- > test :: QC.Testable prop => String -> prop -> Cabal.Test
-- > test n p = Cabal.impure $ QCTest n p
-- >
-- > instance Cabal.TestOptions QCTest where
-- >     name (QCTest n _) = n
-- >
-- >     options _ =
-- >         [ ("std-gen", typeOf (undefined :: String))
-- >         , ("max-success", typeOf (undefined :: Int))
-- >         , ("max-discard", typeOf (undefined :: Int))
-- >         , ("size", typeOf (undefined :: Int))
-- >         ]
-- >
-- >     defaultOptions _ = do
-- >         rng <- newStdGen
-- >         return $ Cabal.Options $
-- >             [ ("std-gen", show rng)
-- >             , ("max-success", show $ QC.maxSuccess QC.stdArgs)
-- >             , ("max-discard", show $ QC.maxDiscard QC.stdArgs)
-- >             , ("size", show $ QC.maxSize QC.stdArgs)
-- >             ]
-- >
-- >     check t (Cabal.Options opts) = catMaybes
-- >         [ maybeNothing "max-success" ([] :: [(Int, String)])
-- >         , maybeNothing "max-discard" ([] :: [(Int, String)])
-- >         , maybeNothing "size" ([] :: [(Int, String)])
-- >         ]
-- >         -- There is no need to check the parsability of "std-gen"
-- >         -- because the Read instance for StdGen always succeeds.
-- >         where
-- >             maybeNothing n x =
-- >                 maybe Nothing (\str ->
-- >                     if reads str == x then Just n else Nothing)
-- >                     $ lookup n opts
-- >
-- > instance Cabal.ImpureTestable QCTest where
-- >     runM (QCTest _ prop) o =
-- >         catch go (return . Cabal.Error . show)
-- >         where
-- >             go = do
-- >                 result <- QC.quickCheckWithResult args prop
-- >                 return $ case result of
-- >                         QC.Success {} -> Cabal.Pass
-- >                         QC.GaveUp {}->
-- >                             Cabal.Fail $ "gave up after "
-- >                                        ++ show (QC.numTests result)
-- >                                        ++ " tests"
-- >                         QC.Failure {} -> Cabal.Fail $ QC.reason result
-- >                         QC.NoExpectedFailure {} ->
-- >                             Cabal.Fail "passed (expected failure)"
-- >             args = QC.Args
-- >                 { QC.replay = Just
-- >                     ( Cabal.lookupOption "std-gen" o
-- >                     , Cabal.lookupOption "size" o
-- >                     )
-- >                 , QC.maxSuccess = Cabal.lookupOption "max-success" o
-- >                 , QC.maxDiscard = Cabal.lookupOption "max-discard" o
-- >                 , QC.maxSize = Cabal.lookupOption "size" o
-- >                 }
