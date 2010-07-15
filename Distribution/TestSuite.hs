{-# LANGUAGE ExistentialQuantification #-}
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

module Distribution.TestSuite
    ( Test
    , pure, impure
    , Options(..)
    , Result(..)
    , TestOptions(..)
    , PureTestable(..)
    , ImpureTestable(..)
    ) where

import Data.Dynamic ( Dynamic() )
import Data.Function ( on )
import Data.List ( unionBy )
import Data.Monoid ( Monoid(..) )
import Data.Typeable ( TypeRep )

newtype Options = Options [(String, Dynamic)]

instance Monoid Options where
    mempty = Options []

    -- | Combine two sets of 'Options'.  If an option is named in only one of
    -- the sets of 'Options', the associated value is used.  If an option is
    -- named in both arguments, the value specified in the left argument is
    -- used.
    mappend (Options a) (Options b) = Options $ unionBy ((==) `on` fst) a b

data Result
    = Pass          -- ^ The value indicating a successful test.
    | Fail String   -- ^ The value indicating a test completed
                    -- unsuccessfully.  The 'String' value should be a
                    -- human-readable message indicating how the test
                    -- failed.
    | Error String  -- ^ The value indicating a test that could not be
                    -- completed due to some error.  The test framework
                    -- should provide a message indicating the
                    -- nature of the error.
    deriving (Read, Show, Eq)

class TestOptions t where
    -- | The name of the test.
    name :: t -> String

    -- | A list of the options a test recognizes.  The name and 'TypeRep' are
    -- provided so that test runners can ensure that user-specified options are
    -- correctly typed.
    options :: t -> [(String, TypeRep)]

    -- | The default options for a test.  Test frameworks should provide a new
    -- random seed, if appropriate.
    defaultOptions :: t -> IO Options

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
-- this class over 'ImpureTestable'.
class TestOptions t => PureTestable t where
    -- | The result of a pure test.  Test frameworks implementing this class
    -- are responsible for converting any exceptions to the correct 'Result'
    -- value.
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

instance ImpureTestable Test where
    runM (PureTest p) o = return $ run p o
    runM (ImpureTest i) o = runM i o