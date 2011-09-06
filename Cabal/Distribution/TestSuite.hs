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
    ( TestInstance(..)
    , OptionDescr(..)
    , OptionType(..)
    , Test(..)
    , Options
    , Progress(..)
    , Result(..)
    , testGroup
    ) where

data TestInstance = TestInstance
    { run       :: IO Progress      -- ^ Perform the test.
    , name      :: String           -- ^ A name for the test, unique within a
                                    -- test suite.
    , tags      :: [String]         -- ^ Users can select groups of tests by
                                    -- their tags.
    , options   :: [OptionDescr]    -- ^ Descriptions of the options recognized
                                    -- by this test.
    , setOption :: String -> String -> Either String TestInstance
        -- ^ Try to set the named option to the given value. Returns an error
        -- message if the option is not supported or the value could not be
        -- correctly parsed; otherwise, a 'TestInstance' with the option set to
        -- the given value is returned.
    }

data OptionDescr = OptionDescr
    { optionName        :: String
    , optionDescription :: String       -- ^ A human-readable description of the
                                        -- option to guide the user setting it.
    , optionType        :: OptionType
    , optionDefault     :: Maybe String
    }
  deriving (Eq, Read, Show)

data OptionType
    = OptionFile
        { optionFileMustExist   :: Bool
        , optionFileIsDir       :: Bool
        , optionFileExtensions  :: [String]
        }
    | OptionString
        { optionStringMultiline :: Bool
        }
    | OptionNumber
        { optionNumberIsInt     :: Bool
        , optionNumberBounds    :: (Maybe String, Maybe String)
        }
    | OptionBool
    | OptionEnum [String]
    | OptionSet [String]
    | OptionRngSeed
  deriving (Eq, Read, Show)

data Test
    = Test TestInstance
    | Group
        { groupName     :: String
        , concurrently  :: Bool
            -- ^ If true, then children of this group may be run in parallel.
            -- Note that this setting is not inherited by children. In
            -- particular, consider a group F with "concurrently = False" that
            -- has some children, including a group T with "concurrently =
            -- True". The children of group T may be run concurrently with each
            -- other, as long as none are run at the same time as any of the
            -- direct children of group F.
        , groupTests    :: [Test]
        }
    | ExtraOptions [OptionDescr] Test

type Options = [(String, String)]

data Progress = Finished Result
              | Progress String (IO Progress)

data Result = Pass
            | Fail String
            | Error String
  deriving (Eq, Read, Show)

-- | Create a named group of tests, which are assumed to be safe to run in
-- parallel.
testGroup :: String -> [Test] -> Test
testGroup n ts = Group { groupName = n, concurrently = True, groupTests = ts }
