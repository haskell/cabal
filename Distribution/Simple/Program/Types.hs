-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Program.Types
-- Copyright   :  Isaac Jones 2006, Duncan Coutts 2007-2009
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This provides an abstraction which deals with configuring and running
-- programs. A 'Program' is a static notion of a known program. A
-- 'ConfiguredProgram' is a 'Program' that has been found on the current
-- machine and is ready to be run (possibly with some user-supplied default
-- args). Configuring a program involves finding its location and if necessary
-- finding its version. There's reasonable default behavior for trying to find
-- \"foo\" in PATH, being able to override its location, etc.
--
module Distribution.Simple.Program.Types (
    -- * Program and functions for constructing them
    Program(..),
    internalProgram,
    simpleProgram,

    -- * Configured program and related functions
    ConfiguredProgram(..),
    programPath,
    ProgArg,
    ProgramLocation(..),
  ) where

import Data.List (nub) 
import System.FilePath ((</>))

import Distribution.Simple.Utils
         ( findProgramLocation, findFirstFile )
import Distribution.Version
         ( Version )
import Distribution.Verbosity
         ( Verbosity )

-- | Represents a program which can be configured.
data Program = Program {
       -- | The simple name of the program, eg. ghc
       programName :: String,

       -- | A function to search for the program if it's location was not
       -- specified by the user. Usually this will just be a
       programFindLocation :: Verbosity -> IO (Maybe FilePath),

       -- | Try to find the version of the program. For many programs this is
       -- not possible or is not necessary so it's ok to return Nothing.
       programFindVersion :: Verbosity -> FilePath -> IO (Maybe Version),

       -- | A function to do any additional configuration after we have
       -- located the program (and perhaps identified its version). It is
       -- allowed to return additional flags that will be passed to the
       -- program on every invocation.
       programPostConf :: Verbosity -> ConfiguredProgram -> IO [ProgArg]
     }

type ProgArg = String

data ConfiguredProgram = ConfiguredProgram {
       -- | Just the name again
       programId :: String,

       -- | The version of this program, if it is known.
       programVersion :: Maybe Version,

       -- | Default command-line args for this program.
       -- These flags will appear first on the command line, so they can be
       -- overridden by subsequent flags.
       programDefaultArgs :: [String],

       -- | Override command-line args for this program.
       -- These flags will appear last on the command line, so they override
       -- all earlier flags.
       programOverrideArgs :: [String],

       -- | Location of the program. eg. @\/usr\/bin\/ghc-6.4@
       programLocation :: ProgramLocation
     } deriving (Read, Show, Eq)

-- | Where a program was found. Also tells us whether it's specifed by user or
-- not.  This includes not just the path, but the program as well.
data ProgramLocation
    = UserSpecified { locationPath :: FilePath }
      -- ^The user gave the path to this program,
      -- eg. --ghc-path=\/usr\/bin\/ghc-6.6
    | FoundOnSystem { locationPath :: FilePath }
      -- ^The location of the program, as located by searching PATH.
      deriving (Read, Show, Eq)

-- | The full path of a configured program.
programPath :: ConfiguredProgram -> FilePath
programPath = locationPath . programLocation

-- | Make a simple named program.
--
-- By default we'll just search for it in the path and not try to find the
-- version name. You can override these behaviours if necessary, eg:
--
-- > simpleProgram "foo" { programFindLocation = ... , programFindVersion ... }
--
simpleProgram :: String -> Program
simpleProgram name = Program {
    programName         = name,
    programFindLocation = \v   -> findProgramLocation v name,
    programFindVersion  = \_ _ -> return Nothing,
    programPostConf     = \_ _ -> return []
  }

-- | Make a simple 'internal' program; that is, one that was built as an
-- executable already and is expected to be found in the build directory
internalProgram :: [FilePath] -> String -> Program
internalProgram paths name = Program {
  programName         = name,
  programFindLocation = \_v ->
    findFirstFile id [ path </> name | path <- nub paths ],
    programFindVersion  = \_ _ -> return Nothing,
    programPostConf     = \_ _ -> return []
  }

