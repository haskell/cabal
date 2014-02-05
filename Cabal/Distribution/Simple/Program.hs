-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Program
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
-- finding its version. There is also a 'ProgramConfiguration' type which holds
-- configured and not-yet configured programs. It is the parameter to lots of
-- actions elsewhere in Cabal that need to look up and run programs. If we had
-- a Cabal monad, the 'ProgramConfiguration' would probably be a reader or
-- state component of it. 
--
-- The module also defines all the known built-in 'Program's and the
-- 'defaultProgramConfiguration' which contains them all.
--
-- One nice thing about using it is that any program that is
-- registered with Cabal will get some \"configure\" and \".cabal\"
-- helpers like --with-foo-args --foo-path= and extra-foo-args.
--
-- There's also good default behavior for trying to find \"foo\" in
-- PATH, being able to override its location, etc.
--
-- There's also a hook for adding programs in a Setup.lhs script.  See
-- hookedPrograms in 'Distribution.Simple.UserHooks'.  This gives a
-- hook user the ability to get the above flags and such so that they
-- don't have to write all the PATH logic inside Setup.lhs.

module Distribution.Simple.Program (
    -- * Program and functions for constructing them
      Program(..)
    , ProgramSearchPath
    , ProgramSearchPathEntry(..)
    , simpleProgram
    , findProgramLocation
    , findProgramVersion

    -- * Configured program and related functions
    , ConfiguredProgram(..)
    , programPath
    , ProgArg
    , ProgramLocation(..)
    , runProgram
    , getProgramOutput
    , suppressOverrideArgs

    -- * Program invocations
    , ProgramInvocation(..)
    , emptyProgramInvocation
    , simpleProgramInvocation
    , programInvocation
    , runProgramInvocation
    , getProgramInvocationOutput

    -- * The collection of unconfigured and configured progams
    , builtinPrograms

    -- * The collection of configured programs we can run
    , ProgramConfiguration
    , emptyProgramConfiguration
    , defaultProgramConfiguration
    , restoreProgramConfiguration
    , addKnownProgram
    , addKnownPrograms
    , lookupKnownProgram
    , knownPrograms
    , getProgramSearchPath
    , setProgramSearchPath
    , userSpecifyPath
    , userSpecifyPaths
    , userMaybeSpecifyPath
    , userSpecifyArgs
    , userSpecifyArgss
    , userSpecifiedArgs
    , lookupProgram
    , updateProgram
    , configureProgram
    , configureAllKnownPrograms
    , reconfigurePrograms
    , requireProgram
    , requireProgramVersion
    , runDbProgram
    , getDbProgramOutput

    -- * Programs that Cabal knows about
    , ghcProgram
    , ghcPkgProgram
    , lhcProgram
    , lhcPkgProgram
    , nhcProgram
    , hmakeProgram
    , jhcProgram
    , hugsProgram
    , ffihugsProgram
    , uhcProgram
    , gccProgram
    , arProgram
    , stripProgram
    , happyProgram
    , alexProgram
    , hsc2hsProgram
    , c2hsProgram
    , cpphsProgram
    , hscolourProgram
    , haddockProgram
    , greencardProgram
    , ldProgram
    , tarProgram
    , cppProgram
    , pkgConfigProgram
    , hpcProgram

    -- * deprecated
    , rawSystemProgram
    , rawSystemProgramStdout
    , rawSystemProgramConf
    , rawSystemProgramStdoutConf
    , findProgramOnPath

    ) where

import Distribution.Simple.Program.Types
import Distribution.Simple.Program.Run
import Distribution.Simple.Program.Db
import Distribution.Simple.Program.Builtin

import Distribution.Simple.Utils
         ( die, findProgramLocation, findProgramVersion )
import Distribution.Verbosity
         ( Verbosity )


-- | Runs the given configured program.
--
runProgram :: Verbosity          -- ^Verbosity
           -> ConfiguredProgram  -- ^The program to run
           -> [ProgArg]          -- ^Any /extra/ arguments to add
           -> IO ()
runProgram verbosity prog args =
  runProgramInvocation verbosity (programInvocation prog args)


-- | Runs the given configured program and gets the output.
--
getProgramOutput :: Verbosity          -- ^Verbosity
                 -> ConfiguredProgram  -- ^The program to run
                 -> [ProgArg]          -- ^Any /extra/ arguments to add
                 -> IO String
getProgramOutput verbosity prog args =
  getProgramInvocationOutput verbosity (programInvocation prog args)


-- | Looks up the given program in the program database and runs it.
--
runDbProgram :: Verbosity  -- ^verbosity
             -> Program    -- ^The program to run
             -> ProgramDb  -- ^look up the program here
             -> [ProgArg]  -- ^Any /extra/ arguments to add
             -> IO ()
runDbProgram verbosity prog programDb args =
  case lookupProgram prog programDb of
    Nothing             -> die notFound
    Just configuredProg -> runProgram verbosity configuredProg args
 where
   notFound = "The program '" ++ programName prog
           ++ "' is required but it could not be found"

-- | Looks up the given program in the program database and runs it.
--
getDbProgramOutput :: Verbosity  -- ^verbosity
                   -> Program    -- ^The program to run
                   -> ProgramDb  -- ^look up the program here
                   -> [ProgArg]  -- ^Any /extra/ arguments to add
                   -> IO String
getDbProgramOutput verbosity prog programDb args =
  case lookupProgram prog programDb of
    Nothing             -> die notFound
    Just configuredProg -> getProgramOutput verbosity configuredProg args
 where
   notFound = "The program '" ++ programName prog
           ++ "' is required but it could not be found"


---------------------
-- Deprecated aliases
--

rawSystemProgram :: Verbosity -> ConfiguredProgram
                 -> [ProgArg] -> IO ()
rawSystemProgram = runProgram

rawSystemProgramStdout :: Verbosity -> ConfiguredProgram
                       -> [ProgArg] -> IO String
rawSystemProgramStdout = getProgramOutput

rawSystemProgramConf :: Verbosity  -> Program -> ProgramConfiguration
                     -> [ProgArg] -> IO ()
rawSystemProgramConf = runDbProgram

rawSystemProgramStdoutConf :: Verbosity -> Program -> ProgramConfiguration
                           -> [ProgArg] -> IO String
rawSystemProgramStdoutConf = getDbProgramOutput

type ProgramConfiguration = ProgramDb

emptyProgramConfiguration, defaultProgramConfiguration :: ProgramConfiguration
emptyProgramConfiguration   = emptyProgramDb
defaultProgramConfiguration = defaultProgramDb

restoreProgramConfiguration :: [Program] -> ProgramConfiguration
                                         -> ProgramConfiguration
restoreProgramConfiguration = restoreProgramDb

{-# DEPRECATED findProgramOnPath "use findProgramLocation instead" #-}
findProgramOnPath :: String -> Verbosity -> IO (Maybe FilePath)
findProgramOnPath = flip findProgramLocation
