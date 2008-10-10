-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Program
-- Copyright   :  Isaac Jones 2006
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
    , simpleProgram
    , findProgramOnPath
    , findProgramVersion

    -- * Configured program and related functions
    , ConfiguredProgram(..)
    , programPath
    , ProgArg
    , ProgramLocation(..)
    , rawSystemProgram
    , rawSystemProgramStdout

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
    , rawSystemProgramConf
    , rawSystemProgramStdoutConf

    -- * Programs that Cabal knows about
    , ghcProgram
    , ghcPkgProgram
    , nhcProgram
    , hmakeProgram
    , jhcProgram
    , hugsProgram
    , ffihugsProgram
    , gccProgram
    , ranlibProgram
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
    ) where

import Data.List  (foldl')
import Data.Maybe (catMaybes)
import qualified Data.Map as Map

import Distribution.Simple.Utils
         (die, debug, warn, rawSystemExit, rawSystemStdout)
import Distribution.Version
         ( Version(..), VersionRange(AnyVersion), withinRange )
import Distribution.Text
         ( simpleParse, display )
import Distribution.Verbosity
import System.Directory
         ( doesFileExist, findExecutable )
import Control.Monad (join, foldM)
import Distribution.Compat.Exception (catchExit, catchIO)

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
        programArgs :: [ProgArg],

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

-- ------------------------------------------------------------
-- * Programs functions
-- ------------------------------------------------------------

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
    programFindLocation = findProgramOnPath name,
    programFindVersion  = \_ _ -> return Nothing,
    programPostConf     = \_ _ -> return []
  }

-- | Look for a program on the path.
findProgramOnPath :: FilePath -> Verbosity -> IO (Maybe FilePath)
findProgramOnPath prog verbosity = do
  debug verbosity $ "searching for " ++ prog ++ " in path."
  res <- findExecutable prog
  case res of
      Nothing   -> debug verbosity ("Cannot find " ++ prog ++ " on the path")
      Just path -> debug verbosity ("found " ++ prog ++ " at "++ path)
  return res

-- | Look for a program and try to find it's version number. It can accept
-- either an absolute path or the name of a program binary, in which case we
-- will look for the program on the path.
--
findProgramVersion :: ProgArg            -- ^ version args
                   -> (String -> String) -- ^ function to select version
                                         --   number from program output
                   -> Verbosity
                   -> FilePath           -- ^ location
                   -> IO (Maybe Version)
findProgramVersion versionArg selectVersion verbosity path = do
  str <- rawSystemStdout verbosity path [versionArg]
         `catchIO`   (\_ -> return "")
         `catchExit` (\_ -> return "")
  let version :: Maybe Version
      version = simpleParse (selectVersion str)
  case version of
      Nothing -> warn verbosity $ "cannot determine version of " ++ path
                               ++ " :\n" ++ show str
      Just v  -> debug verbosity $ path ++ " is version " ++ display v
  return version

-- ------------------------------------------------------------
-- * Programs database
-- ------------------------------------------------------------

-- | The configuration is a collection of information about programs. It
-- contains information both about configured programs and also about programs
-- that we are yet to configure.
--
-- The idea is that we start from a collection of unconfigured programs and one
-- by one we try to configure them at which point we move them into the
-- configured collection. For unconfigured programs we record not just the
-- 'Program' but also any user-provided arguments and location for the program.
data ProgramConfiguration = ProgramConfiguration {
        unconfiguredProgs :: UnconfiguredProgs,
        configuredProgs   :: ConfiguredProgs
    }
type UnconfiguredProgram = (Program, Maybe FilePath, [ProgArg])
type UnconfiguredProgs = Map.Map String UnconfiguredProgram
type ConfiguredProgs =   Map.Map String ConfiguredProgram

emptyProgramConfiguration :: ProgramConfiguration
emptyProgramConfiguration = ProgramConfiguration Map.empty Map.empty

defaultProgramConfiguration :: ProgramConfiguration
defaultProgramConfiguration =
  restoreProgramConfiguration builtinPrograms emptyProgramConfiguration

-- internal helpers:
updateUnconfiguredProgs :: (UnconfiguredProgs -> UnconfiguredProgs)
                        -> ProgramConfiguration -> ProgramConfiguration
updateUnconfiguredProgs update conf =
  conf { unconfiguredProgs = update (unconfiguredProgs conf) }
updateConfiguredProgs :: (ConfiguredProgs -> ConfiguredProgs)
                      -> ProgramConfiguration -> ProgramConfiguration
updateConfiguredProgs update conf =
  conf { configuredProgs = update (configuredProgs conf) }

-- Read & Show instances are based on listToFM
-- Note that we only serialise the configured part of the database, this is
-- because we don't need the unconfigured part after the configure stage, and
-- additionally because we cannot read/show 'Program' as it contains functions.
instance Show ProgramConfiguration where
  show = show . Map.toAscList . configuredProgs

instance Read ProgramConfiguration where
  readsPrec p s =
    [ (emptyProgramConfiguration { configuredProgs = Map.fromList s' }, r)
    | (s', r) <- readsPrec p s ]

-- | The Read\/Show instance does not preserve all the unconfigured 'Programs'
-- because 'Program' is not in Read\/Show because it contains functions. So to
-- fully restore a deserialised 'ProgramConfiguration' use this function to add
-- back all the known 'Program's.
--
-- * It does not add the default programs, but you probably want them, use
--   'builtinPrograms' in addition to any extra you might need.
--
restoreProgramConfiguration :: [Program] -> ProgramConfiguration
                                         -> ProgramConfiguration
restoreProgramConfiguration = addKnownPrograms

-- -------------------------------
-- Managing unconfigured programs

-- | Add a known program that we may configure later
addKnownProgram :: Program -> ProgramConfiguration -> ProgramConfiguration
addKnownProgram prog = updateUnconfiguredProgs $
  Map.insertWith combine (programName prog) (prog, Nothing, [])
  where combine _ (_, path, args) = (prog, path, args)

addKnownPrograms :: [Program] -> ProgramConfiguration -> ProgramConfiguration
addKnownPrograms progs conf = foldl' (flip addKnownProgram) conf progs

lookupKnownProgram :: String -> ProgramConfiguration -> Maybe Program
lookupKnownProgram name =
  fmap (\(p,_,_)->p) . Map.lookup name . unconfiguredProgs

knownPrograms :: ProgramConfiguration -> [(Program, Maybe ConfiguredProgram)]
knownPrograms conf =
  [ (p,p') | (p,_,_) <- Map.elems (unconfiguredProgs conf)
           , let p' = Map.lookup (programName p) (configuredProgs conf) ]

-- |User-specify this path.  Basically override any path information
-- for this program in the configuration. If it's not a known
-- program ignore it.
userSpecifyPath :: String   -- ^Program name
                -> FilePath -- ^user-specified path to the program
                -> ProgramConfiguration -> ProgramConfiguration
userSpecifyPath name path = updateUnconfiguredProgs $
  flip Map.update name $ \(prog, _, args) -> Just (prog, Just path, args)

userMaybeSpecifyPath :: String -> Maybe FilePath
                     -> ProgramConfiguration -> ProgramConfiguration
userMaybeSpecifyPath _    Nothing conf     = conf
userMaybeSpecifyPath name (Just path) conf = userSpecifyPath name path conf

-- |User-specify the arguments for this program.  Basically override
-- any args information for this program in the configuration. If it's
-- not a known program, ignore it..
userSpecifyArgs :: String    -- ^Program name
                -> [ProgArg] -- ^user-specified args
                -> ProgramConfiguration
                -> ProgramConfiguration
userSpecifyArgs name args' =
    updateUnconfiguredProgs
      (flip Map.update name $
         \(prog, path, args) -> Just (prog, path, args ++ args'))
  . updateConfiguredProgs
      (flip Map.update name $
         \prog -> Just prog { programArgs = programArgs prog ++ args' })

-- | Like 'userSpecifyPath' but for a list of progs and their paths.
--
userSpecifyPaths :: [(String, FilePath)]
                 -> ProgramConfiguration
                 -> ProgramConfiguration
userSpecifyPaths paths conf =
  foldl' (\conf' (prog, path) -> userSpecifyPath prog path conf') conf paths

-- | Like 'userSpecifyPath' but for a list of progs and their args.
--
userSpecifyArgss :: [(String, [ProgArg])]
                 -> ProgramConfiguration
                 -> ProgramConfiguration
userSpecifyArgss argss conf =
  foldl' (\conf' (prog, args) -> userSpecifyArgs prog args conf') conf argss

-- | Get the path that has been previously specified for a program, if any.
--
userSpecifiedPath :: Program -> ProgramConfiguration -> Maybe FilePath
userSpecifiedPath prog =
  join . fmap (\(_,p,_)->p) . Map.lookup (programName prog) . unconfiguredProgs

-- | Get any extra args that have been previously specified for a program.
--
userSpecifiedArgs :: Program -> ProgramConfiguration -> [ProgArg]
userSpecifiedArgs prog =
  maybe [] (\(_,_,as)->as) . Map.lookup (programName prog) . unconfiguredProgs

-- -----------------------------
-- Managing configured programs

-- | Try to find a configured program
lookupProgram :: Program -> ProgramConfiguration -> Maybe ConfiguredProgram
lookupProgram prog = Map.lookup (programName prog) . configuredProgs

-- | Update a configured program in the database.
updateProgram :: ConfiguredProgram -> ProgramConfiguration
                                   -> ProgramConfiguration
updateProgram prog = updateConfiguredProgs $
  Map.insert (programId prog) prog

-- ---------------------------
-- Configuring known programs

-- | Try to configure a specific program. If the program is already included in
-- the colleciton of unconfigured programs then we use any user-supplied
-- location and arguments. If the program gets configured sucessfully it gets
-- added to the configured collection.
--
-- Note that it is not a failure if the program cannot be configured. It's only
-- a failure if the user supplied a location and the program could not be found
-- at that location.
--
-- The reason for it not being a failure at this stage is that we don't know up
-- front all the programs we will need, so we try to configure them all.
-- To verify that a program was actually sucessfully configured use
-- 'requireProgram'.
--
configureProgram :: Verbosity
                 -> Program
                 -> ProgramConfiguration
                 -> IO ProgramConfiguration
configureProgram verbosity prog conf = do
  let name = programName prog
  maybeLocation <- case userSpecifiedPath prog conf of
    Nothing   -> programFindLocation prog verbosity
             >>= return . fmap FoundOnSystem
    Just path -> do
      absolute <- doesFileExist path
      if absolute
        then return (Just (UserSpecified path))
        else findProgramOnPath path verbosity
         >>= maybe (die notFound) (return . Just . UserSpecified)
      where notFound = "Cannot find the program '" ++ name ++ "' at '"
                     ++ path ++ "' or on the path"
  case maybeLocation of
    Nothing -> return conf
    Just location -> do
      version <- programFindVersion prog verbosity (locationPath location)
      let configuredProg    = ConfiguredProgram {
            programId       = name,
            programVersion  = version,
            programArgs     = userSpecifiedArgs prog conf,
            programLocation = location
          }
      extraArgs <- programPostConf prog verbosity configuredProg
      let configuredProg'   = configuredProg {
            programArgs     = extraArgs ++ programArgs configuredProg
          }
      return (updateConfiguredProgs (Map.insert name configuredProg') conf)

-- | Configure a bunch of programs using 'configureProgram'. Just a 'foldM'.
configurePrograms :: Verbosity
                  -> [Program]
                  -> ProgramConfiguration
                  -> IO ProgramConfiguration
configurePrograms verbosity progs conf =
  foldM (flip (configureProgram verbosity)) conf progs

-- | Try to configure all the known programs that have not yet been configured.
configureAllKnownPrograms :: Verbosity
                          -> ProgramConfiguration
                          -> IO ProgramConfiguration
configureAllKnownPrograms verbosity conf =
  configurePrograms verbosity
    [ prog | (prog,_,_) <- Map.elems notYetConfigured ] conf
  where
    notYetConfigured = unconfiguredProgs conf
      `Map.difference` configuredProgs conf

-- | reconfigure a bunch of programs given new user-specified args. It takes
-- the same inputs as 'userSpecifyPath' and 'userSpecifyArgs' and for all progs
-- with a new path it calls 'configureProgram'.
--
reconfigurePrograms :: Verbosity
                    -> [(String, FilePath)]
                    -> [(String, [ProgArg])]
                    -> ProgramConfiguration
                    -> IO ProgramConfiguration
reconfigurePrograms verbosity paths argss conf = do
  configurePrograms verbosity progs
   . userSpecifyPaths paths
   . userSpecifyArgss argss
   $ conf

  where
    progs = catMaybes [ lookupKnownProgram name conf | (name,_) <- paths ]

-- | Check that a program is configured and available to be run.
--
-- Additionally check that the version of the program number is suitable.
-- For example 'AnyVersion' or @'orLaterVersion' ('Version' [1,0] [])@
--
-- It raises an exception if the program could not be configured or the version
-- is unsuitable, otherwise it returns the configured program.
requireProgram :: Verbosity -> Program -> VersionRange -> ProgramConfiguration
               -> IO (ConfiguredProgram, ProgramConfiguration)
requireProgram verbosity prog range conf = do

  -- If it's not already been configured, try to configure it now
  conf' <- case lookupProgram prog conf of
    Nothing -> configureProgram verbosity prog conf
    Just _  -> return conf

  case lookupProgram prog conf' of
    Nothing                           -> die notFound
    Just configuredProg
      | range == AnyVersion           -> return (configuredProg, conf')
    Just configuredProg@ConfiguredProgram { programLocation = location } ->
      case programVersion configuredProg of
        Just version
          | withinRange version range -> return (configuredProg, conf')
          | otherwise                 -> die (badVersion version location)
        Nothing                       -> die (noVersion location)

  where notFound       = programName prog ++ versionRequirement
                      ++ " is required but it could not be found."
        badVersion v l = programName prog ++ versionRequirement
                      ++ " is required but the version found at "
                      ++ locationPath l ++ " is version " ++ display v
        noVersion l    = programName prog ++ versionRequirement
                      ++ " is required but the version of "
                      ++ locationPath l ++ " could not be determined."
        versionRequirement
          | range == AnyVersion = ""
          | otherwise           = " version " ++ display range

-- ------------------------------------------------------------
-- * Running programs
-- ------------------------------------------------------------

-- | Runs the given configured program.
rawSystemProgram :: Verbosity          -- ^Verbosity
                 -> ConfiguredProgram  -- ^The program to run
                 -> [ProgArg]          -- ^Any /extra/ arguments to add
                 -> IO ()
rawSystemProgram verbosity prog extraArgs
  = rawSystemExit verbosity (programPath prog) (programArgs prog ++ extraArgs)

-- | Runs the given configured program and gets the output.
rawSystemProgramStdout :: Verbosity          -- ^Verbosity
                       -> ConfiguredProgram  -- ^The program to run
                       -> [ProgArg]          -- ^Any /extra/ arguments to add
                       -> IO String
rawSystemProgramStdout verbosity prog extraArgs
  = rawSystemStdout verbosity (programPath prog) (programArgs prog ++ extraArgs)

-- | Looks up the given program in the program configuration and runs it.
rawSystemProgramConf :: Verbosity            -- ^verbosity
                     -> Program              -- ^The program to run
                     -> ProgramConfiguration -- ^look up the program here
                     -> [ProgArg]            -- ^Any /extra/ arguments to add
                     -> IO ()
rawSystemProgramConf verbosity prog programConf extraArgs =
  case lookupProgram prog programConf of
    Nothing -> die ("The program " ++ programName prog ++ " is required but it could not be found")
    Just configuredProg -> rawSystemProgram verbosity configuredProg extraArgs

-- | Looks up the given program in the program configuration and runs it.
rawSystemProgramStdoutConf :: Verbosity            -- ^verbosity
                           -> Program              -- ^The program to run
                           -> ProgramConfiguration -- ^look up the program here
                           -> [ProgArg]            -- ^Any /extra/ arguments to add
                           -> IO String
rawSystemProgramStdoutConf verbosity prog programConf extraArgs =
  case lookupProgram prog programConf of
    Nothing -> die ("The program " ++ programName prog ++ " is required but it could not be found")
    Just configuredProg -> rawSystemProgramStdout verbosity configuredProg extraArgs

-- ------------------------------------------------------------
-- * Known programs
-- ------------------------------------------------------------

-- | The default list of programs.
-- These programs are typically used internally to Cabal.
builtinPrograms :: [Program]
builtinPrograms =
    [
    -- compilers and related progs
      ghcProgram
    , ghcPkgProgram
    , hugsProgram
    , ffihugsProgram
    , nhcProgram
    , hmakeProgram
    , jhcProgram
    -- preprocessors
    , hscolourProgram
    , haddockProgram
    , happyProgram
    , alexProgram
    , hsc2hsProgram
    , c2hsProgram
    , cpphsProgram
    , greencardProgram
    -- platform toolchain
    , gccProgram
    , ranlibProgram
    , arProgram
    , stripProgram
    , ldProgram
    , tarProgram
    -- configuration tools
    , pkgConfigProgram
    ]

ghcProgram :: Program
ghcProgram = (simpleProgram "ghc") {
    programFindVersion = findProgramVersion "--numeric-version" id
  }

ghcPkgProgram :: Program
ghcPkgProgram = (simpleProgram "ghc-pkg") {
    programFindVersion = findProgramVersion "--version" $ \str ->
      -- Invoking "ghc-pkg --version" gives a string like
      -- "GHC package manager version 6.4.1"
      case words str of
        (_:_:_:_:ver:_) -> ver
        _               -> ""
  }

nhcProgram :: Program
nhcProgram = (simpleProgram "nhc98") {
    programFindVersion = findProgramVersion "--version" $ \str ->
      -- Invoking "nhc98 --version" gives a string like
      -- "/usr/local/bin/nhc98: v1.20 (2007-11-22)"
      case words str of
        (_:('v':ver):_) -> ver
        _               -> ""
  }

hmakeProgram :: Program
hmakeProgram = (simpleProgram "hmake") {
    programFindVersion = findProgramVersion "--version" $ \str ->
    -- Invoking "hmake --version" gives a string line
    -- "/usr/local/bin/hmake: 3.13 (2006-11-01)"
      case words str of
        (_:ver:_) -> ver
        _         -> ""
  }

jhcProgram :: Program
jhcProgram = (simpleProgram "jhc") {
    programFindVersion = findProgramVersion "--version" $ \str ->
    -- invoking "jhc --version" gives a string like
    -- "jhc 0.3.20080208 (wubgipkamcep-2)
    -- compiled by ghc-6.8 on a x86_64 running linux"
      case words str of
        (_:ver:_) -> ver
        _         -> ""
  }

-- AArgh! Finding the version of hugs or ffihugs is almost impossible.
hugsProgram :: Program
hugsProgram = simpleProgram "hugs"

ffihugsProgram :: Program
ffihugsProgram = simpleProgram "ffihugs"

happyProgram :: Program
happyProgram = (simpleProgram "happy") {
    programFindVersion = findProgramVersion "--version" $ \str ->
      -- Invoking "happy --version" gives a string like
      -- "Happy Version 1.16 Copyright (c) ...."
      case words str of
        (_:_:ver:_) -> ver
        _           -> ""
  }

alexProgram :: Program
alexProgram = (simpleProgram "alex") {
    programFindVersion = findProgramVersion "--version" $ \str ->
      -- Invoking "alex --version" gives a string like
      -- "Alex version 2.1.0, (c) 2003 Chris Dornan and Simon Marlow"
      case words str of
        (_:_:ver:_) -> takeWhile (`elem` ('.':['0'..'9'])) ver
        _           -> ""
  }

gccProgram :: Program
gccProgram = (simpleProgram "gcc") {
    programFindVersion = findProgramVersion "-dumpversion" id
  }

ranlibProgram :: Program
ranlibProgram = simpleProgram "ranlib"

arProgram :: Program
arProgram = simpleProgram "ar"

stripProgram :: Program
stripProgram = simpleProgram "strip"

hsc2hsProgram :: Program
hsc2hsProgram = (simpleProgram "hsc2hs") {
    programFindVersion =
      findProgramVersion "--version" $ \str ->
        -- Invoking "hsc2hs --version" gives a string like "hsc2hs version 0.66"
        case words str of
          (_:_:ver:_) -> ver
          _           -> ""
  }

c2hsProgram :: Program
c2hsProgram = (simpleProgram "c2hs") {
    programFindVersion = findProgramVersion "--numeric-version" id
  }

cpphsProgram :: Program
cpphsProgram = (simpleProgram "cpphs") {
    programFindVersion = findProgramVersion "--version" $ \str ->
      -- Invoking "cpphs --version" gives a string like "cpphs 1.3"
      case words str of
        (_:ver:_) -> ver
        _         -> ""
  }

hscolourProgram :: Program
hscolourProgram = (simpleProgram "hscolour") {
    programFindLocation = findProgramOnPath "HsColour",
    programFindVersion  = findProgramVersion "-version" $ \str ->
      -- Invoking "HsColour -version" gives a string like "HsColour 1.7"
      case words str of
        (_:ver:_) -> ver
        _         -> ""
  }

haddockProgram :: Program
haddockProgram = (simpleProgram "haddock") {
    programFindVersion = findProgramVersion "--version" $ \str ->
      -- Invoking "haddock --version" gives a string like
      -- "Haddock version 0.8, (c) Simon Marlow 2006"
      case words str of
        (_:_:ver:_) -> takeWhile (`elem` ('.':['0'..'9'])) ver
        _           -> ""
  }

greencardProgram :: Program
greencardProgram = simpleProgram "greencard"

ldProgram :: Program
ldProgram = simpleProgram "ld"

tarProgram :: Program
tarProgram = simpleProgram "tar"

cppProgram :: Program
cppProgram = simpleProgram "cpp"

pkgConfigProgram :: Program
pkgConfigProgram = (simpleProgram "pkg-config") {
    programFindVersion = findProgramVersion "--version" id
  }
