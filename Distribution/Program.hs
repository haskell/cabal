module Distribution.Program( Program(..)
                           , ProgramLocation(..)
                           , ProgramConfiguration(..)
                           , withProgramFlag
                           , programOptsFlag
                           , programOptsField
                           , defaultProgramConfiguration
                           , updateProgram
                           , userSpecifyPath
                           , userSpecifyArgs
                           , lookupProgram
                           , lookupPrograms
                           , rawSystemProgram
                           , rawSystemProgramConf
                           , simpleProgram
                             -- Programs
                           , ghcProgram
                           , ghcPkgProgram
                           , nhcProgram
                           , jhcProgram
                           , hugsProgram
                           , ranlibProgram
                           , arProgram
                           , alexProgram
                           , hsc2hsProgram
                           , c2hsProgram
                           , cpphsProgram
                           , haddockProgram
                           , greencardProgram
                           , ldProgram
                           , cppProgram
                           , pfesetupProgram
                           ) where

import qualified Distribution.Compat.Map as Map
import Control.Monad(when)
import Data.Maybe(catMaybes)
import System.Exit (ExitCode)
import Distribution.Compat.Directory(findExecutable)
import Distribution.Simple.Utils (die, rawSystemVerbose, maybeExit)

-- |Represents a program which cabal may call.
data Program
    = Program { -- |The simple name of the program, eg ghc
               programName :: String
                -- |The name of this program's binary, eg ghc-6.4
              ,programBinName :: String
                -- |Default command-line args for this program
              ,programArgs :: [String]
                -- |Location of the program.  eg. \/usr\/bin\/ghc-6.4
              ,programLocation :: ProgramLocation
              } deriving (Read, Show)

-- |Similar to Maybe, but tells us whether it's specifed by user or
-- not.  This includes not just the path, but the program as well.
data ProgramLocation = EmptyLocation
                     | UserSpecified FilePath
                     | FoundOnSystem FilePath
      deriving (Read, Show)

data ProgramConfiguration = ProgramConfiguration (Map.Map String Program)

-- Read & Show instances are based on listToFM

instance Show ProgramConfiguration where
  show (ProgramConfiguration s) = show $ Map.toAscList s

instance Read ProgramConfiguration where
  readsPrec p s = [(ProgramConfiguration $ Map.fromList $ s', r)
                       | (s', r) <- readsPrec p s ]

-- |The default list of programs and their arguments.  These programs
-- are typically used internally to Cabal.

defaultProgramConfiguration :: ProgramConfiguration
defaultProgramConfiguration = progListToFM 
                              [ haddockProgram
                              , pfesetupProgram
                              , ranlibProgram
                              , simpleProgram "runghc"
                              , simpleProgram "runhugs"
                              , arProgram]
-- haddock is currently the only one that really works.
{-                              [ ghcProgram
                              , ghcPkgProgram
                              , nhcProgram
                              , hugsProgram
                              , alexProgram
                              , hsc2hsProgram
                              , c2hsProgram
                              , cpphsProgram
                              , haddockProgram
                              , greencardProgram
                              , ldProgram
                              , cppProgram
                              , pfesetupProgram
                              , ranlib, ar
                              ]-}

-- |The flag for giving a path to this program.  eg --with-alex=\/usr\/bin\/alex
withProgramFlag :: Program -> String
withProgramFlag Program{programName=n} = "with-" ++ n

-- |The flag for giving args for this program.
--  eg --haddock-options=-s http:\/\/foo
programOptsFlag :: Program -> String
programOptsFlag Program{programName=n} = n ++ "-options"

-- |The foo.cabal field for  giving args for this program.
--  eg haddock-options: -s http:\/\/foo
programOptsField :: Program -> String
programOptsField = programOptsFlag

-- ------------------------------------------------------------
-- * cabal programs
-- ------------------------------------------------------------

ghcProgram :: Program
ghcProgram = simpleProgram "ghc"

ghcPkgProgram :: Program
ghcPkgProgram = simpleProgram "ghc-pkg"

nhcProgram :: Program
nhcProgram = simpleProgram "nhc"

jhcProgram :: Program
jhcProgram = simpleProgram "jhc"

hugsProgram :: Program
hugsProgram = simpleProgram "hugs"

alexProgram :: Program
alexProgram = simpleProgram "alex"

ranlibProgram :: Program
ranlibProgram = simpleProgram "ranlib"

arProgram :: Program
arProgram = simpleProgram "ar"

hsc2hsProgram :: Program
hsc2hsProgram = simpleProgram "hsc2hs"

c2hsProgram :: Program
c2hsProgram = simpleProgram "c2hs"

cpphsProgram :: Program
cpphsProgram = simpleProgram "cpphs"

haddockProgram :: Program
haddockProgram = simpleProgram "haddock"

greencardProgram :: Program
greencardProgram = simpleProgram "greencard"

ldProgram :: Program
ldProgram = simpleProgram "ld"

cppProgram :: Program
cppProgram = simpleProgram "cpp"

pfesetupProgram :: Program
pfesetupProgram = simpleProgram "pfesetup"

-- ------------------------------------------------------------
-- * helpers
-- ------------------------------------------------------------

-- |Looks up a program in the given configuration.  If there's no
-- location information in the configuration, then we use IO to look
-- on the system in PATH for the program.  If the program is not in
-- the configuration at all, we return Nothing.  FIX: should we build
-- a simpleProgram in that case? Do we want a way to specify NOT to
-- find it on the system (populate programLocation).

lookupProgram :: String -- simple name of program
              -> ProgramConfiguration
              -> IO (Maybe Program) -- the full program
lookupProgram name conf = 
  case lookupProgram' name conf of
    Nothing   -> return Nothing
    Just p@Program{ programLocation= configLoc
                  , programBinName = binName}
        -> do newLoc <- case configLoc of
                         EmptyLocation
                             -> do maybeLoc <- findExecutable binName
                                   return $ maybe EmptyLocation FoundOnSystem maybeLoc
                         a   -> return a
              return $ Just p{programLocation=newLoc}

lookupPrograms :: ProgramConfiguration -> IO [(String, Maybe Program)]
lookupPrograms conf@(ProgramConfiguration fm) = do
  let l = Map.elems fm
  mapM (\p -> do fp <- lookupProgram (programName p) conf
                 return (programName p, fp)
       ) l

-- |User-specify this path.  Basically override any path information
-- for this program in the configuration. If it's not a known
-- program, add it.
userSpecifyPath :: String   -- ^Program name
                -> FilePath -- ^user-specified path to filename
                -> ProgramConfiguration
                -> ProgramConfiguration
userSpecifyPath name path conf'@(ProgramConfiguration conf)
    = case Map.lookup name conf of
       Just p  -> updateProgram (Just p{programLocation=UserSpecified path}) conf'
       Nothing -> updateProgram (Just $ Program name name [] (UserSpecified path))
                                conf'

-- |User-specify the arguments for this program.  Basically override
-- any args information for this program in the configuration. If it's
-- not a known program, add it.
userSpecifyArgs :: String -- ^Program name
                -> String -- ^user-specified args
                -> ProgramConfiguration
                -> ProgramConfiguration
userSpecifyArgs name args conf'@(ProgramConfiguration conf)
    = case Map.lookup name conf of
       Just p  -> updateProgram (Just p{programArgs=(words args)}) conf'
       Nothing -> updateProgram (Just $ Program name name (words args) EmptyLocation) conf'

-- |Update this program's entry in the configuration.  No changes if
-- you pass in Nothing.
updateProgram :: Maybe Program -> ProgramConfiguration -> ProgramConfiguration
updateProgram (Just p@Program{programName=n}) (ProgramConfiguration conf)
    = ProgramConfiguration $ Map.insert n p conf
updateProgram Nothing conf = conf

-- |Runs the given program.
rawSystemProgram :: Int      -- ^Verbosity
                 -> Program  -- ^The program to run
                 -> [String] -- ^Any /extra/ arguments to add
                 -> IO ()
rawSystemProgram verbose (Program { programLocation=(UserSpecified p)
                                  , programArgs=args
                                  }) extraArgs
    = maybeExit $ rawSystemVerbose verbose p (extraArgs ++ args)

rawSystemProgram verbose (Program { programLocation=(FoundOnSystem p)
                                  , programArgs=args
                                  }) extraArgs
    = maybeExit $ rawSystemVerbose verbose p (args ++ extraArgs)

rawSystemProgram _ (Program { programLocation=EmptyLocation
                            , programName=n}) _
    = die ("Error: Could not find location for program: " ++ n)

rawSystemProgramConf :: Int -- ^verbosity
                     -> String -- ^The name of the program to run
                     -> ProgramConfiguration -- ^look up the program here
                     -> [String] -- ^Any /extra/ arguments to add
                     -> IO ()
rawSystemProgramConf verbose progName programConf extraArgs 
    = do prog <- do mProg <- lookupProgram progName programConf
                    case mProg of
                        Nothing -> (die (progName ++ " command not found"))
                        Just h  -> return h
         rawSystemProgram verbose prog extraArgs


-- ------------------------------------------------------------
-- * Internal helpers
-- ------------------------------------------------------------

-- Export?
lookupProgram' :: String -> ProgramConfiguration -> Maybe Program
lookupProgram' s (ProgramConfiguration conf) = Map.lookup s conf

progListToFM :: [Program] -> ProgramConfiguration
progListToFM progs = foldl
                     (\ (ProgramConfiguration conf')
                      p@(Program {programName=n})
                          -> ProgramConfiguration (Map.insert n p conf'))
                     (ProgramConfiguration Map.empty)
                     progs

simpleProgram :: String -> Program
simpleProgram s = Program s s [] EmptyLocation
