-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.Setup
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
module Hackage.Setup
    ( parsePackageArgs
    , parseGlobalArgs
    , configFromOptions
    ) where

import Control.Monad (when)
import Distribution.ParseUtils (parseDependency)
import Distribution.Compiler (CompilerFlavor(..))
import Distribution.Simple.InstallDirs (InstallDirTemplates(..), toPathTemplate)
import Distribution.Verbosity
import Data.List (find)
import System.Console.GetOpt (ArgDescr (..), ArgOrder (..), OptDescr (..), usageInfo, getOpt')
import System.Exit (exitWith, ExitCode (..))
import System.Environment (getProgName)

import Hackage.Types (Action (..), Option(..), ConfigFlags(..)
                                      , UnresolvedDependency (..))
import Hackage.Utils (readPToMaybe)


globalOptions :: [OptDescr Option]
globalOptions =
    [ Option "g" ["ghc"] (NoArg (OptCompilerFlavor GHC)) "Compile with GHC"
    , Option "n" ["nhc"] (NoArg (OptCompilerFlavor NHC))  "Compile with NHC"
    , Option "" ["hugs"] (NoArg (OptCompilerFlavor Hugs)) "Compile with hugs"
    , Option "w" ["with-compiler"] (reqPathArg OptCompiler)
                 "Give the path to a particular compiler"
    , Option "" ["with-hc-pkg"] (reqPathArg OptHcPkg)
                 "Give the path to the package tool"
    , Option "c" ["config-file"] (reqPathArg OptConfigFile)
                 ("Override the path to the config dir.")
    , Option "" ["cache-dir"] (reqPathArg OptCacheDir)
                 ("Override the path to the package cache dir.")
    , Option "" ["prefix"] (reqDirArg OptPrefix)
                 "Bake this prefix in preparation of installation"
    , Option "" ["bindir"] (reqDirArg OptBinDir)
                "Installation directory for executables"
    , Option "" ["libdir"] (reqDirArg OptLibDir)
                "Installation directory for libraries"
    , Option "" ["libsubdir"] (reqDirArg OptLibSubDir)
                "Subdirectory of libdir in which libs are installed"
    , Option "" ["libexecdir"] (reqDirArg OptLibExecDir)
                "Installation directory for program executables"
    , Option "" ["datadir"] (reqDirArg OptDataDir)
                "Installation directory for read-only data"
    , Option "" ["datasubdir"] (reqDirArg OptDataSubDir)
                 "Subdirectory of datadir in which data files are installed"
    , Option "" ["docdir"] (reqDirArg OptDocDir)
                 "Installation directory for documentation"
    , Option "" ["htmldir"] (reqDirArg OptHtmlDir)
                 "Installation directory for HTML documentation"
    , Option "" ["user"] (NoArg (OptUserInstall True))
                 "Upon registration, register this package in the user's local package database"
    , Option "" ["global"] (NoArg (OptUserInstall False))
                 "Upon registration, register this package in the system-wide package database"
    , Option "h?" ["help"] (NoArg OptHelp) "Show this help text"
    , Option "v" ["verbose"] (OptArg (OptVerbose . flagToVerbosity) "n")
                 "Control verbosity (n is 0--3, normal verbosity level is 1, -v alone is equivalent to -v2)"
    ]

reqPathArg :: (FilePath -> a) -> ArgDescr a
reqPathArg constr = ReqArg constr "PATH"

reqDirArg :: (FilePath -> a) -> ArgDescr a
reqDirArg constr = ReqArg constr "DIR"

configFromOptions :: ConfigFlags -> [Option] -> ConfigFlags
configFromOptions conf opts = foldr f conf opts
  where 
    -- figure out up front if this is a user or global install
    userInstall = last $ configUserInstall conf : [u | OptUserInstall u <- opts]
    f o cfg = case o of
                    OptCompilerFlavor c -> cfg { configCompiler = c}
                    OptCompiler p       -> cfg { configCompilerPath = Just p }
                    OptHcPkg p          -> cfg { configHcPkgPath = Just p }
                    OptConfigFile _     -> cfg
                    OptCacheDir d       -> cfg { configCacheDir = d }
                    OptPrefix     d     -> lib (\ds x -> ds { prefixDirTemplate  = x }) d
                    OptBinDir     d     -> lib (\ds x -> ds { binDirTemplate     = x }) d
                    OptLibDir     d     -> lib (\ds x -> ds { libDirTemplate     = x }) d
                    OptLibSubDir  d     -> lib (\ds x -> ds { libSubdirTemplate  = x }) d
                    OptLibExecDir d     -> lib (\ds x -> ds { libexecDirTemplate = x }) d
                    OptDataDir    d     -> lib (\ds x -> ds { dataDirTemplate    = x }) d
                    OptDataSubDir d     -> lib (\ds x -> ds { dataSubdirTemplate = x }) d
                    OptDocDir     d     -> lib (\ds x -> ds { docDirTemplate     = x }) d
                    OptHtmlDir    d     -> lib (\ds x -> ds { htmlDirTemplate    = x }) d
                    OptUserInstall u    -> cfg { configUserInstall = u }
                    OptHelp             -> error "Got to setFlagsFromOptions OptHelp"
                    OptVerbose v        -> cfg { configVerbose = v }
         where 
           -- This is a bit of a hack to allow just one set of installdir command-line
           -- options. Settings on the comman-line are for a single install session only,
           -- which will be either a user or global install.
           lib g d | userInstall = cfg { configUserInstallDirs   = g (configUserInstallDirs   cfg) d' }
                   | otherwise   = cfg { configGlobalInstallDirs = g (configGlobalInstallDirs cfg) d' }
               where d' = toPathTemplate d

data Cmd = Cmd {
        cmdName         :: String,
        cmdHelp         :: String, -- Short description
        cmdDescription  :: String, -- Long description
        cmdOptions      :: [OptDescr Option],
        cmdAction       :: Action
        }

commandList :: [Cmd]
commandList = [fetchCmd, installCmd, updateCmd, cleanCmd, listCmd, infoCmd]

lookupCommand :: String -> Maybe Cmd
lookupCommand name = find ((==name) . cmdName) commandList

printGlobalHelp :: IO ()
printGlobalHelp = do pname <- getProgName
                     let syntax_line = concat [ "Usage: ", pname
                                              , " [GLOBAL FLAGS]\n  or:  ", pname
                                              , " COMMAND [FLAGS]\n\nGlobal flags:"]
                     putStrLn (usageInfo syntax_line globalOptions)
                     putStrLn "Commands:"
                     let maxlen = maximum [ length (cmdName cmd) | cmd <- commandList ]
                     sequence_ [ do putStr "  "
                                    putStr (align maxlen (cmdName cmd))
                                    putStr "    "
                                    putStrLn (cmdHelp cmd)
                               | cmd <- commandList ]
  where align n str = str ++ replicate (n - length str) ' '

parseGlobalArgs :: [String] -> IO (Action,[Option],[String])
parseGlobalArgs opts =
  do let (flags, args, unrec, errs) = getOpt' RequireOrder globalOptions opts
     when (OptHelp `elem` flags) $ 
          do printGlobalHelp
             exitWith ExitSuccess
     when (not (null errs)) $ 
          do putStrLn "Errors:"
             mapM_ putStrLn errs
             exitWith (ExitFailure 1)
     when (not (null unrec)) $ 
          do putStrLn "Unrecognized options:"
             mapM_ putStrLn unrec
             exitWith (ExitFailure 1)
     case args of
       []          -> do putStrLn $ "No command given (try --help)"
                         exitWith (ExitFailure 1)
       cname:cargs -> case lookupCommand cname of
                        Just cmd -> return (cmdAction cmd, flags, cargs)
                        Nothing  -> do putStrLn $ "Unrecognised command: " ++ cname ++ " (try --help)"
                                       exitWith (ExitFailure 1)

mkCmd :: String -> String -> String -> Action -> Cmd
mkCmd name help desc action =
    Cmd { cmdName        = name
        , cmdHelp        = help
        , cmdDescription = desc
        , cmdOptions     = []
        , cmdAction      = action
        }

fetchCmd :: Cmd
fetchCmd = mkCmd "fetch" "Downloads packages for later installation or study." "" FetchCmd

installCmd :: Cmd
installCmd = mkCmd "install" "Installs a list of packages." "" InstallCmd

listCmd :: Cmd
listCmd = mkCmd "list" "List available packages on the server." "" ListCmd

updateCmd :: Cmd
updateCmd = mkCmd "update" "Updates list of known packages" "" UpdateCmd

cleanCmd :: Cmd
cleanCmd = mkCmd "clean" "Removes downloaded files" "" CleanCmd

infoCmd :: Cmd
infoCmd = mkCmd "info" "Emit some info"
           "Emits information about dependency resolution" InfoCmd

parsePackageArgs :: Action -> [String] -> IO ([String],[UnresolvedDependency])
parsePackageArgs _ args
    = return (globalArgs,parsePkgArgs pkgs)
    where (globalArgs,pkgs) = break (not.(==)'-'.head) args
          parseDep dep
              = case readPToMaybe parseDependency dep of
                  Nothing -> error ("Failed to parse package dependency: " ++ show dep)
                  Just x  -> x
          parsePkgArgs [] = []
          parsePkgArgs (x:xs)
              = let (args',rest) = break (not.(==) '-'.head) xs
                in (UnresolvedDependency
                    { dependency = parseDep x
                    , depOptions = args' }
                   ):parsePkgArgs rest

