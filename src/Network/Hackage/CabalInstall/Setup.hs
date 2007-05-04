-----------------------------------------------------------------------------
-- |
-- Module      :  Network.Hackage.CabalInstall.Setup
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
--
-----------------------------------------------------------------------------
module Network.Hackage.CabalInstall.Setup
    ( emptyTempFlags
    , parsePackageArgs
    , parseGlobalArgs
    ) where

import Control.Monad (when)
import Data.Maybe (fromMaybe)
import Text.ParserCombinators.ReadP (readP_to_S)
import Distribution.ParseUtils (parseDependency)
import Distribution.Setup (defaultCompilerFlavor, CompilerFlavor(..))
import Data.List (find)
import System.Console.GetOpt (ArgDescr (..), ArgOrder (..), OptDescr (..), usageInfo, getOpt')
import System.Exit (exitWith, ExitCode (..))
import System.Environment (getProgName)

import Network.Hackage.CabalInstall.Types (TempFlags (..), Action (..)
                                      , UnresolvedDependency (..))

emptyTempFlags :: TempFlags
emptyTempFlags = TempFlags {
        tempHcFlavor    = defaultCompilerFlavor, -- Nothing,
        tempHcPath      = Nothing,
        tempConfDir     = Nothing,
        tempCacheDir    = Nothing,
        tempPkgListDir  = Nothing,
        tempHcPkg       = Nothing,
        tempPrefix      = Nothing,
        tempServers     = [],
        tempRunHc       = Nothing,
        tempTarPath     = Nothing,
        tempVerbose     = 1,
--        tempUpgradeDeps = False,
        tempUserIns     = True,
        tempHelp        = False
   }

cmd_verbose :: OptDescr (TempFlags -> TempFlags)
cmd_verbose = Option "v" ["verbose"] (OptArg verboseFlag "n")
              "Control verbosity (n is 0--5, normal verbosity level is 1, -v alone is equivalent to -v2)"
  where
    verboseFlag mb_s t = t { tempVerbose = maybe 2 read mb_s }

globalOptions :: [OptDescr (TempFlags -> TempFlags)]
globalOptions =
    [ Option "h?" ["help"] (NoArg (\t -> t { tempHelp = True })) "Show this help text"
    , cmd_verbose 
    , Option "g" ["ghc"] (NoArg (\t -> t { tempHcFlavor = Just GHC }))  "compile with GHC"
    , Option "n" ["nhc"] (NoArg (\t -> t { tempHcFlavor = Just NHC }))  "compile with NHC"
    , Option "" ["hugs"] (NoArg (\t -> t { tempHcFlavor = Just Hugs })) "compile with hugs"
    , Option "s" ["with-server"] (ReqArg (\url t -> t { tempServers = url:tempServers t }) "URL")
                 "give the URL to a Hackage server"
    , Option "c" ["config-dir"] (ReqArg (\path t -> t { tempConfDir = Just path }) "PATH")
                 ("override the path to the config dir.")
    , Option "" ["cache-dir"] (ReqArg (\path t -> t { tempCacheDir = Just path }) "PATH")
                 ("override the path to the package cache dir.")
    , Option "" ["pkglist-dir"] (ReqArg (\path t -> t { tempPkgListDir = Just path }) "PATH")
                 ("override the path to the package list dir.")
    , Option "" ["tar-path"] (ReqArg (\path t -> t { tempTarPath = Just path }) "PATH")
                 "give the path to tar"
    , Option "w" ["with-compiler"] (ReqArg (\path t -> t { tempHcPath = Just path }) "PATH")
                 "give the path to a particular compiler"
    , Option "" ["with-hc-pkg"] (ReqArg (\path t -> t { tempHcPkg = Just path }) "PATH")
                 "give the path to the package tool"
--    , Option "" ["upgrade-deps"] (NoArg (\t -> t { tempUpgradeDeps = True }))
--                 "Upgrade all dependencies which depend on the newly installed packages"
    , Option "" ["user-install"] (NoArg (\t -> t { tempUserIns     = True }))
                 "upon registration, register this package in the user's local package database"
    , Option "" ["global-install"] (NoArg (\t -> t { tempUserIns     = False }))
                 "upon registration, register this package in the system-wide package database"
    ]

data Cmd = Cmd {
        cmdName         :: String,
        cmdHelp         :: String, -- Short description
        cmdDescription  :: String, -- Long description
        cmdOptions      :: [OptDescr (TempFlags -> TempFlags)],
        cmdAction       :: Action
        }

commandList :: [Cmd]
commandList = [fetchCmd, installCmd, buildDepCmd, updateCmd, cleanCmd, listCmd, infoCmd]

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

printActionHelp :: Action -> IO ()
printActionHelp action = 
    do let [cmd] = [c | c <- commandList, cmdAction c == action]
       pname <- getProgName
       let syntax_line = "Usage: " ++ pname ++ " " ++ cmdName cmd ++ " [FLAGS]\n\nFlags for " ++ cmdName cmd ++ ":"
       putStrLn (usageInfo syntax_line (cmdOptions cmd))
       putStrLn (cmdDescription cmd)

parseGlobalArgs :: [String] -> IO (Action,TempFlags,[String])
parseGlobalArgs opts =
  do let (fs, args, unrec, errs) = getOpt' RequireOrder globalOptions opts
         flags = foldl (flip ($)) emptyTempFlags fs
     when (tempHelp flags) $ do printGlobalHelp
                                exitWith ExitSuccess
     when (not (null errs)) $ do putStrLn "Errors:"
                                 mapM_ putStrLn errs
                                 exitWith (ExitFailure 1)
     when (not (null unrec)) $ do putStrLn "Unrecognized options:"
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

buildDepCmd :: Cmd
buildDepCmd = mkCmd "build-dep" "Installs the dependencies for a list of packages or for a .cabal file." "" BuildDepCmd

updateCmd :: Cmd
updateCmd = mkCmd "update" "Updates list of known packages" "" UpdateCmd

cleanCmd :: Cmd
cleanCmd = mkCmd "clean" "Removes downloaded files" "" CleanCmd

infoCmd :: Cmd
infoCmd = mkCmd "info" "Emit some info"
           "Emits information about dependency resolution" InfoCmd

parsePackageArgs :: Action -> [String] -> IO ([String],[UnresolvedDependency])
parsePackageArgs action [] = do 
  printActionHelp action
  exitWith ExitSuccess
parsePackageArgs _ args
    = return (globalArgs,parsePkgArgs pkgs)
    where (globalArgs,pkgs) = break (not.(==)'-'.head) args
          parseDep dep
              = case readP_to_S parseDependency dep of
                 [] -> error ("Failed to parse package dependency: " ++ show dep)
                 x  -> fst (last x)
          parsePkgArgs [] = []
          parsePkgArgs (x:xs)
              = let (args,rest) = break (not.(==) '-'.head) xs
                in (UnresolvedDependency
                    { dependency = parseDep x
                    , depOptions = args }
                   ):parsePkgArgs rest

