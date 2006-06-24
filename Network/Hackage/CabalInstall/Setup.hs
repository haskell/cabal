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
    , parseInstallArgs
    , parseGlobalArgs
    ) where

import Text.ParserCombinators.ReadP (readP_to_S)
import Distribution.ParseUtils (parseDependency)
import Distribution.Setup (defaultCompilerFlavor, CompilerFlavor(..))
import Data.List (find)
import System.Console.GetOpt (ArgDescr (..), ArgOrder (..), OptDescr (..), usageInfo, getOpt')
import System.Exit (exitWith, ExitCode (..))
import System.Environment (getProgName)

import Network.Hackage.CabalInstall.Types (TempFlags (..), Flag (..), Action (..)
                                      , UnresolvedDependency (..))

emptyTempFlags :: TempFlags
emptyTempFlags = TempFlags {
        tempHcFlavor    = defaultCompilerFlavor, -- Nothing,
        tempHcPath      = Nothing,
        tempConfPath    = Nothing,
        tempHcPkg       = Nothing,
        tempPrefix      = Nothing,
        tempServers     = [],
        tempRunHc       = Nothing,
        tempTarPath     = Nothing,
        tempVerbose     = 3,
--        tempUpgradeDeps = False,
        tempUser        = False,
        tempUserIns     = False
   }

cmd_verbose :: OptDescr Flag
cmd_verbose = Option "v" ["verbose"] (OptArg verboseFlag "n")
              "Control verbosity (n is 0--5, normal verbosity level is 1, -v alone is equivalent to -v3)"
  where
    verboseFlag mb_s = Verbose (maybe 3 read mb_s)

globalOptions :: [OptDescr Flag]
globalOptions =
    [ Option "h?" ["help"] (NoArg HelpFlag) "Show this help text"
    , cmd_verbose 
    , Option "g" ["ghc"] (NoArg GhcFlag) "compile with GHC"
    , Option "n" ["nhc"] (NoArg NhcFlag) "compile with NHC"
    , Option "" ["hugs"] (NoArg HugsFlag) "compile with hugs"
    , Option "s" ["with-server"] (ReqArg WithServer "URL")
                 "give the URL to a Hackage server"
    , Option "c" ["config-path"] (ReqArg WithConfPath "PATH")
                 "give the path to the config dir. Default is /etc/cabal-install"
    , Option "" ["tar-path"] (ReqArg WithTarPath "PATH")
                 "give the path to tar"
    , Option "w" ["with-compiler"] (ReqArg WithCompiler "PATH")
                 "give the path to a particular compiler"
    , Option "" ["with-hc-pkg"] (ReqArg WithHcPkg "PATH")
                 "give the path to the package tool"
--    , Option "" ["upgrade-deps"] (NoArg UpgradeDeps)
--                 "Upgrade all dependencies which depend on the newly installed packages"
    , Option "" ["user-install"] (NoArg UserInstallFlag)
                 "upon registration, register this package in the user's local package database"
    , Option "" ["global-install"] (NoArg GlobalInstallFlag)
                 "upon registration, register this package in the system-wide package database"
    , Option "" ["user-deps"] (NoArg UserFlag)
                 "allow dependencies to be satisfied from the user package database"
    , Option "" ["global-deps"] (NoArg GlobalFlag)
                 "(default) dependencies must be satisfied from the global package database"
    ]

data Cmd = Cmd {
        cmdName         :: String,
        cmdHelp         :: String, -- Short description
        cmdDescription  :: String, -- Long description
        cmdOptions      :: [OptDescr Flag ],
        cmdAction       :: Action
        }


commandList :: [Cmd]
commandList = [fetchCmd, installCmd, buildDepCmd, updateCmd, cleanCmd, listCmd, infoCmd]

lookupCommand :: String -> [Cmd] -> Maybe Cmd
lookupCommand name = find ((==name) . cmdName)

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

printCmdHelp :: Cmd -> IO ()
printCmdHelp cmd = do pname <- getProgName
                      let syntax_line = "Usage: " ++ pname ++ " " ++ cmdName cmd ++ " [FLAGS]\n\nFlags for " ++ cmdName cmd ++ ":"
                      putStrLn (usageInfo syntax_line (cmdOptions cmd))
                      putStr (cmdDescription cmd)


-- We don't want to use elem, because that imposes Eq a
hasHelpFlag :: [Flag] -> Bool
hasHelpFlag flags = not . null $ [ () | HelpFlag <- flags ]

parseGlobalArgs :: [String] -> IO (Action,TempFlags,[String])
parseGlobalArgs args =
  case getOpt' RequireOrder globalOptions args of
    (flags, _, _, []) | hasHelpFlag flags -> do
      printGlobalHelp
      exitWith ExitSuccess
    (flags, cname:cargs, _, []) -> do
      case lookupCommand cname commandList of
        Just cmd -> return (cmdAction cmd,mkTempFlags flags emptyTempFlags, cargs)
        Nothing  -> do putStrLn $ "Unrecognised command: " ++ cname ++ " (try --help)"
                       exitWith (ExitFailure 1)
    (_, [], _, [])  -> do putStrLn $ "No command given (try --help)"
                          exitWith (ExitFailure 1)
    (_, _, _, errs) -> do putStrLn "Errors:"
                          mapM_ putStrLn errs
                          exitWith (ExitFailure 1)

mkTempFlags :: [Flag] -> TempFlags -> TempFlags
mkTempFlags = updateCfg
  where updateCfg (fl:flags) t = updateCfg flags $
          case fl of
            GhcFlag           -> t { tempHcFlavor    = Just GHC }
            NhcFlag           -> t { tempHcFlavor    = Just NHC }
            HugsFlag          -> t { tempHcFlavor    = Just Hugs }
            WithCompiler path -> t { tempHcPath      = Just path }
            WithConfPath path -> t { tempConfPath    = Just path }
            WithHcPkg path    -> t { tempHcPkg       = Just path }
            WithServer url    -> t { tempServers     = url:tempServers t }
            Verbose n         -> t { tempVerbose     = n }
--            UpgradeDeps       -> t { tempUpgradeDeps = True }
            UserFlag          -> t { tempUser        = True }
            GlobalFlag        -> t { tempUser        = False }
            UserInstallFlag   -> t { tempUserIns     = True }
            GlobalInstallFlag -> t { tempUserIns     = False }
            _                 -> error $ "Unexpected flag!"
        updateCfg [] t = t

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
buildDepCmd = mkCmd "build-dep" "Installs the dependencies for a list of packages." "" BuildDepCmd

updateCmd :: Cmd
updateCmd = mkCmd "update" "Updates list of known packages" "" UpdateCmd

cleanCmd :: Cmd
cleanCmd = mkCmd "clean" "Removes downloaded files" "" CleanCmd

infoCmd :: Cmd
infoCmd = mkCmd "info" "Emit some info"
           "Emits information about dependency resolution" InfoCmd

parseInstallArgs :: [String] -> IO ([String],[UnresolvedDependency])
parseInstallArgs [] = do printCmdHelp installCmd
                         exitWith ExitSuccess
parseInstallArgs args
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

