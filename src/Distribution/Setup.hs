-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Setup
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  GHC, Hugs
--
-- Explanation: Data types and parser for the standard command-line
-- setup.  Will also return commands it doesn't know about.

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

module Distribution.Setup (--parseArgs,
                           module Distribution.Compiler,
                           Action(..), ConfigFlags(..),
                           CopyFlags, InstallFlags, RegisterFlags, SDistFlags,
			   --optionHelpString,
#ifdef DEBUG
                           hunitTests,
#endif
                           parseGlobalArgs,
                           parseConfigureArgs, parseBuildArgs, parseCleanArgs,
                           parseHaddockArgs, parseProgramaticaArgs, parseTestArgs,
                           parseInstallArgs, parseSDistArgs, parseRegisterArgs,
                           parseUnregisterArgs, parseCopyArgs
                           ) where


-- Misc:
#ifdef DEBUG
import HUnit (Test(..))
#endif

import Distribution.Compiler (CompilerFlavor(..), Compiler(..))
import Distribution.Simple.Utils (die)
import Data.List(find)
import Distribution.GetOpt
import System.Exit
import System.Environment

-- type CommandLineOpts = (Action,
--                         [String]) -- The un-parsed remainder

data Action = ConfigCmd ConfigFlags       -- config
            | BuildCmd                    -- build
            | CleanCmd                    -- clean
            | CopyCmd (Maybe FilePath)    -- copy
            | HaddockCmd                  -- haddock
            | ProgramaticaCmd             -- pfesetup
            | InstallCmd Bool -- install (install-prefix) (--user flag)
            | SDistCmd                    -- sdist
            | TestCmd                     -- test
            | RegisterCmd   Bool Bool     -- register (--user flag, --gen-script)
            | UnregisterCmd Bool Bool     -- unregister (--user flag, --gen-script)
	    | HelpCmd			  -- help
--            | NoCmd -- error case, help case.
--             | TestCmd 1.0?
--             | BDist -- 1.0
--            | CleanCmd                 -- clean
--            | NoCmd -- error case?
    deriving (Show, Eq)

-- | Flags to @configure@ command
data ConfigFlags = ConfigFlags {
        configHcFlavor :: Maybe CompilerFlavor,
        configHcPath   :: Maybe FilePath, -- ^given compiler location
        configHcPkg    :: Maybe FilePath, -- ^given hc-pkg location
        configHaddock  :: Maybe FilePath, -- ^Haddock path
        configHappy    :: Maybe FilePath, -- ^Happy path
        configAlex     :: Maybe FilePath, -- ^Alex path
        configHsc2hs   :: Maybe FilePath, -- ^Hsc2hs path
        configC2hs     :: Maybe FilePath, -- ^C2hs path
        configCpphs    :: Maybe FilePath, -- ^Cpphs path
        configGreencard:: Maybe FilePath, -- ^GreenCard path
        configProfLib  :: Bool,           -- ^Enable profiling in the library
        configProfExe  :: Bool,           -- ^Enable profiling in the executables.
        configPrefix   :: Maybe FilePath, -- ^installation prefix
        configVerbose  :: Int,            -- ^verbosity level
	configUser     :: Bool		  -- ^--user flag?
    }
    deriving (Show, Eq)

emptyConfigFlags :: ConfigFlags
emptyConfigFlags = ConfigFlags {
        configHcFlavor = Nothing,
        configHcPath   = Nothing,
        configHcPkg    = Nothing,
        configHaddock  = Nothing,
        configHappy    = Nothing,
        configAlex     = Nothing,
        configHsc2hs   = Nothing,
        configC2hs     = Nothing,
        configProfLib  = False,
        configProfExe  = False,
        configCpphs    = Nothing,
        configGreencard= Nothing,
        configPrefix   = Nothing,
        configVerbose  = 0,
	configUser     = False
    }

-- |Most of these flags are for Configure, but InstPrefix is for Copy.
data Flag a = GhcFlag | NhcFlag | HugsFlag
          | WithCompiler FilePath | WithHcPkg FilePath | Prefix FilePath
          | WithHaddock FilePath | WithHappy FilePath | WithAlex FilePath
          | WithHsc2hs FilePath | WithC2hs FilePath | WithCpphs FilePath
          | WithGreencard FilePath
          | WithProfLib | WithoutProfLib
          | WithProfExe | WithoutProfExe
          -- For install, register, and unregister:
          | UserFlag | GlobalFlag
          -- for register & unregister
          | GenScriptFlag
          -- For copy:
          | InstPrefix FilePath
          -- For sdist:
          | Snapshot
          -- For everyone:
          | HelpFlag
          | Verbose Int
--          | Version?
          | Lift a
            deriving (Show, Eq)

cmd_help :: OptDescr (Flag a)
cmd_help = Option "h?" ["help"] (NoArg HelpFlag) "Show this help text"

cmd_verbose :: OptDescr (Flag a)
cmd_verbose = Option "v" ["verbose"] (OptArg verboseFlag "n") "Control verbosity (n is 0--5, normal verbosity level is 1, -v alone is equivalent to -v3)"
  where
    verboseFlag mb_s = Verbose (maybe 3 read mb_s)

-- Do we have any other interesting global flags?
globalOptions :: [OptDescr (Flag a)]
globalOptions = [
  cmd_help
  ]

liftCustomOpts :: [OptDescr a] -> [OptDescr (Flag a)]
liftCustomOpts flags = [ Option shopt lopt (f adesc) help
                       | Option shopt lopt adesc help <- flags ]
  where f (NoArg x)    = NoArg (Lift x)
        f (ReqArg g s) = ReqArg (Lift . g) s
        f (OptArg g s) = OptArg (Lift . g) s

data Cmd a = Cmd {
        cmdName         :: String,
        cmdHelp         :: String, -- Short description
        cmdDescription  :: String, -- Long description
        cmdOptions      :: [OptDescr (Flag a)],
        cmdAction       :: Action
        }

commandList :: [Cmd a]
commandList = [configureCmd, buildCmd, cleanCmd, installCmd,
               copyCmd, sdistCmd, testCmd, haddockCmd, programaticaCmd,
               registerCmd, unregisterCmd]

lookupCommand :: String -> [Cmd a] -> Maybe (Cmd a)
lookupCommand name = find ((==name) . cmdName)

printGlobalHelp :: IO ()
printGlobalHelp = do pname <- getProgName
                     let syntax_line = "Usage: " ++ pname ++ " [GLOBAL FLAGS]\n  or:  " ++ pname ++ " COMMAND [FLAGS]\n\nGlobal flags:"
                     putStrLn (usageInfo syntax_line globalOptions)
                     putStrLn "Commands:"
                     let maxlen = maximum [ length (cmdName cmd) | cmd <- commandList ]
                     sequence_ [ do putStr "  "
                                    putStr (align maxlen (cmdName cmd))
                                    putStr "    "
                                    putStrLn (cmdHelp cmd)
                               | cmd <- commandList ]
                     putStrLn $ "\nFor more information about a command, try '" ++ pname ++ " COMMAND --help'."
  where align n str = str ++ replicate (n - length str) ' '

printCmdHelp :: Cmd a -> [OptDescr a] -> IO ()
printCmdHelp cmd opts = do pname <- getProgName
                           let syntax_line = "Usage: " ++ pname ++ " " ++ cmdName cmd ++ " [FLAGS]\n\nFlags for " ++ cmdName cmd ++ ":"
                           putStrLn (usageInfo syntax_line (cmdOptions cmd ++ liftCustomOpts opts))
                           putStr (cmdDescription cmd)

getCmdOpt :: Cmd a -> [OptDescr a] -> [String] -> ([Flag a], [String], [String])
getCmdOpt cmd opts s = let (a,_,c,d) = getOpt' Permute (cmdOptions cmd ++ liftCustomOpts opts) s
                         in (a,c,d)

-- We don't want to use elem, because that imposes Eq a
hasHelpFlag :: [Flag a] -> Bool
hasHelpFlag flags = not . null $ [ () | HelpFlag <- flags ]

parseGlobalArgs :: [String] -> IO (Action,[String])
parseGlobalArgs args =
  case getOpt' RequireOrder globalOptions args of
    (flags, _, _, []) | hasHelpFlag flags -> do
      printGlobalHelp
      exitWith ExitSuccess
    (_, cname:cargs, _, []) -> do
      case lookupCommand cname commandList of
        Just cmd -> return (cmdAction cmd,cargs)
        Nothing  -> die $ "Unrecognised command: " ++ cname ++ " (try --help)"
    (_, [], _, [])  -> die $ "No command given (try --help)"
    (_, _, _, errs) -> putErrors errs

configureCmd :: Cmd a
configureCmd = Cmd {
        cmdName        = "configure",
        cmdHelp        = "Prepare to build the package.",
        cmdDescription = "",  -- This can be a multi-line description
        cmdOptions     = [cmd_help, cmd_verbose,
           Option "g" ["ghc"] (NoArg GhcFlag) "compile with GHC",
           Option "n" ["nhc"] (NoArg NhcFlag) "compile with NHC",
           Option "" ["hugs"] (NoArg HugsFlag) "compile with hugs",
           Option "w" ["with-compiler"] (ReqArg WithCompiler "PATH")
               "give the path to a particular compiler",
           Option "" ["with-hc-pkg"] (ReqArg WithHcPkg "PATH")
               "give the path to the package tool",
           Option "" ["prefix"] (ReqArg Prefix "DIR")
               "bake this prefix in preparation of installation",
           Option "" ["with-haddock"] (ReqArg WithHaddock "PATH")
               "give the path to haddock",
           Option "" ["with-happy"] (ReqArg WithHappy "PATH")
               "give the path to happy",
           Option "" ["with-alex"] (ReqArg WithAlex "PATH")
               "give the path to alex",
           Option "" ["with-hsc2hs"] (ReqArg WithHsc2hs "PATH")
               "give the path to hsc2hs",
           Option "" ["with-c2hs"] (ReqArg WithC2hs "PATH")
               "give the path to c2hs",
           Option "" ["with-cpphs"] (ReqArg WithCpphs "PATH")
               "give the path to cpphs",
           Option "" ["with-greencard"] (ReqArg WithGreencard "PATH")
               "give the path to greencard",
           Option "p" ["enable-library-profiling"] (NoArg WithProfLib)
               "Enable library profiling",
           Option "" ["disable-library-profiling"] (NoArg WithoutProfLib)
               "Disable library profiling",
           Option "" ["enable-executable-profiling"] (NoArg WithProfExe)
               "Enable executable profiling",
           Option "" ["disable-executable-profiling"] (NoArg WithoutProfExe)
               "Disable executable profiling",
           Option "" ["user"] (NoArg UserFlag)
               "allow dependencies to be satisfied from the user package database",
           Option "" ["global"] (NoArg GlobalFlag)
               "(default) dependencies must be satisfied from the global package database"
           ],
        cmdAction      = ConfigCmd emptyConfigFlags
        }

parseConfigureArgs :: ConfigFlags -> [String] -> [OptDescr a] ->
                      IO (ConfigFlags, [a], [String])
parseConfigureArgs = parseArgs configureCmd updateCfg
  where updateCfg t GhcFlag              = t { configHcFlavor = Just GHC }
        updateCfg t NhcFlag              = t { configHcFlavor = Just NHC }
        updateCfg t HugsFlag             = t { configHcFlavor = Just Hugs }
        updateCfg t (WithCompiler path)  = t { configHcPath   = Just path }
        updateCfg t (WithHcPkg path)     = t { configHcPkg    = Just path }
        updateCfg t (WithHaddock path)   = t { configHaddock  = Just path }
        updateCfg t (WithHappy path)     = t { configHappy    = Just path }
        updateCfg t (WithAlex path)      = t { configAlex     = Just path }
        updateCfg t (WithHsc2hs path)    = t { configHsc2hs   = Just path }
        updateCfg t (WithC2hs path)      = t { configC2hs     = Just path }
        updateCfg t (WithCpphs path)     = t { configCpphs    = Just path }
        updateCfg t (WithGreencard path) = t { configGreencard= Just path }
        updateCfg t WithProfLib          = t { configProfLib  = True }
        updateCfg t WithoutProfLib       = t { configProfLib  = False }
        updateCfg t WithProfExe          = t { configProfExe  = True }
        updateCfg t WithoutProfExe       = t { configProfExe  = False }
        updateCfg t (Prefix path)        = t { configPrefix   = Just path }
        updateCfg t (Verbose n)          = t { configVerbose  = n }
        updateCfg t UserFlag             = t { configUser     = True }
        updateCfg t GlobalFlag           = t { configUser     = False }
        updateCfg t (Lift _)             = t
        updateCfg t _                    = error $ "Unexpected flag!"

buildCmd :: Cmd a
buildCmd = Cmd {
        cmdName        = "build",
        cmdHelp        = "Make this package ready for installation.",
        cmdDescription = "",  -- This can be a multi-line description
        cmdOptions     = [cmd_help, cmd_verbose],
        cmdAction      = BuildCmd
        }

parseBuildArgs :: [String] -> [OptDescr a] -> IO (Int, [a], [String])
parseBuildArgs = parseNoArgs buildCmd

haddockCmd :: Cmd a
haddockCmd = Cmd {
        cmdName        = "haddock",
        cmdHelp        = "Generate Haddock HTML code from Exposed-Modules.",
        cmdDescription = "Requires cpphs and haddock.",
        cmdOptions     = [cmd_help, cmd_verbose],
        cmdAction      = HaddockCmd
        }

programaticaCmd :: Cmd a
programaticaCmd = Cmd {
        cmdName        = "pfe",
        cmdHelp        = "Generate Programatica Project.",
        cmdDescription = "",
        cmdOptions     = [cmd_help, cmd_verbose],
        cmdAction      = ProgramaticaCmd
        }

parseProgramaticaArgs :: [String] -> [OptDescr a] -> IO (Int, [a], [String])
parseProgramaticaArgs  = parseNoArgs programaticaCmd

parseHaddockArgs :: [String] -> [OptDescr a] -> IO (Int, [a], [String])
parseHaddockArgs  = parseNoArgs haddockCmd

cleanCmd :: Cmd a
cleanCmd = Cmd {
        cmdName        = "clean",
        cmdHelp        = "Clean up after a build.",
        cmdDescription = "Removes .hi, .o, preprocessed sources, etc.\n", -- Multi-line!
        cmdOptions     = [cmd_help, cmd_verbose],
        cmdAction      = CleanCmd
        }

parseCleanArgs :: [String] -> [OptDescr a] -> IO (Int, [a], [String])
parseCleanArgs = parseNoArgs cleanCmd

installCmd :: Cmd a
installCmd = Cmd {
        cmdName        = "install",
        cmdHelp        = "Copy the files into the install locations. Run register.",
        cmdDescription = "Unlike the copy command, install calls the register command.\nIf you want to install into a location that is not what was\nspecified in the configure step, use the copy command.\n",
        cmdOptions     = [cmd_help, cmd_verbose,
           Option "" ["install-prefix"] (ReqArg InstPrefix "DIR")
               "[DEPRECATED, use copy]",
           Option "" ["user"] (NoArg UserFlag)
               "upon registration, register this package in the user's local package database",
           Option "" ["global"] (NoArg GlobalFlag)
               "(default) upon registration, register this package in the system-wide package database"
           ],
        cmdAction      = InstallCmd False
        }

copyCmd :: Cmd a
copyCmd = Cmd {
        cmdName        = "copy",
        cmdHelp        = "Copy the files into the install locations.",
        cmdDescription = "Does not call register, and allows a prefix at install time\nWithout the copy-prefix flag, configure determines location.\n",
        cmdOptions     = [cmd_help, cmd_verbose,
           Option "" ["copy-prefix"] (ReqArg InstPrefix "DIR")
               "specify the directory in which to place installed files"
           ],
        cmdAction      = CopyCmd Nothing
        }

-- | Flags to @copy@: (Copy Location, verbose)
type CopyFlags = (Maybe FilePath,Int)

parseCopyArgs :: CopyFlags -> [String] -> [OptDescr a] ->
                    IO (CopyFlags, [a], [String])
parseCopyArgs = parseArgs copyCmd updateCfg
  where updateCfg (mprefix,verbose) fl = case fl of
            InstPrefix path -> (Just path, verbose)
            Verbose n       -> (mprefix,   n)
            _               -> error $ "Unexpected flag!"

-- | Flags to @install@: (user package, verbose)
type InstallFlags = (Bool,Int)

parseInstallArgs :: InstallFlags -> [String] -> [OptDescr a] ->
                    IO (InstallFlags, [a], [String])
parseInstallArgs = parseArgs installCmd updateCfg
  where updateCfg (uFlag,verbose) fl = case fl of
            InstPrefix _ -> error "--install-prefix is obsolete. Use copy command instead."
            UserFlag     -> (True,  verbose)
            GlobalFlag   -> (False, verbose)
            Verbose n    -> (uFlag, n)
            _            -> error $ "Unexpected flag!"

sdistCmd :: Cmd a
sdistCmd = Cmd {
        cmdName        = "sdist",
        cmdHelp        = "Generate a source distribution file (.tar.gz or .zip).",
        cmdDescription = "",  -- This can be a multi-line description
        cmdOptions     = [cmd_help,cmd_verbose,
           Option "" ["snapshot"] (NoArg Snapshot)
               "Produce a snapshot source distribution"
           ],
        cmdAction      = SDistCmd
        }

-- | Flags to @sdist@: (snapshot, verbose)
type SDistFlags = (Bool, Int)

parseSDistArgs :: [String] -> [OptDescr a] -> IO (SDistFlags, [a], [String])
parseSDistArgs = parseArgs sdistCmd updateCfg (False, 0)
  where updateCfg (snapshot, verbose) fl = case fl of
            Snapshot        -> (True,     verbose)
            Verbose n       -> (snapshot, n)
            _               -> error $ "Unexpected flag!"

testCmd :: Cmd a
testCmd = Cmd {
        cmdName        = "test",
        cmdHelp        = "Run the test suite, if any (configure with UserHooks).",
        cmdDescription = "",  -- This can be a multi-line description
        cmdOptions     = [cmd_help,cmd_verbose],
        cmdAction      = TestCmd
        }

parseTestArgs :: [String] -> [OptDescr a] -> IO (Int, [a], [String])
parseTestArgs = parseNoArgs testCmd

registerCmd :: Cmd a
registerCmd = Cmd {
        cmdName        = "register",
        cmdHelp        = "Register this package with the compiler.",
        cmdDescription = "",  -- This can be a multi-line description
        cmdOptions     = [cmd_help, cmd_verbose,
           Option "" ["user"] (NoArg UserFlag)
               "upon registration, register this package in the user's local package database",
           Option "" ["global"] (NoArg GlobalFlag)
               "(default) upon registration, register this package in the system-wide package database",
           Option "" ["gen-script"] (NoArg GenScriptFlag)
               "Instead of performing the register command, generate a script to register later"
           ],
        cmdAction      = RegisterCmd False False
        }

-- | Flags to @register@ and @unregister@: (user package, gen-script, verbose)
type RegisterFlags = (Bool, Bool, Int)

parseRegisterArgs :: RegisterFlags -> [String] -> [OptDescr a] ->
                     IO (RegisterFlags, [a], [String])
parseRegisterArgs = parseArgs registerCmd updateCfg
  where updateCfg (uFlag, genScriptFlag, verbose) fl = case fl of
            UserFlag        -> (True,  genScriptFlag, verbose)
            GlobalFlag      -> (False, genScriptFlag, verbose)
            Verbose n       -> (uFlag, genScriptFlag, n)
            GenScriptFlag   -> (uFlag, True,          verbose)
            _               -> error $ "Unexpected flag!"

unregisterCmd :: Cmd a
unregisterCmd = Cmd {
        cmdName        = "unregister",
        cmdHelp        = "Unregister this package with the compiler.",
        cmdDescription = "",  -- This can be a multi-line description
        cmdOptions     = [cmd_help, cmd_verbose,
           Option "" ["user"] (NoArg UserFlag)
               "unregister this package in the user's local package database",
           Option "" ["global"] (NoArg GlobalFlag)
               "(default) unregister this package in the system-wide package database",
           Option "" ["gen-script"] (NoArg GenScriptFlag)
               "Instead of performing the unregister command, generate a script to unregister later"

           ],
        cmdAction      = UnregisterCmd False False
        }

parseUnregisterArgs :: RegisterFlags -> [String] -> [OptDescr a] ->
                       IO (RegisterFlags, [a], [String])
parseUnregisterArgs = parseRegisterArgs

-- |Helper function for commands with no arguments except for verbose
-- and help.

parseNoArgs :: (Cmd a) -> [String] -> [OptDescr a] -> IO (Int, [a], [String])
parseNoArgs cmd = parseArgs cmd updateCfg 0
  where
    updateCfg _ (Verbose n) = n
    updateCfg _ _           = error "Unexpected flag!"

-- |Helper function for commands with more options.

parseArgs :: Cmd a -> (cfg -> Flag a -> cfg) -> cfg ->
        [String] -> [OptDescr a] -> IO (cfg, [a], [String])
parseArgs cmd updateCfg cfg args customOpts =
  case getCmdOpt cmd customOpts args of
    (flags, _, []) | hasHelpFlag flags -> do
      printCmdHelp cmd customOpts
      exitWith ExitSuccess
    (flags, args', []) ->
      let flags' = filter (not.isLift) flags in
      return (foldl updateCfg cfg flags', unliftFlags flags, args')
    (_, _, errs) -> putErrors errs
  where
    isLift (Lift _) = True
    isLift _        = False
    unliftFlags :: [Flag a] -> [a]
    unliftFlags flags = [ fl | Lift fl <- flags ]

putErrors :: [String] -> IO a
putErrors errs = die $ "Errors:" ++ concat ['\n':err | err <- errs]


#ifdef DEBUG
hunitTests :: [Test]
hunitTests = []
-- The test cases kinda have to be rewritten from the ground up... :/
--hunitTests =
--    let m = [("ghc", GHC), ("nhc", NHC), ("hugs", Hugs)]
--        (flags, commands', unkFlags, ers)
--               = getOpt Permute options ["configure", "foobar", "--prefix=/foo", "--ghc", "--nhc", "--hugs", "--with-compiler=/comp", "--unknown1", "--unknown2", "--install-prefix=/foo", "--user", "--global"]
--       in  [TestLabel "very basic option parsing" $ TestList [
--                 "getOpt flags" ~: "failed" ~:
--                 [Prefix "/foo", GhcFlag, NhcFlag, HugsFlag,
--                  WithCompiler "/comp", InstPrefix "/foo", UserFlag, GlobalFlag]
--                 ~=? flags,
--                 "getOpt commands" ~: "failed" ~: ["configure", "foobar"] ~=? commands',
--                 "getOpt unknown opts" ~: "failed" ~:
--                      ["--unknown1", "--unknown2"] ~=? unkFlags,
--                 "getOpt errors" ~: "failed" ~: [] ~=? ers],
--
--               TestLabel "test location of various compilers" $ TestList
--               ["configure parsing for prefix and compiler flag" ~: "failed" ~:
--                    (Right (ConfigCmd (Just comp, Nothing, Just "/usr/local"), []))
--                   ~=? (parseArgs ["--prefix=/usr/local", "--"++name, "configure"])
--                   | (name, comp) <- m],
--
--               TestLabel "find the package tool" $ TestList
--               ["configure parsing for prefix comp flag, withcompiler" ~: "failed" ~:
--                    (Right (ConfigCmd (Just comp, Just "/foo/comp", Just "/usr/local"), []))
--                   ~=? (parseArgs ["--prefix=/usr/local", "--"++name,
--                                   "--with-compiler=/foo/comp", "configure"])
--                   | (name, comp) <- m],
--
--               TestLabel "simpler commands" $ TestList
--               [flag ~: "failed" ~: (Right (flagCmd, [])) ~=? (parseArgs [flag])
--                   | (flag, flagCmd) <- [("build", BuildCmd),
--                                         ("install", InstallCmd Nothing False),
--                                         ("sdist", SDistCmd),
--                                         ("register", RegisterCmd False)]
--                  ]
--               ]
#endif


{- Testing ideas:
   * IO to look for hugs and hugs-pkg (which hugs, etc)
   * quickCheck to test permutations of arguments
   * what other options can we over-ride with a command-line flag?
-}

