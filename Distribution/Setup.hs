{-# OPTIONS_GHC -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Setup
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
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
                           Action(..),
                           ConfigFlags(..), emptyConfigFlags, configureArgs,
                           CopyFlags(..), CopyDest(..), 
			   InstallFlags(..), emptyInstallFlags,
                           HaddockFlags(..), emptyHaddockFlags,
                           BuildFlags(..), CleanFlags(..), PFEFlags(..),
                           RegisterFlags(..), emptyRegisterFlags,
			   SDistFlags(..),
                           MaybeUserFlag(..), userOverride,
			   --optionHelpString,
#ifdef DEBUG
                           hunitTests,
#endif
                           parseGlobalArgs, defaultCompilerFlavor,
                           parseConfigureArgs, parseBuildArgs, parseCleanArgs,
                           parseHaddockArgs, parseProgramaticaArgs, parseTestArgs,
                           parseInstallArgs, parseSDistArgs, parseRegisterArgs,
                           parseUnregisterArgs, parseCopyArgs,
                           reqPathArg, reqDirArg
                           ) where


-- Misc:
#ifdef DEBUG
import HUnit (Test(..))
#endif

import Distribution.Compiler (CompilerFlavor(..), Compiler(..))
import Distribution.Simple.Utils (die)
import Distribution.Program(ProgramConfiguration(..),
                            userSpecifyPath, userSpecifyArgs)
import Data.List(find)
import Distribution.Compat.Map (keys)
import Distribution.GetOpt
import Distribution.Compat.FilePath (platformPath)
import System.Exit
import System.Environment

-- type CommandLineOpts = (Action,
--                         [String]) -- The un-parsed remainder

data Action = ConfigCmd ConfigFlags   -- config
            | BuildCmd                -- build
            | CleanCmd                -- clean
            | CopyCmd CopyDest        -- copy (--destdir flag)
            | HaddockCmd              -- haddock
            | ProgramaticaCmd         -- pfesetup
            | InstallCmd              -- install (install-prefix)
            | SDistCmd                -- sdist
            | TestCmd                 -- test
            | RegisterCmd    	      -- register
            | UnregisterCmd           -- unregister
	    | HelpCmd		      -- help
--            | NoCmd -- error case, help case.
--             | TestCmd 1.0?
--             | BDist -- 1.0
--            | CleanCmd                 -- clean
--            | NoCmd -- error case?

-- ------------------------------------------------------------
-- * Flag-related types
-- ------------------------------------------------------------

-- | Flags to @configure@ command
data ConfigFlags = ConfigFlags {
        configPrograms :: ProgramConfiguration, -- ^All programs that cabal may run
        configHcFlavor :: Maybe CompilerFlavor,
        configHcPath   :: Maybe FilePath, -- ^given compiler location
        configHcPkg    :: Maybe FilePath, -- ^given hc-pkg location
        configHappy    :: Maybe FilePath, -- ^Happy path
        configAlex     :: Maybe FilePath, -- ^Alex path
        configHsc2hs   :: Maybe FilePath, -- ^Hsc2hs path
        configC2hs     :: Maybe FilePath, -- ^C2hs path
        configCpphs    :: Maybe FilePath, -- ^Cpphs path
        configGreencard:: Maybe FilePath, -- ^GreenCard path
        configVanillaLib  :: Bool,        -- ^Enable vanilla library
        configProfLib  :: Bool,           -- ^Enable profiling in the library
        configProfExe  :: Bool,           -- ^Enable profiling in the executables.
        configPrefix   :: Maybe FilePath,
		-- ^installation prefix
	configBinDir   :: Maybe FilePath, 
		-- ^installation dir for binaries,
	configLibDir   :: Maybe FilePath, 
		-- ^installation dir for object code libraries, 
	configLibSubDir :: Maybe FilePath,
		-- ^subdirectory of libdir in which libs are installed
	configLibExecDir :: Maybe FilePath,
		-- ^installation dir for program executables,
	configDataDir  :: Maybe FilePath,
		-- ^installation dir for read-only arch-independent data,
	configDataSubDir :: Maybe FilePath,
		-- ^subdirectory of datadir in which data files are installed

        configVerbose  :: Int,            -- ^verbosity level
	configUser     :: Bool,		  -- ^--user flag?
	configGHCiLib  :: Bool,           -- ^Enable compiling library for GHCi
	configSplitObjs :: Bool		  -- ^Enable -split-objs with GHC
    }

emptyConfigFlags :: ProgramConfiguration -> ConfigFlags
emptyConfigFlags progConf = ConfigFlags {
        configPrograms = progConf,
        configHcFlavor = defaultCompilerFlavor,
        configHcPath   = Nothing,
        configHcPkg    = Nothing,
--        configHaddock  = EmptyLocation,
        configHappy    = Nothing,
        configAlex     = Nothing,
        configHsc2hs   = Nothing,
        configC2hs     = Nothing,
        configVanillaLib  = True,
        configProfLib  = False,
        configProfExe  = False,
        configCpphs    = Nothing,
        configGreencard= Nothing,
        configPrefix   = Nothing,
	configBinDir   = Nothing,
	configLibDir   = Nothing,
	configLibSubDir = Nothing,
	configLibExecDir = Nothing,
	configDataDir  = Nothing,
	configDataSubDir = Nothing,
        configVerbose  = 0,
	configUser     = False,
	configGHCiLib  = True,
	configSplitObjs = False -- takes longer, so turn off by default
    }

-- | Flags to @copy@: (destdir, copy-prefix (backwards compat), verbose)
data CopyFlags = CopyFlags {copyDest      :: CopyDest
                           ,copyVerbose :: Int}

data CopyDest
  = NoCopyDest
  | CopyTo FilePath
  | CopyPrefix FilePath		-- DEPRECATED
  deriving (Eq, Show)

data MaybeUserFlag  = MaybeUserNone   -- ^no --user OR --global flag.
                    | MaybeUserUser   -- ^--user flag
                    | MaybeUserGlobal -- ^--global flag

-- |A 'MaybeUserFlag' overrides the default --user setting
userOverride :: MaybeUserFlag -> Bool -> Bool
MaybeUserUser   `userOverride` _ = True
MaybeUserGlobal `userOverride` _ = False
_               `userOverride` r = r

-- | Flags to @install@: (user package, verbose)
data InstallFlags = InstallFlags {installUserFlags::MaybeUserFlag
                                 ,installVerbose :: Int}

emptyInstallFlags :: InstallFlags
emptyInstallFlags = InstallFlags{ installUserFlags=MaybeUserNone,
				  installVerbose=0 }

-- | Flags to @sdist@: (snapshot, verbose)
data SDistFlags = SDistFlags {sDistSnapshot::Bool
                             ,sDistVerbose:: Int}

-- | Flags to @register@ and @unregister@: (user package, gen-script, 
-- in-place, verbose)
data RegisterFlags = RegisterFlags {regUser::MaybeUserFlag
                                   ,regGenScript::Bool
				   ,regInPlace::Bool
				   ,regWithHcPkg::Maybe FilePath
                                   ,regVerbose::Int}


emptyRegisterFlags :: RegisterFlags
emptyRegisterFlags = RegisterFlags { regUser=MaybeUserNone,
				     regGenScript=False,
				     regInPlace=False,
				     regWithHcPkg=Nothing,
				     regVerbose=0 }

data HaddockFlags = HaddockFlags {haddockHoogle :: Bool
                                 ,haddockVerbose :: Int}

emptyHaddockFlags :: HaddockFlags
emptyHaddockFlags = HaddockFlags {haddockHoogle = False, haddockVerbose = 0}

-- Following only have verbose flags, but for consistency and
-- extensibility we make them into a type.
data BuildFlags   = BuildFlags   {buildVerbose   :: Int}
data CleanFlags   = CleanFlags   {cleanVerbose   :: Int}
data PFEFlags     = PFEFlags     {pfeVerbose     :: Int}

-- |Most of these flags are for Configure, but InstPrefix is for Copy.
data Flag a = GhcFlag | NhcFlag | HugsFlag | JhcFlag
          | WithCompiler FilePath | WithHcPkg FilePath
          | WithHappy FilePath | WithAlex FilePath
          | WithHsc2hs FilePath | WithC2hs FilePath | WithCpphs FilePath
          | WithGreencard FilePath
          | WithVanillaLib | WithoutVanillaLib
          | WithProfLib | WithoutProfLib
          | WithProfExe | WithoutProfExe
	  | WithGHCiLib | WithoutGHCiLib
	  | WithSplitObjs | WithoutSplitObjs

	  | Prefix FilePath
	  | BinDir FilePath
	  | LibDir FilePath
	  | LibSubDir FilePath
	  | LibExecDir FilePath
	  | DataDir FilePath
	  | DataSubDir FilePath

          | ProgramArgs String String   -- program name, arguments
          | WithProgram String FilePath -- program name, location

          -- For install, register, and unregister:
          | UserFlag | GlobalFlag
          -- for register & unregister
          | GenScriptFlag
	  | InPlaceFlag
          -- For copy:
          | InstPrefix FilePath
	  | DestDir FilePath
          -- For sdist:
          | Snapshot
          -- For haddock:
          | HaddockHoogle
          -- For everyone:
          | HelpFlag
          | Verbose Int
--          | Version?
          | Lift a
            deriving (Show, Eq)


-- ------------------------------------------------------------
-- * Mostly parsing functions
-- ------------------------------------------------------------

defaultCompilerFlavor :: Maybe CompilerFlavor
defaultCompilerFlavor =
#if defined(__GLASGOW_HASKELL__)
   Just GHC
#elif defined(__NHC__)
   Just NHC
#elif defined(__JHC__)
   Just JHC
#elif defined(__HUGS__)
   Just Hugs
#else
   Nothing
#endif

-- | Arguments to pass to a @configure@ script, e.g. generated by
-- @autoconf@.
configureArgs :: ConfigFlags -> [String]
configureArgs flags
  = hc_flag ++
        optFlag "with-hc-pkg" configHcPkg ++
        optFlag "prefix" configPrefix ++
        optFlag "bindir" configBinDir ++
        optFlag "libdir" configLibDir ++
        optFlag "libexecdir" configLibExecDir ++
        optFlag "datadir" configDataDir
  where
        hc_flag = case (configHcFlavor flags, configHcPath flags) of
                        (_, Just hc_path)  -> ["--with-hc=" ++ hc_path]
                        (Just hc, Nothing) -> ["--with-hc=" ++ showHC hc]
                        (Nothing,Nothing)  -> []
        optFlag name config_field = case config_field flags of
                        Just p -> ["--" ++ name ++ "=" ++ p]
                        Nothing -> []

        showHC GHC = "ghc"
        showHC NHC = "nhc98"
        showHC JHC = "jhc"
        showHC Hugs = "hugs"
        showHC c    = "unknown compiler: " ++ (show c)


cmd_help :: OptDescr (Flag a)
cmd_help = Option "h?" ["help"] (NoArg HelpFlag) "Show this help text"

cmd_verbose :: OptDescr (Flag a)
cmd_verbose = Option "v" ["verbose"] (OptArg verboseFlag "n") "Control verbosity (n is 0--5, normal verbosity level is 1, -v alone is equivalent to -v3)"
  where
    verboseFlag mb_s = Verbose (maybe 3 read mb_s)

cmd_with_hc_pkg :: OptDescr (Flag a)
cmd_with_hc_pkg = Option "" ["with-hc-pkg"] (reqPathArg WithHcPkg)
               "give the path to the package tool"

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

commandList :: ProgramConfiguration -> [Cmd a]
commandList progConf = [(configureCmd progConf), buildCmd, cleanCmd, installCmd,
                        copyCmd, sdistCmd, testCmd, haddockCmd, programaticaCmd,
                        registerCmd, unregisterCmd]

lookupCommand :: String -> [Cmd a] -> Maybe (Cmd a)
lookupCommand name = find ((==name) . cmdName)

printGlobalHelp :: ProgramConfiguration -> IO ()
printGlobalHelp progConf = 
                  do pname <- getProgName
                     let syntax_line = "Usage: " ++ pname ++ " [GLOBAL FLAGS]\n  or:  " ++ pname ++ " COMMAND [FLAGS]\n\nGlobal flags:"
                     putStrLn (usageInfo syntax_line globalOptions)
                     putStrLn "Commands:"
                     let maxlen = maximum [ length (cmdName cmd) | cmd <- (commandList progConf) ]
                     sequence_ [ do putStr "  "
                                    putStr (align maxlen (cmdName cmd))
                                    putStr "    "
                                    putStrLn (cmdHelp cmd)
                               | cmd <- (commandList progConf) ]
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

parseGlobalArgs :: ProgramConfiguration -> [String] -> IO (Action,[String])
parseGlobalArgs progConf args =
  case getOpt' RequireOrder globalOptions args of
    (flags, _, _, []) | hasHelpFlag flags -> do
      (printGlobalHelp progConf)
      exitWith ExitSuccess
    (_, cname:cargs, extra_args, []) -> do
      case lookupCommand cname (commandList progConf) of
        Just cmd -> return (cmdAction cmd, extra_args ++ cargs)
        Nothing  -> die $ "Unrecognised command: " ++ cname ++ " (try --help)"
    (_, [], _, [])  -> die $ "No command given (try --help)"
    (_, _, _, errs) -> putErrors errs

configureCmd :: ProgramConfiguration -> Cmd a
configureCmd progConf = Cmd {
        cmdName        = "configure",
        cmdHelp        = "Prepare to build the package.",
        cmdDescription = "",  -- This can be a multi-line description
        cmdOptions     = [cmd_help, cmd_verbose,
           Option "g" ["ghc"] (NoArg GhcFlag) "compile with GHC",
           Option "n" ["nhc"] (NoArg NhcFlag) "compile with NHC",
           Option "" ["jhc"]  (NoArg JhcFlag) "compile with JHC",
           Option "" ["hugs"] (NoArg HugsFlag) "compile with hugs",
           Option "w" ["with-compiler"] (reqPathArg WithCompiler)
               "give the path to a particular compiler",
	   cmd_with_hc_pkg,
           Option "" ["prefix"] (reqDirArg Prefix)
               "bake this prefix in preparation of installation",
	   Option "" ["bindir"] (reqDirArg BinDir)
		"installation directory for executables",
	   Option "" ["libdir"] (reqDirArg LibDir)
		"installation directory for libraries",
	   Option "" ["libsubdir"] (reqDirArg LibSubDir)
		"subdirectory of libdir in which libs are installed",
	   Option "" ["libexecdir"] (reqDirArg LibExecDir)
		"installation directory for program executables",
	   Option "" ["datadir"] (reqDirArg DataDir)
		"installation directory for read-only data",
	   Option "" ["datasubdir"] (reqDirArg DataSubDir)
		"subdirectory of datadir in which data files are installed",
           Option "" ["with-happy"] (reqPathArg WithHappy)
               "give the path to happy",
           Option "" ["with-alex"] (reqPathArg WithAlex)
               "give the path to alex",
           Option "" ["with-hsc2hs"] (reqPathArg WithHsc2hs)
               "give the path to hsc2hs",
           Option "" ["with-c2hs"] (reqPathArg WithC2hs)
               "give the path to c2hs",
           Option "" ["with-cpphs"] (reqPathArg WithCpphs)
               "give the path to cpphs",
           Option "" ["with-greencard"] (reqPathArg WithGreencard)
               "give the path to greencard",
           Option "" ["enable-library-vanilla"] (NoArg WithVanillaLib)
               "Enable vanilla libraries",
           Option "" ["disable-library-vanilla"] (NoArg WithoutVanillaLib)
               "Disable vanilla libraries",
           Option "p" ["enable-library-profiling"] (NoArg WithProfLib)
               "Enable library profiling",
           Option "" ["disable-library-profiling"] (NoArg WithoutProfLib)
               "Disable library profiling",
           Option "" ["enable-executable-profiling"] (NoArg WithProfExe)
               "Enable executable profiling",
           Option "" ["disable-executable-profiling"] (NoArg WithoutProfExe)
               "Disable executable profiling",
	   Option "" ["enable-library-for-ghci"] (NoArg WithGHCiLib)
               "compile library for use with GHCi",
	   Option "" ["disable-library-for-ghci"] (NoArg WithoutGHCiLib)
               "do not compile libraries for GHCi",
	   Option "" ["enable-split-objs"] (NoArg WithSplitObjs)
	       "split library into smaller objects to reduce binary sizes (GHC 6.6+)",
	   Option "" ["disable-split-objs"] (NoArg WithoutSplitObjs)
	       "split library into smaller objects to reduce binary sizes (GHC 6.6+)",
           Option "" ["user"] (NoArg UserFlag)
               "allow dependencies to be satisfied from the user package database. also implies install --user",
           Option "" ["global"] (NoArg GlobalFlag)
               "(default) dependencies must be satisfied from the global package database"
           ]
{- 
   FIX: Instead of using ++ here, we might add extra arguments.  That
   way, we can condense the help out put to something like
   --with-{haddock,happy,alex,etc}

   FIX: shouldn't use default. Look in hooks?.
-}
         ++ (withProgramOptions progConf)
         ++ (programArgsOptions progConf),
        cmdAction      = ConfigCmd (emptyConfigFlags progConf)
        }

programArgsOptions :: ProgramConfiguration -> [OptDescr (Flag a)]
programArgsOptions (ProgramConfiguration conf) = map f (keys conf)
    where f name = Option "" [name ++ "-args"] (reqPathArg (ProgramArgs name))
                   ("give the args to " ++ name)

withProgramOptions :: ProgramConfiguration -> [OptDescr (Flag a)]
withProgramOptions (ProgramConfiguration conf) = map f (keys conf)
    where f name = Option "" ["with-" ++ name] (reqPathArg (WithProgram name))
                   ("give the path to " ++ name)

reqPathArg :: (FilePath -> a) -> ArgDescr a
reqPathArg constr = ReqArg (constr . platformPath) "PATH"

reqDirArg :: (FilePath -> a) -> ArgDescr a
reqDirArg constr = ReqArg (constr . platformPath) "DIR"

parseConfigureArgs :: ProgramConfiguration -> ConfigFlags -> [String] -> [OptDescr a] ->
                      IO (ConfigFlags, [a], [String])
parseConfigureArgs progConf = parseArgs (configureCmd progConf) updateCfg
  where updateCfg t GhcFlag              = t { configHcFlavor = Just GHC }
        updateCfg t NhcFlag              = t { configHcFlavor = Just NHC }
        updateCfg t JhcFlag              = t { configHcFlavor = Just JHC }
        updateCfg t HugsFlag             = t { configHcFlavor = Just Hugs }
        updateCfg t (WithCompiler path)  = t { configHcPath   = Just path }
        updateCfg t (WithHcPkg path)     = t { configHcPkg    = Just path }
        updateCfg t (WithHappy path)     = t { configHappy    = Just path }
        updateCfg t (WithAlex path)      = t { configAlex     = Just path }
        updateCfg t (WithHsc2hs path)    = t { configHsc2hs   = Just path }
        updateCfg t (WithC2hs path)      = t { configC2hs     = Just path }
        updateCfg t (WithCpphs path)     = t { configCpphs    = Just path }
        updateCfg t (WithGreencard path) = t { configGreencard= Just path }
        updateCfg t (ProgramArgs name args) = t { configPrograms = (userSpecifyArgs
                                                                 name
                                                                 args (configPrograms t))}
        updateCfg t (WithProgram name path) = t { configPrograms = (userSpecifyPath
                                                                 name
                                                                 path (configPrograms t))}
        updateCfg t WithVanillaLib       = t { configVanillaLib  = True }
        updateCfg t WithoutVanillaLib    = t { configVanillaLib  = False, configGHCiLib = False }
        updateCfg t WithProfLib          = t { configProfLib  = True }
        updateCfg t WithoutProfLib       = t { configProfLib  = False }
        updateCfg t WithProfExe          = t { configProfExe  = True }
        updateCfg t WithoutProfExe       = t { configProfExe  = False }
	updateCfg t WithGHCiLib          = t { configGHCiLib  = True }
	updateCfg t WithoutGHCiLib       = t { configGHCiLib  = False }
        updateCfg t (Prefix path)        = t { configPrefix   = Just path }
        updateCfg t (BinDir path)        = t { configBinDir   = Just path }
        updateCfg t (LibDir path)        = t { configLibDir   = Just path }
        updateCfg t (LibSubDir path)     = t { configLibSubDir= Just path }
        updateCfg t (LibExecDir path)    = t { configLibExecDir = Just path }
        updateCfg t (DataDir path)       = t { configDataDir  = Just path }
        updateCfg t (DataSubDir path)    = t { configDataSubDir = Just path }
        updateCfg t (Verbose n)          = t { configVerbose  = n }
        updateCfg t UserFlag             = t { configUser     = True }
        updateCfg t GlobalFlag           = t { configUser     = False }
	updateCfg t WithSplitObjs	 = t { configSplitObjs = True }
	updateCfg t WithoutSplitObjs	 = t { configSplitObjs = False }
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

parseBuildArgs :: [String] -> [OptDescr a] -> IO (BuildFlags, [a], [String])
parseBuildArgs = parseNoArgs buildCmd BuildFlags

haddockCmd :: Cmd a
haddockCmd = Cmd {
        cmdName        = "haddock",
        cmdHelp        = "Generate Haddock HTML code from Exposed-Modules.",
        cmdDescription = "Requires cpphs and haddock.",
        cmdOptions     = [cmd_help, cmd_verbose,
                          Option "" ["hoogle"] (NoArg HaddockHoogle) "Generate a hoogle database"],
        cmdAction      = HaddockCmd
        }

parseHaddockArgs :: HaddockFlags -> [String] -> [OptDescr a] -> IO (HaddockFlags, [a], [String])
parseHaddockArgs  = parseArgs haddockCmd updateCfg
  where updateCfg (HaddockFlags hoogle verbose) fl = case fl of
            HaddockHoogle -> HaddockFlags True verbose
            Verbose n     -> HaddockFlags hoogle n
            _             -> error "Unexpected flag!"

programaticaCmd :: Cmd a
programaticaCmd = Cmd {
        cmdName        = "pfe",
        cmdHelp        = "Generate Programatica Project.",
        cmdDescription = "",
        cmdOptions     = [cmd_help, cmd_verbose],
        cmdAction      = ProgramaticaCmd
        }

parseProgramaticaArgs :: [String] -> [OptDescr a] -> IO (PFEFlags, [a], [String])
parseProgramaticaArgs  = parseNoArgs programaticaCmd PFEFlags

cleanCmd :: Cmd a
cleanCmd = Cmd {
        cmdName        = "clean",
        cmdHelp        = "Clean up after a build.",
        cmdDescription = "Removes .hi, .o, preprocessed sources, etc.\n", -- Multi-line!
        cmdOptions     = [cmd_help, cmd_verbose],
        cmdAction      = CleanCmd
        }

parseCleanArgs :: [String] -> [OptDescr a] -> IO (CleanFlags, [a], [String])
parseCleanArgs = parseNoArgs cleanCmd CleanFlags

installCmd :: Cmd a
installCmd = Cmd {
        cmdName        = "install",
        cmdHelp        = "Copy the files into the install locations. Run register.",
        cmdDescription = "Unlike the copy command, install calls the register command.\nIf you want to install into a location that is not what was\nspecified in the configure step, use the copy command.\n",
        cmdOptions     = [cmd_help, cmd_verbose,
           Option "" ["install-prefix"] (reqDirArg InstPrefix)
               "[DEPRECATED, use copy]",
           Option "" ["user"] (NoArg UserFlag)
               "upon registration, register this package in the user's local package database",
           Option "" ["global"] (NoArg GlobalFlag)
               "(default; override with configure) upon registration, register this package in the system-wide package database"
           ],
        cmdAction      = InstallCmd
        }

copyCmd :: Cmd a
copyCmd = Cmd {
        cmdName        = "copy",
        cmdHelp        = "Copy the files into the install locations.",
        cmdDescription = "Does not call register, and allows a prefix at install time\nWithout the --destdir flag, configure determines location.\n",
        cmdOptions     = [cmd_help, cmd_verbose,
           Option "" ["destdir"] (reqDirArg DestDir)
               "directory to copy files to, prepended to installation directories",
           Option "" ["copy-prefix"] (reqDirArg InstPrefix)
               "[DEPRECATED, directory to copy files to instead of prefix]"
           ],
        cmdAction      = CopyCmd NoCopyDest
        }

parseCopyArgs :: CopyFlags -> [String] -> [OptDescr a] ->
                    IO (CopyFlags, [a], [String])
parseCopyArgs = parseArgs copyCmd updateCfg
  where updateCfg (CopyFlags copydest verbose) fl = case fl of
            InstPrefix path -> (CopyFlags (CopyPrefix path) verbose)
	    DestDir path    -> (CopyFlags (CopyTo path) verbose)
            Verbose n       -> (CopyFlags copydest n)
            _               -> error $ "Unexpected flag!"


parseInstallArgs :: InstallFlags -> [String] -> [OptDescr a] ->
                    IO (InstallFlags, [a], [String])
parseInstallArgs = parseArgs installCmd updateCfg
  where updateCfg (InstallFlags uFlag verbose) fl = case fl of
            InstPrefix _ -> error "--install-prefix is obsolete. Use copy command instead."
            UserFlag     -> (InstallFlags MaybeUserUser  verbose)
            GlobalFlag   -> (InstallFlags MaybeUserGlobal verbose)
            Verbose n    -> (InstallFlags uFlag n)
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

parseSDistArgs :: [String] -> [OptDescr a] -> IO (SDistFlags, [a], [String])
parseSDistArgs = parseArgs sdistCmd updateCfg (SDistFlags False 0)
  where updateCfg (SDistFlags snapshot verbose) fl = case fl of
            Snapshot        -> (SDistFlags True verbose)
            Verbose n       -> (SDistFlags snapshot n)
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
parseTestArgs = parseNoArgs testCmd id

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
           Option "" ["inplace"] (NoArg InPlaceFlag)
               "register the package in the build location, so it can be used without being installed",
           Option "" ["gen-script"] (NoArg GenScriptFlag)
               "Instead of performing the register command, generate a script to register later",
	   cmd_with_hc_pkg
           ],
        cmdAction      = RegisterCmd
        }

parseRegisterArgs :: RegisterFlags -> [String] -> [OptDescr a] ->
                     IO (RegisterFlags, [a], [String])
parseRegisterArgs = parseArgs registerCmd updateCfg
  where updateCfg reg fl = case fl of
            UserFlag        -> reg { regUser=MaybeUserUser }
            GlobalFlag      -> reg { regUser=MaybeUserGlobal }
            Verbose n       -> reg { regVerbose=n }
            GenScriptFlag   -> reg { regGenScript=True }
	    InPlaceFlag     -> reg { regInPlace=True }
	    WithHcPkg f	    -> reg { regWithHcPkg=Just f }
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
        cmdAction      = UnregisterCmd
        }

parseUnregisterArgs :: RegisterFlags -> [String] -> [OptDescr a] ->
                       IO (RegisterFlags, [a], [String])
parseUnregisterArgs = parseRegisterArgs

-- |Helper function for commands with no arguments except for verbose
-- and help.

parseNoArgs :: (Cmd a)
            -> (Int -> b) -- Constructor to make this type.
            -> [String] -> [OptDescr a]-> IO (b, [a], [String])
parseNoArgs cmd c = parseArgs cmd updateCfg (c 0)
  where
    updateCfg _ (Verbose n) = c n
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

