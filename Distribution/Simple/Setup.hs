{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Setup
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

module Distribution.Simple.Setup (--parseArgs,
                           module Distribution.Simple.Compiler,
                           Action(..),
                           ConfigFlags(..), emptyConfigFlags, configureArgs,
                           CopyFlags(..), CopyDest(..), emptyCopyFlags,
			   InstallFlags(..), emptyInstallFlags,
                           HaddockFlags(..), emptyHaddockFlags,
                           HscolourFlags(..), emptyHscolourFlags,
                           BuildFlags(..), emptyBuildFlags,
                           CleanFlags(..), emptyCleanFlags,
                           PFEFlags(..),
                           MakefileFlags(..), emptyMakefileFlags,
                           RegisterFlags(..), emptyRegisterFlags,
			   SDistFlags(..),
			   --optionHelpString,
#ifdef DEBUG
                           hunitTests,
#endif
                           parseGlobalArgs,
                           parseConfigureArgs, parseBuildArgs, parseCleanArgs,
                           parseMakefileArgs,
                           parseHscolourArgs, parseHaddockArgs, parseProgramaticaArgs, parseTestArgs,
                           parseInstallArgs, parseSDistArgs, parseRegisterArgs,
                           parseUnregisterArgs, parseCopyArgs,
                           reqPathArg, reqDirArg
                           ) where


-- Misc:
#ifdef DEBUG
import Test.HUnit (Test(..))
#endif

import Distribution.Simple.Compiler (CompilerFlavor(..), Compiler(..),
                                     defaultCompilerFlavor, PackageDB(..))
import Distribution.Simple.Utils (die, wrapText)
import Distribution.Simple.Program (Program(..), ProgramConfiguration,
                             knownPrograms, userSpecifyPath, userSpecifyArgs)
import Data.List (find, sort)
import Data.Char( toLower, isSpace )
import Distribution.GetOpt
import Distribution.Verbosity
import System.Exit
import System.Environment

-- type CommandLineOpts = (Action,
--                         [String]) -- The un-parsed remainder

data Action = ConfigCmd ConfigFlags   -- config
            | BuildCmd                -- build
            | CleanCmd                -- clean
            | CopyCmd CopyDest        -- copy (--destdir flag)
            | HscolourCmd             -- hscolour
            | HaddockCmd              -- haddock
            | ProgramaticaCmd         -- pfesetup
            | InstallCmd              -- install (install-prefix)
            | SDistCmd                -- sdist
            | MakefileCmd             -- makefile
            | TestCmd                 -- test
            | RegisterCmd    	      -- register
            | UnregisterCmd           -- unregister
	    | HelpCmd		      -- help
--            | NoCmd -- error case, help case.
--            | BDist -- 1.0
    deriving Show

-- ------------------------------------------------------------
-- * Flag-related types
-- ------------------------------------------------------------

-- | Flags to @configure@ command
data ConfigFlags = ConfigFlags {
        configPrograms :: ProgramConfiguration, -- ^All programs that cabal may run
        configHcFlavor :: Maybe CompilerFlavor, -- ^The \"flavor\" of the compiler, sugh as GHC or Hugs.
        configHcPath   :: Maybe FilePath, -- ^given compiler location
        configHcPkg    :: Maybe FilePath, -- ^given hc-pkg location
        configVanillaLib  :: Bool,        -- ^Enable vanilla library
        configProfLib  :: Bool,           -- ^Enable profiling in the library
        configSharedLib  :: Bool,         -- ^Build shared library
        configProfExe  :: Bool,           -- ^Enable profiling in the executables.
        configConfigureArgs :: [String],  -- ^Extra arguments to @configure@
        configOptimization :: Bool,       -- ^Enable optimization.
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
	configDocDir   :: Maybe FilePath,
		-- ^installation dir for documentation
	configHtmlDir   :: Maybe FilePath,
		-- ^installation dir for HTML documentation

        configVerbose  :: Verbosity,      -- ^verbosity level
	configPackageDB:: PackageDB,	  -- ^ the --user flag?
	configGHCiLib  :: Bool,           -- ^Enable compiling library for GHCi
	configSplitObjs :: Bool,	  -- ^Enable -split-objs with GHC
        configConfigurationsFlags :: [(String, Bool)]
    }
    deriving Show

-- |The default configuration of a package, before running configure,
-- most things are \"Nothing\", zero, etc.
emptyConfigFlags :: ProgramConfiguration -> ConfigFlags
emptyConfigFlags progConf = ConfigFlags {
        configPrograms = progConf,
        configHcFlavor = defaultCompilerFlavor,
        configHcPath   = Nothing,
        configHcPkg    = Nothing,
        configVanillaLib  = True,
        configProfLib  = False,
        configSharedLib  = False,
        configProfExe  = False,
        configConfigureArgs = [],
        configOptimization = True,
        configPrefix   = Nothing,
	configBinDir   = Nothing,
	configLibDir   = Nothing,
	configLibSubDir = Nothing,
	configLibExecDir = Nothing,
	configDataDir  = Nothing,
	configDataSubDir = Nothing,
	configDocDir = Nothing,
	configHtmlDir = Nothing,
        configVerbose  = normal,
	configPackageDB = GlobalPackageDB,
	configGHCiLib  = True,
	configSplitObjs = False, -- takes longer, so turn off by default
        configConfigurationsFlags = []
    }

-- | Flags to @copy@: (destdir, copy-prefix (backwards compat), verbosity)
data CopyFlags = CopyFlags {copyDest :: CopyDest
                           ,copyVerbose :: Verbosity}
    deriving Show

-- |The location prefix for the /copy/ command.
data CopyDest
  = NoCopyDest
  | CopyTo FilePath
  | CopyPrefix FilePath		-- DEPRECATED
  deriving (Eq, Show)

emptyCopyFlags :: CopyDest -> CopyFlags
emptyCopyFlags mprefix = CopyFlags{ copyDest = mprefix,
                                    copyVerbose = normal }

-- | Flags to @install@: (package db, verbosity)
data InstallFlags = InstallFlags {installPackageDB :: Maybe PackageDB
                                 ,installVerbose :: Verbosity}
    deriving Show

emptyInstallFlags :: InstallFlags
emptyInstallFlags = InstallFlags{ installPackageDB=Nothing,
                                  installVerbose = normal }

-- | Flags to @sdist@: (snapshot, verbosity)
data SDistFlags = SDistFlags {sDistSnapshot :: Bool
                             ,sDistVerbose :: Verbosity}
    deriving Show

-- | Flags to @register@ and @unregister@: (user package, gen-script,
-- in-place, verbosity)
data RegisterFlags = RegisterFlags { regPackageDB :: Maybe PackageDB
                                   , regGenScript :: Bool
                                   , regGenPkgConf :: Bool
                                   , regPkgConfFile :: Maybe FilePath
                                   , regInPlace :: Bool
                                   , regVerbose :: Verbosity }
    deriving Show


emptyRegisterFlags :: RegisterFlags
emptyRegisterFlags = RegisterFlags { regPackageDB = Nothing,
                                     regGenScript = False,
                                     regGenPkgConf = False,
                                     regPkgConfFile = Nothing,
                                     regInPlace = False,
                                     regVerbose = normal }

data HscolourFlags = HscolourFlags {hscolourCSS :: Maybe FilePath
                                   ,hscolourExecutables :: Bool
                                   ,hscolourVerbose :: Verbosity}
    deriving Show

emptyHscolourFlags :: HscolourFlags
emptyHscolourFlags = HscolourFlags {hscolourCSS = Nothing
                                   ,hscolourExecutables = False
                                   ,hscolourVerbose = normal}

data HaddockFlags = HaddockFlags {haddockHoogle :: Bool
                                 ,haddockHtmlLocation :: Maybe String
                                 ,haddockExecutables :: Bool
                                 ,haddockCss :: Maybe FilePath
                                 ,haddockHscolour :: Bool
                                 ,haddockHscolourCss :: Maybe FilePath
                                 ,haddockVerbose :: Verbosity}
    deriving Show

emptyHaddockFlags :: HaddockFlags
emptyHaddockFlags = HaddockFlags {haddockHoogle = False
                                 ,haddockHtmlLocation = Nothing
                                 ,haddockExecutables = False
                                 ,haddockCss = Nothing
                                 ,haddockHscolour = False
                                 ,haddockHscolourCss = Nothing
                                 ,haddockVerbose = normal}

data CleanFlags   = CleanFlags   {cleanSaveConf  :: Bool
                                 ,cleanVerbose   :: Verbosity}
    deriving Show
emptyCleanFlags :: CleanFlags
emptyCleanFlags = CleanFlags {cleanSaveConf = False, cleanVerbose = normal}

data BuildFlags   = BuildFlags   {buildVerbose   :: Verbosity,
                                  buildPrograms  :: ProgramConfiguration}
    deriving Show

emptyBuildFlags :: ProgramConfiguration -> BuildFlags
emptyBuildFlags progs = BuildFlags {buildVerbose  = normal,
                                    buildPrograms = progs}

data MakefileFlags = MakefileFlags {makefileVerbose :: Verbosity,
                                    makefileFile :: Maybe FilePath}
    deriving Show
emptyMakefileFlags :: MakefileFlags
emptyMakefileFlags = MakefileFlags {makefileVerbose = normal,
                                    makefileFile = Nothing}

data PFEFlags     = PFEFlags     {pfeVerbose     :: Verbosity}
    deriving Show

-- | All the possible flags
data Flag a = GhcFlag | NhcFlag | HugsFlag | JhcFlag
          | WithCompiler FilePath | WithHcPkg FilePath
          | WithVanillaLib | WithoutVanillaLib
          | WithProfLib | WithoutProfLib
          | WithSharedLib | WithoutSharedLib
          | WithProfExe | WithoutProfExe
          | WithOptimization | WithoutOptimization
	  | WithGHCiLib | WithoutGHCiLib
	  | WithSplitObjs | WithoutSplitObjs
          | ConfigureOption String

	  | Prefix FilePath
	  | BinDir FilePath
	  | LibDir FilePath
	  | LibSubDir FilePath
	  | LibExecDir FilePath
	  | DataDir FilePath
	  | DataSubDir FilePath
	  | DocDir FilePath
	  | HtmlDir FilePath
          | ConfigurationsFlags [(String, Bool)]

          | ProgramArgs String String   -- program name, arguments
	  | ProgramArg  String String   -- program name, single argument
          | WithProgram String FilePath -- program name, location

          -- For install, register, and unregister:
          | UserFlag | GlobalFlag
          -- for register & unregister
          | GenScriptFlag
          | GetPkgConfFlag (Maybe FilePath)
	  | InPlaceFlag
          -- For copy:
          | InstPrefix FilePath
	  | DestDir FilePath
          -- For sdist:
          | Snapshot
          -- For hscolour:
          | HscolourCss FilePath
          | HscolourExecutables
          -- For haddock:
          | HaddockHoogle
          | HaddockExecutables
          | HaddockCss FilePath
          | HaddockHscolour
          | HaddockHscolourCss FilePath
          | HaddockHtmlLocation String
          -- For clean:
          | SaveConfigure -- ^don't delete dist\/setup-config during clean
          -- For makefile:
          | MakefileFile FilePath
          -- For everyone:
          | HelpFlag
          | Verbose Verbosity
--          | Version?
          | Lift a
            deriving (Show, Eq)


-- ------------------------------------------------------------
-- * Mostly parsing functions
-- ------------------------------------------------------------

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
        optFlag "datadir" configDataDir ++
        reverse (configConfigureArgs flags)
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
cmd_verbose = Option "v" ["verbose"] (OptArg (Verbose . flagToVerbosity) "n")
              "Control verbosity (n is 0--3, default verbosity level is 1)"

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
        cmdOptions      :: ShowOrParseArgs -> [OptDescr (Flag a)],
        cmdAction       :: Action
        }

data ShowOrParseArgs = ShowArgs | ParseArgs

commandList :: ProgramConfiguration -> [Cmd a]
commandList progConf = [configureCmd progConf, buildCmd progConf, makefileCmd,
                        cleanCmd, installCmd,
                        copyCmd, sdistCmd, testCmd,
                        haddockCmd, hscolourCmd, programaticaCmd,
                        registerCmd, unregisterCmd]

lookupCommand :: String -> [Cmd a] -> Maybe (Cmd a)
lookupCommand name = find ((==name) . cmdName)

printGlobalHelp :: ProgramConfiguration -> IO ()
printGlobalHelp progConf = 
                  do pname <- getProgName
                     let syntax_line = "Usage: " ++ pname ++ " [GLOBAL FLAGS]\n  or:  " ++ pname ++ " COMMAND [FLAGS]\n\nGlobal flags:"
                     putStrLn (usageInfo syntax_line globalOptions)
                     putStrLn "Typical steps for installing Cabal packages:"
                     mapM (\x -> putStrLn $ "  " ++ pname ++ " " ++ x)
                              ["configure", "build", "install"]
                     putStrLn "\nCommands:"
                     let maxlen = maximum [ length (cmdName cmd) | cmd <- (commandList progConf) ]
                     sequence_ [ do putStr "  "
                                    putStr (align maxlen (cmdName cmd))
                                    putStr "    "
                                    putStrLn (cmdHelp cmd)
                               | cmd <- (commandList progConf) ]
                     putStrLn $ "\nFor more information about a command, try '" ++ pname ++ " COMMAND --help'."
                     putStrLn $ "\nThis Setup program uses the Haskell Cabal Infrastructure."
                     putStrLn $"See http://www.haskell.org/cabal/ for more information."
  where align n str = str ++ replicate (n - length str) ' '

printCmdHelp :: Cmd a -> [OptDescr a] -> IO ()
printCmdHelp cmd opts = do pname <- getProgName
                           let syntax_line = "Usage: " ++ pname ++ " " ++ cmdName cmd ++ " [FLAGS]\n\nFlags for " ++ cmdName cmd ++ ":"
                           putStrLn (usageInfo syntax_line (cmdOptions cmd ShowArgs ++ liftCustomOpts opts))
                           putStr (cmdDescription cmd)

getCmdOpt :: Cmd a -> [OptDescr a] -> [String] -> ([Flag a], [String], [String])
getCmdOpt cmd opts s = (flags, other_opts, errs++errs')
  where
    (flags, nonopts, other_opts, errs) =
      getOpt' RequireOrder (cmdOptions cmd ParseArgs ++ liftCustomOpts opts) s
    errs' = ["unexpected argument: " ++ nonopt | nonopt <- nonopts]

-- We don't want to use elem, because that imposes Eq a
hasHelpFlag :: [Flag a] -> Bool
hasHelpFlag flags = not . null $ [ () | HelpFlag <- flags ]

parseGlobalArgs :: ProgramConfiguration -> [String] -> IO (Action,[String])
parseGlobalArgs progConf args =
  case getOpt' RequireOrder globalOptions args of
    (flags, _, _, []) | hasHelpFlag flags -> do
      printGlobalHelp progConf
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
        cmdDescription = programFlagsDescription progConf,
        cmdOptions     = \showOrParseArgs -> [cmd_help, cmd_verbose,
           Option "g" ["ghc"] (NoArg GhcFlag) "compile with GHC",
           Option "" ["nhc98"] (NoArg NhcFlag) "compile with NHC",
           Option "" ["jhc"]  (NoArg JhcFlag) "compile with JHC",
           Option "" ["hugs"] (NoArg HugsFlag) "compile with hugs",
           Option "w" ["with-compiler"] (reqPathArg WithCompiler)
               "give the path to a particular compiler",
	   Option "" ["with-hc-pkg"] (reqPathArg WithHcPkg)
		"give the path to the package tool",
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
	   Option "" ["docdir"] (reqDirArg DocDir)
		"installation directory for documentation",
	   Option "" ["htmldir"] (reqDirArg HtmlDir)
		"installation directory for HTML documentation",
           Option "" ["enable-library-vanilla"] (NoArg WithVanillaLib)
               "Enable vanilla libraries",
           Option "" ["disable-library-vanilla"] (NoArg WithoutVanillaLib)
               "Disable vanilla libraries",
           Option "p" ["enable-library-profiling"] (NoArg WithProfLib)
               "Enable library profiling",
           Option "" ["disable-library-profiling"] (NoArg WithoutProfLib)
               "Disable library profiling",
           Option "" ["enable-shared"] (NoArg WithSharedLib)
               "Enable shared library",
           Option "" ["disable-shared"] (NoArg WithoutSharedLib)
               "Disable shared library",
           Option "" ["enable-executable-profiling"] (NoArg WithProfExe)
               "Enable executable profiling",
           Option "" ["disable-executable-profiling"] (NoArg WithoutProfExe)
               "Disable executable profiling",
           Option "O" ["enable-optimization"] (NoArg WithOptimization)
               "Build with optimization",
           Option "" ["disable-optimization"] (NoArg WithoutOptimization)
               "Build without optimization",
	   Option "" ["enable-library-for-ghci"] (NoArg WithGHCiLib)
               "compile library for use with GHCi",
	   Option "" ["disable-library-for-ghci"] (NoArg WithoutGHCiLib)
               "do not compile libraries for GHCi",
	   Option "" ["enable-split-objs"] (NoArg WithSplitObjs)
	       "split library into smaller objects to reduce binary sizes (GHC 6.6+)",
	   Option "" ["disable-split-objs"] (NoArg WithoutSplitObjs)
	       "split library into smaller objects to reduce binary sizes (GHC 6.6+)",
           Option "" ["configure-option"] (ReqArg ConfigureOption "OPT") "Extra option for configure",
           Option "" ["user"] (NoArg UserFlag)
               "allow dependencies to be satisfied from the user package database. also implies install --user",
           Option "" ["global"] (NoArg GlobalFlag)
               "(default) dependencies must be satisfied from the global package database",
           Option "f" ["flags"] (reqFlagsArgs ConfigurationsFlags)
               "Force values for the given flags in Cabal conditionals in the .cabal file.  E.g., --flags=\"debug -usebytestrings\" forces the flag \"debug\" to true and \"usebytestrings\" to false."       
           ]
        ++ programConfigurationPaths   progConf showOrParseArgs
        ++ programConfigurationOptions progConf showOrParseArgs,
        cmdAction      = ConfigCmd (emptyConfigFlags progConf)
        }

programFlagsDescription :: ProgramConfiguration -> String
programFlagsDescription progConf =
     "The flags --with-PROG and --PROG-arg(s) can be used with"
  ++ " the following programs:"
  ++ (concatMap ("\n  "++) . wrapText 77 . sort)
     [ programName prog | (prog, _) <- knownPrograms progConf ]
  ++ "\n"

programConfigurationPaths :: ProgramConfiguration -> ShowOrParseArgs
                          -> [OptDescr (Flag a)]
programConfigurationPaths progConf args = case args of
-- we don't want a verbose help text list so we just show a generic one:
  ShowArgs  -> [withProgramPath "PROG"]
  ParseArgs -> map (withProgramPath . programName . fst) (knownPrograms progConf)
  where
    withProgramPath :: String -> OptDescr (Flag a)
    withProgramPath prog =
      Option "" ["with-" ++ prog] (reqPathArg (WithProgram prog))
        ("give the path to " ++ prog)

programConfigurationOptions :: ProgramConfiguration -> ShowOrParseArgs
                            -> [OptDescr (Flag a)]
programConfigurationOptions progConf args = case args of
-- we don't want a verbose help text list so we just show a generic one:
  ShowArgs  -> [programOptions  "PROG", programOption   "PROG"]
  ParseArgs -> map (programOptions . programName . fst) (knownPrograms progConf)
            ++ map (programOption  . programName . fst) (knownPrograms progConf)
  where
    programOptions :: String -> OptDescr (Flag a)
    programOptions prog =
      Option "" [prog ++ "-options"] (ReqArg (ProgramArgs prog) "OPTS")
        ("give extra options to " ++ prog)

    programOption :: String -> OptDescr (Flag a)
    programOption prog =
      Option "" [prog ++ "-option"] (ReqArg (ProgramArg prog) "OPT")
        ("give an extra option to " ++ prog ++
         " (no need to quote options containing spaces)")

reqPathArg :: (FilePath -> a) -> ArgDescr a
reqPathArg constr = ReqArg constr "PATH"

reqDirArg :: (FilePath -> a) -> ArgDescr a
reqDirArg constr = ReqArg constr "DIR"

reqFlagsArgs :: ([(String,Bool)] -> a) -> ArgDescr a
reqFlagsArgs constr = ReqArg (constr . flagList) "FLAGS"

flagList :: String -> [(String, Bool)]
flagList = map tagWithValue . words
  where tagWithValue ('-':name) = (map toLower name, False)
        tagWithValue name       = (map toLower name, True)

parseConfigureArgs :: ProgramConfiguration -> ConfigFlags -> [String] -> [OptDescr a] ->
                      IO (ConfigFlags, [a], [String])
parseConfigureArgs progConf = parseArgs (configureCmd progConf) updateCfg
  where updateCfg t GhcFlag              = t { configHcFlavor = Just GHC }
        updateCfg t NhcFlag              = t { configHcFlavor = Just NHC }
        updateCfg t JhcFlag              = t { configHcFlavor = Just JHC }
        updateCfg t HugsFlag             = t { configHcFlavor = Just Hugs }
        updateCfg t (WithCompiler path)  = t { configHcPath   = Just path }
        updateCfg t (WithHcPkg path)     = t { configHcPkg    = Just path }
        updateCfg t (ProgramArgs name args) = t { configPrograms = 
                                                    userSpecifyArgs name
                                                      (splitArgs args)
                                                      (configPrograms t) }
        updateCfg t (ProgramArg  name arg)  = t { configPrograms =
                                                    userSpecifyArgs name [arg]
						      (configPrograms t) }
	updateCfg t (WithProgram name path) = t { configPrograms =
                                                    userSpecifyPath
                                                      name path
                                                      (configPrograms t) }
        updateCfg t WithVanillaLib       = t { configVanillaLib  = True }
        updateCfg t WithoutVanillaLib    = t { configVanillaLib  = False,
                                               configGHCiLib = False }
        updateCfg t WithProfLib          = t { configProfLib  = True }
        updateCfg t WithoutProfLib       = t { configProfLib  = False }
        updateCfg t WithSharedLib          = t { configSharedLib  = True }
        updateCfg t WithoutSharedLib       = t { configSharedLib  = False }
        updateCfg t WithProfExe          = t { configProfExe  = True }
        updateCfg t WithoutProfExe       = t { configProfExe  = False }
        updateCfg t WithOptimization     = t { configOptimization = True }
        updateCfg t WithoutOptimization  = t { configOptimization = False }
	updateCfg t WithGHCiLib          = t { configGHCiLib  = True }
	updateCfg t WithoutGHCiLib       = t { configGHCiLib  = False }
        updateCfg t (Prefix path)        = t { configPrefix   = Just path }
        updateCfg t (BinDir path)        = t { configBinDir   = Just path }
        updateCfg t (LibDir path)        = t { configLibDir   = Just path }
        updateCfg t (LibSubDir path)     = t { configLibSubDir= Just path }
        updateCfg t (LibExecDir path)    = t { configLibExecDir = Just path }
        updateCfg t (DataDir path)       = t { configDataDir  = Just path }
        updateCfg t (DataSubDir path)    = t { configDataSubDir = Just path }
        updateCfg t (DocDir path)        = t { configDocDir  = Just path }
        updateCfg t (HtmlDir path)       = t { configHtmlDir  = Just path }
        updateCfg t (Verbose n)          = t { configVerbose  = n }
        updateCfg t UserFlag             = t { configPackageDB = UserPackageDB }
        updateCfg t GlobalFlag           = t { configPackageDB = GlobalPackageDB }
	updateCfg t WithSplitObjs	 = t { configSplitObjs = True }
	updateCfg t WithoutSplitObjs	 = t { configSplitObjs = False }
        updateCfg t (ConfigurationsFlags fs)  = t { configConfigurationsFlags =
                                                        fs ++ configConfigurationsFlags t }
        updateCfg t (ConfigureOption o) = t { configConfigureArgs = o : configConfigureArgs t }
        updateCfg t (Lift _)             = t
        updateCfg _ _                    = error $ "Unexpected flag!"

buildCmd :: ProgramConfiguration -> Cmd a
buildCmd progConf = Cmd {
        cmdName        = "build",
        cmdHelp        = "Make this package ready for installation.",
        cmdDescription = "",  -- This can be a multi-line description
        cmdOptions     = \showOrParseArgs -> [cmd_help, cmd_verbose]
          ++ programConfigurationOptions progConf showOrParseArgs,
        cmdAction      = BuildCmd
        }

parseBuildArgs :: ProgramConfiguration -> BuildFlags -> [String] -> [OptDescr a] -> IO (BuildFlags, [a], [String])
parseBuildArgs progConf = parseArgs (buildCmd progConf) updateArgs
  where updateArgs bflags fl =
           case fl of
                Verbose n             -> bflags{buildVerbose=n}
                ProgramArgs name args -> bflags{buildPrograms =
                                                  userSpecifyArgs name
                                                    (splitArgs args)
                                                    (buildPrograms bflags)}
                ProgramArg  name arg ->  bflags{buildPrograms =
                                                  userSpecifyArgs name [arg]
						    (buildPrograms bflags)}
                _                    -> error "Unexpected flag!"

makefileCmd :: Cmd a
makefileCmd = Cmd {
        cmdName        = "makefile",
        cmdHelp        = "Perform any necessary makefileing.",
        cmdDescription = "",  -- This can be a multi-line description
        cmdOptions     = \_ -> [cmd_help, cmd_verbose,
           Option "f" ["file"] (reqPathArg MakefileFile)
               "Filename to use (default: Makefile)."],
        cmdAction      = MakefileCmd
        }

parseMakefileArgs :: MakefileFlags -> [String] -> [OptDescr a] -> IO (MakefileFlags, [a], [String])
parseMakefileArgs = parseArgs makefileCmd updateCfg
  where updateCfg mflags fl =
           case fl of
                Verbose n      -> mflags{makefileVerbose=n}
                MakefileFile f -> mflags{makefileFile=Just f}
                _              -> error "Unexpected flag!"

hscolourCmd :: Cmd a
hscolourCmd = Cmd {
        cmdName        = "hscolour",
        cmdHelp        = "Generate HsColour colourised code, in HTML format.",
        cmdDescription = "Requires hscolour.\n",
        cmdOptions     = \_ -> [cmd_help, cmd_verbose,
                          Option "" ["executables"] (NoArg HscolourExecutables)
                            "Run hscolour for Executables targets",
                          Option "" ["css"] (reqPathArg HscolourCss)
                            "Use a cascading style sheet"],
        cmdAction      = HscolourCmd
        }

parseHscolourArgs :: HscolourFlags -> [String] -> [OptDescr a] -> IO (HscolourFlags, [a], [String])
parseHscolourArgs  = parseArgs hscolourCmd updateCfg
  where updateCfg (HscolourFlags css doExe verbosity) fl = case fl of
            HscolourCss c       -> HscolourFlags (Just c) doExe verbosity
            HscolourExecutables -> HscolourFlags css      True  verbosity
            Verbose n           -> HscolourFlags css      doExe n
            _                   -> error "Unexpected flag!"

haddockCmd :: Cmd a
haddockCmd = Cmd {
        cmdName        = "haddock",
        cmdHelp        = "Generate Haddock HTML documentation.",
        cmdDescription = "Requires cpphs and haddock.\n",
        cmdOptions     = \_ ->
         [cmd_help, cmd_verbose,
          Option "" ["hoogle"] (NoArg HaddockHoogle)
            "Generate a hoogle database",
          Option "" ["html-location"] (ReqArg HaddockHtmlLocation "URL")
            "Location of HTML documentation for pre-requisite packages",
          Option "" ["executables"] (NoArg HaddockExecutables)
            "Run haddock for Executables targets",
          Option "" ["css"] (reqPathArg HaddockCss)
            "Use PATH as the haddock stylesheet",
          Option "" ["hyperlink-source"] (NoArg HaddockHscolour)
            "Hyperlink the documentation to the source code (using HsColour)",
          Option "" ["hscolour-css"] (reqPathArg HaddockHscolourCss)
            "Use PATH as the HsColour stylesheet"],
        cmdAction      = HaddockCmd
        }

parseHaddockArgs :: HaddockFlags -> [String] -> [OptDescr a] -> IO (HaddockFlags, [a], [String])
parseHaddockArgs  = parseArgs haddockCmd updateCfg
  where updateCfg hflags fl = case fl of
            HaddockHoogle         -> hflags{haddockHoogle = True}
            HaddockHtmlLocation s -> hflags{haddockHtmlLocation=Just s}
            HaddockExecutables    -> hflags{haddockExecutables = True}
            HaddockCss h          -> hflags{haddockCss = Just h}
            HaddockHscolour       -> hflags{haddockHscolour = True}
            HaddockHscolourCss h  -> hflags{haddockHscolourCss = Just h}
            Verbose n             -> hflags{haddockVerbose = n}
            _                     -> error "Unexpected flag!"

programaticaCmd :: Cmd a
programaticaCmd = Cmd {
        cmdName        = "pfe",
        cmdHelp        = "Generate Programatica Project.",
        cmdDescription = "",
        cmdOptions     = \_ -> [cmd_help, cmd_verbose],
        cmdAction      = ProgramaticaCmd
        }

parseProgramaticaArgs :: [String] -> [OptDescr a] -> IO (PFEFlags, [a], [String])
parseProgramaticaArgs  = parseNoArgs programaticaCmd PFEFlags

cleanCmd :: Cmd a
cleanCmd = Cmd {
        cmdName        = "clean",
        cmdHelp        = "Clean up after a build.",
        cmdDescription = "Removes .hi, .o, preprocessed sources, etc.\n", -- Multi-line!
        cmdOptions     = \_ -> [cmd_help, cmd_verbose,
           Option "s" ["save-configure"] (NoArg SaveConfigure)
               "Do not remove the configuration file (dist/setup-config) during cleaning.  Saves need to reconfigure."],
        cmdAction      = CleanCmd
        }

parseCleanArgs :: CleanFlags -> [String] -> [OptDescr a] ->
                    IO (CleanFlags, [a], [String])
parseCleanArgs  = parseArgs cleanCmd updateCfg
  where updateCfg (CleanFlags saveConfigure verbosity) fl = case fl of
            SaveConfigure -> CleanFlags True verbosity
            Verbose n     -> CleanFlags saveConfigure n
            _             -> error "Unexpected flag!"

installCmd :: Cmd a
installCmd = Cmd {
        cmdName        = "install",
        cmdHelp        = "Copy the files into the install locations. Run register.",
        cmdDescription = "Unlike the copy command, install calls the register command.\nIf you want to install into a location that is not what was\nspecified in the configure step, use the copy command.\n",
        cmdOptions     = \_ -> [cmd_help, cmd_verbose,
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
        cmdOptions     = \_ -> [cmd_help, cmd_verbose,
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
  where updateCfg (CopyFlags copydest verbosity) fl = case fl of
            InstPrefix path -> (CopyFlags (CopyPrefix path) verbosity)
	    DestDir path    -> (CopyFlags (CopyTo path) verbosity)
            Verbose n       -> (CopyFlags copydest n)
            _               -> error $ "Unexpected flag!"


parseInstallArgs :: InstallFlags -> [String] -> [OptDescr a] ->
                    IO (InstallFlags, [a], [String])
parseInstallArgs = parseArgs installCmd updateCfg
  where updateCfg (InstallFlags uFlag verbosity) fl = case fl of
            InstPrefix _ -> error "--install-prefix is obsolete. Use copy command instead."
            UserFlag     -> (InstallFlags (Just UserPackageDB)   verbosity)
            GlobalFlag   -> (InstallFlags (Just GlobalPackageDB) verbosity)
            Verbose n    -> (InstallFlags uFlag n)
            _            -> error $ "Unexpected flag!"

sdistCmd :: Cmd a
sdistCmd = Cmd {
        cmdName        = "sdist",
        cmdHelp        = "Generate a source distribution file (.tar.gz or .zip).",
        cmdDescription = "",  -- This can be a multi-line description
        cmdOptions     = \_ -> [cmd_help,cmd_verbose,
           Option "" ["snapshot"] (NoArg Snapshot)
               "Produce a snapshot source distribution"
           ],
        cmdAction      = SDistCmd
        }

parseSDistArgs :: [String] -> [OptDescr a] -> IO (SDistFlags, [a], [String])
parseSDistArgs = parseArgs sdistCmd updateCfg (SDistFlags False normal)
  where updateCfg (SDistFlags snapshot verbosity) fl = case fl of
            Snapshot        -> (SDistFlags True verbosity)
            Verbose n       -> (SDistFlags snapshot n)
            _               -> error $ "Unexpected flag!"

testCmd :: Cmd a
testCmd = Cmd {
        cmdName        = "test",
        cmdHelp        = "Run the test suite, if any (configure with UserHooks).",
        cmdDescription = "",  -- This can be a multi-line description
        cmdOptions     = \_ -> [cmd_help,cmd_verbose],
        cmdAction      = TestCmd
        }

parseTestArgs :: [String] -> [OptDescr a] -> IO (Verbosity, [a], [String])
parseTestArgs = parseNoArgs testCmd id

registerCmd :: Cmd a
registerCmd = Cmd {
        cmdName        = "register",
        cmdHelp        = "Register this package with the compiler.",
        cmdDescription = "",  -- This can be a multi-line description
        cmdOptions     = \_ -> [cmd_help, cmd_verbose,
           Option "" ["user"] (NoArg UserFlag)
               "upon registration, register this package in the user's local package database",
           Option "" ["global"] (NoArg GlobalFlag)
               "(default) upon registration, register this package in the system-wide package database",
           Option "" ["inplace"] (NoArg InPlaceFlag)
               "register the package in the build location, so it can be used without being installed",
           Option "" ["gen-script"] (NoArg GenScriptFlag)
               "instead of registering, generate a script to register later",
           Option "" ["gen-pkg-config"] (OptArg GetPkgConfFlag "PKG")
               "instead of registering, generate a package registration file"
           ],
        cmdAction      = RegisterCmd
        }

parseRegisterArgs :: RegisterFlags -> [String] -> [OptDescr a] ->
                     IO (RegisterFlags, [a], [String])
parseRegisterArgs = parseArgs registerCmd registerUpdateCfg

registerUpdateCfg :: RegisterFlags -> Flag a -> RegisterFlags
registerUpdateCfg reg fl = case fl of
            UserFlag        -> reg { regPackageDB=Just UserPackageDB }
            GlobalFlag      -> reg { regPackageDB=Just GlobalPackageDB }
            Verbose n       -> reg { regVerbose=n }
            GenScriptFlag   -> reg { regGenScript=True }
            GetPkgConfFlag
              Nothing       -> reg { regGenPkgConf=True }
            GetPkgConfFlag
              (Just f)      -> reg { regGenPkgConf=True,
                                     regPkgConfFile=Just f }
            InPlaceFlag     -> reg { regInPlace=True }
            _               -> error $ "Unexpected flag!"

unregisterCmd :: Cmd a
unregisterCmd = Cmd {
        cmdName        = "unregister",
        cmdHelp        = "Unregister this package with the compiler.",
        cmdDescription = "",  -- This can be a multi-line description
        cmdOptions     = \_ -> [cmd_help, cmd_verbose,
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
parseUnregisterArgs = parseArgs unregisterCmd registerUpdateCfg

-- |Helper function for commands with no arguments except for verbosity
-- and help.

parseNoArgs :: (Cmd a)
            -> (Verbosity -> b) -- Constructor to make this type.
            -> [String] -> [OptDescr a]-> IO (b, [a], [String])
parseNoArgs cmd c = parseArgs cmd updateCfg (c normal)
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

-- |Helper function to split a string into a list of arguments.
-- It's supposed to handle quoted things sensibly, eg:
--
-- > splitArgs "--foo=\"C:\Program Files\Bar\" --baz"
-- >   = ["--foo=C:\Program Files\Bar", "--baz"]
--
splitArgs :: String -> [String]
splitArgs  = space []
  where
    space :: String -> String -> [String]
    space w []      = word w []
    space w ( c :s)
        | isSpace c = word w (space [] s)
    space w ('"':s) = string w s
    space w s       = nonstring w s

    string :: String -> String -> [String]
    string w []      = word w []
    string w ('"':s) = space w s
    string w ( c :s) = string (c:w) s

    nonstring :: String -> String -> [String]
    nonstring w  []      = word w []
    nonstring w  ('"':s) = string w s
    nonstring w  ( c :s) = space (c:w) s

    word [] s = s
    word w  s = reverse w : s

putErrors :: [String] -> IO a
putErrors errs = die $ "Errors:" ++ concat ['\n':err | err <- errs]


#ifdef DEBUG
hunitTests :: [Test]
hunitTests = []
-- The test cases kinda have to be rewritten from the ground up... :/
--hunitTests =
--    let m = [("ghc", GHC), ("nhc98", NHC), ("hugs", Hugs)]
--        (flags, commands', unkFlags, ers)
--               = getOpt Permute options ["configure", "foobar", "--prefix=/foo", "--ghc", "--nhc98", "--hugs", "--with-compiler=/comp", "--unknown1", "--unknown2", "--install-prefix=/foo", "--user", "--global"]
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

