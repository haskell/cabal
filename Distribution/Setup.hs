-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Setup
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  
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
                           Action(..), ConfigFlags,
                           CompilerFlavor(..), Compiler(..),
			   --optionHelpString,
#ifdef DEBUG
                           hunitTests,
#endif
                           parseGlobalArgs, commandList,
                           parseConfigureArgs, parseBuildArgs, parseCleanArgs,
                           parseInstallArgs, parseSDistArgs, parseRegisterArgs,
                           parseUnregisterArgs
                           ) where

-- Misc:
#ifdef DEBUG
import HUnit (Test(..), (~:), (~=?))
#endif

import Data.List(find)
import System.Console.GetOpt
import System.Exit
import System.Environment

import Compat.H98 () -- FIX: is this line necessary for some platform?

-- ------------------------------------------------------------
-- * Command Line Types and Exports
-- ------------------------------------------------------------

data CompilerFlavor = GHC | NHC | Hugs | HBC | Helium | OtherCompiler String
              deriving (Show, Read, Eq)

data Compiler = Compiler {compilerFlavor:: CompilerFlavor,
                          compilerPath  :: FilePath,
                          compilerPkgTool :: FilePath}
                deriving (Show, Read, Eq)

-- type CommandLineOpts = (Action,
--                         [String]) -- The un-parsed remainder

data Action = ConfigCmd ConfigFlags       -- config
            | BuildCmd                    -- build
            | CleanCmd                    -- clean
            | InstallCmd (Maybe FilePath) Bool -- install (install-prefix) (--user flag)
            | SDistCmd                    -- sdist
            | RegisterCmd Bool            -- register (--user flag)
            | UnregisterCmd               -- unregister
	    | HelpCmd			  -- help
--            | NoCmd -- error case, help case.
--             | TestCmd 1.0?
--             | BDist -- 1.0
--            | CleanCmd                 -- clean
--            | NoCmd -- error case?
    deriving (Show, Eq)

type ConfigFlags = (Maybe CompilerFlavor,
                    Maybe FilePath, -- given compiler location
                    Maybe FilePath, -- given hc-pkg location
                    Maybe FilePath) -- prefix

-- |Most of these flags are for Configure, but InstPrefix is for Install.
data Flag a = GhcFlag | NhcFlag | HugsFlag
          | WithCompiler FilePath | WithHcPkg FilePath | Prefix FilePath
          | UserFlag | GlobalFlag
          | HelpFlag
          -- For install:
          | InstPrefix FilePath
--          | Verbose | Version?
          | Lift a
            deriving (Show, Eq)

cmd_help :: OptDescr (Flag a)
cmd_help = Option "h?" ["help"] (NoArg HelpFlag) "Show this help text"

-- Do we have any other interesting global flags? Verbose?
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

unliftFlags :: [Flag a] -> [a]
unliftFlags flags = [ fl | Lift fl <- flags ]

data Cmd a = Cmd {
        cmdName         :: String,
        cmdHelp         :: String, -- Short description
        cmdDescription  :: String, -- Long description
        cmdOptions      :: [OptDescr (Flag a)],
        cmdAction       :: Action
        }

commandList :: [Cmd a]
commandList = [configureCmd, buildCmd, cleanCmd, installCmd,
               sdistCmd, registerCmd, unregisterCmd]

lookupCommand :: String -> [Cmd a] -> Maybe (Cmd a)
lookupCommand name = find ((==name) . cmdName)

printGlobalHelp :: IO ()
printGlobalHelp = do pname <- getProgName
                     let syntax_line = "Usage: " ++ pname ++ " [GLOBAL FLAGS] COMMAND [FLAGS]\n\nGlobal flags:"
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
                           let syntax_line = "Usage: " ++ pname ++ " [GLOBAL FLAGS] " ++ cmdName cmd ++ " [FLAGS]\n\nFlags for " ++ cmdName cmd ++ ":"
                           putStrLn (usageInfo syntax_line (cmdOptions cmd ++ liftCustomOpts opts))
                           putStr (cmdDescription cmd)

getCmdOpt :: Cmd a -> [OptDescr a] -> [String] -> ([Flag a], [String], [String])
getCmdOpt cmd opts = getOpt Permute (cmdOptions cmd ++ liftCustomOpts opts)

-- We don't want to use elem, because that imposes Eq a
hasHelpFlag :: [Flag a] -> Bool
hasHelpFlag flags = not . null $ [ () | HelpFlag <- flags ]

parseGlobalArgs :: [String] -> IO (Action,[String])
parseGlobalArgs args =
  case getOpt RequireOrder globalOptions args of
    (flags, _, []) | hasHelpFlag flags -> do
      printGlobalHelp
      exitWith ExitSuccess
    (flags, cname:cargs, []) -> do
      case lookupCommand cname commandList of
        Just cmd -> return (cmdAction cmd,cargs)
        Nothing  -> do putStrLn $ "Unrecognised command: " ++ cname ++ " (try --help)"
                       exitWith (ExitFailure 1)
    (_, [], [])  -> do putStrLn $ "No command given (try --help)"
                       exitWith (ExitFailure 1)
    (_, _, errs) -> do mapM_ putStrLn errs
                       exitWith (ExitFailure 1)

configureCmd :: Cmd a
configureCmd = Cmd {
        cmdName        = "configure",
        cmdHelp        = "Prepare to build the package.",
        cmdDescription = "This is the long description for configure.\nMulti-line!\n",
        cmdOptions     = [cmd_help,
           Option "g" ["ghc"] (NoArg GhcFlag) "compile with GHC",
           Option "n" ["nhc"] (NoArg NhcFlag) "compile with NHC",
           Option "" ["hugs"] (NoArg HugsFlag) "compile with hugs",
           Option "w" ["with-compiler"] (ReqArg WithCompiler "PATH")
               "give the path to a particular compiler",
           Option "w" ["with-hc-pkg"] (ReqArg WithHcPkg "PATH")
               "give the path to the package tool",
           Option "" ["prefix"] (ReqArg Prefix "DIR")
               "bake this prefix in preparation of installation"
           ],
        cmdAction      = ConfigCmd (Nothing, Nothing, Nothing, Nothing)
        }

parseConfigureArgs :: ConfigFlags -> [String] -> [OptDescr a] ->
                      IO (ConfigFlags, [a], [String])
parseConfigureArgs cfg args customOpts =
  case getCmdOpt configureCmd customOpts args of
    (flags, _, []) | hasHelpFlag flags -> do
      printCmdHelp configureCmd customOpts
      exitWith ExitSuccess
    (flags, args', []) ->
      return (updateCfg flags cfg, unliftFlags flags, args')
    (_, _, errs) -> do mapM_ putStrLn errs
                       exitWith (ExitFailure 1)
  where updateCfg (fl:flags) t@(mcf, mpath, mhcpkg, mprefix) = updateCfg flags $
          case fl of
            GhcFlag  -> (Just GHC,  mpath, mhcpkg, mprefix)
            NhcFlag  -> (Just NHC,  mpath, mhcpkg, mprefix)
            HugsFlag -> (Just Hugs, mpath, mhcpkg, mprefix)
            WithCompiler path -> (mcf, Just path, mhcpkg, mprefix)
            WithHcPkg path    -> (mcf, mpath, Just path, mprefix)
            Prefix path       -> (mcf, mpath, mhcpkg, Just path)
            Lift _            -> t
            _                 -> error $ "Unexpected flag!"
        updateCfg [] t = t

buildCmd :: Cmd a
buildCmd = Cmd {
        cmdName        = "build",
        cmdHelp        = "Make this package ready for installation.",
        cmdDescription = "This is the long description for build.\nMulti-line!\n",
        cmdOptions     = [cmd_help],
        cmdAction      = BuildCmd
        }

parseBuildArgs :: [String] -> [OptDescr a] -> IO ([a], [String])
parseBuildArgs args customOpts =
  case getCmdOpt buildCmd customOpts args of
    (flags, _, []) | hasHelpFlag flags -> do
      printCmdHelp buildCmd customOpts
      exitWith ExitSuccess
    (flags, args', []) ->
      return (unliftFlags flags, args')
    (_, _, errs) -> do mapM_ putStrLn errs
                       exitWith (ExitFailure 1)

cleanCmd :: Cmd a
cleanCmd = Cmd {
        cmdName        = "clean",
        cmdHelp        = "Clean up after a build.",
        cmdDescription = "This is the long description for clean.\nMulti-line!\n",
        cmdOptions     = [cmd_help],
        cmdAction      = CleanCmd
        }

parseCleanArgs :: [String] -> [OptDescr a] -> IO ([a], [String])
parseCleanArgs args customOpts =
  case getCmdOpt cleanCmd customOpts args of
    (flags, _, []) | hasHelpFlag flags -> do
      printCmdHelp cleanCmd customOpts
      exitWith ExitSuccess
    (flags, args', []) ->
      return (unliftFlags flags, args')
    (_, _, errs) -> do mapM_ putStrLn errs
                       exitWith (ExitFailure 1)

installCmd :: Cmd a
installCmd = Cmd {
        cmdName        = "install",
        cmdHelp        = "Copy the files into the install locations.",
        cmdDescription = "This is the long description for install.\nMulti-line!\n",
        cmdOptions     = [cmd_help,
           Option "" ["install-prefix"] (ReqArg InstPrefix "DIR")
               "specify the directory in which to place installed files",
           Option "" ["user"] (NoArg UserFlag)
               "upon registration, register this package in the user's local package database",
           Option "" ["global"] (NoArg GlobalFlag)
               "(default) upon registration, register this package in the system-wide package database"
           ],
        cmdAction      = InstallCmd Nothing False
        }

parseInstallArgs :: (Maybe FilePath, Bool) -> [String] -> [OptDescr a] ->
                    IO ((Maybe FilePath, Bool), [a], [String])
parseInstallArgs cfg args customOpts =
  case getCmdOpt installCmd customOpts args of
    (flags, _, []) | hasHelpFlag flags -> do
      printCmdHelp installCmd customOpts
      exitWith ExitSuccess
    (flags, args', []) ->
      return (updateCfg flags cfg, unliftFlags flags, args')
    (_, _, errs) -> do mapM_ putStrLn errs
                       exitWith (ExitFailure 1)
  where updateCfg (fl:flags) t@(mprefix, uFlag) = updateCfg flags $
          case fl of
            InstPrefix path -> (Just path, uFlag)
            UserFlag        -> (mprefix, True)
            GlobalFlag      -> (mprefix, False)
            Lift _          -> t
            _               -> error $ "Unexpected flag!"
        updateCfg [] t = t

sdistCmd :: Cmd a
sdistCmd = Cmd {
        cmdName        = "sdist",
        cmdHelp        = "Generate a source distribution file (.tar.gz or .zip).",
        cmdDescription = "This is the long description for sdist.\nMulti-line!\n",
        cmdOptions     = [cmd_help],
        cmdAction      = SDistCmd
        }

parseSDistArgs :: [String] -> [OptDescr a] -> IO ([a], [String])
parseSDistArgs args customOpts =
  case getCmdOpt sdistCmd customOpts args of
    (flags, _, []) | hasHelpFlag flags -> do
      printCmdHelp sdistCmd customOpts
      exitWith ExitSuccess
    (flags, args', []) ->
      return (unliftFlags flags, args')
    (_, _, errs) -> do mapM_ putStrLn errs
                       exitWith (ExitFailure 1)

registerCmd :: Cmd a
registerCmd = Cmd {
        cmdName        = "register",
        cmdHelp        = "Register this package with the compiler.",
        cmdDescription = "This is the long description for register.\nMulti-line!\n",
        cmdOptions     = [cmd_help,
           Option "" ["user"] (NoArg UserFlag)
               "upon registration, register this package in the user's local package database",
           Option "" ["global"] (NoArg GlobalFlag)
               "(default) upon registration, register this package in the system-wide package database"
           ],
        cmdAction      = RegisterCmd False
        }

parseRegisterArgs :: Bool -> [String] -> [OptDescr a] ->
                     IO (Bool, [a], [String])
parseRegisterArgs cfg args customOpts =
  case getCmdOpt registerCmd customOpts args of
    (flags, _, []) | hasHelpFlag flags -> do
      printCmdHelp registerCmd customOpts
      exitWith ExitSuccess
    (flags, args', []) ->
      return (updateCfg flags cfg, unliftFlags flags, args')
    (_, _, errs) -> do mapM_ putStrLn errs
                       exitWith (ExitFailure 1)
  where updateCfg (fl:flags) uFlag = updateCfg flags $
          case fl of
            UserFlag        -> True
            GlobalFlag      -> False
            Lift _          -> uFlag
            _               -> error $ "Unexpected flag!"
        updateCfg [] t = t

unregisterCmd :: Cmd a
unregisterCmd = Cmd {
        cmdName        = "unregister",
        cmdHelp        = "Unregister this package with the compiler.",
        cmdDescription = "This is the long description for unregister.\nMulti-line!\n",
        cmdOptions     = [cmd_help],
        cmdAction      = UnregisterCmd
        }

parseUnregisterArgs :: [String] -> [OptDescr a] -> IO ([a], [String])
parseUnregisterArgs args customOpts =
  case getCmdOpt unregisterCmd customOpts args of
    (flags, _, []) | hasHelpFlag flags -> do
      printCmdHelp unregisterCmd customOpts
      exitWith ExitSuccess
    (flags, args', []) ->
      return (unliftFlags flags, args')
    (_, _, errs) -> do mapM_ putStrLn errs
                       exitWith (ExitFailure 1)

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

