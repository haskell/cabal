{-# OPTIONS -cpp -DDEBUG #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Setup
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  
--
-- Explanation: <FIX>
-- WHERE DOES THIS MODULE FIT IN AT A HIGH-LEVEL <FIX>

{- Copyright (c) 2003-2004, Isaac Jones
All rights reserved.

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

module Distribution.Setup (parseArgs, Action(..), ConfigFlags,
                           CompilerFlavor(..), Compiler(..),
			   optionHelpString,
#ifdef DEBUG
                           hunitTests,
#endif
                           ) where

 -- Local
import Distribution.GetOpt

-- Misc:
#ifdef DEBUG
import HUnit (Test(..), (~:), (~=?))
#endif

import Control.Monad.Error
import Data.List(intersperse)
import Data.Maybe(listToMaybe)

-- ------------------------------------------------------------
-- * Command Line Types and Exports
-- ------------------------------------------------------------

data CompilerFlavor = GHC | NHC | Hugs | HBC | Helium | OtherCompiler String
              deriving (Show, Read, Eq)

data Compiler = Compiler {compilerFlavor:: CompilerFlavor,
                          compilerPath  :: FilePath,
                          compilerPkgTool :: FilePath}
                deriving (Show, Read, Eq)

type CommandLineOpts = (Action,
                        [String]) -- The un-parsed remainder

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
                    Maybe FilePath) -- prefix

-- |Parse the standard command-line arguments.
parseArgs :: [String] -> Either [String] CommandLineOpts
parseArgs args
    = let (flags, commands', unkFlags, ers) = getOpt Permute options args
          in case ers of
             _:_ -> Left ers
             []  -> if HelpFlag `elem` flags then
			Right (HelpCmd, unkFlags)
		    else case commands' of
                     		[]  -> Left ["Missing command"]
                     		[h] -> parseCommands h flags unkFlags
                     		c   -> Left ["Multiple commands: " ++ (concat $ intersperse ", " c)]
    where
    -- FIX: really must clean up all this parsing code.
    parseCommands :: String -- command
                  -> [Flag]
                  -> [String] -- unknown flags
                  -> Either [String] CommandLineOpts
    parseCommands str flags unkFlags
	= case str of
		"configure"  -> parseConfigure flags unkFlags
		"install"    -> parseInstall flags unkFlags
		"build"      -> noFlags str BuildCmd flags unkFlags
		"clean"      -> noFlags str CleanCmd flags unkFlags
		"sdist"      -> noFlags str SDistCmd flags unkFlags
        	"register"   -> parseRegister flags unkFlags
        	"unregister" -> noFlags str UnregisterCmd flags unkFlags
    		_            -> Left ["Unrecognised command: " ++ str]

    parseConfigure flags unkFlags
        | not (any isInstallPrefix flags)
          = case getConfigFlags flags of
               Left err          -> Left [err]
               Right configFlags -> Right (ConfigCmd configFlags, unkFlags)
	| otherwise
	  = commandSyntaxError "configure"

    -- | FIX: no error checking for bad flags.
    parseInstall flags unkFlags
        = let pref = listToMaybe [f | InstPrefix f <- flags]
              in isUser flags (\x -> Right (InstallCmd pref x, unkFlags))

    parseRegister flags unkFlags
        =  isUser flags (\x -> Right (RegisterCmd x, unkFlags))

    isUser flags f
        = if length (filter isUserGlobFlag flags) <= 1
          then f $ not $ null (filter isUserFlag flags)
          else commandSyntaxError "Specify only one of --user and --global"

    isUserGlobFlag UserFlag = True
    isUserGlobFlag GlobalFlag = True
    isUserGlobFlag _ = False
    isUserFlag UserFlag = True
    isUserFlag _ = False

    noFlags _ cmd [] unkFlags 
	= Right (cmd, unkFlags)
    noFlags str _ _ _
	= commandSyntaxError str

    commandSyntaxError c = Left ["Syntax error for command: " ++ c]

    isInstallPrefix :: Flag -> Bool
    isInstallPrefix (InstPrefix _) = True
    isInstallPrefix _              = False

-- |Converts the abstract "flag" type to a more concrete type.
getConfigFlags :: [Flag] -> Either String ConfigFlags
getConfigFlags flags
    = do flavor  <- getOneOpt [f | Just f <- map convert flags]
         prefix  <- getOneOpt [f | Prefix f <- flags]
         withCom <- getOneOpt [f | WithCompiler f <- flags]
         return (flavor,withCom,prefix)
    where
    convert GhcFlag  = Just GHC
    convert NhcFlag  = Just NHC
    convert HugsFlag = Just Hugs
    convert _        = Nothing

getOneOpt :: Show a => [a] -> Either String (Maybe a)
getOneOpt [] = return Nothing
getOneOpt [one] = return (Just one)
getOneOpt o = fail $ "Multiple options where one expected: "
                ++ (concat $ intersperse ", " (map show o))

-- ------------------------------------------------------------
-- * Option Specifications
-- ------------------------------------------------------------

-- |Most of these flags are for Configure, but InstPrefix is for Install.
data Flag = GhcFlag | NhcFlag | HugsFlag
          | WithCompiler FilePath | Prefix FilePath
          | UserFlag | GlobalFlag
          | HelpFlag
          -- For install:
          | InstPrefix FilePath
--          | Verbose | Version?
            deriving (Show, Eq)

optionHelpString :: String -> String
optionHelpString prefix = usageInfo prefix options

-- |Flag-type options (not commands)
options :: [OptDescr Flag]
options = [Option "g" ["ghc"] (NoArg GhcFlag) "compile with GHC",
           Option "n" ["nhc"] (NoArg NhcFlag) "compile with NHC",
           Option "" ["hugs"] (NoArg HugsFlag) "compile with hugs",
           Option "w" ["with-compiler"] (ReqArg WithCompiler "PATH")
               "give the path to a particular compiler",
           Option "" ["prefix"] (ReqArg Prefix "DIR")
               "bake this prefix in preparation of installation",
           Option "" ["install-prefix"] (ReqArg InstPrefix "DIR")
               "specify the directory in which to place installed files",
           Option "" ["user"] (NoArg UserFlag)
               "upon registration, register this package in the user's local package database",
           Option "" ["global"] (NoArg GlobalFlag)
               "(default) upon registration, register this package in the system-wide package database",
           Option "h?" ["help"] (NoArg HelpFlag)
               "get information on options and commands"
          ]

-- |command, help string
commands :: [(String, String)]
commands = [("configure", "configure this package"),
            ("build", ""),
            ("install", ""),
            ("sdist", ""),
            ("register", ""),
            ("unregister","")
           ]

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------
#ifdef DEBUG
hunitTests :: [Test]
hunitTests =
    let m = [("ghc", GHC), ("nhc", NHC), ("hugs", Hugs)]
        (flags, commands', unkFlags, ers)
               = getOpt Permute options ["configure", "foobar", "--prefix=/foo", "--ghc", "--nhc", "--hugs", "--with-compiler=/comp", "--unknown1", "--unknown2", "--install-prefix=/foo", "--user", "--global"]
       in  [TestLabel "very basic option parsing" $ TestList [
                 "getOpt flags" ~: "failed" ~:
                 [Prefix "/foo", GhcFlag, NhcFlag, HugsFlag,
                  WithCompiler "/comp", InstPrefix "/foo", UserFlag, GlobalFlag]
                 ~=? flags,
                 "getOpt commands" ~: "failed" ~: ["configure", "foobar"] ~=? commands',
                 "getOpt unknown opts" ~: "failed" ~:
                      ["--unknown1", "--unknown2"] ~=? unkFlags,
                 "getOpt errors" ~: "failed" ~: [] ~=? ers],

               TestLabel "test location of various compilers" $ TestList
               ["configure parsing for prefix and compiler flag" ~: "failed" ~:
                    (Right (ConfigCmd (Just comp, Nothing, Just "/usr/local"), []))
                   ~=? (parseArgs ["--prefix=/usr/local", "--"++name, "configure"])
                   | (name, comp) <- m],

               TestLabel "find the package tool" $ TestList
               ["configure parsing for prefix comp flag, withcompiler" ~: "failed" ~:
                    (Right (ConfigCmd (Just comp, Just "/foo/comp", Just "/usr/local"), []))
                   ~=? (parseArgs ["--prefix=/usr/local", "--"++name,
                                   "--with-compiler=/foo/comp", "configure"])
                   | (name, comp) <- m],

               TestLabel "simpler commands" $ TestList
               [flag ~: "failed" ~: (Right (flagCmd, [])) ~=? (parseArgs [flag])
                   | (flag, flagCmd) <- [("build", BuildCmd),
                                         ("install", InstallCmd Nothing False),
                                         ("sdist", SDistCmd),
                                         ("register", RegisterCmd False)]
                  ]
               ]
#endif

{- Testing ideas:
   * IO to look for hugs and hugs-pkg (which hugs, etc)
   * quickCheck to test permutations of arguments
   * what other options can we over-ride with a command-line flag?
-}
