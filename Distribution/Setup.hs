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

-- ------------------------------------------------------------
-- * Command Line Types and Exports
-- ------------------------------------------------------------

data CompilerFlavor = GHC | NHC | Hugs | HBC | Helium | OtherCompiler String
              deriving (Show, Read, Eq)

data Compiler = Compiler {compilerFlavor:: CompilerFlavor,
                          compilerPath  :: FilePath,
                          compilerPkgTool :: FilePath}
                deriving (Show, Read, Eq)

emptyCompiler :: Compiler
emptyCompiler = Compiler (OtherCompiler "") "" ""

type CommandLineOpts = (Action,
                        [String]) -- The un-parsed remainder

data Action = ConfigCmd ConfigFlags       -- config
            | BuildCmd                    -- build
            | InstallCmd (Maybe FilePath) -- install
            | SDistCmd                    -- sdist
            | InfoCmd                     -- info
            | RegisterCmd                 -- register
            | UnregisterCmd               -- unregister
	    | HelpCmd			  -- help
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
                     		[]  -> Left ["No command detected"]
                     		[h] -> parseCommands h flags unkFlags
                     		_:_ -> Left ["More than one command detected"]
    where
    parseCommands :: String -- command
                  -> [Flag]
                  -> [String] -- unknown flags
                  -> Either [String] CommandLineOpts
    parseCommands "configure" flags unkFlags
        | not (any isInstallPrefix flags)
          = case getConfigFlags flags of
               Left err          -> Left [err]
               Right configFlags -> Right (ConfigCmd configFlags, unkFlags)
    parseCommands "install" [InstPrefix m] unkFlags
        = Right (InstallCmd $ Just m, unkFlags)
    parseCommands "install" [] unkFlags
        = Right (InstallCmd Nothing, unkFlags)
    parseCommands "build" [] unkFlags
        = Right (BuildCmd, unkFlags)
    parseCommands "sdist" [] unkFlags
        = Right (SDistCmd, unkFlags)
    parseCommands "info" [] unkFlags
        = Right (InfoCmd, unkFlags)
    parseCommands "register" [] unkFlags
        = Right (RegisterCmd, unkFlags)
    parseCommands "unregister" [] unkFlags
        = Right (UnregisterCmd, unkFlags)
    parseCommands c _ _
        = Left $ ["command line syntax error for command: " ++ c]

    isInstallPrefix :: Flag -> Bool
    isInstallPrefix (InstPrefix _) = True
    isInstallPrefix _              = False

-- Converts the abstract "flag" type to a more concrete type.
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

    getOneOpt [] = return Nothing
    getOneOpt [one] = return (Just one)
    getOneOpt _ = fail "Multiple prefix options"

-- ------------------------------------------------------------
-- * Option Specifications
-- ------------------------------------------------------------

-- |Most of these flags are for Configure, but InstPrefix is for Install.
data Flag = GhcFlag | NhcFlag | HugsFlag
          | WithCompiler FilePath | Prefix FilePath
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
           Option "w" ["with-compiler"] (ReqArg WithCompiler "COMPILER PATH")
               "give the path to a particular compiler",
           Option "" ["prefix"] (ReqArg Prefix "DIR")
               "bake this prefix in preparation of installation",
           Option "" ["install-prefix"] (ReqArg InstPrefix "DIR")
               "specify the directory in which to place installed files",
           Option "h?" ["help"] (NoArg HelpFlag)
               "get information on options and commands"
          ]

-- |command, help string
commands :: [(String, String)]
commands = [("configure", "configure this package"),
            ("build", ""),
            ("install", ""),
            ("sdist", ""),
            ("info", ""),
            ("register", ""),
            ("unregister","")
           ]

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------
#ifdef DEBUG
hunitTests :: IO [Test]
hunitTests =
    do let m = [("ghc", GHC), ("nhc", NHC), ("hugs", Hugs)]
       let (flags, commands', unkFlags, ers)
               = getOpt Permute options ["configure", "foobar", "--prefix=/foo", "--ghc", "--nhc", "--hugs", "--with-compiler=/comp", "--unknown1", "--unknown2", "--install-prefix=/foo"]
       return $ [TestLabel "very basic option parsing" $ TestList [
                 "getOpt flags" ~: "failed" ~:
                 [Prefix "/foo", GhcFlag, NhcFlag, HugsFlag,
                  WithCompiler "/comp", InstPrefix "/foo"]
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
                                         ("install", InstallCmd Nothing),
                                         ("sdist", SDistCmd),
                                         ("info", InfoCmd),
                                         ("register", RegisterCmd)]
                  ]
               ]
#endif

{- Testing ideas:
   * IO to look for hugs and hugs-pkg (which hugs, etc)
   * quickCheck to test permutations of arguments
   * what other options can we over-ride with a command-line flag?
-}
