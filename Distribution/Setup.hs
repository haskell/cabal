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

module Distribution.Setup (parseArgs, defaultMain) where

-- Base
import System(getArgs)

-- Local
import Distribution.Build(build)
import Distribution.Install(install)
import Distribution.Misc(CommandLineOpts(..), Action(..),
                         LocalBuildInfo, getPersistBuildConfig)
import Distribution.Package(PackageConfig)

-- |Reads local build info, executes function
doBuildInstall :: (PackageConfig -> LocalBuildInfo -> IO ()) -- ^function to apply
               -> PackageConfig
               -> IO ()
doBuildInstall f pkgConf
    = do lbi <- getPersistBuildConfig
         f pkgConf lbi

-- |Parse the standard command-line arguments
parseArgs :: [String] -> CommandLineOpts
parseArgs _ = (BuildCmd, [])

defaultMain :: PackageConfig -> IO ()
defaultMain p
    = do args <- getArgs
         case parseArgs args of
          (BuildCmd,       _) -> doBuildInstall build p
          (InstallCmd,     _) -> doBuildInstall install p
          (PackageInfoCmd, _) -> print p
         return ()

type CommandLineOpts = (Action,
                        [String]) -- The un-parsed remainder

data Action = ConfigCmd LocalBuildInfo
            | BuildCmd
            | InstallCmd
            | SDistCmd
            | PackageInfoCmd
            | UseInfoCmd
            | TestCmd
--             | Register
--             | BDist


-- options :: [ OptDescr (Options -> IO Options) ]
-- options = 
--     [Option "p" ["prefix"]
--        (OptArg
--           (\arg opt -> return opt{command=useinfo...
