-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.JHC
-- Copyright   :  Isaac Jones 2003-2006
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--

{- Copyright (c) 2003-2005, Isaac Jones
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

module Distribution.Simple.JHC (
	build, installLib, installExe
 ) where

import Distribution.PackageDescription
				( PackageDescription(..), BuildInfo(..),
				  withLib,
				  Executable(..), withExe, Library(..),
				  libModules, hcOptions )
import Distribution.Simple.LocalBuildInfo
				( LocalBuildInfo(..), 
				  autogenModulesDir )
import Distribution.Compiler 	( Compiler(..), CompilerFlavor(..),
				  extensionsToJHCFlag )
import Distribution.Package  	( showPackageId )
import Distribution.Simple.Utils( rawSystemExit, copyFileVerbose )
import Distribution.Compat.FilePath
				( joinFileName, exeExtension )
import Distribution.Compat.Directory
				( createDirectoryIfMissing )

import Control.Monad		( when )
import Data.List		( nub, intersperse )


-- -----------------------------------------------------------------------------
-- Building

-- | Building a package for JHC.
-- Currently C source files are not supported.
build :: PackageDescription -> LocalBuildInfo -> Int -> IO ()
build pkg_descr lbi verbose = do
  let jhcPath = compilerPath (compiler lbi)
  withLib pkg_descr () $ \lib -> do
      when (verbose > 3) (putStrLn "Building library...")
      let libBi = libBuildInfo lib
      let args  = constructJHCCmdLine lbi libBi (buildDir lbi) verbose
      rawSystemExit verbose jhcPath (["-c"] ++ args ++ libModules pkg_descr)
      let pkgid = showPackageId (package pkg_descr)
          pfile = buildDir lbi `joinFileName` "jhc-pkg.conf"
          hlfile= buildDir lbi `joinFileName` (pkgid ++ ".hl")
      writeFile pfile $ jhcPkgConf pkg_descr
      rawSystemExit verbose jhcPath ["--build-hl="++pfile, "-o", hlfile]
  withExe pkg_descr $ \exe -> do
      when (verbose > 3) (putStrLn ("Building executable "++exeName exe))
      let exeBi = buildInfo exe
      let out   = buildDir lbi `joinFileName` exeName exe
      let args  = constructJHCCmdLine lbi exeBi (buildDir lbi) verbose
      rawSystemExit verbose jhcPath (["-o",out] ++ args ++ [modulePath exe])

constructJHCCmdLine :: LocalBuildInfo -> BuildInfo -> FilePath -> Int -> [String]
constructJHCCmdLine lbi bi odir verbose =
        (if verbose > 4 then ["-v"] else [])
     ++ snd (extensionsToJHCFlag (extensions bi))
     ++ hcOptions JHC (options bi)
     ++ ["--noauto","-i-"]
     ++ ["-i", autogenModulesDir lbi]
     ++ concat [["-i", l] | l <- nub (hsSourceDirs bi)]
     ++ ["-optc" ++ opt | opt <- ccOptions bi]
     ++ (concat [ ["-p", showPackageId pkg] | pkg <- packageDeps lbi ])

jhcPkgConf :: PackageDescription -> String
jhcPkgConf pd =
  let sline name sel = name ++ ": "++sel pd
      Just lib = library pd
      comma f l = concat $ intersperse "," $ map f l
  in unlines [sline "name" (showPackageId . package)
             ,"exposed-modules: " ++ (comma id (exposedModules lib))
             ,"hidden-modules: " ++ (comma id (otherModules $ libBuildInfo lib))
             ]
                         
installLib :: Int -> FilePath -> FilePath -> PackageDescription -> Library -> IO ()
installLib verb dest build pkg_descr _ = do
    let p = showPackageId (package pkg_descr)++".hl"
    createDirectoryIfMissing True dest
    copyFileVerbose verb (joinFileName build p) (joinFileName dest p)

installExe :: Int -> FilePath -> FilePath -> PackageDescription -> Executable -> IO ()
installExe verb dest build pkg_descr exe = do
    let out   = exeName exe `joinFileName` exeExtension
    createDirectoryIfMissing True dest
    copyFileVerbose verb (joinFileName build out) (joinFileName dest out)
