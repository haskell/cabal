{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.SrcDist
-- Copyright   :  Simon Marlow 2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  
--

{- Copyright (c) 2003-2004, Simon Marlow
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

-- NOTE: FIX: we don't have a great way of testing this module, since
-- we can't easily look inside a tarball once its created.

module Distribution.Simple.SrcDist (
	sdist
#ifdef DEBUG        
        ,hunitTests
#endif
  )  where

import Distribution.PackageDescription
	(PackageDescription(..), BuildInfo(..), buildInfo, setupMessage, biModules)
import Distribution.Package (showPackageId)
import Distribution.Simple.Utils(moveSources, die, sequenceMap)
import Distribution.PreProcess (PPSuffixHandler, ppSuffixes, removePreprocessed)

import Control.Monad(when)
import System.Cmd (system)
import System.Directory (doesDirectoryExist)
import Distribution.Compat.FilePath (joinFileName)

#ifdef DEBUG
import HUnit (Test)
#endif

-- |Create a source distribution. FIX: Calls tar directly (won't work
-- on windows).
sdist :: FilePath -- ^build prefix (temp dir)
      -> FilePath -- ^TargetPrefix
      -> [PPSuffixHandler]  -- ^ extra preprocessors (includes suffixes)
      -> PackageDescription
      -> IO ()
sdist tmpDir targetPref pps pkg_descr = do
  setupMessage "Building source dist for" pkg_descr
  ex <- doesDirectoryExist tmpDir
  when ex (die $ "Source distribution already in place. please move: " ++ tmpDir)
  let targetDir = tmpDir `joinFileName` (nameVersion pkg_descr)
  -- maybe move the library files into place
  maybe (return ()) (prepareDir targetDir pps) (library pkg_descr)
  -- move the executables into place
  sequenceMap (prepareDir targetDir pps) (map buildInfo (executables pkg_descr))
  -- setup isn't listed in the description file.
  moveSources ""     targetDir ["Setup"] ["lhs", "hs"]
  system $ "tar --directory=" ++ tmpDir ++ " -zcf " ++
	     (targetPref `joinFileName` (tarBallName pkg_descr))
		    ++ " " ++ (nameVersion pkg_descr)
  system $ "rm -rf " ++ tmpDir
  putStrLn "Source tarball created."

-- |Move the sources into place based on buildInfo
prepareDir :: FilePath  -- ^TargetPrefix
           -> [PPSuffixHandler]  -- ^ extra preprocessors (includes suffixes)
           -> BuildInfo
           -> IO ()
prepareDir inPref pps bi@BuildInfo{hsSourceDir=srcDir}
    = do let pref = inPref `joinFileName` srcDir
         let suff = ppSuffixes pps
         moveSources srcDir pref (biModules bi) (suff ++ ["hs", "lhs"])
         removePreprocessed pref (biModules bi) (suff ++ ["hs", "lhs"])

------------------------------------------------------------

-- |The file name of the tarball
tarBallName :: PackageDescription -> FilePath
tarBallName p = (nameVersion p) ++ ".tgz"

nameVersion :: PackageDescription -> String
nameVersion = showPackageId . package

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------

#ifdef DEBUG
hunitTests :: [Test]
hunitTests = []
#endif
