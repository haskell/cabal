-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.PreProcess
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--
{- Copyright (c) 2003-2005, Isaac Jones, Malcolm Wallace
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

module Distribution.PreProcess (preprocessSources, knownSuffixHandlers,
                                ppSuffixes, PPSuffixHandler, PreProcessor,
                                removePreprocessed, removePreprocessedPackage,
                                ppCpp, ppCpp', ppGreenCard, ppC2hs, ppHsc2hs,
				ppHappy, ppAlex, ppUnlit
                               )
    where

import Distribution.PreProcess.Unlit(unlit)
import Distribution.PackageDescription (setupMessage, PackageDescription(..),
                                        BuildInfo(..), Executable(..), withExe,
					Library(..), withLib, libModules)
import Distribution.Compiler (CompilerFlavor(..), Compiler(..))
import Distribution.Simple.LocalBuildInfo (LocalBuildInfo(..))
import Distribution.Simple.Utils (rawSystemVerbose,
                                  moduleToFilePath, die, dieWithLocation)
import Distribution.Version (Version(..))
import Control.Monad (unless)
import Data.Maybe (fromMaybe)
import Data.List (nub)
import System.Exit (ExitCode(..))
import System.Directory (removeFile, getModificationTime)
import System.Info (os, arch)
import Distribution.Compat.FilePath
	(splitFileExt, joinFileName, joinFileExt)

-- |The interface to a preprocessor, which may be implemented using an
-- external program, but need not be.  The arguments are the name of
-- the input file, the name of the output file and a verbosity level.
-- Here is a simple example that merely prepends a comment to the given
-- source file:
--
-- > ppTestHandler :: PreProcessor
-- > ppTestHandler inFile outFile verbose
-- >     = do when (verbose > 0) $
-- >            putStrLn (inFile++" has been preprocessed to "++outFile)
-- >          stuff <- readFile inFile
-- >          writeFile outFile ("-- preprocessed as a test\n\n" ++ stuff)
-- >          return ExitSuccess
--
type PreProcessor = FilePath  -- Location of the source file in need of preprocessing
                  -> FilePath -- Output filename
                  -> Int      -- verbose
                  -> IO ExitCode


-- |A preprocessor for turning non-Haskell files with the given extension
-- into plain Haskell source files.
type PPSuffixHandler
    = (String, BuildInfo -> LocalBuildInfo -> PreProcessor)

-- |Apply preprocessors to the sources from 'hsSourceDirs', to obtain
-- a Haskell source file for each module.
preprocessSources :: PackageDescription 
		  -> LocalBuildInfo 
		  -> Int                -- ^ verbose
                  -> [PPSuffixHandler]  -- ^ preprocessors to try
		  -> IO ()

preprocessSources pkg_descr lbi verbose handlers = do
    withLib pkg_descr () $ \ lib -> do
        setupMessage "Preprocessing library" pkg_descr
        let bi = libBuildInfo lib
	let biHandlers = localHandlers bi
	sequence_ [do retVal <- preprocessModule (hsSourceDirs bi) modu
                                                 verbose builtinSuffixes biHandlers
                      unless (retVal == ExitSuccess)
                             (die $ "got error code while preprocessing: " ++ modu)
                   | modu <- libModules pkg_descr]
    unless (null (executables pkg_descr)) $
        setupMessage "Preprocessing executables for" pkg_descr
    withExe pkg_descr $ \ theExe -> do
        let bi = buildInfo theExe
	let biHandlers = localHandlers bi
	sequence_ [do retVal <- preprocessModule (nub $ (hsSourceDirs bi)
                                     ++(maybe [] (hsSourceDirs . libBuildInfo) (library pkg_descr)))
                                     modu verbose builtinSuffixes biHandlers
                      unless (retVal == ExitSuccess)
                             (die $ "got error code while preprocessing: " ++ modu)
                   | modu <- otherModules bi]
  where hc = compilerFlavor (compiler lbi)
	builtinSuffixes
	  | hc == NHC = ["hs", "lhs", "gc"]
	  | otherwise = ["hs", "lhs"]
	localHandlers bi = [(ext, h bi lbi) | (ext, h) <- handlers]

-- |Find the first extension of the file that exists, and preprocess it
-- if required.
preprocessModule
    :: [FilePath]			-- ^source directories
    -> String				-- ^module name
    -> Int				-- ^verbose
    -> [String]				-- ^builtin suffixes
    -> [(String, PreProcessor)]		-- ^possible preprocessors
    -> IO ExitCode
preprocessModule searchLoc modu verbose builtinSuffixes handlers = do
    bsrcFiles <- moduleToFilePath searchLoc modu builtinSuffixes
    psrcFiles <- moduleToFilePath searchLoc modu (map fst handlers)
    case psrcFiles of
	[] -> case bsrcFiles of
	          [] -> die ("can't find source for " ++ modu ++ " in " ++ show searchLoc)
	          _  -> return ExitSuccess
	(psrcFile:_) -> do
	    let (srcStem, ext) = splitFileExt psrcFile
	        pp = fromMaybe (error "Internal error in preProcess module: Just expected")
	                       (lookup ext handlers)
	    recomp <- case bsrcFiles of
	                  [] -> return True
	                  (bsrcFile:_) -> do
	                      btime <- getModificationTime bsrcFile
	                      ptime <- getModificationTime psrcFile
	                      return (btime < ptime)
	    if recomp
	      then pp psrcFile (srcStem `joinFileExt` "hs") verbose
	      else return ExitSuccess

removePreprocessedPackage :: PackageDescription
                          -> FilePath -- ^root of source tree (where to look for hsSources)
                          -> [String] -- ^suffixes
                          -> IO ()
removePreprocessedPackage  pkg_descr r suff
    = do withLib pkg_descr () (\lib -> do
                     let bi = libBuildInfo lib
                     removePreprocessed (map (joinFileName r) (hsSourceDirs bi)) (libModules pkg_descr) suff)
         withExe pkg_descr (\theExe -> do
                     let bi = buildInfo theExe
                     removePreprocessed (map (joinFileName r) (hsSourceDirs bi)) (otherModules bi) suff)

-- |Remove the preprocessed .hs files. (do we need to get some .lhs files too?)
removePreprocessed :: [FilePath] -- ^search Location
                   -> [String] -- ^Modules
                   -> [String] -- ^suffixes
                   -> IO ()
removePreprocessed searchLocs mods suffixesIn
    = mapM_ removePreprocessedModule mods
  where removePreprocessedModule m = do
	    -- collect related files
	    fs <- moduleToFilePath searchLocs m otherSuffixes
	    -- does M.hs also exist?
	    hs <- moduleToFilePath searchLocs m ["hs"]
	    unless (null fs) (mapM_ removeFile hs)
	otherSuffixes = filter (/= "hs") suffixesIn

-- ------------------------------------------------------------
-- * known preprocessors
-- ------------------------------------------------------------

ppGreenCard :: BuildInfo -> LocalBuildInfo -> PreProcessor
ppGreenCard = ppGreenCard' []

ppGreenCard' :: [String] -> BuildInfo -> LocalBuildInfo -> PreProcessor
ppGreenCard' inputArgs bi lbi
    = maybe (ppNone "greencard") pp (withGreencard lbi)
    where pp greencard inFile outFile verbose
              = rawSystemVerbose verbose greencard (["-tffi", "-o" ++ outFile, inFile] ++ inputArgs)

-- This one is useful for preprocessors that can't handle literate source.
-- We also need a way to chain preprocessors.
ppUnlit :: PreProcessor
ppUnlit inFile outFile verbose = do
    contents <- readFile inFile
    writeFile outFile (unlit inFile contents)
    return ExitSuccess

ppCpp :: BuildInfo -> LocalBuildInfo -> PreProcessor
ppCpp = ppCpp' []

ppCpp' :: [String] -> BuildInfo -> LocalBuildInfo -> PreProcessor
ppCpp' inputArgs bi lbi =
  case withCpphs lbi of
     Just path                          -> use_cpphs path
     Nothing | compilerFlavor hc == GHC -> use_ghc
     _otherwise                         -> ppNone "cpphs (or GHC)"
  where 
	hc = compiler lbi

	use_cpphs cpphs inFile outFile verbose
	  = rawSystemVerbose verbose cpphs cpphsArgs
	  where cpphsArgs = ("-O"++outFile) : inFile : "--noline" : "--strip"
				 : extraArgs

        extraArgs = sysDefines ++ cppOptions bi lbi ++ inputArgs

        sysDefines =
                ["-D" ++ os ++ "_" ++ loc ++ "_OS" | loc <- locations] ++
                ["-D" ++ arch ++ "_" ++ loc ++ "_ARCH" | loc <- locations]
        locations = ["BUILD", "HOST"]

	use_ghc inFile outFile verbose
	  = rawSystemVerbose verbose (compilerPath hc) 
		(["-E", "-cpp", "-optP-P", "-o", outFile, inFile] ++ extraArgs)

ppHsc2hs :: BuildInfo -> LocalBuildInfo -> PreProcessor
ppHsc2hs bi lbi
    = maybe (ppNone "hsc2hs") pp (withHsc2hs lbi)
  where pp n = standardPP n (hcDefines (compiler lbi)
                         ++ ["-I" ++ dir | dir <- includeDirs bi]
                         ++ [opt | opt@('-':c:_) <- ccOptions bi, c == 'D' || c == 'I']
                         ++ ["--cflag=" ++ opt | opt@('-':'U':_) <- ccOptions bi]
                         ++ ["--lflag=-L" ++ dir | dir <- extraLibDirs bi]
                         ++ ["--lflag=-l" ++ lib | lib <- extraLibs bi])

ppC2hs :: BuildInfo -> LocalBuildInfo -> PreProcessor
ppC2hs bi lbi
    = maybe (ppNone "c2hs") pp (withC2hs lbi)
  where pp n = standardPP n (concat [["-C", opt] | opt <- cppOptions bi lbi])

cppOptions :: BuildInfo -> LocalBuildInfo -> [String]
cppOptions bi lbi
    = hcDefines (compiler lbi) ++
            ["-I" ++ dir | dir <- includeDirs bi] ++
            [opt | opt@('-':c:_) <- ccOptions bi, c `elem` "DIU"]

hcDefines :: Compiler -> [String]
hcDefines Compiler { compilerFlavor=GHC, compilerVersion=version }
  = ["-D__GLASGOW_HASKELL__=" ++ versionInt version]
hcDefines Compiler { compilerFlavor=JHC, compilerVersion=version }
  = ["-D__JHC__=" ++ versionInt version]
hcDefines Compiler { compilerFlavor=NHC, compilerVersion=version }
  = ["-D__NHC__=" ++ versionInt version]
hcDefines Compiler { compilerFlavor=Hugs }
  = ["-D__HUGS__"]
hcDefines _ = []

versionInt :: Version -> String
versionInt (Version { versionBranch = [] }) = "1"
versionInt (Version { versionBranch = [n] }) = show n
versionInt (Version { versionBranch = n1:n2:_ })
  = show n1 ++ take 2 ('0' : show n2)

ppHappy :: BuildInfo -> LocalBuildInfo -> PreProcessor
ppHappy _ lbi
    = maybe (ppNone "happy") pp (withHappy lbi)
  where pp n = standardPP n (hcFlags hc)
        hc = compilerFlavor (compiler lbi)
	hcFlags GHC = ["-agc"]
	hcFlags _ = []

ppAlex :: BuildInfo -> LocalBuildInfo -> PreProcessor
ppAlex _ lbi
    = maybe (ppNone "alex") pp (withAlex lbi)
  where pp n = standardPP n (hcFlags hc)
        hc = compilerFlavor (compiler lbi)
	hcFlags GHC = ["-g"]
	hcFlags _ = []

standardPP :: String -> [String] -> PreProcessor
standardPP eName args inFile outFile verbose
    = rawSystemVerbose verbose eName (args ++ ["-o", outFile, inFile])

ppNone :: String -> PreProcessor
ppNone name inFile _ _ =
    dieWithLocation inFile Nothing $ "no " ++ name ++ " preprocessor available"

-- |Convenience function; get the suffixes of these preprocessors.
ppSuffixes :: [ PPSuffixHandler ] -> [String]
ppSuffixes = map fst

-- |Standard preprocessors: GreenCard, c2hs, hsc2hs, happy, alex and cpphs.
knownSuffixHandlers :: [ PPSuffixHandler ]
knownSuffixHandlers =
  [ ("gc",     ppGreenCard)
  , ("chs",    ppC2hs)
  , ("hsc",    ppHsc2hs)
  , ("x",      ppAlex)
  , ("y",      ppHappy)
  , ("ly",     ppHappy)
  , ("cpphs",  ppCpp)
  ]
