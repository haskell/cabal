-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.PreProcess
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  GHC, Hugs
--
{- Copyright (c) 2003-2004, Isaac Jones, Malcolm Wallace
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
                                removePreprocessed, removePreprocessedPackage)
    where

import Distribution.PreProcess.Unlit(plain, unlit)
import Distribution.PackageDescription (setupMessage, PackageDescription(..),
                             BuildInfo(..), Executable(..))
import Distribution.Simple.Configure (LocalBuildInfo(..))
import Distribution.Simple.Utils (rawSystemPath, moduleToFilePath,
                                  sequenceMap, removeFiles)
import Control.Monad(when)
import System.Exit (ExitCode(..), exitWith)
import Distribution.Compat.FilePath
	(splitFilePath, splitFileExt, joinFileName, joinFileExt)

-- |A preprocessor must fulfill this basic interface.  It can be an
-- external program, or just a function.
type PreProcessor = FilePath  -- ^Location of the source file in need of preprocessing
                  -> FilePath -- ^Output filename
                  -> IO ExitCode


-- |How to dispatch this file to a preprocessor.  Is there a better
-- way to handle this "Unlit" business?  It is nice that it can handle
-- happy, for instance.  Maybe we need a way to chain preprocessors
-- that would solve this problem.

type PPSuffixHandler
    = (String, (String->String->String), PreProcessor)

-- |Copy and (possibly) preprocess sources from hsSourceDirs
preprocessSources :: PackageDescription 
		  -> LocalBuildInfo 
                  -> [PPSuffixHandler]  -- ^ preprocessors to try
		  -> IO ()

preprocessSources pkg_descr _ handlers = 
    do
    setupMessage "Preprocessing" pkg_descr
    allSources <- findAllSourceFiles pkg_descr (ppSuffixes handlers)
    sequence [dispatchPP src handlers | src <- allSources] -- FIX: output errors?
    return ()

dispatchPP :: FilePath -> [ PPSuffixHandler ] -> IO ExitCode
dispatchPP p handlers
    = do let (dir, file, ext) = splitFilePath p
         let (Just (lit, pp)) = findPP ext handlers --FIX: Nothing case?
         pp p (joinFileName dir (joinFileExt file "hs"))

findPP :: String -- ^Extension
       -> [PPSuffixHandler]
       -> Maybe ((String -> String -> String), PreProcessor)
findPP ext ((e2, lit, pp):t)
    | e2 == ext = Just (lit, pp)
    | otherwise = findPP ext t
findPP _ [] = Nothing


-- |Locate the source files based on the module names, the search
-- paths (both in PackageDescription) and the suffixes we might be
-- interested in.
findAllSourceFiles :: PackageDescription
                   -> [String] -- ^search suffixes
                   -> IO [FilePath]
findAllSourceFiles PackageDescription{executables=execs, library=lib} allSuffixes
    = do exeFiles <- sequence [buildInfoSources (buildInfo e) allSuffixes | e <- execs]
         libFiles <- case lib of 
                       Just bi -> buildInfoSources bi allSuffixes
                       Nothing -> return []
         return $ ((concat exeFiles) ++ libFiles)

        where buildInfoSources :: BuildInfo -> [String] -> IO [FilePath]
              buildInfoSources BuildInfo{modules=mods, hsSourceDir=dir} suffixes
                  = sequence [moduleToFilePath dir modu suffixes | modu <- mods] >>= return . concat

removePreprocessedPackage :: PackageDescription
                          -> FilePath -- ^root of source tree (where to look for hsSources)
                          -> [String] -- ^suffixes
                          -> IO ()
removePreprocessedPackage pkg_descr r suff
    = do maybe (return ()) removePPBuildInfo (library pkg_descr)
         sequenceMap removePPBuildInfo (map buildInfo (executables pkg_descr))
         return ()
    where removePPBuildInfo :: BuildInfo -> IO ()
          removePPBuildInfo bi
              = removePreprocessed (r `joinFileName` (hsSourceDir bi)) (modules bi) suff

-- |Remove the preprocessed .hs files. (do we need to get some .lhs files too?)
removePreprocessed :: FilePath -- ^search Location
                   -> [String] -- ^Modules
                   -> [String] -- ^suffixes
                   -> IO ()
removePreprocessed searchLoc mods suffixesIn
    = sequenceMap (\m -> moduleToFilePath searchLoc m suffixesIn) mods -- collect related files
         >>= sequenceMap removeIfDup -- delete the .hs stuff.
         >>  return ()
         where -- ^Should give a list of files that only differ by the extension.
               removeIfDup :: [FilePath] -> IO ()
               removeIfDup []  = return ()
               removeIfDup [x] = return () -- if there's only one, it needs to stay
               removeIfDup l = do when (not $ extensionProp l)
                                    (putStrLn "Internal Error: attempt to remove source with no matching preprocessed element."
                                     >> exitWith (ExitFailure 1))
                                  let hsFiles = (filter (\x -> snd (splitFileExt x) == "hs") l)
                                  when (length hsFiles > 1)
                                    (putStrLn "Internal Error: multiple \".hs\" files found while removing preprocessed element."
                                     >> exitWith (ExitFailure 1))
                                  removeFiles hsFiles
                                  return ()
               -- the files in this list only differ by their extension
               extensionProp []  = True
               extensionProp [x] = True
               extensionProp (x1:x2:xs)
                   = let (dir1, name1, _) = splitFilePath x1
                         (dir2, name2, _) = splitFilePath x2
                         in dir1 == dir2 && name1 == name2 && (extensionProp (x2:xs))

-- ------------------------------------------------------------
-- * known preprocessors
-- ------------------------------------------------------------

ppCpp, ppGreenCard, ppHsc2hs, ppC2hs, ppHappy, ppNone :: PreProcessor

ppCpp inFile outFile
    = rawSystemPath "cpphs" ["-O" ++ outFile, inFile]
ppGreenCard inFile outFile
    = rawSystemPath "green-card" ["-tffi", "-o" ++ outFile, inFile]
ppHsc2hs = standardPP "hsc2hs"
ppC2hs inFile outFile
    = rawSystemPath "c2hs" ["-o " ++ outFile, inFile]
ppHappy = standardPP "happy"
ppNone _ _  = return ExitSuccess

ppTestHandler :: FilePath -- ^InFile
              -> FilePath -- ^OutFile
              -> IO ExitCode
ppTestHandler inFile outFile
    = do stuff <- readFile inFile
         writeFile outFile ("-- this file has been preprocessed as a test\n\n" ++ stuff)
         return ExitSuccess

standardPP :: String -> PreProcessor
standardPP eName inFile outFile
    = rawSystemPath eName ["-o" ++ outFile, inFile]

-- |Convinience function; get the suffixes of these preprocessors.
ppSuffixes :: [ PPSuffixHandler ] -> [String]
ppSuffixes h = [s | (s, _, _) <- h]

-- |Leave in unlit since some preprocessors can't handle literated
-- source?
knownSuffixHandlers :: [ PPSuffixHandler ]
knownSuffixHandlers =
  [ ("gc",     plain, ppGreenCard)
  , ("chs",    plain, ppC2hs)
  , ("hsc",    plain, ppHsc2hs)
  , ("y",      plain, ppHappy)
  , ("ly",     unlit, ppHappy)
  , ("cpphs",  plain, ppCpp)
  , ("gc",     plain, ppNone)	-- note, for nhc98 only
  , ("hs",     plain, ppNone)
  , ("lhs",    unlit, ppNone)
  , ("testSuffix", plain, ppTestHandler)
  ] 
