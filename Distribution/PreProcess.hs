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

module Distribution.PreProcess (preprocessSources, knownSuffixes,
                                PPSuffixHandler, PreProcessor)
    where

import Distribution.PreProcess.Unlit(plain, unlit)
import Distribution.Package (PackageDescription(..), BuildInfo(..), Executable(..))
import Distribution.Simple.Configure (LocalBuildInfo(..))
import Distribution.Simple.Utils (setupMessage,moveSources, pathJoin,
                                  withLib, rawSystemPath, splitFilePath)
import System.Exit (ExitCode(..))

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
		  -> FilePath           {- ^ Directory to put preprocessed 
				             sources in -}
		  -> IO ()

preprocessSources pkg_descr _ _ pref = 
    do
    setupMessage "Preprocessing" pkg_descr
    withLib pkg_descr $ \lib ->
        moveSources (hsSourceDir lib) (pathJoin [pref, hsSourceDir lib]) (modules lib) ["hs","lhs"] 
    sequence_ [ moveSources (hsSourceDir exeBi) (pathJoin [pref, hsSourceDir exeBi]) (modules exeBi) ["hs","lhs"]
              | Executable _ _ exeBi <- executables pkg_descr]

dispatchPP :: FilePath -> [ PPSuffixHandler ] -> IO ()
dispatchPP p handlers
    = do let (dir, file, ext) = splitFilePath p
         let (Just (lit, pp)) = findPP ext handlers
         return ()

findPP :: String -- ^Extension
       -> [PPSuffixHandler]
       -> Maybe ((String -> String -> String), PreProcessor)
findPP ext ((e2, lit, pp):t)
    | e2 == ext = Just (lit, pp)
    | otherwise = findPP ext t
findPP _ [] = Nothing


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

standardPP :: String -> PreProcessor
standardPP eName inFile outFile
    = rawSystemPath eName ["-o" ++ outFile, inFile]

-- |Leave in unlit since some preprocessors can't handle literated
-- source?
knownSuffixes :: [ PPSuffixHandler ]
knownSuffixes =
  [ ("gc",     plain, ppGreenCard)
  , ("chs",    plain, ppC2hs)
  , ("hsc",    plain, ppHsc2hs)
  , ("y",      plain, ppHappy)
  , ("ly",     unlit, ppHappy)
  , ("cpphs",  plain, ppCpp)
  , ("gc",     plain, ppNone)	-- note, for nhc98 only
  , ("hs",     plain, ppNone)
  , ("lhs",    unlit, ppNone)
  ] 
