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
                                PPSuffixHandler, PreProcessor(..))
    where

import Distribution.PreProcess.Unlit(plain, unlit)
import Distribution.Package (PackageDescription(..), BuildInfo(..), Executable(..))
import Distribution.Simple.Configure (LocalBuildInfo(..))
import Distribution.Simple.Utils (setupMessage,moveSources, pathJoin,
                                  withLib, rawSystemPath)
import Distribution.Setup (CompilerFlavor(..))

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

data PreProcessor = PreProcessor
	{ ppExecutableName   :: String,
          ppDefaultOptions   :: [String],
        -- |How to construct the output option
	  ppOutputFileOption :: FilePath -> String,
        -- |Whether the pp produces source appropriate for this compiler.
	  ppSuitable         :: CompilerFlavor -> Bool
	}
      | PreProcessAction
        {ppFun :: (FilePath    -- Input file
                   -> FilePath -- output file
                   -> IO ()),
         ppSuitable :: CompilerFlavor -> Bool}

type PPSuffixHandler
    = (String, (String->String->String), PreProcessor)

-- |FIX: Some preprocessors aren't respecting the output location; for
-- these, we should move the file?  Should we change it to "directory"?

executePreprocessor :: PreProcessor
                    -> FilePath     -- ^Location of the source file
                    -> FilePath     -- ^Location of the output file
                    -> IO ()        -- ^The constructed command-line
executePreprocessor (PreProcessor exeName inOpts outFun _) sourceFile outFile
    = let opts = if (null (outFun outFile))
                 then inOpts ++ [sourceFile]
                 else (inOpts ++ [outFun outFile, sourceFile])
          in rawSystemPath exeName opts >> return ()
executePreprocessor (PreProcessAction f _) sourceFile outFile
    = f sourceFile outFile

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
--  , ("lhs",    unlit, ppNone)
  ] 

ppCpp, ppGreenCard, ppHsc2hs, ppC2hs, ppHappy, ppNone :: PreProcessor
ppCpp = PreProcessor
 	{ ppExecutableName = "cpphs"
 	, ppDefaultOptions = []
 	, ppOutputFileOption = \f-> "-O"++f
 	, ppSuitable = \_-> True
 	}
ppGreenCard = PreProcessor
	{ ppExecutableName = "green-card"
	, ppDefaultOptions = ["-tffi"]	-- + includePath of compiler?
	, ppOutputFileOption = \f-> "-o "++f
	, ppSuitable = \hc-> hc == GHC
	}
ppHsc2hs = PreProcessor
	{ ppExecutableName = "hsc2hs"
	, ppDefaultOptions = []
	, ppOutputFileOption = \_-> ""
	, ppSuitable = \hc-> hc `elem` [GHC,NHC]
	}
ppC2hs = PreProcessor
	{ ppExecutableName = "c2hs"
	, ppDefaultOptions = []
	, ppOutputFileOption = \_-> ""
	, ppSuitable = \hc-> hc `elem` [GHC,NHC]
	}
ppHappy = PreProcessor
	{ ppExecutableName = "happy"
	, ppDefaultOptions = []
	, ppOutputFileOption = \_-> ""
	, ppSuitable = \_-> True
	}
ppNone = PreProcessor
	{ ppExecutableName = ""
	, ppDefaultOptions = []
	, ppOutputFileOption = \_-> ""
	, ppSuitable = \hc-> True
	}
