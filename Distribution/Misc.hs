-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Misc
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

module Distribution.Misc(Compiler, License(..), Dependency,
                         Extension, Opt, LocalBuildInfo,
                         CompilerFlavor(..),
                         writePersistBuildConfig, getPersistBuildConfig)
    where

import Distribution.Version(VersionRange)

-- ------------------------------------------------------------
-- * Compiler
-- ------------------------------------------------------------

data CompilerFlavor = GHC | NHC | Hugs | HBC | Helium | OtherCompiler String
              deriving Show

data Compiler = Compiler {flavor        :: CompilerFlavor,
                          path          :: FilePath,
                          packagingTool :: FilePath}
                deriving Show

emptyCompiler :: Compiler
emptyCompiler = Compiler (OtherCompiler "") "" ""

-- ------------------------------------------------------------
-- * build config
-- ------------------------------------------------------------

-- |Data cached after configuration step.
data LocalBuildInfo = LocalBuildInfo {prefix :: String,
                                      compiler :: Compiler}

emptyLocalBuildInfo :: LocalBuildInfo
emptyLocalBuildInfo = LocalBuildInfo "" emptyCompiler

getPersistBuildConfig :: IO LocalBuildInfo
getPersistBuildConfig = return emptyLocalBuildInfo -- FIX

writePersistBuildConfig :: LocalBuildInfo -> IO ()
writePersistBuildConfig _ = return () --FIX

-- ------------------------------------------------------------
-- * Misc
-- ------------------------------------------------------------

data License = GPL | LGPL | BSD3 | BSD4 | PublicDomain | AllRightsReserved
             | {- ... | -} OtherLicense FilePath
               deriving (Read, Show)

-- |Maybe move to Distribution.Version?
data Dependency = Dependency String VersionRange
                  deriving (Read, Show)

-- |This represents non-standard compiler extensions which each
-- package might employ.  Not yet implemented.

data Extension = Foo | Bar deriving Show

type Opt = String
