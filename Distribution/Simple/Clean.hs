{-# OPTIONS -cpp -DDEBUG #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Clean
-- Copyright   :  Isaac Jones 2003-2004
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  
--

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

module Distribution.Simple.Clean (
        clean
#ifdef DEBUG        
        ,hunitTests
#endif
  ) where

import Distribution.Simple.Utils (moduleToFilePath, mkLibName)
import Distribution.Package(PackageDescription(..), showPackageId)

import Control.Monad(when)
import Data.Maybe(catMaybes)
import Data.List(intersperse)
import System.Directory(removeFile)

#ifdef DEBUG
import HUnit (Test)
#endif

-- |FIX: Doesn't remove directories created.  Perhaps it should just
-- delete the .hi and .o files?  Perhaps it should just delete the
-- entire build location recursively?

clean :: FilePath -- ^Build location
      -> PackageDescription
      -> IO ()
clean buildLoc pkg_descr
    = do let modules = (allModules pkg_descr) ++ (mainModules pkg_descr)
         removeMeo  <- sequence [moduleToFilePath buildLoc m ["o"] | m <- modules]
         removeMehi <- sequence [moduleToFilePath buildLoc m ["hi"]| m <- modules]
         removeMehs <- sequence [moduleToFilePath buildLoc m ["hs"]| m <- modules]
         let lib = mkLibName buildLoc (showPackageId (package pkg_descr))
         let removeMe = lib:(catMaybes (removeMeo ++ removeMehi ++ removeMehs))
         when (not $ null removeMe)
                  (putStrLn $ "Removing: " ++ (concat (intersperse ", " (map show removeMe))))
         sequence $ map removeFile removeMe
         return ()

-- ------------------------------------------------------------
-- * Testing
-- ------------------------------------------------------------

#ifdef DEBUG
hunitTests :: [Test]
hunitTests = []
#endif
