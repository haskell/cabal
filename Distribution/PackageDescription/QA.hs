{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.PackageDescription.QA
-- Copyright   :  Lennart Kolmodin 2008
--
-- Maintainer  :  Lennart Kolmodin <kolmodin@gentoo.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Quality Assurance for package descriptions.

{- All rights reserved.

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

module Distribution.PackageDescription.QA (
        -- * Quality Assurance
        qaCheckPackage
  ) where

import Control.Monad(when,unless)
import Data.List(intersperse)
import System.Directory(doesFileExist)

import Distribution.Compiler(CompilerFlavor(..))
import Distribution.PackageDescription

-- ------------------------------------------------------------
-- * Quality Assurance
-- ------------------------------------------------------------

-- |Quality Assurance for package descriptions.
qaCheckPackage :: PackageDescription -> IO [String]
qaCheckPackage pkg_descr = fmap fst . runQA $ do

    flip mapM_ ghc_options $ \ flags -> do
        let has_Wall = "-Wall" `elem` flags
            has_Werr = "-Werror" `elem` flags
        when (has_Wall && has_Werr) $
            qa $ "Using both -Wall and -Werror makes the package easy to "
                 ++ "break with future GHC versions."

    ghcWarn "-fasm" $
        "flag -fasm is unnecessary and breaks on all "
        ++ "arches except for x86, x86-64 and ppc."

    ghcWarn "-O" $
        "Cabal automatically add the -O flag and setting it yourself "
        ++ "will disable the use of the --disable-optimization flag."

    ghcWarn "-O2" $
        "-O2 is rarely needed as it often prolong the compile time "
        ++ "with usually with little benefit."

    let ffi_msg = "Instead of using -ffi or -fffi, use extensions: ForeignFunctionInterface"

    ghcWarn  "-ffi" ffi_msg
    ghcWarn "-fffi" ffi_msg

    checkLicenseExists pkg_descr

    -- TODO: keep an eye on #190 and implement when/if it's closed.
    -- warn for ghc-options: -fvia-C when ForeignFunctionInterface is set
    -- http://hackage.haskell.org/trac/hackage/ticket/190

    where
    ghc_options = [ strs | bi <- allBuildInfo pkg_descr
                         , (GHC, strs) <- options bi ]
    all_ghc_options = concat ghc_options

    ghcWarn :: String -> String -> QA ()
    ghcWarn flag msg =
        warnWhenFlag all_ghc_options flag ("ghc-options: " ++ msg)

checkLicenseExists :: PackageDescription -> QA ()
checkLicenseExists pkg = do
    exists <- io $ doesFileExist file
    unless exists $
        qa $ "Cabal file refers to license file \"" ++ file
             ++ "\" which does not exist."
    where
    file = licenseFile pkg

warnWhenFlag :: [String] -> String -> String -> QA ()
warnWhenFlag flags flag msg =
    when (flag `elem` flags) (qa msg)

-- the WriterT monad over IO
data QA a = QA { runQA :: IO ([String], a) }

instance Monad QA where
    a >>= mb = QA $ do
        (warnings, x) <- runQA a
        (warnings', x') <- runQA (mb x)
        return (warnings ++ warnings', x')
    return x = QA $ return ([], x)

qa :: String -> QA ()
qa msg = QA $ return ([withLines pretty msg], ())
    where
    -- like (unlines . f . lines) except no trailing \n
    withLines f = concat . intersperse "\n" . f . lines
    pretty [] = []
    pretty (x:xs) = ("QA: " ++ x) : map ("    "++) xs

io :: IO a -> QA a
io action = QA $ do
    x <- action
    return ([], x)
