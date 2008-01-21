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
-- 
-- This module provides functionality to check for common mistakes.

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
        qaCheckPackage,
        QANotice(..)
  ) where

import Control.Monad(when,unless)
import System.Directory(doesFileExist)

import Distribution.Compiler(CompilerFlavor(..))
import Distribution.PackageDescription
import Distribution.License (License(..))

-- ------------------------------------------------------------
-- * Quality Assurance
-- ------------------------------------------------------------

-- TODO: give hints about old extentions. see Simple.GHC, reverse mapping
-- TODO: and allmost ghc -X flags should be extensions
-- TODO: Once we implement striping (ticket #88) we should also reject
--       ghc-options: -optl-Wl,-s.
-- TODO: keep an eye on #190 and implement when/if it's closed.
-- warn for ghc-options: -fvia-C when ForeignFunctionInterface is set
-- http://hackage.haskell.org/trac/hackage/ticket/190

data QANotice
    = QAWarning { qaMessage :: String }
    | QAFailure { qaMessage :: String }

instance Show QANotice where
    show notice = qaMessage notice

-- |Quality Assurance for package descriptions.
qaCheckPackage :: PackageDescription -> IO [QANotice]
qaCheckPackage pkg_descr = fmap fst . runQA $ do
    ghcSpecific pkg_descr
    cabalFormat pkg_descr

    checkLicense pkg_descr

cabalFormat :: PackageDescription -> QA ()
cabalFormat pkg_descr = do
    when (null (category pkg_descr)) $
        warn "No category field."
    when (null (description pkg_descr)) $
        warn "No description field."
    when (null (maintainer pkg_descr)) $
        warn "No maintainer field."
    when (null (synopsis pkg_descr)) $
        warn "No synopsis field."
    when (length (synopsis pkg_descr) >= 80) $
        warn "Over-long synopsis field"


ghcSpecific :: PackageDescription -> QA ()
ghcSpecific pkg_descr = do
    let has_WerrorWall = flip any ghc_options $ \opts ->
                               "-Werror" `elem` opts
                           && ("-Wall"   `elem` opts || "-W" `elem` opts)
        has_Werror     = any (\opts -> "-Werror" `elem` opts) ghc_options
    when has_WerrorWall $
        critical $ "ghc-options: -Wall -Werror makes the package "
                 ++ "very easy to break with future GHC versions."
    when (not has_WerrorWall && has_Werror) $
        warn $ "ghc-options: -Werror makes the package easy to "
            ++ "break with future GHC versions."

    ghcFail "-fasm" $
        "flag -fasm is unnecessary and breaks on all "
        ++ "arches except for x86, x86-64 and ppc."

    ghcFail "-O" $
        "Cabal automatically add the -O flag and setting it yourself "
        ++ "will disable the use of the --disable-optimization flag."

    ghcWarn "-O2" $
        "-O2 is rarely needed as it often prolong the compile time "
        ++ "with usually with little benefit."

    -- most important at this stage to get the framework right
    when (any (`elem` all_ghc_options) ["-ffi", "-fffi"]) $
    	critical $ "Instead of using -ffi or -fffi, use extensions: "
    		 ++"ForeignFunctionInterface"

    where
    ghc_options = [ strs | bi <- allBuildInfo pkg_descr
                         , (GHC, strs) <- options bi ]
    all_ghc_options = concat ghc_options


    ghcWarn :: String -> String -> QA ()
    ghcWarn flag msg =
        when (flag `elem` all_ghc_options) $
            warn ("ghc-options: " ++ msg)

    ghcFail :: String -> String -> QA ()
    ghcFail flag msg =
        when (flag `elem` all_ghc_options) $
            critical ("ghc-options: " ++ msg)


checkLicense :: PackageDescription -> QA ()
checkLicense pkg
    | license pkg == AllRightsReserved
    = critical "license field missing or specified as AllRightsReserved"

    | null (licenseFile pkg)
    = warn "license-file not specified"

    | otherwise = do
        exists <- io $ doesFileExist file
        unless exists $
            critical $ "license-file field refers to the file \"" ++ file
                     ++ "\" which does not exist."
        where file = licenseFile pkg


-- the WriterT monad over IO
data QA a = QA { runQA :: IO ([QANotice], a) }

instance Monad QA where
    a >>= mb = QA $ do
        (warnings, x) <- runQA a
        (warnings', x') <- runQA (mb x)
        return (warnings ++ warnings', x')
    return x = QA $ return ([], x)

qa :: QANotice -> QA ()
qa notice = QA $ return ([notice], ())

warn :: String -> QA ()
warn = qa . QAWarning

critical :: String -> QA ()
critical = qa . QAFailure

io :: IO a -> QA a
io action = QA $ do
    x <- action
    return ([], x)
