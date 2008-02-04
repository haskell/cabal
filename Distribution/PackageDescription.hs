-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.PackageDescription
-- Copyright   :  Isaac Jones 2003-2005
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Package description and parsing.

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

module Distribution.PackageDescription (
        -- * Package descriptions
        PackageDescription(..),
        GenericPackageDescription(..),
        finalizePackageDescription,
        flattenPackageDescription,
        emptyPackageDescription,
        readPackageDescription,
        writePackageDescription,
        parsePackageDescription,
        showPackageDescription,
        BuildType(..),

        -- ** Libraries
        Library(..),
        withLib,
        hasLibs,
        libModules,

        -- ** Executables
        Executable(..),
        withExe,
        hasExes,
        exeModules,

        -- ** Parsing
        FieldDescr(..),
        LineNo,

        -- ** Sanity checking
        sanityCheckPackage,

        -- * Build information
        BuildInfo(..),
        emptyBuildInfo,
        allBuildInfo,
        unionBuildInfo,

        -- ** Supplementary build information
        HookedBuildInfo,
        emptyHookedBuildInfo,
        readHookedBuildInfo,
        parseHookedBuildInfo,
        writeHookedBuildInfo,
        showHookedBuildInfo,        
        updatePackageDescription,

        -- * Utilities
        satisfyDependency,
        ParseResult(..),
        hcOptions,
        autogenModuleName,
        haddockName,
        setupMessage,
        cabalVersion,
  ) where

import Data.Maybe  (isJust, maybeToList)
import Data.List   (nub, maximumBy)
import Data.Monoid (Monoid(..))
import Text.PrettyPrint.HughesPJ
import System.FilePath((<.>))

import Distribution.ParseUtils
import Distribution.Package   (PackageIdentifier(..), showPackageId)
import Distribution.Version   (Version(..), Dependency(..), withinRange)
import Distribution.Verbosity (Verbosity)
import Distribution.Compiler  (CompilerFlavor(..))
import Distribution.Simple.Utils (currentDir, notice)

import Distribution.PackageDescription.Types
import Distribution.PackageDescription.Parse
import Distribution.PackageDescription.QA (cabalVersion, sanityCheckPackage)
import Distribution.Configuration

-- -----------------------------------------------------------------------------
-- The PackageDescription type

-- XXX: I think we really want a PPrint or Pretty or ShowPretty class.
instance Show GenericPackageDescription where
    show (GenericPackageDescription pkg flgs mlib exes) =
        showPackageDescription pkg ++ "\n" ++
        (render $ vcat $ map ppFlag flgs) ++ "\n" ++
        render (maybe empty (\l -> showStanza "Library" (ppCondTree l showDeps)) mlib)
        ++ "\n" ++
        (render $ vcat $ 
            map (\(n,ct) -> showStanza ("Executable " ++ n) (ppCondTree ct showDeps)) exes)
      where
        ppFlag (MkFlag name desc dflt) =
            showStanza ("Flag " ++ name)
              ((if (null desc) then empty else 
                   text ("Description: " ++ desc)) $+$
              text ("Default: " ++ show dflt))
        showDeps = fsep . punctuate comma . map showDependency
        showStanza h b = text h <+> lbrace $+$ nest 2 b $+$ rbrace

data PDTagged = Lib Library | Exe String Executable | PDNull

instance Monoid PDTagged where
    mempty = PDNull
    PDNull `mappend` x = x
    x `mappend` PDNull = x
    Lib l `mappend` Lib l' = Lib (l `mappend` l')
    Exe n e `mappend` Exe n' e' | n == n' = Exe n (e `mappend` e')
    _ `mappend` _ = bug "Cannot combine incompatible tags"

finalizePackageDescription 
  :: [(String,Bool)]  -- ^ Explicitly specified flag assignments
  -> Maybe [PackageIdentifier] -- ^ Available dependencies. Pass 'Nothing' if this
                               -- is unknown.
  -> String -- ^ OS-name
  -> String -- ^ Arch-name
  -> (String, Version) -- ^ Compiler + Version
  -> GenericPackageDescription
  -> Either [Dependency]
            (PackageDescription, [(String,Bool)])
	     -- ^ Either missing dependencies or the resolved package
	     -- description along with the flag assignments chosen.
finalizePackageDescription userflags mpkgs os arch impl 
        (GenericPackageDescription pkg flags mlib0 exes0) =
    case resolveFlags of 
      Right ((mlib, exes'), deps, flagVals) ->
        Right ( pkg { library = mlib                            
                    , executables = exes'
                    , buildDepends = nub deps
                    }
              , flagVals )
      Left missing -> Left $ nub missing
  where
    -- Combine lib and exes into one list of @CondTree@s with tagged data
    condTrees = maybeToList (fmap (mapTreeData Lib) mlib0 )
                ++ map (\(name,tree) -> mapTreeData (Exe name) tree) exes0

    untagRslts = foldr untag (Nothing, [])
      where
        untag (Lib _) (Just _, _) = bug "Only one library expected"
        untag (Lib l) (Nothing, exes) = (Just l, exes)
        untag (Exe n e) (mlib, exes)
         | any ((== n) . fst) exes = bug "Exe with same name found"
         | otherwise = (mlib, exes ++ [(n, e)])
        untag PDNull x = x  -- actually this should not happen, but let's be liberal

    resolveFlags =
        case resolveWithFlags flagChoices os arch impl condTrees check of
          Right (as, ds, fs) ->
              let (mlib, exes) = untagRslts as in
              Right ( (fmap libFillInDefaults mlib,
                       map (\(n,e) -> (exeFillInDefaults e) { exeName = n }) exes),
                     ds, fs)
          Left missing      -> Left missing

    flagChoices  = map (\(MkFlag n _ d) -> (n, d2c n d)) flags
    d2c n b      = maybe [b, not b] (\x -> [x]) $ lookup n userflags
    --flagDefaults = map (\(n,x:_) -> (n,x)) flagChoices 
    check ds     = if all satisfyDep ds
                   then DepOk
                   else MissingDeps $ filter (not . satisfyDep) ds
    -- if we don't know which packages are present, we just accept any
    -- dependency
    satisfyDep   = maybe (const True) 
                         (\pkgs -> isJust . satisfyDependency pkgs) 
                         mpkgs


-- | Flatten a generic package description by ignoring all conditions and just
-- join the field descriptors into on package description.  Note, however,
-- that this may lead to inconsistent field values, since all values are
-- joined into one field, which may not be possible in the original package
-- description, due to the use of exclusive choices (if ... else ...).
--
-- XXX: One particularly tricky case is defaulting.  In the original package
-- description, e.g., the source dirctory might either be the default or a
-- certain, explicitly set path.  Since defaults are filled in only after the
-- package has been resolved and when no explicit value has been set, the
-- default path will be missing from the package description returned by this
-- function.
flattenPackageDescription :: GenericPackageDescription -> PackageDescription
flattenPackageDescription (GenericPackageDescription pkg _ mlib0 exes0) =
    pkg { library = mlib
        , executables = reverse exes
        , buildDepends = nub $ ldeps ++ reverse edeps
        }
  where
    (mlib, ldeps) = case mlib0 of
        Just lib -> let (l,ds) = ignoreConditions lib in 
                    (Just (libFillInDefaults l), ds)
        Nothing -> (Nothing, [])
    (exes, edeps) = foldr flattenExe ([],[]) exes0
    flattenExe (n, t) (es, ds) = 
        let (e, ds') = ignoreConditions t in
        ( (exeFillInDefaults $ e { exeName = n }) : es, ds' ++ ds ) 

-- This is in fact rather a hack.  The original version just overrode the
-- default values, however, when adding conditions we had to switch to a
-- modifier-based approach.  There, nothing is ever overwritten, but only
-- joined together.
--
-- This is the cleanest way i could think of, that doesn't require
-- changing all field parsing functions to return modifiers instead.
libFillInDefaults :: Library -> Library
libFillInDefaults lib@(Library { libBuildInfo = bi }) = 
    lib { libBuildInfo = biFillInDefaults bi }

exeFillInDefaults :: Executable -> Executable
exeFillInDefaults exe@(Executable { buildInfo = bi }) = 
    exe { buildInfo = biFillInDefaults bi }

biFillInDefaults :: BuildInfo -> BuildInfo
biFillInDefaults bi =
    if null (hsSourceDirs bi)
    then bi { hsSourceDirs = [currentDir] }
    else bi

-- ------------------------------------------------------------
-- * Utils
-- ------------------------------------------------------------

satisfyDependency :: [PackageIdentifier] -> Dependency
	-> Maybe PackageIdentifier
satisfyDependency pkgs (Dependency pkgname vrange) =
  case filter ok pkgs of
    [] -> Nothing 
    qs -> Just (maximumBy versions qs)
  where
	ok p = pkgName p == pkgname && pkgVersion p `withinRange` vrange
        versions a b = pkgVersion a `compare` pkgVersion b

-- |Select options for a particular Haskell compiler.
hcOptions :: CompilerFlavor -> [(CompilerFlavor, [String])] -> [String]
hcOptions hc hc_opts = [opt | (hc',opts) <- hc_opts, hc' == hc, opt <- opts]

-- |The name of the auto-generated module associated with a package
autogenModuleName :: PackageDescription -> String
autogenModuleName pkg_descr =
    "Paths_" ++ map fixchar (pkgName (package pkg_descr))
  where fixchar '-' = '_'
        fixchar c   = c

haddockName :: PackageDescription -> FilePath
haddockName pkg_descr = pkgName (package pkg_descr) <.> "haddock"

setupMessage :: Verbosity -> String -> PackageDescription -> IO ()
setupMessage verbosity msg pkg_descr =
    notice verbosity (msg ++ ' ':showPackageId (package pkg_descr) ++ "...")

bug :: String -> a
bug msg = error $ msg ++ ". Consider this a bug."
