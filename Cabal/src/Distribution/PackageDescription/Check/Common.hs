-- |
-- Module      :  Distribution.PackageDescription.Check.Common
-- Copyright   :  Francesco Ariis 2022
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Common types/functions to various check modules which are *no* part of
-- Distribution.PackageDescription.Check.Monad.
module Distribution.PackageDescription.Check.Common
  ( AssocDep
  , CabalField
  , PathKind (..)
  , checkCustomField
  , partitionDeps
  , checkPVP
  , checkPVPs
  , checkDependencyVersionRange
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Compat.NonEmptySet (toNonEmpty)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Check.Monad
import Distribution.Utils.Generic (isAscii)
import Distribution.Version

import Control.Monad

-- Type of FilePath.
data PathKind
  = PathKindFile
  | PathKindDirectory
  | PathKindGlob
  deriving (Eq)

-- | .cabal field we are referring to. As now it is just a synonym to help
-- reading the code, in the future it might take advantage of typification
-- in Cabal-syntax.
type CabalField = String

checkCustomField :: Monad m => (String, String) -> CheckM m ()
checkCustomField (n, _) =
  checkP
    (any (not . isAscii) n)
    (PackageDistInexcusable $ NonASCIICustomField [n])

-- ------------------------------------------------------------
-- PVP types/functions
-- ------------------------------------------------------------

-- A library name / dependencies association list. Ultimately to be
-- fed to PVP check.
type AssocDep = (UnqualComponentName, [Dependency])

-- Convenience function to partition important dependencies by name. To
-- be used together with checkPVP. Important: usually “base” or “Cabal”,
-- as the error is slightly different.
-- Note that `partitionDeps` will also filter out dependencies which are
-- already present in a inherited fashion (e.g. an exe which imports the
-- main library will not need to specify upper bounds on shared dependencies,
-- hence we do not return those).
--
partitionDeps
  :: Monad m
  => [AssocDep] -- Possibly inherited dependencies, i.e.
  -- dependencies from internal/main libs.
  -> [UnqualComponentName] -- List of package names ("base", "Cabal"…)
  -> [Dependency] -- Dependencies to check.
  -> CheckM m ([Dependency], [Dependency])
partitionDeps ads ns ds = do
  -- Shared dependencies from “intra .cabal” libraries.
  let
    -- names of our dependencies
    dqs = map unqualName ds
    -- shared targets that match
    fads = filter (flip elem dqs . fst) ads
    -- the names of such targets
    inName = nub $ map fst fads :: [UnqualComponentName]
    -- the dependencies of such targets
    inDep = concatMap snd fads :: [Dependency]

  -- We exclude from checks:
  -- 1. dependencies which are shared with main library / a
  --    sublibrary; and of course
  -- 2. the names of main library / sub libraries themselves.
  --
  -- So in myPackage.cabal
  -- library
  --      build-depends: text < 5
  -- ⁝
  --      build-depends: myPackage,        ← no warning, internal
  --                     text,             ← no warning, inherited
  --                     monadacme         ← warning!
  let fFun d =
        notElem (unqualName d) inName
          && notElem
            (unqualName d)
            (map unqualName inDep)
      ds' = filter fFun ds

  return $ partition (flip elem ns . unqualName) ds'
  where
    -- Return *sublibrary* name if exists (internal),
    -- otherwise package name.
    unqualName :: Dependency -> UnqualComponentName
    unqualName (Dependency n _ nel) =
      case head (toNonEmpty nel) of
        (LSubLibName ln) -> ln
        _ -> packageNameToUnqualComponentName n

-- PVP dependency check (one warning message per dependency, usually
-- for important dependencies like base).
checkPVP
  :: Monad m
  => (Dependency -> Bool)
  -> (String -> PackageCheck) -- Warn message depends on name
  -- (e.g. "base", "Cabal").
  -> [Dependency]
  -> CheckM m ()
checkPVP p ckf ds = do
  let ods = filter p ds
  mapM_ (tellP . ckf . unPackageName . depPkgName) ods

-- PVP dependency check for a list of dependencies. Some code duplication
-- is sadly needed to provide more ergonimic error messages.
checkPVPs
  :: Monad m
  => (Dependency -> Bool)
  -> ( [String]
       -> PackageCheck -- Grouped error message, depends on a
       -- set of names.
     )
  -> [Dependency] -- Deps to analyse.
  -> CheckM m ()
checkPVPs p cf ds
  | null ns = return ()
  | otherwise = tellP (cf ns)
  where
    ods = filter p ds
    ns = map (unPackageName . depPkgName) ods

checkDependencyVersionRange :: (VersionRange -> Bool) -> Dependency -> Bool
checkDependencyVersionRange p (Dependency _ ver _) = p ver
