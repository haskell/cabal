{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.Dependency.Pretty
  ( Dependency (..)
  , mkDependency
  , depPkgName
  , depVerRange
  , depLibraries
  , simplifyDependency
  , mainLibSet
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Types.Dependency.Internal
import Distribution.Types.VersionRange (isAnyVersionLight)
import Distribution.Version (VersionRange, anyVersion, simplifyVersionRange)

import Distribution.CabalSpecVersion
import Distribution.Compat.CharParsing (char, spaces)
import Distribution.Compat.Parsing (between, option)
import Distribution.Pretty
import Distribution.Types.LibraryName.Internal
import Distribution.Types.LibraryName.Pretty
import Distribution.Types.PackageName.Internal
import Distribution.Types.PackageName.Pretty
import Distribution.Types.VersionRange.Pretty
import Distribution.Types.UnqualComponentName.Internal

import Distribution.Types.AnnotationNamespace
import Distribution.Types.AnnotationTrivium

import qualified Data.Map as M
import qualified Distribution.Compat.NonEmptySet as NES
import qualified Text.PrettyPrint as PP

-- |
--
-- >>> prettyShow $ Dependency (mkPackageName "pkg") anyVersion mainLibSet
-- "pkg"
--
-- >>> prettyShow $ Dependency (mkPackageName "pkg") anyVersion $ NES.insert (LSubLibName $ mkUnqualComponentName "sublib") mainLibSet
-- "pkg:{pkg,sublib}"
--
-- >>> prettyShow $ Dependency (mkPackageName "pkg") anyVersion $ NES.singleton (LSubLibName $ mkUnqualComponentName "sublib")
-- "pkg:sublib"
--
-- >>> prettyShow $ Dependency (mkPackageName "pkg") anyVersion $ NES.insert (LSubLibName $ mkUnqualComponentName "sublib-b") $ NES.singleton (LSubLibName $ mkUnqualComponentName "sublib-a")
-- "pkg:{sublib-a,sublib-b}"
instance Pretty Dependency where
  prettier t0 dep0@(Dependency name ver sublibs) =
    let isHere :: Namespace -> Bool
        isHere (NSDependency dep Nothing) | dep == dep0 = True
        isHere _ = True

        projectBelow :: Namespace -> [Namespace]
        projectBelow (NSDependency dep (Just s)) | dep == dep0 = [s]
        projectBelow s = []

      -- NOTE(leana8959): Here, we can see how the mapping method breaks down
      -- We don't have a filterMap
      -- The Nothing values are supposed to mean what's in this scope, but they will be
      -- mixed up with what are not in this scope which we want to discard.
      --
      -- Maybe come up with a better representation in the future.

        here = [ v | (k, v) <- M.toList t0, isHere k ]
        below = M.fromList [ (k', v) | (k, v) <- M.toList t0, k' <- projectBelow k ]

        !() = trace ("Printed from \"instance Pretty Dependency\", \"here\"\n" <> show here) ()
        !() = trace ("Printed from \"instance Pretty Dependency\", \"below\"\n" <> show below) ()
    in  prettierLibraryNames below name (NES.toNonEmpty sublibs) <+> pver
    where
      -- TODO: change to isAnyVersion after #6736
      pver
        | isAnyVersionLight ver = PP.empty
        | otherwise = pretty ver
