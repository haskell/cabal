-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.IndexUtils
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Extra utils related to the package indexes.
-----------------------------------------------------------------------------
module Hackage.IndexUtils (
  disambiguatePackageName,
  disambiguateDependencies
  ) where

import qualified Hackage.RepoIndex as RepoIndex
import Hackage.RepoIndex (RepoIndex)
import Hackage.Types (UnresolvedDependency(..), PkgInfo(..))
import Hackage.Utils (intercalate)

import Distribution.Package (PackageIdentifier(..))
import Distribution.Version (Dependency(Dependency))
import Distribution.Simple.Utils as Utils (die)


-- | Disambiguate a set of packages using 'disambiguatePackage' and report any
-- ambiguities to the user.
--
disambiguateDependencies :: RepoIndex
                         -> [UnresolvedDependency]
                         -> IO [UnresolvedDependency]
disambiguateDependencies index deps = do
  let names = [ (name, disambiguatePackageName index name)
              | UnresolvedDependency (Dependency name _) _ <- deps ]
   in case [ (name, matches) | (name, Right matches) <- names ] of
        []        -> return
          [ UnresolvedDependency (Dependency name vrange) flags
          | (UnresolvedDependency (Dependency _ vrange) flags,
             (_, Left name)) <- zip deps names ]
        ambigious -> die $ unlines
          [ if null matches
              then "There is no package named " ++ name
              else "The package name " ++ name ++ "is ambigious. "
                ++ "It could be: " ++ intercalate ", " matches
          | (name, matches) <- ambigious ]

-- | Given an index of known packages and a package name, figure out which one it
-- might be referring to. If there is an exact case-sensitive match then that's
-- ok. If it matches just one package case-insensitively then that's also ok.
-- The only problem is if it matches multiple packages case-insensitively, in
-- that case it is ambigious.
--
disambiguatePackageName :: RepoIndex -> String
                                     -> Either String [String]
disambiguatePackageName index name =
    case RepoIndex.lookupPackageName index name of
      RepoIndex.None              -> Right []
      RepoIndex.Unambiguous pkgs  -> Left (pkgName (pkgInfoId (head pkgs)))
      RepoIndex.Ambiguous   pkgss -> Right [ pkgName (pkgInfoId pkg)
                                           | (pkg:_) <- pkgss ]
