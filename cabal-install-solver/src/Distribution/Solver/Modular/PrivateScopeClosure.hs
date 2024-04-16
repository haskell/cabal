{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Distribution.Solver.Modular.PrivateScopeClosure where

import Data.Maybe
import qualified Data.Set as S
import Prelude hiding (cycle)
import qualified Data.Map as M

import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Tree
import qualified Distribution.Solver.Modular.ConflictSet as CS
import Distribution.Solver.Types.PackagePath

-- | Given the reverse dependency map from a node in the tree, check if the
-- solution has any bad closures. If it does, return the conflict set containing
-- the variables violating private deps closures.
--
-- INVARIANT: The RevDepMap forms an acyclic graph (guaranteed by this check running after 'findCycles')
findBadPrivClosures :: QPN
                    -- ^ Newly added package that I need to check if violates property
                    -> RevDepMap
                    -> Maybe (ConflictSet, FailReason)
{-
Note [The Private Scope Closure Property Check]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
In practice, for every package that is added to the solution (the 'QPN'), we ask:
Does choosing this package violate the closure property?
By case analysis:

 (a) If the package X introduced is in a private scope S, then S may now
   violate the closure property. To determine so,
     * We take the list of packages in that same private scope S[1] (the
     so-called "roots" of the check)

     * And compute, for each root, the up-to-private reverse-dependency
     closure[2] of that package X qualified with QualTopLevel instead of the
     private scope[3].

     * If we were able to reach the private scope S from the "special closure" of the
     top-level-qualified roots, the closure property is violated!

 (b) If the package X introduced is NOT in a private scope, we need to
 determine all packages Ys in private scopes PSs that depend on this package X.
   * We do so by traversing upwards from X, ie computing the up-to-private
   reverse-dependency closure[2] of X.

   * For every private scope PS reached from traversing upwards from X
   (possibleBadPrivateScopes), we get the list of packages in that private
   scope PS (see [1] again), the "roots" of this branch of the check.

   * For every package (aka root) in that private scope, we traverse upwards
   from the top-level-qualified root[3] until we find another private scope
   or until we find the "original" package X from which we originally
   computed the reachable roots.

   This is similar to up-to-private reverse-dependency closure[2] of the
   root but we also stop traversing upwards if we find the package X.

   * If the package X is reachable from any of the private scopes' roots
   then the property is violated. That is, if we can reach PS from the
   (non-private) X, and one of the roots of PS can reach X too, then X sits
   unqualified between two packages in the private scope (violating the
   closure property)

Additionally, when the private closure property is violated for some private
qualifier Q, we compute the packages missing from the private scope for it
to be valid (see 'computeBadPkgs').

[1] We associate private scopes to the set of packages contained in it in a
cache in the 'RevDepMap' (the 'privScopes' field). We insert into this
mapping around the same time a package is introduced in the reverse
dependency map, if the package has a private scope.

This cache is needed for the 'findBadPrivClosures' check to be fast, because
we always need to find the set of packages in a given private scope to perform the check.

[2] The up-to-private reverse-dependency closure of X is the
reverse-dependency closure of X, constructed by traversing upwards the
reverse dependency map, with the twist of stopping at any
privately-qualified reverse dependency.

The function which computes this 'upwardsClosure' local to 'findBadPrivClosures'.
When the first argument is 'True', we additionally stop when the package X is reached.

We use this "special closure" because dependencies introduced by packages in
a private scope can only violate the property of that scope (it is
impossible for them to violate the property of scopes further upward)

[3] When we traverse upwards from a privately-qualified root, we want to
first unqualify the root and search the unqualified space. The local function
'findRevDepsTopLevel' explains in more detail with an example.

-}
findBadPrivClosures pkg rdm = do
  -- Maybe a bad scope and the reachable-from-roots map
  let badScopeAndPaths =
        case pkg of
          Q (PackagePath _ ql@QualAlias{}) _ -> do
            -- (a) case
            let
              roots = pkgsInPrivScope ql
              reachableTopLevel = upwardsClosure False (concatMap findRevDepsTopLevel roots)
              isViolated =
                -- From the top level roots there can be no package in the same private scope reachable
                any ((== ql) . getQual) (M.keys reachableTopLevel)
             in
              if isViolated then Just (ql, reachableTopLevel) else Nothing

          Q (PackagePath _ _) _ -> do
            -- (b) case
            let
              upClosureFromPkg = upwardsClosure False [pkg]
              roots =
                -- Filter out from the scope packages which were reached from `pkg`:
                S.filter (not . (`M.member` upClosureFromPkg)) $
                M.foldrWithKey (\p _ acc ->
                  if hasPrivateQual p -- privately-qualified package reached from `pkg`
                     then (pkgsInPrivScope (getQual p)) <> acc
                     else acc) S.empty upClosureFromPkg
              reachableTopLevel =
                S.map (\x -> (getQual x, upwardsClosure True $ findRevDepsTopLevel x)) roots
              invalidScopes =
                -- For any of the qualifiers, if the package is reachable from
                -- the roots the prv scope of that root is bad
                S.filter (any (== pkg) . M.keys . snd) reachableTopLevel
             in
              case S.toList invalidScopes of
                -- We only report one bad scope. Eventually we could report more.
                (ql,rbl):_ -> Just (ql, M.unionWith (<>) rbl upClosureFromPkg {- report with the remaining half of the closure too -})
                [] -> Nothing


  (badScope, paths) <- badScopeAndPaths
  -- All packages in paths from roots to terminal nodes in the same private scope
  let badPkgs = concat . M.elems . M.filterWithKey (\k _ -> badScope == getQual k) $ paths
  return (CS.fromList $ map P badPkgs, InvalidPrivateScope badScope)


  where

    pkgsInPrivScope :: Qualifier -> S.Set QPN
    pkgsInPrivScope ql' = fromMaybe (error "all private scopes should be cached") $ M.lookup ql' (privScopes rdm)

    -- Find the reverse dependencies of this QPN, but in the top-level scope.
    --
    -- Recall that the private closure property may only be violated by pkgA
    -- depending privately on pkgB and pkgD, but not on pkgC, when
    --    pkgB --top-level-depends-> pkgC
    --    pkgC --top-level-depends-> pkgD
    --
    -- In the reverse dependency map we'd have
    --    pkgD(private) --rd-> pkgA(top)
    --    pkgB(private) --rd-> pkgA(top)
    --    pkgC(top)     --rd-> pkgB(private)
    --    pkgD(top)     --rd-> pkgC(top)
    --
    -- So, for each pkg with a private scope (like pkgD or pkgB), we look for
    -- the top-level equivalent of that package (this function), and will
    -- traverse up until we find (or not) that there is a reverse dep on a
    -- package in the same private scope as the root from which we started.
    findRevDepsTopLevel (Q (PackagePath namespace _) pn) =
      case (Q (PackagePath namespace QualToplevel) pn) `M.lookup` revDeps rdm of
        Nothing ->
          -- If this package wasn't at the top-level means there is no
          -- top-level package publicly depending on it. Nothing to worry about then.
          []
        Just rdeps -> map snd rdeps

    getQual (Q (PackagePath _ ql) _) = ql

    hasPrivateQual :: QPN -> Bool
    hasPrivateQual = \case
      Q (PackagePath _ QualAlias{}) _ -> True
      _ -> False

    upwardsClosure :: Bool {- Whether to stop traversing when `pkg` is found -}
                   -> [QPN] {- ps roots in qualtoplevel -}
                   -> M.Map QPN [QPN] {- all the things you can reach (stopping at pkg and on any private thing); only lookup privately qualified packages!-}
    upwardsClosure stopAtPkg = fst . foldl go mempty
      where
        go :: (M.Map QPN [QPN] {- de-duplicate things we've seen -}, [QPN])
           -> QPN -> (M.Map QPN [QPN], [QPN])
        go (s,path) x
          | x `M.member` s
          = (s, path)
          | hasPrivateQual x
          = (M.insert x path s, path)
          | stopAtPkg && x == pkg
          = (M.insert x (dontLook x) s, path)
          | otherwise
          = foldl go (M.insert x (dontLook x) s, x:path) $ neighbors x

        dontLook x = error $ "We should only lookup privately-qualified pkgs, but instead " ++ show x ++ " was looked up -- it is only inserted in the map for de-duplication purposes."

    neighbors :: QPN -> [QPN]
    neighbors x = case x `M.lookup` revDeps rdm of
                    Nothing -> error $ "cannot find node: " ++ show x
                    Just xs -> map snd xs

