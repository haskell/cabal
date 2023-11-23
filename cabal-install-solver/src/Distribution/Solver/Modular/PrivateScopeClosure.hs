{-# LANGUAGE TupleSections #-}
module Distribution.Solver.Modular.PrivateScopeClosure where

import Control.Exception (assert)
import Prelude hiding (cycle)
import qualified Data.Map as M

import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.Tree
import qualified Distribution.Solver.Modular.ConflictSet as CS
import Distribution.Solver.Types.PackagePath

-- | Find and reject any nodes that would violate the private-dependencies
-- closure property, which states that all packages within the closure of a
-- private scope must also be included in the private scope.
detectInvalidPrivateScopesPhase :: Tree d c -> Tree d c
detectInvalidPrivateScopesPhase = go
  where
    -- Similar to detectCyclesPhase, maybe we could deduplicate
    go :: Tree d c -> Tree d c
    go (PChoice qpn rdm gr                         cs) =
        PChoice qpn rdm gr     $ fmap (checkChild qpn)   (fmap go cs)
    go (FChoice qfn@(FN qpn _) rdm gr w m d cs) =
        FChoice qfn rdm gr w m d $ fmap (checkChild qpn) (fmap go cs)
    go (SChoice qsn@(SN qpn _) rdm gr w     cs) =
        SChoice qsn rdm gr w   $ fmap (checkChild qpn)   (fmap go cs)
    go (GoalChoice rdm cs) = GoalChoice rdm (fmap go cs)
    go x@(Fail _ _) = x
    go x@(Done _ _) = x

    checkChild :: QPN -> Tree d c -> Tree d c
    checkChild qpn x@(PChoice _  rdm _       _) = failIfBadClosure qpn rdm x
    checkChild qpn x@(FChoice _  rdm _ _ _ _ _) = failIfBadClosure qpn rdm x
    checkChild qpn x@(SChoice _  rdm _ _     _) = failIfBadClosure qpn rdm x
    checkChild qpn x@(GoalChoice rdm         _) = failIfBadClosure qpn rdm x
    checkChild _   x@(Fail _ _)                 = x
    checkChild qpn x@(Done       rdm _)         = failIfBadClosure qpn rdm x

    failIfBadClosure :: QPN -> RevDepMap -> Tree d c -> Tree d c
    -- An already qualified package can't violate the closure property
    failIfBadClosure (Q (PackagePath _ (QualAlias _ _ _)) _) _ x = x
    failIfBadClosure qpn rdm x =
      case findBadClosures qpn rdm of
        Nothing     -> x
        Just (relSet, qual) -> Fail relSet (InvalidPrivateScope qual)

-- | Given the reverse dependency map from a node in the tree, check if the
-- solution has any bad closures. If it is, return the conflict set containing
-- the variables violating private deps closures.
findBadClosures :: QPN -> RevDepMap -> Maybe (ConflictSet, Qualifier)
findBadClosures pkg rdm =
  case concatMap (\root@(Q (PackagePath _ ps) _) -> (root,) <$> concatMap (step ps False . snd) (findRevDepsTopLevel root)) roots of
    (closureBegin@(Q (PackagePath _ ql) _), closureEnd@(Q (PackagePath _ ql') _)):_ ->
      assert (ql == ql') $
      return (CS.singletonWithConflict (P pkg) (CS.PrivateScopeClosureConflict closureBegin closureEnd), ql)
    [] -> Nothing
  where

    -- Roots of the rev dep map with QualAlias/private scope
    roots :: [QPN]
    roots = flip M.foldMapWithKey rdm $
      \key _ -> case key of
        Q (PackagePath _ (QualAlias _ _ _)) _ -> [key]
        _ -> []

    -- Traverse up from a root until a reverse dep in the same private scope is
    -- found. We only traverse up until we find another private dep in the same
    -- scope because that is sufficient to complete a "local closure", and
    -- because we traverse from all root deps in private scopes, we will
    -- traverse all the "local" closures thus the full closure of each scope... REWRITE and RENAME
    step :: Qualifier -- ^ This root's qualifier/private scope
         -> Bool -- ^ Have we found the "goal" package in the "local" closure
         -> QPN -- ^ Next package in the closure traversal
         -> [QPN]
         -- ^ The terminal nodes for each closure violated by this package.
         -- Empty if the closure property is kept.
    step rootQual hasFoundGoal next
      -- We stop at any qualified reverse dep, even if it does not belong to
      -- the same scope as the one we are checking for the closure property.
      -- By case analysis:
      --  * If it is the same scope, we've reached the end of the local
      --  closure, and if the package has been seen as non-qualified then the
      --  property is violated
      --
      --  * If it is not the same scope, that means "next" in that branch is a
      --  dep of a private scope goal, but it may not violate the closure
      --  property for that one. Even if it were to violate the property
      --  outside of a nested private scope, it doesn't matter because within a
      --  (nested) private scope it just has to be consistent in
      --  itself.........
      | Q (PackagePath _ ps@(QualAlias _ _ _)) _ <- next
      = if ps == rootQual && hasFoundGoal
           then [next]
           else []
      | otherwise
      = case findRevDepsTopLevel next of
        -- If there are no more deps (meaning we didn't stop at any rev-dep in
        -- a private scope), then we don't have a private scope closure and the
        -- property is preserved.
        [] -> []
        -- Step through all the next reverse deps, failing (by adding terminal
        -- nodes to the result) if any of the steps violates the closure
        -- property
        xs ->
          -- If the next pkg is our goal, we recurse with "hasFoundGoal =
          -- True", otherwise with what we had previously
          let hasFoundGoal' = next == pkg || hasFoundGoal
           in concatMap (step rootQual hasFoundGoal' . snd) xs

    -- Find the reverse dependencies of this QPN, but in the top-level scope.
    -- When constructing the closure, starting from a qualified root, we need
    -- to take into account that the dependencies introduced by the
    -- private-scoped-depends will be in the top level scope...
    findRevDepsTopLevel qpn@(Q (PackagePath namespace _) pn) =
      case (Q (PackagePath namespace QualToplevel) pn) `M.lookup` rdm of
        Nothing ->
          -- This means the package we are looking up in the map has only been
          -- introduced qualified, not at the QualToplevel. This means we look
          -- it up as is.
          case qpn `M.lookup` rdm of
            Nothing -> findError "cannot find node"
            Just rdeps -> rdeps
        Just rdeps -> rdeps

    findError = error . ("Distribution.Solver.Modular.PrivateScopeClosure.findBadClosures: " ++)
