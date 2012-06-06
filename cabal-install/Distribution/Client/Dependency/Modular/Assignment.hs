module Distribution.Client.Dependency.Modular.Assignment where

import Control.Applicative
import Control.Monad
import Data.Array as A
import Data.List as L
import Data.Map as M
import Data.Maybe
import Data.Graph
import Prelude hiding (pi)

import Distribution.PackageDescription (FlagAssignment) -- from Cabal
import Distribution.Client.Types (OptionalStanza)

import Distribution.Client.Dependency.Modular.Configured
import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Index
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.Version

-- | A (partial) package assignment. Qualified package names
-- are associated with instances.
type PAssignment    = Map QPN I

-- | A (partial) package preassignment. Qualified package names
-- are associated with constrained instances. Constrained instances
-- record constraints about the instances that can still be chosen,
-- and in the extreme case fix a concrete instance.
type PPreAssignment = Map QPN (CI QPN)
type FAssignment    = Map QFN Bool
type SAssignment    = Map QSN Bool

-- | A (partial) assignment of variables.
data Assignment = A PAssignment FAssignment SAssignment
  deriving (Show, Eq)

-- | A preassignment comprises knowledge about variables, but not
-- necessarily fixed values.
data PreAssignment = PA PPreAssignment FAssignment SAssignment

-- | Extend a package preassignment.
--
-- Takes the variable that causes the new constraints, a current preassignment
-- and a set of new dependency constraints.
--
-- We're trying to extend the preassignment with each dependency one by one.
-- Each dependency is for a particular variable. We check if we already have
-- constraints for that variable in the current preassignment. If so, we're
-- trying to merge the constraints.
--
-- Either returns a witness of the conflict that would arise during the merge,
-- or the successfully extended assignment.
extend :: Var QPN -> PPreAssignment -> [Dep QPN] -> Either (ConflictSet QPN, [Dep QPN]) PPreAssignment
extend var pa qa = foldM (\ a (Dep qpn ci) ->
                     let ci' = M.findWithDefault (Constrained []) qpn a
                     in  case (\ x -> M.insert qpn x a) <$> merge ci' ci of
                           Left (c, (d, d')) -> Left  (c, L.map (Dep qpn) (simplify (P qpn) d d'))
                           Right x           -> Right x)
                    pa qa
  where
    -- We're trying to remove trivial elements of the conflict. If we're just
    -- making a choice pkg == instance, and pkg => pkg == instance is a part
    -- of the conflict, then this info is clear from the context and does not
    -- have to be repeated.
    simplify v (Fixed _ (Goal var' _)) c | v == var && var' == var = [c]
    simplify v c (Fixed _ (Goal var' _)) | v == var && var' == var = [c]
    simplify _ c                       d                           = [c, d]

-- | Delivers an ordered list of fully configured packages.
--
-- TODO: This function is (sort of) ok. However, there's an open bug
-- w.r.t. unqualification. There might be several different instances
-- of one package version chosen by the solver, which will lead to
-- clashes.
toCPs :: Assignment -> RevDepMap -> [CP QPN]
toCPs (A pa fa sa) rdm =
  let
    -- get hold of the graph
    g   :: Graph
    vm  :: Vertex -> ((), QPN, [QPN])
    cvm :: QPN -> Maybe Vertex
    -- Note that the RevDepMap contains duplicate dependencies. Therefore the nub.
    (g, vm, cvm) = graphFromEdges (L.map (\ (x, xs) -> ((), x, nub xs))
                                  (M.toList rdm))
    tg :: Graph
    tg = transposeG g
    -- Topsort the dependency graph, yielding a list of pkgs in the right order.
    -- The graph will still contain all the installed packages, and it might
    -- contain duplicates, because several variables might actually resolve to
    -- the same package in the presence of qualified package names.
    ps :: [PI QPN]
    ps = L.map ((\ (_, x, _) -> PI x (pa M.! x)) . vm) $
         topSort g
    -- Determine the flags per package, by walking over and regrouping the
    -- complete flag assignment by package.
    fapp :: Map QPN FlagAssignment
    fapp = M.fromListWith (++) $
           L.map (\ ((FN (PI qpn _) fn), b) -> (qpn, [(fn, b)])) $
           M.toList $
           fa
    -- Stanzas per package.
    sapp :: Map QPN [OptionalStanza]
    sapp = M.fromListWith (++) $
           L.map (\ ((SN (PI qpn _) sn), b) -> (qpn, if b then [sn] else [])) $
           M.toList $
           sa
    -- Dependencies per package.
    depp :: QPN -> [PI QPN]
    depp qpn = let v :: Vertex
                   v   = fromJust (cvm qpn)
                   dvs :: [Vertex]
                   dvs = tg A.! v
               in L.map (\ dv -> case vm dv of (_, x, _) -> PI x (pa M.! x)) dvs
  in
    L.map (\ pi@(PI qpn _) -> CP pi
                                 (M.findWithDefault [] qpn fapp)
                                 (M.findWithDefault [] qpn sapp)
                                 (depp qpn))
          ps

-- | Finalize an assignment and a reverse dependency map.
--
-- This is preliminary, and geared towards output right now.
finalize :: Index -> Assignment -> RevDepMap -> IO ()
finalize idx (A pa fa _) rdm =
  let
    -- get hold of the graph
    g  :: Graph
    vm :: Vertex -> ((), QPN, [QPN])
    (g, vm) = graphFromEdges' (L.map (\ (x, xs) -> ((), x, xs)) (M.toList rdm))
    -- topsort the dependency graph, yielding a list of pkgs in the right order
    f :: [PI QPN]
    f = L.filter (not . instPI) (L.map ((\ (_, x, _) -> PI x (pa M.! x)) . vm) (topSort g))
    fapp :: Map QPN [(QFN, Bool)] -- flags per package
    fapp = M.fromListWith (++) $
           L.map (\ (qfn@(FN (PI qpn _) _), b) -> (qpn, [(qfn, b)])) $ M.toList $ fa
    -- print one instance
    ppi pi@(PI qpn _) = showPI pi ++ status pi ++ " " ++ pflags (M.findWithDefault [] qpn fapp)
    -- print install status
    status :: PI QPN -> String
    status (PI (Q _ pn) _) =
      case insts of
        [] -> " (new)"
        vs -> " (" ++ intercalate ", " (L.map showVer vs) ++ ")"
      where insts = L.map (\ (I v _) -> v) $ L.filter isInstalled $
                    M.keys (M.findWithDefault M.empty pn idx)
            isInstalled (I _ (Inst _ )) = True
            isInstalled _               = False
    -- print flag assignment
    pflags = unwords . L.map (uncurry showFBool)
  in
    -- show packages with associated flag assignments
    putStr (unlines (L.map ppi f))
