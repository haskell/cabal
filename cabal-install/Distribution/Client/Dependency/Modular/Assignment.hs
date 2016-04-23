module Distribution.Client.Dependency.Modular.Assignment
    ( Assignment(..)
    , FAssignment
    , SAssignment
    , PreAssignment(..)
    , extend
    , toCPs
    ) where

import Control.Applicative
import Control.Monad
import Data.Array as A
import Data.List as L
import Data.Map as M
import Data.Maybe
import Prelude hiding (pi)

import Language.Haskell.Extension (Extension, Language)

import Distribution.PackageDescription (FlagAssignment) -- from Cabal
import Distribution.Client.Types (OptionalStanza)
import Distribution.Client.Utils.LabeledGraph
import Distribution.Client.ComponentDeps (ComponentDeps, Component)
import qualified Distribution.Client.ComponentDeps as CD

import Distribution.Client.Dependency.Modular.Configured
import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Flag
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
extend :: (Extension -> Bool) -- ^ is a given extension supported
       -> (Language  -> Bool) -- ^ is a given language supported
       -> (PN -> VR  -> Bool) -- ^ is a given pkg-config requirement satisfiable
       -> Var QPN
       -> PPreAssignment -> [Dep QPN] -> Either (ConflictSet QPN, [Dep QPN]) PPreAssignment
extend extSupported langSupported pkgPresent var = foldM extendSingle
  where

    extendSingle :: PPreAssignment -> Dep QPN
                 -> Either (ConflictSet QPN, [Dep QPN]) PPreAssignment
    extendSingle a (Ext  ext )  =
      if extSupported  ext  then Right a
                            else Left (varToConflictSet var, [Ext ext])
    extendSingle a (Lang lang)  =
      if langSupported lang then Right a
                            else Left (varToConflictSet var, [Lang lang])
    extendSingle a (Pkg pn vr)  =
      if pkgPresent pn vr then Right a
                          else Left (varToConflictSet var, [Pkg pn vr])
    extendSingle a (Dep qpn ci) =
      let ci' = M.findWithDefault (Constrained []) qpn a
      in  case (\ x -> M.insert qpn x a) <$> merge ci' ci of
            Left (c, (d, d')) -> Left  (c, L.map (Dep qpn) (simplify (P qpn) d d'))
            Right x           -> Right x

    -- We're trying to remove trivial elements of the conflict. If we're just
    -- making a choice pkg == instance, and pkg => pkg == instance is a part
    -- of the conflict, then this info is clear from the context and does not
    -- have to be repeated.
    simplify v (Fixed _ var') c | v == var && var' == var = [c]
    simplify v c (Fixed _ var') | v == var && var' == var = [c]
    simplify _ c              d                           = [c, d]

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
    g   :: Graph Component
    vm  :: Vertex -> ((), QPN, [(Component, QPN)])
    cvm :: QPN -> Maybe Vertex
    -- Note that the RevDepMap contains duplicate dependencies. Therefore the nub.
    (g, vm, cvm) = graphFromEdges (L.map (\ (x, xs) -> ((), x, nub xs))
                                  (M.toList rdm))
    tg :: Graph Component
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
    depp :: QPN -> [(Component, PI QPN)]
    depp qpn = let v :: Vertex
                   v   = fromJust (cvm qpn)
                   dvs :: [(Component, Vertex)]
                   dvs = tg A.! v
               in L.map (\ (comp, dv) -> case vm dv of (_, x, _) -> (comp, PI x (pa M.! x))) dvs
    -- Translated to PackageDeps
    depp' :: QPN -> ComponentDeps [PI QPN]
    depp' = CD.fromList . L.map (\(comp, d) -> (comp, [d])) . depp
  in
    L.map (\ pi@(PI qpn _) -> CP pi
                                 (M.findWithDefault [] qpn fapp)
                                 (M.findWithDefault [] qpn sapp)
                                 (depp' qpn))
          ps
