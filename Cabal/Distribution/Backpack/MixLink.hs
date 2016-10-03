-- | See <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>
module Distribution.Backpack.MixLink (
    mixLink,
) where

import Prelude ()
import Distribution.Compat.Prelude hiding (mod)

import Distribution.Backpack.UnifyM
import Distribution.Backpack.FullUnitId

import qualified Distribution.Utils.UnionFind as UnionFind
import Distribution.ModuleName
import Distribution.Text
import Distribution.Types.IncludeRenaming
import Distribution.Package

import Control.Monad
import qualified Data.Map as Map
import qualified Data.Foldable as F

-----------------------------------------------------------------------
-- Linking

-- | Given to scopes of provisions and requirements, link them together.
mixLink :: ModuleScopeU s -> ModuleScopeU s -> UnifyM s (ModuleScopeU s)
mixLink (provs1, reqs1) (provs2, reqs2) = do
    F.sequenceA_ (Map.intersectionWithKey linkProvision provs1 reqs2)
    F.sequenceA_ (Map.intersectionWithKey linkProvision provs2 reqs1)
    -- TODO: would be more efficient to collapse provision lists when we
    -- unify them.
    return (Map.unionWith (++) provs1 provs2,
            -- NB: NOT the difference of the unions.  That implies
            -- self-unification not allowed.  (But maybe requirement prov is disjoint
            -- from reqs makes this a moot point?)
            Map.union (Map.difference reqs1 provs2)
                      (Map.difference reqs2 provs1))

displaySource :: ModuleSourceU s -> String
displaySource src
 | isDefaultIncludeRenaming (usrc_renaming src)
 = display (usrc_pkgname src)
 | otherwise
 = display (usrc_pkgname src) ++ " with renaming " ++ display (usrc_renaming src)

-- | Link a list of possibly provided modules to a single
-- requirement.  This applies a side-condition that all
-- of the provided modules at the same name are *actually*
-- the same module.
linkProvision :: ModuleName -> [ModuleSourceU s] -> ModuleU s
              -> UnifyM s [ModuleSourceU s]
linkProvision _ [] _reqs = error "linkProvision"
linkProvision mod_name ret@(prov:provs) req = do
    forM_ provs $ \prov' -> do
        let msg = "Ambiguous module " ++ display mod_name ++ " " ++
                  "when trying to fill requirement. It could refer to " ++
                  "a module included from " ++ displaySource prov ++ " " ++
                  "or module included from " ++ displaySource prov' ++ ". " ++
                  "Ambiguity occurred because "
        withContext msg (usrc_module prov) (usrc_module prov') $
            unifyModule (usrc_module prov) (usrc_module prov')
    let msg = "Could not fill requirement " ++ display mod_name ++ "because "
    withContext msg (usrc_module prov) req $
        unifyModule (usrc_module prov) req
    return ret



-----------------------------------------------------------------------
-- The unification algorithm

-- This is based off of https://gist.github.com/amnn/559551517d020dbb6588
-- which is a translation from Huet's thesis.

unifyUnitId :: UnitIdU s -> UnitIdU s -> UnifyM s ()
unifyUnitId uid1_u uid2_u
    | uid1_u == uid2_u = return ()
    | otherwise = do
        xuid1 <- liftST $ UnionFind.find uid1_u
        xuid2 <- liftST $ UnionFind.find uid2_u
        case (xuid1, xuid2) of
            (UnitIdThunkU u1, UnitIdThunkU u2)
                | u1 == u2  -> return ()
                | otherwise ->
                    unifyFail $
                        "pre-installed unit IDs " ++ display u1 ++
                        " and " ++ display u2 ++ " do not match."
            (UnitIdThunkU uid1, UnitIdU _ cid2 insts2)
                -> unifyThunkWith cid2 insts2 uid2_u uid1 uid1_u
            (UnitIdU _ cid1 insts1, UnitIdThunkU uid2)
                -> unifyThunkWith cid1 insts1 uid1_u uid2 uid2_u
            (UnitIdU _ cid1 insts1, UnitIdU _ cid2 insts2)
                -> unifyInner cid1 insts1 uid1_u cid2 insts2 uid2_u

unifyThunkWith :: ComponentId
               -> Map ModuleName (ModuleU s)
               -> UnitIdU s
               -> UnitId
               -> UnitIdU s
               -> UnifyM s ()
unifyThunkWith cid1 insts1 uid1_u uid2 uid2_u = do
    db <- fmap unify_db getUnifEnv
    let FullUnitId cid2 insts2' = expandUnitId db uid2
    insts2 <- convertModuleSubst insts2'
    unifyInner cid1 insts1 uid1_u cid2 insts2 uid2_u

unifyInner :: ComponentId
           -> Map ModuleName (ModuleU s)
           -> UnitIdU s
           -> ComponentId
           -> Map ModuleName (ModuleU s)
           -> UnitIdU s
           -> UnifyM s ()
unifyInner cid1 insts1 uid1_u cid2 insts2 uid2_u = do
    when (cid1 /= cid2) $
        -- TODO: if we had a package identifier, could be an
        -- easier to understand error message.
        unifyFail $
            "component IDs " ++
            display cid1 ++ " and " ++ display cid2 ++ " do not match."
    -- The KEY STEP which makes this a Huet-style unification
    -- algorithm.  (Also a payoff of using union-find.)
    -- We can build infinite unit IDs this way, which is necessary
    -- for support mutual recursion. NB: union keeps the SECOND
    -- descriptor, so we always arrange for a UnitIdThunkU to live
    -- there.
    liftST $ UnionFind.union uid1_u uid2_u
    F.sequenceA_ $ Map.intersectionWith unifyModule insts1 insts2

-- | Imperatively unify two modules.
unifyModule :: ModuleU s -> ModuleU s -> UnifyM s ()
unifyModule mod1_u mod2_u
    | mod1_u == mod2_u = return ()
    | otherwise = do
        mod1 <- liftST $ UnionFind.find mod1_u
        mod2 <- liftST $ UnionFind.find mod2_u
        case (mod1, mod2) of
            (ModuleVarU _, _) -> liftST $ UnionFind.union mod1_u mod2_u
            (_, ModuleVarU _) -> liftST $ UnionFind.union mod2_u mod1_u
            (ModuleU uid1 mod_name1, ModuleU uid2 mod_name2) -> do
                when (mod_name1 /= mod_name2) $
                    unifyFail $
                        "module names " ++
                        display mod_name1 ++ " and " ++
                        display mod_name2 ++ " disagree."
                -- NB: this is not actually necessary (because we'll
                -- detect loops eventually in 'unifyUnitId'), but it
                -- seems harmless enough
                liftST $ UnionFind.union mod1_u mod2_u
                unifyUnitId uid1 uid2
