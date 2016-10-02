{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternGuards #-}
-- | See <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>
module Distribution.Backpack.ReadyComponent (
    ReadyComponent(..),
    InstantiatedComponent(..),
    IndefiniteComponent(..),
    rc_compat_name,
    rc_compat_key,
    dispReadyComponent,
    toReadyComponents,
) where

import Prelude ()
import Distribution.Compat.Prelude hiding ((<>))

import Distribution.Backpack
import Distribution.Backpack.Id
import Distribution.Backpack.LinkedComponent
import Distribution.Backpack.ModuleShape

import Distribution.Types.ModuleRenaming
import Distribution.Types.Component
import Distribution.Compat.Graph (IsNode(..))

import Distribution.ModuleName
import Distribution.Package
import Distribution.Simple.Utils
import Distribution.Simple.Compiler

import qualified Control.Applicative as A
import qualified Data.Traversable as T

import Control.Monad
import Text.PrettyPrint
import qualified Data.Map as Map

import Distribution.Text

-- | An instantiated component is simply a linked component which
-- may have a fully instantiated 'UnitId'. When we do mix-in linking,
-- we only do each component in its most general form; instantiation
-- then takes all of the fully instantiated components and recursively
-- discovers what other instantiated components we need to build
-- before we can build them.
--

data InstantiatedComponent
    = InstantiatedComponent {
        instc_insts    :: [(ModuleName, Module)],
        instc_provides :: Map ModuleName Module,
        instc_includes :: [(UnitId, ModuleRenaming)]
    }

data IndefiniteComponent
    = IndefiniteComponent {
        indefc_requires :: [ModuleName],
        indefc_provides :: Map ModuleName OpenModule,
        indefc_includes :: [(OpenUnitId, ModuleRenaming)]
    }

data ReadyComponent
    = ReadyComponent {
        rc_uid          :: UnitId,
        rc_pkgid        :: PackageId,
        rc_component    :: Component,
        -- build-tools don't participate in mix-in linking.
        -- (but what if they cold?)
        rc_internal_build_tools :: [UnitId],
        rc_public       :: Bool,
        -- PackageId here is a bit dodgy, but its just for
        -- BC so it shouldn't matter.
        rc_depends      :: [(UnitId, PackageId)],
        rc_i :: Either IndefiniteComponent InstantiatedComponent
    }

instance Package ReadyComponent where
    packageId = rc_pkgid

instance HasUnitId ReadyComponent where
    installedUnitId = rc_uid

instance IsNode ReadyComponent where
    type Key ReadyComponent = UnitId
    nodeKey = rc_uid
    nodeNeighbors rc =
      (case rc_i rc of
        Right _ | UnitId cid (Just _)
                    <- rc_uid rc -> [newSimpleUnitId cid]
        _ -> []) ++
      ordNub (map fst (rc_depends rc))

rc_compat_name :: ReadyComponent -> PackageName
rc_compat_name ReadyComponent{
        rc_pkgid = PackageIdentifier pkg_name _,
        rc_component = component,
        rc_uid = uid
    }
    = computeCompatPackageName pkg_name (componentName component) (Just uid)

rc_compat_key :: ReadyComponent -> Compiler -> String
rc_compat_key rc@ReadyComponent {
        rc_pkgid = PackageIdentifier _ pkg_ver,
        rc_uid = uid
    } comp -- TODO: A wart. But the alternative is to store
           -- the Compiler in the LinkedComponent
    = computeCompatPackageKey comp (rc_compat_name rc) pkg_ver uid

dispReadyComponent :: ReadyComponent -> Doc
dispReadyComponent rc =
    hang (text (case rc_i rc of
                    Left  _ -> "indefinite"
                    Right _ -> "definite")
            <+> disp (nodeKey rc)
            {- <+> dispModSubst (Map.fromList (lc_insts lc)) -} ) 4 $
        vcat [ text "depends" <+> disp uid
             | uid <- nodeNeighbors rc ]

-- | The state of 'InstM'; a mapping from 'UnitId's to their
-- ready component, or @Nothing@ if its an external
-- component which we don't know how to build.
type InstS = Map UnitId (Maybe ReadyComponent)

-- | A state monad for doing instantiations (can't use actual
-- State because that would be an extra dependency.)
newtype InstM a = InstM { runInstM :: InstS -> (a, InstS) }

instance Functor InstM where
    fmap f (InstM m) = InstM $ \s -> let (x, s') = m s
                                     in (f x, s')

instance A.Applicative InstM where
    pure a = InstM $ \s -> (a, s)
    InstM f <*> InstM x = InstM $ \s -> let (f', s') = f s
                                            (x', s'') = x s'
                                        in (f' x', s'')

instance Monad InstM where
    return = A.pure
    InstM m >>= f = InstM $ \s -> let (x, s') = m s
                                  in runInstM (f x) s'

-- | Given a list of 'LinkedComponent's, expand the module graph
-- so that we have an instantiated graph containing all of the
-- instantiated components we need to build.
--
-- Instantiation intuitively follows the following algorithm:
--
--      instantiate a definite unit id p[S]:
--          recursively instantiate each module M in S
--          recursively instantiate modules exported by this unit
--          recursively instantiate dependencies substituted by S
--
-- The implementation is a bit more involved to memoize instantiation
-- if we have done it already.
--
-- We also call 'improveUnitId' during this process, so that fully
-- instantiated components are given 'HashedUnitId'.
--
toReadyComponents
    :: Map ComponentId PackageId
    -> Map ModuleName Module -- subst for the public component
    -> [LinkedComponent]
    -> [ReadyComponent]
toReadyComponents pid_map subst0 comps
    = catMaybes (Map.elems ready_map)
  where
    cmap = Map.fromList [ (lc_cid lc, lc) | lc <- comps ]

    instantiateUnitId :: ComponentId -> Map ModuleName Module
                      -> InstM UnitId
    instantiateUnitId cid insts = InstM $ \s ->
        case Map.lookup uid s of
            Nothing ->
                -- Knot tied
                let (r, s') = runInstM (instantiateComponent uid cid insts)
                                       (Map.insert uid r s)
                in (uid, Map.insert uid r s')
            Just _ -> (uid, s)
      where
        -- The hashModuleSubst here indicates that we assume
        -- that Cabal handles unit id hash allocation.
        -- Good thing about hashing here: map is only on string.
        -- Bad thing: have to repeatedly hash.
        uid = UnitId cid (hashModuleSubst insts)

    instantiateComponent
        :: UnitId -> ComponentId -> Map ModuleName Module
        -> InstM (Maybe ReadyComponent)
    instantiateComponent uid cid insts
      | Just lc <- Map.lookup cid cmap = do
            deps <- forM (lc_depends lc) $ \(x, y) -> do
                x' <- substUnitId insts x
                return (x', y)
            provides <- T.mapM (substModule insts) (modShapeProvides (lc_shape lc))
            includes <- forM (lc_includes lc) $ \(x, y) -> do
                x' <- substUnitId insts x
                return (x', y)
            build_tools <- mapM (substUnitId insts) (lc_internal_build_tools lc)
            let getDep (Module dep_uid _)
                    | Just pid <- Map.lookup (unitIdComponentId dep_uid) pid_map
                    = [(dep_uid, pid)]
                getDep _ = []
                instc = InstantiatedComponent {
                            instc_insts = Map.toList insts,
                            instc_provides = provides,
                            instc_includes = includes
                        }
            return $ Just ReadyComponent {
                    rc_uid          = uid,
                    rc_pkgid        = lc_pkgid lc,
                    rc_component    = lc_component lc,
                    rc_internal_build_tools = build_tools,
                    rc_public       = lc_public lc,
                    rc_depends      = ordNub $
                                        -- NB: don't put the dep on the indef
                                        -- package here, since we DO NOT want
                                        -- to put it in 'depends' in the IPI
                                        deps ++ concatMap getDep (Map.elems insts),
                    rc_i            = Right instc
                   }
      | otherwise = return Nothing

    substUnitId :: Map ModuleName Module -> OpenUnitId -> InstM UnitId
    substUnitId _ (DefiniteUnitId uid) =
        return uid
    substUnitId subst (IndefFullUnitId cid insts) = do
        insts' <- substSubst subst insts
        instantiateUnitId cid insts'

    -- NB: NOT composition
    substSubst :: Map ModuleName Module
               -> Map ModuleName OpenModule
               -> InstM (Map ModuleName Module)
    substSubst subst insts = T.mapM (substModule subst) insts

    substModule :: Map ModuleName Module -> OpenModule -> InstM Module
    substModule subst (OpenModuleVar mod_name)
        | Just m <- Map.lookup mod_name subst = return m
        | otherwise = error "substModule: non-closing substitution"
    substModule subst (OpenModule uid mod_name) = do
        uid' <- substUnitId subst uid
        return (Module uid' mod_name)

    indefiniteUnitId :: ComponentId -> InstM UnitId
    indefiniteUnitId cid = do
        let uid = newSimpleUnitId cid
        r <- indefiniteComponent uid cid
        InstM $ \s -> (uid, Map.insert uid r s)

    indefiniteComponent :: UnitId -> ComponentId -> InstM (Maybe ReadyComponent)
    indefiniteComponent uid cid
      | Just lc <- Map.lookup cid cmap = do
            -- TODO: Goofy
            build_tools <- mapM (substUnitId Map.empty) (lc_internal_build_tools lc)
            let indefc = IndefiniteComponent {
                        indefc_requires = map fst (lc_insts lc),
                        indefc_provides = modShapeProvides (lc_shape lc),
                        indefc_includes = lc_includes lc
                    }
            return $ Just ReadyComponent {
                    rc_uid = uid,
                    rc_pkgid        = lc_pkgid lc,
                    rc_component    = lc_component lc,
                    rc_internal_build_tools = build_tools,
                    rc_public       = lc_public lc,
                    rc_depends      = ordNub (map (\(x,y) -> (abstractUnitId x, y)) (lc_depends lc)),
                    rc_i            = Left indefc
                }
      | otherwise = return Nothing

    ready_map = snd $ runInstM work Map.empty

    work
        | not (Map.null subst0)
        , [lc] <- filter lc_public (Map.elems cmap)
        = do _ <- instantiateUnitId (lc_cid lc) subst0
             return ()
        | otherwise
        = forM_ (Map.elems cmap) $ \lc ->
            if null (lc_insts lc)
                then instantiateUnitId (lc_cid lc) Map.empty
                else indefiniteUnitId (lc_cid lc)
