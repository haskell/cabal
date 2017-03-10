{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternGuards #-}
-- | See <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>
module Distribution.Backpack.ReadyComponent (
    ReadyComponent(..),
    InstantiatedComponent(..),
    IndefiniteComponent(..),
    rc_compat_name,
    rc_compat_key,
    rc_depends,
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
import Distribution.Types.ComponentInclude
import Distribution.Types.ComponentId
import Distribution.Types.PackageId
import Distribution.Types.UnitId
import Distribution.Compat.Graph (IsNode(..))
import Distribution.Types.Module
import Distribution.Types.MungedPackageId
import Distribution.Types.MungedPackageName

import Distribution.ModuleName
import Distribution.Package
import Distribution.Simple.Utils
import Distribution.Simple.Compiler

import qualified Control.Applicative as A
import qualified Data.Traversable as T

import Control.Monad
import Text.PrettyPrint
import qualified Data.Map as Map

import Distribution.Version
import Distribution.Text

-- | A 'ReadyComponent' is one that we can actually generate build
-- products for.  We have a ready component for the typecheck-only
-- products of every indefinite package, as well as a ready component
-- for every way these packages can be fully instantiated.
--
data ReadyComponent
    = ReadyComponent {
        -- | The final, string 'UnitId' that will uniquely identify
        -- the compilation products of this component.
        rc_uid          :: UnitId,
        -- | The 'OpenUnitId' for this package.  At the moment, this
        -- is used in only one case, which is to determine if an
        -- export is of a module from this library (indefinite
        -- libraries record these exports as 'OpenModule');
        -- 'rc_open_uid' can be conveniently used to test for
        -- equality, whereas 'UnitId' cannot always be used in this
        -- case.
        rc_open_uid     :: OpenUnitId,
        -- | Corresponds to 'lc_cid'.  Invariant: if 'rc_open_uid'
        -- records a 'ComponentId', it coincides with this one.
        rc_cid          :: ComponentId,
        -- | Corresponds to 'lc_pkgid'.
        rc_pkgid        :: PackageId,
        -- | Corresponds to 'lc_component'.
        rc_component    :: Component,
        -- | Corresponds to 'lc_internal_build_tools'.
        -- Build-tools don't participate in mix-in linking.
        -- (but what if they could?)
        rc_internal_build_tools :: [DefUnitId],
        -- | Corresponds to 'lc_public'.
        rc_public       :: Bool,
        -- | Extra metadata depending on whether or not this is an
        -- indefinite library (typechecked only) or an instantiated
        -- component (can be compiled).
        rc_i            :: Either IndefiniteComponent InstantiatedComponent
    }

-- | An 'InstantiatedComponent' is a library which is fully instantiated
-- (or, possibly, has no requirements at all.)
data InstantiatedComponent
    = InstantiatedComponent {
        -- | How this library was instantiated.
        instc_insts    :: [(ModuleName, Module)],
        -- | Dependencies induced by 'instc_insts'.  These are recorded
        -- here because there isn't a convenient way otherwise to get
        -- the 'PackageId' we need to fill 'componentPackageDeps' as needed.
        instc_insts_deps :: [(UnitId, MungedPackageId)],
        -- | The modules exported/reexported by this library.
        instc_provides :: Map ModuleName Module,
        -- | The dependencies which need to be passed to the compiler
        -- to bring modules into scope.  These always refer to installed
        -- fully instantiated libraries.
        instc_includes :: [ComponentInclude DefUnitId ModuleRenaming]
    }

-- | An 'IndefiniteComponent' is a library with requirements
-- which we will typecheck only.
data IndefiniteComponent
    = IndefiniteComponent {
        -- | The requirements of the library.
        indefc_requires :: [ModuleName],
        -- | The modules exported/reexported by this library.
        indefc_provides :: Map ModuleName OpenModule,
        -- | The dependencies which need to be passed to the compiler
        -- to bring modules into scope.  These are 'OpenUnitId' because
        -- these may refer to partially instantiated libraries.
        indefc_includes :: [ComponentInclude OpenUnitId ModuleRenaming]
    }

-- | Compute the dependencies of a 'ReadyComponent' that should
-- be recorded in the @depends@ field of 'InstalledPackageInfo'.
rc_depends :: ReadyComponent -> [(UnitId, MungedPackageId)]
rc_depends rc = ordNub $
    case rc_i rc of
        Left indefc ->
            map (\ci -> (abstractUnitId $ ci_id ci, toMungedPackageId ci))
                (indefc_includes indefc)
        Right instc ->
            map (\ci -> (unDefUnitId $ ci_id ci, toMungedPackageId ci))
                (instc_includes instc)
              ++ instc_insts_deps instc
  where
    toMungedPackageId :: ComponentInclude x y -> MungedPackageId
    toMungedPackageId ci = computeCompatPackageId (ci_pkgid ci) (ci_compname ci)

instance Package ReadyComponent where
    packageId = rc_pkgid

instance HasMungedPackageId ReadyComponent where
    mungedId ReadyComponent { rc_pkgid = pkgid, rc_component = component }
      = computeCompatPackageId pkgid (componentName component)

instance HasUnitId ReadyComponent where
    installedUnitId = rc_uid

instance IsNode ReadyComponent where
    type Key ReadyComponent = UnitId
    nodeKey = rc_uid
    nodeNeighbors rc =
      (case rc_i rc of
        Right inst | [] <- instc_insts inst
                   -> []
                   | otherwise
                   -> [newSimpleUnitId (rc_cid rc)]
        _ -> []) ++
      ordNub (map fst (rc_depends rc))

rc_compat_name :: ReadyComponent -> MungedPackageName
rc_compat_name ReadyComponent {
        rc_pkgid = PackageIdentifier pkg_name _,
        rc_component = component
    }
    = computeCompatPackageName pkg_name (componentName component)

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
    :: Map UnitId MungedPackageId
    -> Map ModuleName Module -- subst for the public component
    -> [LinkedComponent]
    -> [ReadyComponent]
toReadyComponents pid_map subst0 comps
    = catMaybes (Map.elems ready_map)
  where
    cmap = Map.fromList [ (lc_cid lc, lc) | lc <- comps ]

    instantiateUnitId :: ComponentId -> Map ModuleName Module
                      -> InstM DefUnitId
    instantiateUnitId cid insts = InstM $ \s ->
        case Map.lookup uid s of
            Nothing ->
                -- Knot tied
                let (r, s') = runInstM (instantiateComponent uid cid insts)
                                       (Map.insert uid r s)
                in (def_uid, Map.insert uid r s')
            Just _ -> (def_uid, s)
      where
        -- The mkDefUnitId here indicates that we assume
        -- that Cabal handles unit id hash allocation.
        -- Good thing about hashing here: map is only on string.
        -- Bad thing: have to repeatedly hash.
        def_uid = mkDefUnitId cid insts
        uid = unDefUnitId def_uid

    instantiateComponent
        :: UnitId -> ComponentId -> Map ModuleName Module
        -> InstM (Maybe ReadyComponent)
    instantiateComponent uid cid insts
      | Just lc <- Map.lookup cid cmap = do
            provides <- T.mapM (substModule insts) (modShapeProvides (lc_shape lc))
            includes <- forM (lc_includes lc) $ \ci -> do
                uid' <- substUnitId insts (ci_id ci)
                return ci { ci_id = uid' }
            build_tools <- mapM (substUnitId insts) (lc_internal_build_tools lc)
            s <- InstM $ \s -> (s, s)
            let getDep (Module dep_def_uid _)
                    | let dep_uid = unDefUnitId dep_def_uid
                    -- Lose DefUnitId invariant for rc_depends
                    = [(dep_uid,
                          fromMaybe err_pid $
                            Map.lookup dep_uid pid_map A.<|>
                            fmap mungedId (join (Map.lookup dep_uid s)))]
                  where
                    err_pid = MungedPackageId
                        (mkMungedPackageName "nonexistent-package-this-is-a-cabal-bug")
                        (mkVersion [0])
                instc = InstantiatedComponent {
                            instc_insts = Map.toList insts,
                            instc_insts_deps = concatMap getDep (Map.elems insts),
                            instc_provides = provides,
                            instc_includes = includes
                            -- NB: there is no dependency on the
                            -- indefinite version of this instantiated package here,
                            -- as (1) it doesn't go in depends in the
                            -- IPI: it's not a run time dep, and (2)
                            -- we don't have to tell GHC about it, it
                            -- will match up the ComponentId
                            -- automatically
                        }
            return $ Just ReadyComponent {
                    rc_uid          = uid,
                    rc_open_uid     = DefiniteUnitId (unsafeMkDefUnitId uid),
                    rc_cid          = lc_cid lc,
                    rc_pkgid        = lc_pkgid lc,
                    rc_component    = lc_component lc,
                    rc_internal_build_tools = build_tools,
                    rc_public       = lc_public lc,
                    rc_i            = Right instc
                   }
      | otherwise = return Nothing

    substUnitId :: Map ModuleName Module -> OpenUnitId -> InstM DefUnitId
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
                        indefc_includes = lc_includes lc ++ lc_sig_includes lc
                    }
            return $ Just ReadyComponent {
                    rc_uid          = uid,
                    rc_open_uid     = lc_uid lc,
                    rc_cid          = lc_cid lc,
                    rc_pkgid        = lc_pkgid lc,
                    rc_component    = lc_component lc,
                    rc_internal_build_tools = build_tools,
                    rc_public       = lc_public lc,
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
                then instantiateUnitId (lc_cid lc) Map.empty >> return ()
                else indefiniteUnitId (lc_cid lc) >> return ()
