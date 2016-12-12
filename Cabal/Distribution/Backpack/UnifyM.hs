{-# LANGUAGE Rank2Types #-}
-- | See <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>
module Distribution.Backpack.UnifyM (
    -- * Unification monad
    UnifyM,
    runUnifyM,
    unifyFail,
    withContext,
    liftST,

    UnifEnv(..),
    getUnifEnv,

    -- * Modules and unit IDs
    ModuleU,
    ModuleU'(..),
    convertModule,
    convertModuleU,

    UnitIdU,
    UnitIdU'(..),
    convertUnitId,
    convertUnitIdU,

    ModuleSubstU,
    convertModuleSubstU,
    convertModuleSubst,

    ModuleScopeU,
    emptyModuleScopeU,
    convertModuleScopeU,

    ModuleSourceU(..),

    convertInclude,
    convertModuleProvides,
    convertModuleProvidesU,

) where

import Prelude ()
import Distribution.Compat.Prelude hiding (mod)

import Distribution.Backpack.ModuleShape
import Distribution.Backpack.ModuleScope
import Distribution.Backpack.ModSubst
import Distribution.Backpack.FullUnitId
import Distribution.Backpack

import qualified Distribution.Utils.UnionFind as UnionFind
import Distribution.ModuleName
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Text
import Distribution.Types.IncludeRenaming
import Distribution.Verbosity

import Data.STRef
import Control.Monad.ST
import Control.Monad
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Traversable as T

-- TODO: more detailed trace output on high verbosity would probably
-- be appreciated by users debugging unification errors.  Collect
-- some good examples!

-- | The unification monad, this monad encapsulates imperative
-- unification.
newtype UnifyM s a = UnifyM { unUnifyM :: UnifEnv s -> ST s (Either String a) }

-- | Run a computation in the unification monad.
runUnifyM :: Verbosity -> FullDb -> (forall s. UnifyM s a) -> Either String a
runUnifyM verbosity db m
    = runST $ do i    <- newSTRef 0
                 hmap <- newSTRef Map.empty
                 unUnifyM m (UnifEnv i hmap verbosity Nothing db)
-- NB: GHC 7.6 throws a hissy fit if you pattern match on 'm'.

-- | The unification environment.
data UnifEnv s = UnifEnv {
        -- | A supply of unique integers to label 'UnitIdU'
        -- cells.  This is used to determine loops in unit
        -- identifiers (which can happen with mutual recursion.)
        unify_uniq :: UnifRef s UnitIdUnique,
        -- | The set of requirements in scope.  When
        -- a provision is brought into scope, we unify with
        -- the requirement at the same module name to fill it.
        -- This mapping grows monotonically.
        unify_reqs :: UnifRef s (Map ModuleName (ModuleU s)),
        -- | How verbose the error message should be
        unify_verbosity :: Verbosity,
        -- | The error reporting context
        unify_ctx :: Maybe (String, ModuleU s, ModuleU s),
        -- | The package index for expanding unit identifiers
        unify_db :: FullDb
    }

instance Functor (UnifyM s) where
    fmap f (UnifyM m) = UnifyM (fmap (fmap (fmap f)) m)

instance Applicative (UnifyM s) where
    pure = UnifyM . pure . pure . pure
    UnifyM f <*> UnifyM x = UnifyM $ \r -> do
        f' <- f r
        case f' of
          Left err -> return (Left err)
          Right f'' -> do
              x' <- x r
              case x' of
                  Left err -> return (Left err)
                  Right x'' -> return (Right (f'' x''))

instance Monad (UnifyM s) where
    return = pure
    UnifyM m >>= f = UnifyM $ \r -> do
        x <- m r
        case x of
            Left err -> return (Left err)
            Right x' -> unUnifyM (f x') r

-- | Lift a computation from 'ST' monad to 'UnifyM' monad.
-- Internal use only.
liftST :: ST s a -> UnifyM s a
liftST m = UnifyM $ \_ -> fmap Right m

unifyFail :: String -> UnifyM s a
unifyFail err = do
    env <- getUnifEnv
    msg <- case unify_ctx env of
        Nothing -> return ("Unspecified unification error: " ++ err)
        Just (ctx, mod1, mod2)
            | unify_verbosity env > normal
            -> do mod1' <- convertModuleU mod1
                  mod2' <- convertModuleU mod2
                  let extra = " (was unifying " ++ display mod1'
                                     ++ " and " ++ display mod2' ++ ")"
                  return (ctx ++ err ++ extra)
            | otherwise
            -> return (ctx ++ err ++ " (for more information, pass -v flag)")
    UnifyM $ \_ -> return (Left msg)

-- | A convenient alias for mutable references in the unification monad.
type UnifRef s a = STRef s a

-- | Imperatively read a 'UnifRef'.
readUnifRef :: UnifRef s a -> UnifyM s a
readUnifRef = liftST . readSTRef

-- | Imperatively write a 'UnifRef'.
writeUnifRef :: UnifRef s a -> a -> UnifyM s ()
writeUnifRef x = liftST . writeSTRef x

-- | Get the current unification environment.
getUnifEnv :: UnifyM s (UnifEnv s)
getUnifEnv = UnifyM $ \r -> return (Right r)

-- | Run a unification in some context
withContext :: String -> ModuleU s -> ModuleU s -> UnifyM s a -> UnifyM s a
withContext ctx mod1 mod2 m =
    UnifyM $ \r -> unUnifyM m r { unify_ctx = Just (ctx, mod1, mod2) }

-----------------------------------------------------------------------
-- The "unifiable" variants of the data types
--
-- In order to properly do unification over infinite trees, we
-- need to union find over 'Module's and 'UnitId's.  The pure
-- representation is ill-equipped to do this, so we convert
-- from the pure representation into one which is indirected
-- through union-find.  'ModuleU' handles hole variables;
-- 'UnitIdU' handles mu-binders.

-- | Contents of a mutable 'ModuleU' reference.
data ModuleU' s
    = ModuleU (UnitIdU s) ModuleName
    | ModuleVarU ModuleName

-- | Contents of a mutable 'UnitIdU' reference.
data UnitIdU' s
    = UnitIdU UnitIdUnique ComponentId (Map ModuleName (ModuleU s))
    | UnitIdThunkU DefUnitId

-- | A mutable version of 'Module' which can be imperatively unified.
type ModuleU s = UnionFind.Point s (ModuleU' s)

-- | A mutable version of 'UnitId' which can be imperatively unified.
type UnitIdU s = UnionFind.Point s (UnitIdU' s)

-- | An integer for uniquely labeling 'UnitIdU' nodes.  We need
-- these labels in order to efficiently serialize 'UnitIdU's into
-- 'UnitId's (we use the label to check if any parent is the
-- node in question, and if so insert a deBruijn index instead.)
-- These labels must be unique across all 'UnitId's/'Module's which
-- participate in unification!
type UnitIdUnique = Int


-----------------------------------------------------------------------
-- Conversion to the unifiable data types

-- An environment for tracking the mu-bindings in scope.
-- The invariant for a state @(m, i)@ is that [0..i] are
-- keys of @m@; in fact, the @i-k@th entry is the @k@th
-- de Bruijn index (this saves us from having to shift as
-- we enter mu-binders.)
type MuEnv s = (IntMap (UnitIdU s), Int)

extendMuEnv :: MuEnv s -> UnitIdU s -> MuEnv s
extendMuEnv (m, i) x =
    (IntMap.insert (i + 1) x m, i + 1)

{-
lookupMuEnv :: MuEnv s -> Int {- de Bruijn index -} -> UnitIdU s
lookupMuEnv (m, i) k =
    case IntMap.lookup (i - k) m of
        -- Technically a user can trigger this by giving us a
        -- bad 'UnitId', so handle this better.
        Nothing -> error "lookupMuEnv: out of bounds (malformed de Bruijn index)"
        Just v -> v
-}

emptyMuEnv :: MuEnv s
emptyMuEnv = (IntMap.empty, -1)

-- The workhorse functions.  These share an environment:
--   * @UnifRef s UnitIdUnique@ - the unique label supply for 'UnitIdU' nodes
--   * @UnifRef s (Map ModuleName moduleU)@ - the (lazily initialized)
--     environment containing the implicitly universally quantified
--     @hole:A@ binders.
--   * @MuEnv@ - the environment for mu-binders.

convertUnitId' :: MuEnv s
               -> OpenUnitId
               -> UnifyM s (UnitIdU s)
-- TODO: this could be more lazy if we know there are no internal
-- references
convertUnitId' _ (DefiniteUnitId uid) =
    liftST $ UnionFind.fresh (UnitIdThunkU uid)
convertUnitId' stk (IndefFullUnitId cid insts) = do
    fs <- fmap unify_uniq getUnifEnv
    x <- liftST $ UnionFind.fresh (error "convertUnitId") -- tie the knot later
    insts_u <- T.forM insts $ convertModule' (extendMuEnv stk x)
    u <- readUnifRef fs
    writeUnifRef fs (u+1)
    y <- liftST $ UnionFind.fresh (UnitIdU u cid insts_u)
    liftST $ UnionFind.union x y
    return y
-- convertUnitId' stk (UnitIdVar i) = return (lookupMuEnv stk i)

convertModule' :: MuEnv s
               -> OpenModule -> UnifyM s (ModuleU s)
convertModule' _stk (OpenModuleVar mod_name) = do
    hmap <- fmap unify_reqs getUnifEnv
    hm <- readUnifRef hmap
    case Map.lookup mod_name hm of
        Nothing -> do mod <- liftST $ UnionFind.fresh (ModuleVarU mod_name)
                      writeUnifRef hmap (Map.insert mod_name mod hm)
                      return mod
        Just mod -> return mod
convertModule' stk (OpenModule uid mod_name) = do
    uid_u <- convertUnitId' stk uid
    liftST $ UnionFind.fresh (ModuleU uid_u mod_name)

convertUnitId :: OpenUnitId -> UnifyM s (UnitIdU s)
convertUnitId = convertUnitId' emptyMuEnv

convertModule :: OpenModule -> UnifyM s (ModuleU s)
convertModule = convertModule' emptyMuEnv



-----------------------------------------------------------------------
-- Substitutions

-- | The mutable counterpart of a 'ModuleSubst' (not defined here).
type ModuleSubstU s = Map ModuleName (ModuleU s)

-- | Conversion of 'ModuleSubst' to 'ModuleSubstU'
convertModuleSubst :: Map ModuleName OpenModule -> UnifyM s (Map ModuleName (ModuleU s))
convertModuleSubst = T.mapM convertModule

-- | Conversion of 'ModuleSubstU' to 'ModuleSubst'
convertModuleSubstU :: ModuleSubstU s -> UnifyM s OpenModuleSubst
convertModuleSubstU = T.mapM convertModuleU

-----------------------------------------------------------------------
-- Conversion from the unifiable data types

-- An environment for tracking candidates for adding a mu-binding.
-- The invariant for a state @(m, i)@, is that if we encounter a node
-- labeled @k@ such that @m[k -> v]@, then we can replace this
-- node with the de Bruijn index @i-v@ referring to an enclosing
-- mu-binder; furthermore, @range(m) = [0..i]@.
type MooEnv = (IntMap Int, Int)

emptyMooEnv :: MooEnv
emptyMooEnv = (IntMap.empty, -1)

extendMooEnv :: MooEnv -> UnitIdUnique -> MooEnv
extendMooEnv (m, i) k = (IntMap.insert k (i + 1) m, i + 1)

lookupMooEnv :: MooEnv -> UnitIdUnique -> Maybe Int
lookupMooEnv (m, i) k =
    case IntMap.lookup k m of
        Nothing -> Nothing
        Just v -> Just (i-v) -- de Bruijn indexize

-- The workhorse functions

convertUnitIdU' :: MooEnv -> UnitIdU s -> UnifyM s OpenUnitId
convertUnitIdU' stk uid_u = do
    x <- liftST $ UnionFind.find uid_u
    case x of
        UnitIdThunkU uid -> return (DefiniteUnitId uid)
        UnitIdU u cid insts_u ->
            case lookupMooEnv stk u of
                Just _i -> error "convertUnitIdU': mutual recursion" -- return (UnitIdVar i)
                Nothing -> do
                    insts <- T.forM insts_u $ convertModuleU' (extendMooEnv stk u)
                    return (IndefFullUnitId cid insts)

convertModuleU' :: MooEnv -> ModuleU s -> UnifyM s OpenModule
convertModuleU' stk mod_u = do
    mod <- liftST $ UnionFind.find mod_u
    case mod of
        ModuleVarU mod_name -> return (OpenModuleVar mod_name)
        ModuleU uid_u mod_name -> do
            uid <- convertUnitIdU' stk uid_u
            return (OpenModule uid mod_name)

-- Helper functions

convertUnitIdU :: UnitIdU s -> UnifyM s OpenUnitId
convertUnitIdU = convertUnitIdU' emptyMooEnv

convertModuleU :: ModuleU s -> UnifyM s OpenModule
convertModuleU = convertModuleU' emptyMooEnv

-- | An empty 'ModuleScopeU'.
emptyModuleScopeU :: ModuleScopeU s
emptyModuleScopeU = (Map.empty, Map.empty)


-- | The mutable counterpart of 'ModuleScope'.
type ModuleScopeU s = (ModuleProvidesU s, ModuleSubstU s)
-- | The mutable counterpart of 'ModuleProvides'
type ModuleProvidesU s = Map ModuleName [ModuleSourceU s]
data ModuleSourceU s =
    ModuleSourceU {
        -- We don't have line numbers, but if we did the
        -- package name and renaming could be associated
        -- with that as well
        usrc_pkgname :: PackageName,
        usrc_renaming :: IncludeRenaming,
        usrc_module :: ModuleU s
    }

-- | Convert a 'ModuleShape' into a 'ModuleScopeU', so we can do
-- unification on it.
convertInclude
    :: ((OpenUnitId, ModuleShape), PackageId, IncludeRenaming)
    -> UnifyM s (ModuleScopeU s, (UnitIdU s, PackageId, ModuleRenaming))
convertInclude ((uid, ModuleShape provs reqs), pid, incl@(IncludeRenaming prov_rns req_rns)) = do
    let pn = packageName pid

    -- Suppose our package has two requirements A and B, and
    -- we include it with @requires (A as X)@
    -- There are three closely related things we compute based
    -- off of @reqs@ and @reqs_rns@:
    --
    --      1. The requirement renaming (A -> X)
    --      2. The requirement substitution (A -> <X>, B -> <B>)

    -- Requirement renaming.  This is read straight off the syntax:
    --
    --      [nothing]          ==>  [empty]
    --      requires (B as Y)  ==>  B -> Y
    --
    -- Requirement renamings are NOT injective: if two requirements
    -- are mapped to the same name, the intent is to merge them
    -- together.  But they are *functions*, so @B as X, B as Y@ is
    -- illegal.
    let insertDistinct m (k,v) =
            if Map.member k m
                then error ("Duplicate requirement renaming " ++ display k)
                else return (Map.insert k v m)
    req_rename <- foldM insertDistinct Map.empty =<<
                      case req_rns of
                        DefaultRenaming -> return []
                        -- Not valid here, but whatever
                        HidingRenaming _ -> error "Cannot use hiding in requirement renaming"
                        ModuleRenaming rns -> return rns
    let req_rename_fn k = case Map.lookup k req_rename of
                            Nothing -> k
                            Just v  -> v

    -- Requirement substitution.
    --
    --      A -> X      ==>     A -> <X>
    let req_subst = fmap OpenModuleVar req_rename

    uid_u <- convertUnitId (modSubst req_subst uid)

    -- Requirement mapping.  This is just taking the range of the
    -- requirement substitution, and making a mapping so that it is
    -- convenient to merge things together.  It INCLUDES the implicit
    -- mappings.
    --
    --      A -> X      ==>     X -> <X>, B -> <B>
    reqs_u <- convertModuleSubst . Map.fromList $
                [ (k, OpenModuleVar k)
                | k <- map req_rename_fn (Set.toList reqs)
                ]

    -- Report errors if there were unused renamings
    let leftover = Map.keysSet req_rename `Set.difference` reqs
    unless (Set.null leftover) $
        error $ "Attempted to rename the following requirements, which " ++
                "were not actually requirements of " ++ display uid ++ ": " ++
                intercalate ", " (map display (Set.toList leftover))

    -- Provision computation is more complex.
    -- For example, if we have:
    --
    --      include p (A as X) requires (B as Y)
    --          where A -> q[B=<B>]:A
    --
    -- Then we need:
    --
    --      X -> [("p", q[B=<B>]:A)]
    --
    -- There are a bunch of clever ways to present the algorithm
    -- but here is the simple one:
    --
    --      1. If we have a default renaming, apply req_subst
    --      to provs and use that.
    --
    --      2. Otherwise, build a map by successively looking
    --      up the referenced modules in the renaming in provs.
    --
    -- Importantly, overlapping rename targets get accumulated
    -- together.  It's not an (immediate) error.
    (pre_prov_scope, prov_rns') <-
        case prov_rns of
            DefaultRenaming -> return (Map.toList provs, prov_rns)
            HidingRenaming hides ->
                let hides_set = Set.fromList hides
                in let r = [ (k,v)
                           | (k,v) <- Map.toList provs
                           , not (k `Set.member` hides_set) ]
                   -- GHC doesn't understand hiding, so expand it out!
                   in return (r, ModuleRenaming (map ((\x -> (x,x)).fst) r))
            ModuleRenaming rns -> do
              r <- sequence
                [ case Map.lookup from provs of
                    Just m -> return (to, m)
                    Nothing -> error ("Tried to rename non-existent module " ++ display from)
                | (from, to) <- rns ]
              return (r, prov_rns)
    let prov_scope = modSubst req_subst
                   $ Map.fromListWith (++)
                   [ (k, [ModuleSource pn incl v])
                   | (k, v) <- pre_prov_scope ]

    provs_u <- convertModuleProvides prov_scope

    return ((provs_u, reqs_u), (uid_u, pid, prov_rns'))

-- | Convert a 'ModuleScopeU' to a 'ModuleScope'.
convertModuleScopeU :: ModuleScopeU s -> UnifyM s ModuleScope
convertModuleScopeU (provs_u, reqs_u) = do
    provs <- convertModuleProvidesU provs_u
    reqs  <- convertModuleSubstU reqs_u
    -- TODO: Test that the requirements are still free. If they
    -- are not, they got unified, and that's dodgy at best.
    return (ModuleScope provs (Map.keysSet reqs))

-- | Convert a 'ModuleProvides' to a 'ModuleProvidesU'
convertModuleProvides :: ModuleProvides -> UnifyM s (ModuleProvidesU s)
convertModuleProvides = T.mapM $ \ms ->
    mapM (\(ModuleSource pn incl m)
            -> do m' <- convertModule m
                  return (ModuleSourceU pn incl m')) ms

-- | Convert a 'ModuleProvidesU' to a 'ModuleProvides'
convertModuleProvidesU :: ModuleProvidesU s -> UnifyM s ModuleProvides
convertModuleProvidesU = T.mapM $ \ms ->
    mapM (\(ModuleSourceU pn incl m)
            -> do m' <- convertModuleU m
                  return (ModuleSource pn incl m')) ms
