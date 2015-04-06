{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Distribution.Client.Dependency.Modular.Linking (
    addLinking
  , validateLinking
  ) where

import Prelude hiding (pi)
import Control.Exception (assert)
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe (catMaybes)
import Data.Map (Map, (!))
import Data.List (intercalate)
import Data.Set (Set)
import qualified Data.Map         as M
import qualified Data.Set         as S
import qualified Data.Traversable as T

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

import Distribution.Client.Dependency.Modular.Assignment
import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Flag
import Distribution.Client.Dependency.Modular.Index
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.Tree
import qualified Distribution.Client.Dependency.Modular.PSQ as P

import Distribution.Client.Types (OptionalStanza(..))
import Distribution.Client.ComponentDeps (Component)

{-------------------------------------------------------------------------------
  Add linking
-------------------------------------------------------------------------------}

type RelatedGoals = Map (PN, I) [PP]
type Linker       = Reader RelatedGoals

addLinking :: Tree QGoalReasonChain -> Tree QGoalReasonChain
addLinking = (`runReader` M.empty) .  cata go
  where
    go :: TreeF QGoalReasonChain (Linker (Tree QGoalReasonChain)) -> Linker (Tree QGoalReasonChain)

    -- The only nodes of interest are package nodes
    go (PChoiceF qpn gr cs) = do
      env <- ask
      cs' <- T.sequence $ P.mapWithKey (goP qpn) cs
      let newCs = concatMap (linkChoices env qpn) (P.toList cs')
      return $ PChoice qpn gr (cs' `P.union` P.fromList newCs)

    -- For all other nodes we just recurse
    go (FChoiceF qfn gr t m cs)       = FChoice qfn gr t m  <$> T.sequence cs
    go (SChoiceF qsn gr t   cs)       = SChoice qsn gr t    <$> T.sequence cs
    go (GoalChoiceF         cs)       = GoalChoice          <$> T.sequence cs
    go (DoneF revDepMap)              = return $ Done revDepMap
    go (FailF conflictSet failReason) = return $ Fail conflictSet failReason

    -- Recurse underneath package choices. Here we just need to make sure
    -- that we record the package choice so that it is available below
    goP :: QPN -> POption -> Linker (Tree QGoalReasonChain) -> Linker (Tree QGoalReasonChain)
    goP (Q pp pn) (POption i Nothing) = local (M.insertWith (++) (pn, i) [pp])
    goP _ _ = alreadyLinked

linkChoices :: RelatedGoals -> QPN -> (POption, Tree QGoalReasonChain) -> [(POption, Tree QGoalReasonChain)]
linkChoices related (Q _pp pn) (POption i Nothing, subtree) =
    map aux (M.findWithDefault [] (pn, i) related)
  where
    aux :: PP -> (POption, Tree QGoalReasonChain)
    aux pp = (POption i (Just pp), subtree)
linkChoices _ _ (POption _ (Just _), _) =
    alreadyLinked

alreadyLinked :: a
alreadyLinked = error "addLinking called on tree that already contains linked nodes"

{-------------------------------------------------------------------------------
  Validation
-------------------------------------------------------------------------------}

data ValidateState = VS {
      vsIndex    :: Index
    , vsLinks    :: Map QPN LinkGroup
    , vsFlags    :: FAssignment
    , vsStanzas  :: SAssignment
    }
    deriving Show

type Validate = Reader ValidateState

-- | Validate linked packages
--
-- Verify that linked packages have
--
-- * Linked dependencies,
-- * Equal flag assignments
-- * And something to do with stanzas (TODO)
validateLinking :: Index -> Tree QGoalReasonChain -> Tree QGoalReasonChain
validateLinking index = (`runReader` initVS) . cata go
  where
    go :: TreeF QGoalReasonChain (Validate (Tree QGoalReasonChain)) -> Validate (Tree QGoalReasonChain)

    go (PChoiceF qpn gr cs) =
      PChoice qpn gr     <$> T.sequence (P.mapWithKey (goP qpn) cs)
    go (FChoiceF qfn gr t m cs) =
      FChoice qfn gr t m <$> T.sequence (P.mapWithKey (goF qfn) cs)
    go (SChoiceF qsn gr t cs) =
      SChoice qsn gr t   <$> T.sequence (P.mapWithKey (goS qsn) cs)

    -- For the other nodes we just recurse
    go (GoalChoiceF         cs)       = GoalChoice          <$> T.sequence cs
    go (DoneF revDepMap)              = return $ Done revDepMap
    go (FailF conflictSet failReason) = return $ Fail conflictSet failReason

    -- Package choices
    goP :: QPN -> POption -> Validate (Tree QGoalReasonChain) -> Validate (Tree QGoalReasonChain)
    goP qpn@(Q _pp pn) opt@(POption i _) r = do
      vs <- ask
      let PInfo deps _ _ = vsIndex vs ! pn ! i
          qdeps          = qualifyDeps qpn deps
      case execUpdateState (pickPOption qpn opt qdeps) vs of
        Left  (cs, err) -> return $ Fail cs (DependenciesNotLinked err)
        Right vs'       -> local (const vs') r

    -- Flag choices
    goF :: QFN -> Bool -> Validate (Tree QGoalReasonChain) -> Validate (Tree QGoalReasonChain)
    goF qfn b r = do
      vs <- ask
      case execUpdateState (pickFlag qfn b) vs of
        Left  (cs, err) -> return $ Fail cs (DependenciesNotLinked err)
        Right vs'       -> local (const vs') r

    -- Stanza choices (much the same as flag choices)
    goS :: QSN -> Bool -> Validate (Tree QGoalReasonChain) -> Validate (Tree QGoalReasonChain)
    goS qsn b r = do
      vs <- ask
      case execUpdateState (pickStanza qsn b) vs of
        Left  (cs, err) -> return $ Fail cs (DependenciesNotLinked err)
        Right vs'       -> local (const vs') r

    initVS :: ValidateState
    initVS = VS {
        vsIndex   = index
      , vsLinks   = M.empty
      , vsFlags   = M.empty
      , vsStanzas = M.empty
      }

{-------------------------------------------------------------------------------
  Updating the validation state
-------------------------------------------------------------------------------}

type Conflict = (ConflictSet QPN, String)

newtype UpdateState a = UpdateState {
    unUpdateState :: StateT ValidateState (Either Conflict) a
  }
  deriving (Functor, Applicative, Monad, MonadState ValidateState)

lift' :: Either Conflict a -> UpdateState a
lift' = UpdateState . lift

conflict :: Conflict -> UpdateState a
conflict = lift' . Left

execUpdateState :: UpdateState () -> ValidateState -> Either Conflict ValidateState
execUpdateState = execStateT . unUpdateState

pickPOption :: QPN -> POption -> FlaggedDeps comp QPN -> UpdateState ()
pickPOption qpn (POption i Nothing)    _deps = pickConcrete qpn i
pickPOption qpn (POption i (Just pp'))  deps = pickLink     qpn i pp' deps

pickConcrete :: QPN -> I -> UpdateState ()
pickConcrete qpn@(Q pp _) i = do
    vs <- get
    case M.lookup qpn (vsLinks vs) of
      -- Package is not yet in a LinkGroup. Create a new singleton link group.
      Nothing -> do
        let lg = (lgSingleton qpn (Just i)) { lgCanon = Just pp }
        updateLinkGroup lg

      -- Package is already in a link group. Since we are picking a concrete
      -- instance here, it must by definition by the canonical package.
      Just lg ->
        makeCanonical lg qpn

pickLink :: QPN -> I -> PP -> FlaggedDeps comp QPN -> UpdateState ()
pickLink qpn@(Q _ pn) i pp' deps = do
    vs <- get
    -- Find the link group for the package we are linking to, and add this package
    --
    -- Since the builder never links to a package without having first picked a
    -- concrete instance for that package, and since we create singleton link
    -- groups for concrete instances, this  link group must exist.
    let lg = vsLinks vs ! Q pp' pn
    lg' <- lift' $ lgAddMember qpn i lg
    updateLinkGroup lg'
    linkDeps [P qpn] pp' deps

makeCanonical :: LinkGroup -> QPN -> UpdateState ()
makeCanonical lg qpn@(Q pp _) =
    case lgCanon lg of
      -- There is already a canonical member. Fail.
      Just _ ->
        conflict ( S.fromList (P qpn : lgBlame lg)
                 ,    "cannot make " ++ showQPN qpn
                   ++ " canonical member of " ++ showLinkGroup lg
                 )
      Nothing -> do
        let lg' = lg { lgCanon = Just pp }
        updateLinkGroup lg'

linkDeps :: [Var QPN] -> PP -> FlaggedDeps comp QPN -> UpdateState ()
linkDeps parents pp' = mapM_ go
  where
    go :: FlaggedDep comp QPN -> UpdateState ()
    go (Simple (Dep qpn@(Q _ pn) _) _) = do
      vs <- get
      let qpn' = Q pp' pn
          lg   = M.findWithDefault (lgSingleton qpn  Nothing) qpn  $ vsLinks vs
          lg'  = M.findWithDefault (lgSingleton qpn' Nothing) qpn' $ vsLinks vs
      lg'' <- lift' $ lgMerge parents lg lg'
      updateLinkGroup lg''
    go (Flagged fn _ t f) = do
      vs <- get
      case M.lookup fn (vsFlags vs) of
        Nothing    -> return () -- flag assignment not yet known
        Just True  -> linkDeps (F fn:parents) pp' t
        Just False -> linkDeps (F fn:parents) pp' f
    go (Stanza sn t) = do
      vs <- get
      case M.lookup sn (vsStanzas vs) of
        Nothing    -> return () -- stanza assignment not yet known
        Just True  -> linkDeps (S sn:parents) pp' t
        Just False -> return () -- stanza not enabled; no new deps

pickFlag :: QFN -> Bool -> UpdateState ()
pickFlag qfn b = do
    modify $ \vs -> vs { vsFlags = M.insert qfn b (vsFlags vs) }
    verifyFlag qfn
    linkNewDeps (F qfn) b

pickStanza :: QSN -> Bool -> UpdateState ()
pickStanza qsn b = do
    modify $ \vs -> vs { vsStanzas = M.insert qsn b (vsStanzas vs) }
    verifyStanza qsn
    linkNewDeps (S qsn) b

linkNewDeps :: Var QPN -> Bool -> UpdateState ()
linkNewDeps var b = do
    vs <- get
    let (qpn@(Q pp pn), Just i) = varPI var
        PInfo deps _ _          = vsIndex vs ! pn ! i
        qdeps                   = qualifyDeps qpn deps
        lg                      = vsLinks vs ! qpn
        (parents, newDeps)      = findNewDeps vs qdeps
        linkedTo                = S.delete pp (lgMembers lg)
    forM_ (S.toList linkedTo) $ \pp' -> linkDeps (P qpn : parents) pp' newDeps
  where
    findNewDeps :: ValidateState -> FlaggedDeps comp QPN -> ([Var QPN], FlaggedDeps Component QPN)
    findNewDeps vs = concatMapUnzip (findNewDeps' vs)

    findNewDeps' :: ValidateState -> FlaggedDep comp QPN -> ([Var QPN], FlaggedDeps Component QPN)
    findNewDeps' _  (Simple _ _)        = ([], [])
    findNewDeps' vs (Flagged qfn _ t f) =
      case (F qfn == var, M.lookup qfn (vsFlags vs)) of
        (True, _)    -> ([F qfn], if b then t else f)
        (_, Nothing) -> ([], []) -- not yet known
        (_, Just b') -> let (parents, deps) = findNewDeps vs (if b' then t else f)
                        in (F qfn:parents, deps)
    findNewDeps' vs (Stanza qsn t) =
      case (S qsn == var, M.lookup qsn (vsStanzas vs)) of
        (True, _)    -> ([S qsn], if b then t else [])
        (_, Nothing) -> ([], []) -- not yet known
        (_, Just b') -> let (parents, deps) = findNewDeps vs (if b' then t else [])
                        in (S qsn:parents, deps)

updateLinkGroup :: LinkGroup -> UpdateState ()
updateLinkGroup lg = do
    verifyLinkGroup lg
    modify $ \vs -> vs {
        vsLinks =           M.fromList (map aux (S.toList (lgMembers lg)))
                  `M.union` vsLinks vs
      }
  where
    aux pp = (Q pp (lgPackage lg), lg)

{-------------------------------------------------------------------------------
  Verification
-------------------------------------------------------------------------------}

verifyLinkGroup :: LinkGroup -> UpdateState ()
verifyLinkGroup lg =
    case lgInstance lg of
      -- No instance picked yet. Nothing to verify
      Nothing ->
        return ()

      -- We picked an instance. Verify flags and stanzas
      -- TODO: The enumeration of OptionalStanza names is very brittle;
      -- if a constructor is added to the datatype we won't notice it here
      Just i -> do
        vs <- get
        let PInfo _deps finfo _ = vsIndex vs ! lgPackage lg ! i
            flags   = M.keys finfo
            stanzas = [TestStanzas, BenchStanzas]
        forM_ flags $ \fn -> do
          let flag = FN (PI (lgPackage lg) i) fn
          verifyFlag' flag lg
        forM_ stanzas $ \sn -> do
          let stanza = SN (PI (lgPackage lg) i) sn
          verifyStanza' stanza lg

verifyFlag :: QFN -> UpdateState ()
verifyFlag (FN (PI qpn@(Q _pp pn) i) fn) = do
    vs <- get
    -- We can only pick a flag after picking an instance; link group must exist
    verifyFlag' (FN (PI pn i) fn) (vsLinks vs ! qpn)

verifyStanza :: QSN -> UpdateState ()
verifyStanza (SN (PI qpn@(Q _pp pn) i) sn) = do
    vs <- get
    -- We can only pick a stanza after picking an instance; link group must exist
    verifyStanza' (SN (PI pn i) sn) (vsLinks vs ! qpn)

verifyFlag' :: FN PN -> LinkGroup -> UpdateState ()
verifyFlag' (FN (PI pn i) fn) lg = do
    vs <- get
    let flags = map (\pp' -> FN (PI (Q pp' pn) i) fn) (S.toList (lgMembers lg))
        vals  = map (`M.lookup` vsFlags vs) flags
    if allEqual (catMaybes vals) -- We ignore not-yet assigned flags
      then return ()
      else conflict ( S.fromList (map F flags) `S.union` lgConflictSet lg
                    , "flag " ++ show fn ++ " incompatible"
                    )

verifyStanza' :: SN PN -> LinkGroup -> UpdateState ()
verifyStanza' (SN (PI pn i) sn) lg = do
    vs <- get
    let stanzas = map (\pp' -> SN (PI (Q pp' pn) i) sn) (S.toList (lgMembers lg))
        vals    = map (`M.lookup` vsStanzas vs) stanzas
    if allEqual (catMaybes vals) -- We ignore not-yet assigned stanzas
      then return ()
      else conflict ( S.fromList (map S stanzas) `S.union` lgConflictSet lg
                    , "stanza " ++ show sn ++ " incompatible"
                    )

{-------------------------------------------------------------------------------
  Link groups
-------------------------------------------------------------------------------}

-- | Set of packages that must be linked together
data LinkGroup = LinkGroup {
      -- | The name of the package of this link group
      lgPackage :: PN

      -- | The version of the package of this link group
      --
      -- We may not know this version yet (if we are constructing link groups
      -- for dependencies)
    , lgInstance :: Maybe I

      -- | The canonical member of this link group (the one where we picked
      -- a concrete instance). Once we have picked a canonical member, all
      -- other packages must link to this one.
    , lgCanon :: Maybe PP

      -- | The members of the link group
    , lgMembers :: Set PP

      -- | The set of variables that should be added to the conflict set if
      -- something goes wrong with this link set (in addition to the members
      -- of the link group itself)
    , lgBlame :: [Var QPN]
    }
    deriving Show

showLinkGroup :: LinkGroup -> String
showLinkGroup lg =
    "{" ++ intercalate "," (map showMember (S.toList (lgMembers lg))) ++ "}"
  where
    showMember :: PP -> String
    showMember pp = (if lgCanon lg == Just pp then "*" else "")
                 ++ case lgInstance lg of
                      Nothing -> showQPN (qpn pp)
                      Just i  -> showPI (PI (qpn pp) i)

    qpn :: PP -> QPN
    qpn pp = Q pp (lgPackage lg)

lgSingleton :: QPN -> Maybe I -> LinkGroup
lgSingleton (Q pp pn) inst = LinkGroup {
      lgPackage  = pn
    , lgInstance = inst
    , lgCanon    = Nothing
    , lgMembers  = S.singleton pp
    , lgBlame    = []
    }

lgMerge :: [Var QPN] -> LinkGroup -> LinkGroup -> Either Conflict LinkGroup
lgMerge blame lg lg' = do
    canon <- pick (lgCanon    lg) (lgCanon    lg')
    inst  <- pick (lgInstance lg) (lgInstance lg')
    return LinkGroup {
        lgPackage  = lgPackage lg
      , lgInstance = inst
      , lgCanon    = canon
      , lgMembers  = lgMembers lg `S.union` lgMembers lg'
      , lgBlame    = blame ++ lgBlame lg ++ lgBlame lg'
      }
  where
    pick :: Eq a => Maybe a -> Maybe a -> Either Conflict (Maybe a)
    pick Nothing  Nothing  = Right Nothing
    pick (Just x) Nothing  = Right $ Just x
    pick Nothing  (Just y) = Right $ Just y
    pick (Just x) (Just y) =
      if x == y then Right $ Just x
                else Left ( S.unions [
                               S.fromList blame
                             , lgConflictSet lg
                             , lgConflictSet lg'
                             ]
                          ,    "cannot merge "++ showLinkGroup lg
                            ++ " and " ++ showLinkGroup lg'
                          )

lgConflictSet :: LinkGroup -> ConflictSet QPN
lgConflictSet lg = S.fromList (map aux (S.toList (lgMembers lg)) ++ lgBlame lg)
  where
    aux pp = P (Q pp (lgPackage lg))

lgAddMember :: QPN -> I -> LinkGroup -> Either Conflict LinkGroup
lgAddMember qpn@(Q pp pn) i lg = do
    assert (pn == lgPackage lg) $ Right ()
    let lg' = lg { lgMembers = S.insert pp (lgMembers lg) }
    case lgInstance lg of
      Nothing             -> Right $ lg' { lgInstance = Just i }
      Just i' | i == i'   -> Right lg'
              | otherwise -> Left ( lgConflictSet lg'
                                  ,    "cannot add " ++ showQPN qpn
                                    ++ " to " ++ showLinkGroup lg
                                  )

{-------------------------------------------------------------------------------
  Auxiliary
-------------------------------------------------------------------------------}

-- | Extract the package instance from a Var
varPI :: Var QPN -> (QPN, Maybe I)
varPI (P qpn)               = (qpn, Nothing)
varPI (F (FN (PI qpn i) _)) = (qpn, Just i)
varPI (S (SN (PI qpn i) _)) = (qpn, Just i)

allEqual :: Eq a => [a] -> Bool
allEqual []       = True
allEqual [_]      = True
allEqual (x:y:ys) = x == y && allEqual (y:ys)

concatMapUnzip :: (a -> ([b], [c])) -> [a] -> ([b], [c])
concatMapUnzip f = (\(xs, ys) -> (concat xs, concat ys)) . unzip . map f
