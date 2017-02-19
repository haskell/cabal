{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Distribution.Solver.Modular.Validate (validateTree) where

-- Validation of the tree.
--
-- The task here is to make sure all constraints hold. After validation, any
-- assignment returned by exploration of the tree should be a complete valid
-- assignment, i.e., actually constitute a solution.

import Control.Applicative
import Control.Monad.Reader hiding (sequence)
import Data.List as L
import Data.Map as M
import Data.Set as S
import Data.Traversable
import Prelude hiding (sequence)

import Language.Haskell.Extension (Extension, Language)

import Distribution.Compiler (CompilerInfo(..))

import Distribution.Solver.Modular.Assignment
import Distribution.Solver.Modular.Dependency
import Distribution.Solver.Modular.Flag
import Distribution.Solver.Modular.Index
import Distribution.Solver.Modular.Package
import Distribution.Solver.Modular.Tree
import Distribution.Solver.Modular.Version (VR)
import qualified Distribution.Solver.Modular.WeightedPSQ as W

import Distribution.Solver.Types.ComponentDeps (Component)

import Distribution.Solver.Types.PackagePath
import Distribution.Solver.Types.PkgConfigDb (PkgConfigDb, pkgConfigPkgIsPresent)

-- In practice, most constraints are implication constraints (IF we have made
-- a number of choices, THEN we also have to ensure that). We call constraints
-- that for which the preconditions are fulfilled ACTIVE. We maintain a set
-- of currently active constraints that we pass down the node.
--
-- We aim at detecting inconsistent states as early as possible.
--
-- Whenever we make a choice, there are two things that need to happen:
--
--   (1) We must check that the choice is consistent with the currently
--       active constraints.
--
--   (2) The choice increases the set of active constraints. For the new
--       active constraints, we must check that they are consistent with
--       the current state.
--
-- We can actually merge (1) and (2) by saying the the current choice is
-- a new active constraint, fixing the choice.
--
-- If a test fails, we have detected an inconsistent state. We can
-- disable the current subtree and do not have to traverse it any further.
--
-- We need a good way to represent the current state, i.e., the current
-- set of active constraints. Since the main situation where we have to
-- search in it is (1), it seems best to store the state by package: for
-- every package, we store which versions are still allowed. If for any
-- package, we have inconsistent active constraints, we can also stop.
-- This is a particular way to read task (2):
--
--   (2, weak) We only check if the new constraints are consistent with
--       the choices we've already made, and add them to the active set.
--
--   (2, strong) We check if the new constraints are consistent with the
--       choices we've already made, and the constraints we already have.
--
-- It currently seems as if we're implementing the weak variant. However,
-- when used together with 'preferEasyGoalChoices', we will find an
-- inconsistent state in the very next step.
--
-- What do we do about flags?
--
-- Like for packages, we store the flag choices we have already made.
-- Now, regarding (1), we only have to test whether we've decided the
-- current flag before. Regarding (2), the interesting bit is in discovering
-- the new active constraints. To this end, we look up the constraints for
-- the package the flag belongs to, and traverse its flagged dependencies.
-- Wherever we find the flag in question, we start recording dependencies
-- underneath as new active dependencies. If we encounter other flags, we
-- check if we've chosen them already and either proceed or stop.

-- | The state needed during validation.
data ValidateState = VS {
  supportedExt  :: Extension -> Bool,
  supportedLang :: Language  -> Bool,
  presentPkgs   :: PkgconfigName -> VR  -> Bool,
  index :: Index,
  saved :: Map QPN (FlaggedDeps Component QPN), -- saved, scoped, dependencies
  pa    :: PreAssignment,
  qualifyOptions :: QualifyOptions
}

newtype Validate a = Validate (Reader ValidateState a)
  deriving (Functor, Applicative, Monad, MonadReader ValidateState)

runValidate :: Validate a -> ValidateState -> a
runValidate (Validate r) = runReader r

validate :: Tree d c -> Validate (Tree d c)
validate = cata go
  where
    go :: TreeF d c (Validate (Tree d c)) -> Validate (Tree d c)

    go (PChoiceF qpn rdm gr       ts) = PChoice qpn rdm gr <$> sequence (W.mapWithKey (goP qpn) ts)
    go (FChoiceF qfn rdm gr b m d ts) =
      do
        -- Flag choices may occur repeatedly (because they can introduce new constraints
        -- in various places). However, subsequent choices must be consistent. We thereby
        -- collapse repeated flag choice nodes.
        PA _ pfa _ <- asks pa -- obtain current flag-preassignment
        case M.lookup qfn pfa of
          Just rb -> -- flag has already been assigned; collapse choice to the correct branch
                     case W.lookup rb ts of
                       Just t  -> goF qfn rb t
                       Nothing -> return $ Fail (varToConflictSet (F qfn)) (MalformedFlagChoice qfn)
          Nothing -> -- flag choice is new, follow both branches
                     FChoice qfn rdm gr b m d <$> sequence (W.mapWithKey (goF qfn) ts)
    go (SChoiceF qsn rdm gr b   ts) =
      do
        -- Optional stanza choices are very similar to flag choices.
        PA _ _ psa <- asks pa -- obtain current stanza-preassignment
        case M.lookup qsn psa of
          Just rb -> -- stanza choice has already been made; collapse choice to the correct branch
                     case W.lookup rb ts of
                       Just t  -> goS qsn rb t
                       Nothing -> return $ Fail (varToConflictSet (S qsn)) (MalformedStanzaChoice qsn)
          Nothing -> -- stanza choice is new, follow both branches
                     SChoice qsn rdm gr b <$> sequence (W.mapWithKey (goS qsn) ts)

    -- We don't need to do anything for goal choices or failure nodes.
    go (GoalChoiceF rdm           ts) = GoalChoice rdm <$> sequence ts
    go (DoneF       rdm s           ) = pure (Done rdm s)
    go (FailF    c fr               ) = pure (Fail c fr)

    -- What to do for package nodes ...
    goP :: QPN -> POption -> Validate (Tree d c) -> Validate (Tree d c)
    goP qpn@(Q _pp pn) (POption i _) r = do
      PA ppa pfa psa <- asks pa    -- obtain current preassignment
      extSupported   <- asks supportedExt  -- obtain the supported extensions
      langSupported  <- asks supportedLang -- obtain the supported languages
      pkgPresent     <- asks presentPkgs -- obtain the present pkg-config pkgs
      idx            <- asks index -- obtain the index
      svd            <- asks saved -- obtain saved dependencies
      qo             <- asks qualifyOptions
      -- obtain dependencies and index-dictated exclusions introduced by the choice
      let (PInfo deps _ mfr) = idx ! pn ! i
      -- qualify the deps in the current scope
      let qdeps = qualifyDeps qo qpn deps
      -- the new active constraints are given by the instance we have chosen,
      -- plus the dependency information we have for that instance
      -- TODO: is the False here right?
      let newactives = Dep False {- not exe -} qpn (Fixed i (P qpn)) : L.map (resetVar (P qpn)) (extractDeps pfa psa qdeps)
      -- We now try to extend the partial assignment with the new active constraints.
      let mnppa = extend extSupported langSupported pkgPresent (P qpn) ppa newactives
      -- In case we continue, we save the scoped dependencies
      let nsvd = M.insert qpn qdeps svd
      case mfr of
        Just fr -> -- The index marks this as an invalid choice. We can stop.
                   return (Fail (varToConflictSet (P qpn)) fr)
        _       -> case mnppa of
                     Left (c, d) -> -- We have an inconsistency. We can stop.
                                    return (Fail c (Conflicting d))
                     Right nppa  -> -- We have an updated partial assignment for the recursive validation.
                                    local (\ s -> s { pa = PA nppa pfa psa, saved = nsvd }) r

    -- What to do for flag nodes ...
    goF :: QFN -> Bool -> Validate (Tree d c) -> Validate (Tree d c)
    goF qfn@(FN (PI qpn _i) _f) b r = do
      PA ppa pfa psa <- asks pa -- obtain current preassignment
      extSupported   <- asks supportedExt  -- obtain the supported extensions
      langSupported  <- asks supportedLang -- obtain the supported languages
      pkgPresent     <- asks presentPkgs   -- obtain the present pkg-config pkgs
      svd <- asks saved         -- obtain saved dependencies
      -- Note that there should be saved dependencies for the package in question,
      -- because while building, we do not choose flags before we see the packages
      -- that define them.
      let qdeps = svd ! qpn
      -- We take the *saved* dependencies, because these have been qualified in the
      -- correct scope.
      --
      -- Extend the flag assignment
      let npfa = M.insert qfn b pfa
      -- We now try to get the new active dependencies we might learn about because
      -- we have chosen a new flag.
      let newactives = extractNewDeps (F qfn) b npfa psa qdeps
      -- As in the package case, we try to extend the partial assignment.
      case extend extSupported langSupported pkgPresent (F qfn) ppa newactives of
        Left (c, d) -> return (Fail c (Conflicting d)) -- inconsistency found
        Right nppa  -> local (\ s -> s { pa = PA nppa npfa psa }) r

    -- What to do for stanza nodes (similar to flag nodes) ...
    goS :: QSN -> Bool -> Validate (Tree d c) -> Validate (Tree d c)
    goS qsn@(SN (PI qpn _i) _f) b r = do
      PA ppa pfa psa <- asks pa -- obtain current preassignment
      extSupported   <- asks supportedExt  -- obtain the supported extensions
      langSupported  <- asks supportedLang -- obtain the supported languages
      pkgPresent     <- asks presentPkgs -- obtain the present pkg-config pkgs
      svd <- asks saved         -- obtain saved dependencies
      -- Note that there should be saved dependencies for the package in question,
      -- because while building, we do not choose flags before we see the packages
      -- that define them.
      let qdeps = svd ! qpn
      -- We take the *saved* dependencies, because these have been qualified in the
      -- correct scope.
      --
      -- Extend the flag assignment
      let npsa = M.insert qsn b psa
      -- We now try to get the new active dependencies we might learn about because
      -- we have chosen a new flag.
      let newactives = extractNewDeps (S qsn) b pfa npsa qdeps
      -- As in the package case, we try to extend the partial assignment.
      case extend extSupported langSupported pkgPresent (S qsn) ppa newactives of
        Left (c, d) -> return (Fail c (Conflicting d)) -- inconsistency found
        Right nppa  -> local (\ s -> s { pa = PA nppa pfa npsa }) r

-- | We try to extract as many concrete dependencies from the given flagged
-- dependencies as possible. We make use of all the flag knowledge we have
-- already acquired.
extractDeps :: FAssignment -> SAssignment -> FlaggedDeps comp QPN -> [Dep QPN]
extractDeps fa sa deps = do
  d <- deps
  case d of
    Simple sd _         -> return sd
    Flagged qfn _ td fd -> case M.lookup qfn fa of
                             Nothing    -> mzero
                             Just True  -> extractDeps fa sa td
                             Just False -> extractDeps fa sa fd
    Stanza qsn td       -> case M.lookup qsn sa of
                             Nothing    -> mzero
                             Just True  -> extractDeps fa sa td
                             Just False -> []

-- | We try to find new dependencies that become available due to the given
-- flag or stanza choice. We therefore look for the choice in question, and then call
-- 'extractDeps' for everything underneath.
extractNewDeps :: Var QPN -> Bool -> FAssignment -> SAssignment -> FlaggedDeps comp QPN -> [Dep QPN]
extractNewDeps v b fa sa = go
  where
    go :: FlaggedDeps comp QPN -> [Dep QPN] -- Type annotation necessary (polymorphic recursion)
    go deps = do
      d <- deps
      case d of
        Simple _ _           -> mzero
        Flagged qfn' _ td fd
          | v == F qfn'      -> L.map (resetVar v) $
                                if b then extractDeps fa sa td else extractDeps fa sa fd
          | otherwise        -> case M.lookup qfn' fa of
                                  Nothing    -> mzero
                                  Just True  -> go td
                                  Just False -> go fd
        Stanza qsn' td
          | v == S qsn'      -> L.map (resetVar v) $
                                if b then extractDeps fa sa td else []
          | otherwise        -> case M.lookup qsn' sa of
                                  Nothing    -> mzero
                                  Just True  -> go td
                                  Just False -> []

-- | Interface.
validateTree :: CompilerInfo -> Index -> PkgConfigDb -> Tree d c -> Tree d c
validateTree cinfo idx pkgConfigDb t = runValidate (validate t) VS {
    supportedExt   = maybe (const True) -- if compiler has no list of extensions, we assume everything is supported
                           (\ es -> let s = S.fromList es in \ x -> S.member x s)
                           (compilerInfoExtensions cinfo)
  , supportedLang  = maybe (const True)
                           (flip L.elem) -- use list lookup because language list is small and no Ord instance
                           (compilerInfoLanguages  cinfo)
  , presentPkgs    = pkgConfigPkgIsPresent pkgConfigDb
  , index          = idx
  , saved          = M.empty
  , pa             = PA M.empty M.empty M.empty
  , qualifyOptions = defaultQualifyOptions idx
  }
