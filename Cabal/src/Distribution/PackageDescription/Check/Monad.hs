{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Distribution.PackageDescription.Check.Monad
-- Copyright   :  Francesco Ariis 2022
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Primitives for package checking: check types and monadic interface.
-- Having these primitives in a different module allows us to appropriately
-- limit/manage the interface to suit checking needs.
module Distribution.PackageDescription.Check.Monad
  ( -- * Types and constructors
    CheckM (..)
  , execCheckM
  , CheckInterface (..)
  , CheckPackageContentOps (..)
  , CheckPreDistributionOps (..)
  , TargetAnnotation (..)
  , PackageCheck (..)
  , CheckExplanation (..)
  , CEType (..)
  , WarnLang (..)
  , CheckCtx (..)
  , pristineCheckCtx
  , initCheckCtx
  , PNames (..)

    -- * Operations
  , ppPackageCheck
  , isHackageDistError
  , asksCM
  , localCM
  , checkP
  , checkPkg
  , liftInt
  , tellP
  , checkSpecVer
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.CabalSpecVersion (CabalSpecVersion)
import Distribution.Package (packageName)
import Distribution.PackageDescription.Check.Warning
import Distribution.Simple.BuildToolDepends (desugarBuildToolSimple)
import Distribution.Simple.Glob (Glob, GlobResult)
import Distribution.Types.ExeDependency (ExeDependency)
import Distribution.Types.GenericPackageDescription
import Distribution.Types.LegacyExeDependency (LegacyExeDependency)
import Distribution.Types.PackageDescription (package, specVersion)
import Distribution.Types.PackageId (PackageIdentifier)
import Distribution.Types.UnqualComponentName

import qualified Control.Monad.Reader as Reader
import qualified Control.Monad.Trans.Class as Trans
import qualified Control.Monad.Writer as Writer
import qualified Data.ByteString.Lazy as BS
import qualified Data.Set as Set

import Control.Monad

-- Monadic interface for for Distribution.PackageDescription.Check.
--
-- Monadic checking allows us to have a fine grained control on checks
-- (e.g. omitting warning checks in certain situations).

-- * Interfaces

--

-- | Which interface to we have available/should we use? (to perform: pure
-- checks, package checks, pre-distribution checks.)
data CheckInterface m = CheckInterface
  { ciPureChecks :: Bool
  , -- Perform pure checks?
    ciPackageOps :: Maybe (CheckPackageContentOps m)
  , -- If you want to perform package contents
    -- checks, provide an interface.
    ciPreDistOps :: Maybe (CheckPreDistributionOps m)
    -- If you want to work-tree checks, provide
    -- an interface.
  }

-- | A record of operations needed to check the contents of packages.
-- Abstracted over `m` to provide flexibility (could be IO, a .tar.gz
-- file, etc).
data CheckPackageContentOps m = CheckPackageContentOps
  { doesFileExist :: FilePath -> m Bool
  , doesDirectoryExist :: FilePath -> m Bool
  , getDirectoryContents :: FilePath -> m [FilePath]
  , getFileContents :: FilePath -> m BS.ByteString
  }

-- | A record of operations needed to check contents *of the work tree*
-- (compare it with 'CheckPackageContentOps'). This is still `m` abstracted
-- in case in the future we can obtain the same infos other than from IO
-- (e.g. a VCS work tree).
data CheckPreDistributionOps m = CheckPreDistributionOps
  { runDirFileGlobM :: FilePath -> Glob -> m [GlobResult FilePath]
  , getDirectoryContentsM :: FilePath -> m [FilePath]
  }

-- | Context to perform checks (will be the Reader part in your monad).
data CheckCtx m = CheckCtx
  { ccInterface :: CheckInterface m
  , -- Interface for checks.

    -- Contextual infos for checks.
    ccFlag :: Bool
  , -- Are we under a user flag?

    -- Convenience bits that we prefer to carry
    -- in our Reader monad instead of passing it
    -- via ->, as they are often useful and often
    -- in deeply nested places in the GPD tree.
    ccSpecVersion :: CabalSpecVersion
  , -- Cabal version.
    ccDesugar :: LegacyExeDependency -> Maybe ExeDependency
  , -- A desugaring function from
    -- Distribution.Simple.BuildToolDepends
    -- (desugarBuildToolSimple). Again since it
    -- eats PackageName and a list of executable
    -- names, it is more convenient to pass it
    -- via Reader.
    ccNames :: PNames
    -- Various names (id, libs, execs, tests,
    -- benchs), convenience.
  }

-- | Creates a pristing 'CheckCtx'. With pristine we mean everything that
-- can be deduced by GPD but *not* user flags information.
pristineCheckCtx
  :: Monad m
  => CheckInterface m
  -> GenericPackageDescription
  -> CheckCtx m
pristineCheckCtx ci gpd =
  let ens = map fst (condExecutables gpd)
   in CheckCtx
        ci
        False
        (specVersion . packageDescription $ gpd)
        (desugarBuildToolSimple (packageName gpd) ens)
        (initPNames gpd)

-- | Adds useful bits to 'CheckCtx' (as now, whether we are operating under
-- a user off-by-default flag).
initCheckCtx :: Monad m => TargetAnnotation a -> CheckCtx m -> CheckCtx m
initCheckCtx t c = c{ccFlag = taPackageFlag t}

-- | 'TargetAnnotation' collects contextual information on the target we are
-- realising: a buildup of the various slices of the target (a library,
-- executable, etc. — is a monoid) whether we are under an off-by-default
-- package flag.
data TargetAnnotation a = TargetAnnotation
  { taTarget :: a
  , -- The target we are building (lib, exe, etc.)
    taPackageFlag :: Bool
    -- Whether we are under an off-by-default package flag.
  }
  deriving (Show, Eq, Ord)

-- | A collection os names, shipping tuples around is annoying.
data PNames = PNames
  { pnPackageId :: PackageIdentifier -- Package ID…
  -- … and a bunch of lib, exe, test, bench names.
  , pnSubLibs :: [UnqualComponentName]
  , pnExecs :: [UnqualComponentName]
  , pnTests :: [UnqualComponentName]
  , pnBenchs :: [UnqualComponentName]
  }

-- | Init names from a GPD.
initPNames :: GenericPackageDescription -> PNames
initPNames gpd =
  PNames
    (package . packageDescription $ gpd)
    (map fst $ condSubLibraries gpd)
    (map fst $ condExecutables gpd)
    (map fst $ condTestSuites gpd)
    (map fst $ condBenchmarks gpd)

-- | Check monad, carrying a context, collecting 'PackageCheck's.
-- Using Set for writer (automatic sort) is useful for output stability
-- on different platforms.
-- It is nothing more than a monad stack with Reader+Writer.
-- `m` is the monad that could be used to do package/file checks.
newtype CheckM m a
  = CheckM
      ( Reader.ReaderT
          (CheckCtx m)
          ( Writer.WriterT
              (Set.Set PackageCheck)
              m
          )
          a
      )
  deriving (Functor, Applicative, Monad)

-- Not autoderiving MonadReader and MonadWriter gives us better
-- control on the interface of CheckM.

-- | Execute a CheckM monad, leaving `m [PackageCheck]` which can be
-- run in the appropriate `m` environment (IO, pure, …).
execCheckM :: Monad m => CheckM m () -> CheckCtx m -> m [PackageCheck]
execCheckM (CheckM rwm) ctx =
  let wm = Reader.runReaderT rwm ctx
      m = Writer.execWriterT wm
   in Set.toList <$> m

-- | As 'checkP' but always succeeding.
tellP :: Monad m => PackageCheck -> CheckM m ()
tellP = checkP True

-- | Add a package warning withoutu performing any check.
tellCM :: Monad m => PackageCheck -> CheckM m ()
tellCM ck = do
  cf <- asksCM ccFlag
  unless
    (cf && canSkip ck)
    -- Do not push this message if the warning is not severe *and*
    -- we are under a non-default package flag.
    (CheckM . Writer.tell $ Set.singleton ck)
  where
    -- Check if we can skip this error if we are under a
    -- non-default user flag.
    canSkip :: PackageCheck -> Bool
    canSkip wck = not (isSevereLocal wck) || isErrAllowable wck

    isSevereLocal :: PackageCheck -> Bool
    isSevereLocal (PackageBuildImpossible _) = True
    isSevereLocal (PackageBuildWarning _) = True
    isSevereLocal (PackageDistSuspicious _) = False
    isSevereLocal (PackageDistSuspiciousWarn _) = False
    isSevereLocal (PackageDistInexcusable _) = True

    -- There are some errors which, even though severe, will
    -- be allowed by Hackage *if* under a non-default flag.
    isErrAllowable :: PackageCheck -> Bool
    isErrAllowable c = case extractCheckExplantion c of
      (WErrorUnneeded _) -> True
      (JUnneeded _) -> True
      (FDeferTypeErrorsUnneeded _) -> True
      (DynamicUnneeded _) -> True
      (ProfilingUnneeded _) -> True
      _ -> False

-- | Lift a monadic computation to CM.
liftCM :: Monad m => m a -> CheckM m a
liftCM ma = CheckM . Trans.lift . Trans.lift $ ma

-- | Lift a monadic action via an interface. Missing interface, no action.
liftInt
  :: forall m i
   . Monad m
  => (CheckInterface m -> Maybe (i m))
  -- Check interface, may or may not exist. If it does not,
  -- the check simply will not be performed.
  -> (i m -> m [PackageCheck])
  -- The actual check to perform with the above-mentioned
  -- interface. Note the [] around `PackageCheck`, this is
  -- meant to perform/collect multiple checks.
  -> CheckM m ()
liftInt acc f = do
  ops <- asksCM (acc . ccInterface)
  maybe (return ()) l ops
  where
    l :: i m -> CheckM m ()
    l wi = do
      cks <- liftCM (f wi)
      mapM_ (check True) cks

-- | Most basic check function. You do not want to export this, rather export
-- “smart” functions (checkP, checkPkg) to enforce relevant properties.
check
  :: Monad m
  => Bool -- Is there something to warn about?
  -> PackageCheck -- Warn message.
  -> CheckM m ()
check True ck = tellCM ck
check False _ = return ()

-- | Pure check not requiring IO or other interfaces.
checkP
  :: Monad m
  => Bool -- Is there something to warn about?
  -> PackageCheck -- Warn message.
  -> CheckM m ()
checkP b ck = do
  pb <- asksCM (ciPureChecks . ccInterface)
  when pb (check b ck)

-- Check with 'CheckPackageContentOps' operations (i.e. package file checks).
--
checkPkg
  :: forall m
   . Monad m
  => (CheckPackageContentOps m -> m Bool)
  -- Actual check to perform with CPC interface
  -> PackageCheck
  -- Warn message.
  -> CheckM m ()
checkPkg f ck = checkInt ciPackageOps f ck

-- | Generalised version for checks that need an interface. We pass a Reader
-- accessor to such interface ‘i’, a check function.
checkIntDep
  :: forall m i
   . Monad m
  => (CheckInterface m -> Maybe (i m))
  -- Check interface, may or may not exist. If it does not,
  -- the check simply will not be performed.
  -> (i m -> m (Maybe PackageCheck))
  -- The actual check to perform (single check).
  -> CheckM m ()
checkIntDep acc mck = do
  po <- asksCM (acc . ccInterface)
  maybe (return ()) (lc . mck) po
  where
    lc :: Monad m => m (Maybe PackageCheck) -> CheckM m ()
    lc wmck = do
      b <- liftCM wmck
      maybe (return ()) (check True) b

-- | As 'checkIntDep', but 'PackageCheck' does not depend on the monadic
-- computation.
checkInt
  :: forall m i
   . Monad m
  => (CheckInterface m -> Maybe (i m))
  -- Where to get the interface (if available).
  -> (i m -> m Bool)
  -- Condition to check
  -> PackageCheck
  -- Warning message to add (does not depend on `m`).
  -> CheckM m ()
checkInt acc f ck =
  checkIntDep
    acc
    ( \ops -> do
        b <- f ops
        if b
          then return $ Just ck
          else return Nothing
    )

-- | `local` (from Control.Monad.Reader) for CheckM.
localCM :: Monad m => (CheckCtx m -> CheckCtx m) -> CheckM m () -> CheckM m ()
localCM cf (CheckM im) = CheckM $ Reader.local cf im

-- | `ask` (from Control.Monad.Reader) for CheckM.
asksCM :: Monad m => (CheckCtx m -> a) -> CheckM m a
asksCM f = CheckM $ Reader.asks f

-- As checkP, but with an additional condition: the check will be performed
-- only if our spec version is < `vc`.
checkSpecVer
  :: Monad m
  => CabalSpecVersion -- Perform this check only if our
  -- spec version is < than this.
  -> Bool -- Check condition.
  -> PackageCheck -- Check message.
  -> CheckM m ()
checkSpecVer vc cond c = do
  vp <- asksCM ccSpecVersion
  unless (vp >= vc) (checkP cond c)
