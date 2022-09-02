{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.PackageDescription.Check
-- Copyright   :  Lennart Kolmodin 2008, Francesco Ariis 2022
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This has code for checking for various problems in packages. There is one
-- set of checks that just looks at a 'PackageDescription' in isolation and
-- another set of checks that also looks at files in the package. Some of the
-- checks are basic sanity checks, others are portability standards that we'd
-- like to encourage. There is a 'PackageCheck' type that distinguishes the
-- different kinds of checks so we can see which ones are appropriate to report
-- in different situations. This code gets used when configuring a package when
-- we consider only basic problems. The higher standard is used when
-- preparing a source tarball and by Hackage when uploading new packages. The
-- reason for this is that we want to hold packages that are expected to be
-- distributed to a higher standard than packages that are only ever expected
-- to be used on the author's own environment.
module Distribution.PackageDescription.Check
  ( -- * Package Checking
    CheckExplanation (..)
  , PackageCheck (..)
  , checkPackage
  , checkConfiguredPackage
  , wrapParseWarning
  , ppPackageCheck
  , isHackageDistError

    -- ** Checking package contents
  , checkPackageFiles
  , checkPackageContent
  , CheckPackageContentOps (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Data.List (group)
import Distribution.CabalSpecVersion
import Distribution.Compat.Lens
import Distribution.Compiler
import Distribution.License
import Distribution.ModuleName (ModuleName)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Check.Prim
import Distribution.Parsec.Warning (PWarning)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.BuildPaths (autogenPackageInfoModuleName, autogenPathsModuleName)
import Distribution.Simple.CCompiler
import Distribution.Simple.Glob
import Distribution.Simple.Utils hiding (findPackageDesc, notice)
import Distribution.System
import Distribution.Types.PackageName.Magic
import Distribution.Utils.Generic (isAscii)
import Distribution.Utils.Path
import Distribution.Verbosity
import Distribution.Version
import Language.Haskell.Extension
import System.FilePath
  ( splitDirectories
  , splitExtension
  , splitPath
  , takeExtension
  , takeFileName
  , (<.>)
  , (</>)
  )

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map as Map
import qualified Distribution.SPDX as SPDX
import qualified System.Directory as System

import qualified System.Directory (getDirectoryContents)
import qualified System.FilePath.Windows as FilePath.Windows (isValid)

import qualified Data.Set as Set
import qualified Distribution.Utils.ShortText as ShortText

import qualified Distribution.Types.BuildInfo.Lens as L
import qualified Distribution.Types.GenericPackageDescription.Lens as L

import Control.Monad

-- $setup
-- >>> import Control.Arrow ((&&&))

-- ☞ N.B.
--
-- Part of the tools/scaffold used to perform check is found in
-- Distribution.PackageDescription.Check.Prim. Summary of that module (for
-- how we use it here):
-- 1. we work inside a 'Check m a' monad (where `m` is an abstraction to
--    run non-pure checks);
-- 2. 'checkP', 'checkPre' functions perform checks (respectively pure and
--    non-pure);
-- 3. 'PackageCheck' and 'CheckExplanation' are types for warning severity
--    and description.


-- ------------------------------------------------------------
-- * Checking interface
-- ------------------------------------------------------------

-- | 'checkPackagePrim' is the most general way to invoke package checks.
-- We pass to it two interfaces (one to check contents of packages, the
-- other to inspect working tree for orphan files) and before that a
-- Boolean to indicate whether we want pure checks or not. Based on these
-- parameters, some checks will be performed, some omitted.
-- Generality over @m@ means we could do non pure checks in monads other
-- than IO (e.g. a virtual filesystem, like a zip file, a VCS filesystem,
-- etc).
checkPackagePrim :: Monad m =>
        Bool ->                                -- Perform pure checks?
        Maybe (CheckPackageContentOps m) ->    -- Package content interface.
        Maybe (CheckPreDistributionOps m) ->   -- Predist checks interface.
        GenericPackageDescription ->           -- GPD to check.
        m [PackageCheck]
checkPackagePrim b mco mpdo gpd = do
        let cm = checkGenericPackageDescription gpd
            ci = CheckInterface b mco mpdo
            ctx = pristineCheckCtx ci gpd
        execCheckM cm ctx

-- | Check for common mistakes and problems in package descriptions.
--
-- This is the standard collection of checks covering all aspects except
-- for checks that require looking at files within the package. For those
-- see 'checkPackageFiles'.
--
checkPackage :: GenericPackageDescription -> [PackageCheck]
checkPackage gpd = runIdentity $ checkPackagePrim True Nothing Nothing gpd

-- | This function is an oddity due to the historical
-- GenericPackageDescription/PackageDescription split. It is only maintained
-- not to break interface, use `checkPackage` if possible.
checkConfiguredPackage :: PackageDescription -> [PackageCheck]
checkConfiguredPackage pd = checkPackage (pd2gpd pd)

-- | Sanity check things that requires looking at files in the package.
-- This is a generalised version of 'checkPackageFiles' that can work in any
-- monad for which you can provide 'CheckPackageContentOps' operations.
--
-- The point of this extra generality is to allow doing checks in some virtual
-- file system, for example a tarball in memory.
--
checkPackageContent :: Monad m
                    => CheckPackageContentOps m
                    -> GenericPackageDescription
                    -> m [PackageCheck]
checkPackageContent pops gpd = checkPackagePrim False (Just pops) Nothing gpd

-- | Sanity checks that require IO. 'checkPackageFiles' looks at the files
-- in the package and expects to find the package unpacked at the given
-- filepath.
--
checkPackageFiles :: Verbosity ->           -- Glob warn message verbosity.
                     PackageDescription ->
                     FilePath ->            -- Package root.
                     IO [PackageCheck]
checkPackageFiles verbosity gpd root =
        checkPackagePrim False (Just checkFilesIO) (Just checkPreIO)
                         (pd2gpd gpd)
  where
    checkFilesIO = CheckPackageContentOps {
      doesFileExist        = System.doesFileExist                  . relative,
      doesDirectoryExist   = System.doesDirectoryExist             . relative,
      getDirectoryContents = System.Directory.getDirectoryContents . relative,
      getFileContents      = BS.readFile                           . relative
    }

    checkPreIO = CheckPreDistributionOps {
      runDirFileGlobM = \fp g -> runDirFileGlob verbosity (root </> fp) g,
      getDirectoryContentsM = System.Directory.getDirectoryContents . relative
    }

    relative path = root </> path


-- ------------------------------------------------------------
-- * Package description
-- ------------------------------------------------------------

-- Here lies the meat of the module. Starting from 'GenericPackageDescription',
-- we walk the data while doing a number of checks.
--
-- Where applicable we do a full pattern match (if the data changes, code will
-- break: a gentle reminder to add more checks).
-- Pattern matching variables convention: matching accessor + underscore.
-- This way it is easier to see which one we are missing if we run into
-- an “GPD should have 20 arguments but has been given only 19” error.

-- | 'GenericPackageDescription' checks. Remember that for historical quirks
-- in the cabal codebase we have both `GenericPackageDescription` and
-- `PackageDescription` and that PD is both a *field* of GPD and a concept
-- of its own (i.e. a fully realised GPD).
-- In this case we are checking (correctly) GPD, so for target info/checks
-- you should walk condLibrary_ etc. and *not* the (empty) target info in
-- PD. See 'pd2gpd' for a convenient hack when you only have
-- 'PackageDescription'.
--
checkGenericPackageDescription :: Monad m => GenericPackageDescription ->
                                  CheckM m ()
checkGenericPackageDescription
    gpd@(GenericPackageDescription
      packageDescription_ _gpdScannedVersion_ genPackageFlags_
      condLibrary_ condSubLibraries_ condForeignLibs_ condExecutables_
      condTestSuites_ condBenchmarks_)
    = do
        -- § Description and names.
        checkPackageDescription packageDescription_
          -- Targets should be present...
        let condAllLibraries = maybeToList condLibrary_ ++
                                           (map snd condSubLibraries_)
        checkP (and [null condExecutables_, null condTestSuites_,
                     null condBenchmarks_, null condAllLibraries,
                     null condForeignLibs_])
          (PackageBuildImpossible NoTarget)
          -- ... and have unique names (names are not under conditional, it is
          -- appropriate to check here.
        (nsubs, nexes, ntests, nbenchs) <- asksCM
          ((\n -> (pnSubLibs n, pnExecs n,
                   pnTests n, pnBenchs n)) . ccNames)
        let names = concat [nsubs, nexes, ntests, nbenchs]
            dupes = dups names
        checkP (not . null $ dups names)
          (PackageBuildImpossible $ DuplicateSections dupes)
          -- PackageDescription checks.
        checkPackageDescription packageDescription_
          -- Flag names.
        mapM_ checkFlagName genPackageFlags_

        -- § Feature checks.
        checkSpecVer CabalSpecV2_0 (not . null $ condSubLibraries_)
          (PackageDistInexcusable CVMultiLib)
        checkSpecVer CabalSpecV1_8 (not . null $ condTestSuites_)
          (PackageDistInexcusable CVTestSuite)

        -- § Conditional targets
        case condLibrary_ of
          Just cl -> checkCondTarget
                        genPackageFlags_
                        (checkLibrary False)
                        (const id) (mempty, cl)
          Nothing -> return ()
        mapM_ (checkCondTarget genPackageFlags_
                  (checkLibrary False)
                  (\u l -> l {libName = maybeToLibraryName (Just u)}))
              condSubLibraries_
        mapM_ (checkCondTarget genPackageFlags_
                  checkForeignLib
                  (const id))
              condForeignLibs_
        mapM_ (checkCondTarget genPackageFlags_
                  (checkExecutable (package packageDescription_))
                  (const id))
              condExecutables_
        mapM_ (checkCondTarget genPackageFlags_
                  checkTestSuite
                  (\u l -> l {testName = u}))
              condTestSuites_
        mapM_ (checkCondTarget genPackageFlags_
                  checkBenchmark
                  (\u l -> l {benchmarkName = u}))
              condBenchmarks_

        -- For unused flags it is clearer and more convenient to fold the
        -- data rather than walk it, an exception to the rule.
        checkP (decFlags /= usedFlags)
          (PackageDistSuspicious $ DeclaredUsedFlags decFlags usedFlags)

        -- Duplicate modules.
        mapM_ tellP (checkDuplicateModules gpd)

    where
          -- todo is this caught at parse time?
          checkFlagName :: Monad m => PackageFlag -> CheckM m ()
          checkFlagName pf =
            let fn = unFlagName . flagName $ pf

                invalidFlagName ('-':_) = True -- starts with dash
                invalidFlagName cs = any (not . isAscii) cs -- non ASCII
            in checkP (invalidFlagName fn)
                (PackageDistInexcusable $ SuspiciousFlagName [fn])

          decFlags :: Set.Set FlagName
          decFlags  = toSetOf (L.genPackageFlags . traverse . L.flagName) gpd

          usedFlags :: Set.Set FlagName
          usedFlags = mconcat
            [ toSetOf (L.condLibrary      . traverse      . traverseCondTreeV . L._PackageFlag) gpd
            , toSetOf (L.condSubLibraries . traverse . _2 . traverseCondTreeV . L._PackageFlag) gpd
            , toSetOf (L.condForeignLibs  . traverse . _2 . traverseCondTreeV . L._PackageFlag) gpd
            , toSetOf (L.condExecutables  . traverse . _2 . traverseCondTreeV . L._PackageFlag) gpd
            , toSetOf (L.condTestSuites   . traverse . _2 . traverseCondTreeV . L._PackageFlag) gpd
            , toSetOf (L.condBenchmarks   . traverse . _2 . traverseCondTreeV . L._PackageFlag) gpd
            ]

checkPackageDescription :: Monad m => PackageDescription -> CheckM m ()
checkPackageDescription
    pkg@(PackageDescription
      specVersion_ package_ licenseRaw_ licenseFiles_ _copyright_
      maintainer_ _author_ _stability_ testedWith_ _homepage_ _pkgUrl_
      _bugReports_ sourceRepos_ synopsis_ description_ category_
      customFieldsPD_ buildTypeRaw_ setupBuildInfo_ _library_
      _subLibraries_ _executables_ _foreignLibs_ _testSuites_ _benchmarks_
      dataFiles_ dataDir_ extraSrcFiles_ extraTmpFiles_ extraDocFiles_) = do

        -- § Sanity checks.
        checkPackageId package_
        -- TODO `name` is caught at parse level, remove this test.
        let pn = packageName package_
        checkP (null . unPackageName $ pn)
          (PackageBuildImpossible NoNameField)
        -- TODO `version` is caught at parse level, remove this test.
        checkP (nullVersion == packageVersion package_)
          (PackageBuildImpossible NoVersionField)
        -- But it is OK for executables to have the same name.
        nsubs <- asksCM (pnSubLibs . ccNames)
        checkP (any (== prettyShow pn) (prettyShow <$> nsubs))
          (PackageBuildImpossible $ IllegalLibraryName pn)

        -- § Fields check.
        checkNull category_
          (PackageDistSuspicious $ MissingField CEFCategory)
        checkNull maintainer_
          (PackageDistSuspicious $ MissingField CEFMaintainer)
        checkP (ShortText.null synopsis_ && not (ShortText.null description_))
          (PackageDistSuspicious $ MissingField CEFSynopsis)
        checkP (ShortText.null description_ && not (ShortText.null synopsis_))
          (PackageDistSuspicious $ MissingField CEFDescription)
        checkP (all ShortText.null [synopsis_, description_])
          (PackageDistInexcusable $ MissingField CEFSynOrDesc)
        checkP (ShortText.length synopsis_ > 80)
          (PackageDistSuspicious SynopsisTooLong)
        checkP (not (ShortText.null description_) &&
                 ShortText.length description_ <= ShortText.length synopsis_)
          (PackageDistSuspicious ShortDesc)

        -- § Paths.
        mapM_ (checkPath False "extra-source-files" PathKindGlob) extraSrcFiles_
        mapM_ (checkPath False "extra-tmp-files" PathKindFile) extraTmpFiles_
        mapM_ (checkPath False "extra-doc-files" PathKindGlob) extraDocFiles_
        mapM_ (checkPath False "data-files" PathKindGlob) dataFiles_
        checkPath True "data-dir" PathKindDirectory dataDir_
        let licPaths = map getSymbolicPath licenseFiles_
        mapM_ (checkPath False "license-file" PathKindFile) licPaths
        mapM_ checkLicFileExist licenseFiles_

        -- § Globs.
        dataGlobs <- mapM (checkGlob "data-files") dataFiles_
        extraGlobs <- mapM (checkGlob "extra-source-files") extraSrcFiles_
        docGlobs <- mapM (checkGlob "extra-doc-files") extraDocFiles_
            -- We collect globs to feed them to checkMissingDocs.

        -- § Missing documentation.
        checkMissingDocs (catMaybes dataGlobs)
                         (catMaybes extraGlobs)
                         (catMaybes docGlobs)

        -- § Datafield checks.
        checkSetupBuildInfo setupBuildInfo_
        mapM_ checkTestedWith testedWith_
        either checkNewLicense
               (checkOldLicense $ null licenseFiles_)
               licenseRaw_
        checkSourceRepos sourceRepos_
        mapM_ checkCustomField customFieldsPD_

        -- Feature checks.
        checkSpecVer CabalSpecV1_18 (not . null $ extraDocFiles_)
          (PackageDistInexcusable CVExtraDocFiles)
        checkSpecVer CabalSpecV1_6 (not . null $ sourceRepos_)
          (PackageDistInexcusable CVSourceRepository)
        checkP (specVersion_ >= CabalSpecV1_24 &&
               isNothing setupBuildInfo_ &&
               buildTypeRaw_ == Just Custom)
          (PackageBuildWarning CVCustomSetup)
        checkSpecVer CabalSpecV1_24
           (isNothing setupBuildInfo_ &&
            buildTypeRaw_ == Just Custom)
          (PackageDistSuspiciousWarn CVExpliticDepsCustomSetup)
        checkP (isNothing buildTypeRaw_ && specVersion_ < CabalSpecV2_2)
          (PackageBuildWarning NoBuildType)
        checkP (isJust setupBuildInfo_  && buildType pkg /= Custom)
          (PackageBuildWarning NoCustomSetup)

        -- Contents.
        checkConfigureExists (buildType pkg)
        checkSetupExists (buildType pkg)
        checkCabalFile (packageName pkg)
        mapM_ (checkGlobFile specVersion_ "." "extra-source-files") extraSrcFiles_
        mapM_ (checkGlobFile specVersion_ "." "extra-doc-files") extraDocFiles_
        mapM_ (checkGlobFile specVersion_ dataDir_ "data-files") dataFiles_
    where
          checkNull :: Monad m => ShortText.ShortText -> PackageCheck ->
                       CheckM m ()
          checkNull st c = checkP (ShortText.null st) c

          checkTestedWith :: Monad m => (CompilerFlavor, VersionRange) ->
                             CheckM m ()
          checkTestedWith (OtherCompiler n, _) =
            tellP (PackageBuildWarning $ UnknownCompilers [n])
          checkTestedWith (compiler, versionRange) =
            checkVersionRange compiler versionRange

          checkVersionRange :: Monad m => CompilerFlavor -> VersionRange ->
                               CheckM m ()
          checkVersionRange cmp vr =
            when (isNoVersion vr)
              (let dep = [Dependency (mkPackageName (prettyShow cmp))
                                    vr mainLibSet]
               in tellP (PackageDistInexcusable (InvalidTestWith dep)))

checkSetupBuildInfo :: Monad m => Maybe SetupBuildInfo -> CheckM m ()
checkSetupBuildInfo Nothing = return ()
checkSetupBuildInfo (Just (SetupBuildInfo ds _)) = do
        (is, rs) <- partitionDeps ["base", "Cabal"] ds
        let ick = PackageDistInexcusable . UpperBoundSetup
            rck = PackageDistSuspiciousWarn . MissingUpperBounds
        mapM_ (checkPVP ick) is
        checkPVPs rck rs

checkPackageId :: Monad m => PackageIdentifier -> CheckM m ()
checkPackageId (PackageIdentifier pkgName_ _pkgVersion_) = do
        checkP (not . FilePath.Windows.isValid . prettyShow $ pkgName_)
          (PackageDistInexcusable $ InvalidNameWin pkgName_)
        checkP (isPrefixOf "z-" . prettyShow $ pkgName_) $
          (PackageDistInexcusable ZPrefix)

checkNewLicense :: Monad m => SPDX.License -> CheckM m ()
checkNewLicense lic = do
        checkP (lic == SPDX.NONE)
          (PackageDistInexcusable NONELicense)

checkOldLicense :: Monad m =>
                Bool ->        -- Flag: no license file?
                License ->
                CheckM m ()
checkOldLicense nullLicFiles lic = do
        checkP (lic == UnspecifiedLicense)
          (PackageDistInexcusable NoLicense)
        checkP (lic == AllRightsReserved)
          (PackageDistSuspicious AllRightsReservedLicense)
        checkSpecVer CabalSpecV1_4 (lic `notElem` compatLicenses)
          (PackageDistInexcusable (LicenseMessParse lic))
        checkP (lic == BSD4)
          (PackageDistSuspicious UncommonBSD4)
        case lic of
          UnknownLicense l ->
            tellP (PackageBuildWarning (UnrecognisedLicense l))
          _ -> return ()
        checkP (lic `notElem` [AllRightsReserved,
                               UnspecifiedLicense, PublicDomain] &&
                    -- AllRightsReserved and PublicDomain are not strictly
                    -- licenses so don't need license files.
                nullLicFiles) $
          (PackageDistSuspicious NoLicenseFile)
        case unknownLicenseVersion lic of
          Just knownVersions -> tellP
            (PackageDistSuspicious $ UnknownLicenseVersion lic knownVersions)
          _ -> return ()
    where
          compatLicenses = [GPL Nothing, LGPL Nothing, AGPL Nothing, BSD3,
                            BSD4, PublicDomain, AllRightsReserved,
                            UnspecifiedLicense, OtherLicense]

          unknownLicenseVersion (GPL (Just v))
            | v `notElem` knownVersions = Just knownVersions
            where knownVersions = [ v' | GPL  (Just v') <- knownLicenses ]
          unknownLicenseVersion (LGPL (Just v))
            | v `notElem` knownVersions = Just knownVersions
            where knownVersions = [ v' | LGPL (Just v') <- knownLicenses ]
          unknownLicenseVersion (AGPL (Just v))
            | v `notElem` knownVersions = Just knownVersions
            where knownVersions = [ v' | AGPL (Just v') <- knownLicenses ]
          unknownLicenseVersion (Apache  (Just v))
            | v `notElem` knownVersions = Just knownVersions
            where knownVersions = [ v' | Apache  (Just v') <- knownLicenses ]
          unknownLicenseVersion _ = Nothing

checkSourceRepos :: Monad m => [SourceRepo] -> CheckM m ()
checkSourceRepos rs = do
        mapM_ repoCheck rs
        checkMissingVcsInfo rs
    where
          -- Single repository checks.
          repoCheck :: Monad m => SourceRepo -> CheckM m ()
          repoCheck (SourceRepo repoKind_ repoType_ repoLocation_
                       repoModule_ _repoBranch_ repoTag_ repoSubdir_) = do
              case repoKind_ of
                RepoKindUnknown kind -> tellP
                  (PackageDistInexcusable $ UnrecognisedSourceRepo kind)
                _ -> return ()
              checkP (isNothing repoType_)
                 (PackageDistInexcusable MissingType)
              checkP (isNothing repoLocation_)
               (PackageDistInexcusable MissingLocation)
              checkP (repoType_ == Just (KnownRepoType CVS) &&
                      isNothing repoModule_)
                (PackageDistInexcusable MissingModule)
              checkP (repoKind_ == RepoThis && isNothing repoTag_)
                (PackageDistInexcusable MissingTag)
              checkP (any isAbsoluteOnAnyPlatform repoSubdir_)
                (PackageDistInexcusable SubdirRelPath)
              case join . fmap isGoodRelativeDirectoryPath $ repoSubdir_ of
                Just err -> tellP
                  (PackageDistInexcusable $ SubdirGoodRelPath err)
                Nothing -> return ()

checkMissingVcsInfo :: Monad m => [SourceRepo] -> CheckM m ()
checkMissingVcsInfo rs =
        let rdirs = concatMap repoTypeDirname knownRepoTypes
        in checkPkg
             (\ops -> do us <- or <$> traverse (doesDirectoryExist ops) rdirs
                         return (null rs && us))
             (PackageDistSuspicious MissingSourceControl)
    where
          repoTypeDirname :: KnownRepoType -> [FilePath]
          repoTypeDirname Darcs     = ["_darcs"]
          repoTypeDirname Git       = [".git"]
          repoTypeDirname SVN       = [".svn"]
          repoTypeDirname CVS       = ["CVS"]
          repoTypeDirname Mercurial = [".hg"]
          repoTypeDirname GnuArch   = [".arch-params"]
          repoTypeDirname Bazaar    = [".bzr"]
          repoTypeDirname Monotone  = ["_MTN"]
          repoTypeDirname Pijul     = [".pijul"]


-- ------------------------------------------------------------
-- * Conditional trees
-- ------------------------------------------------------------

-- As a prerequisite to some checks, we transform a target CondTree into
-- a CondTree of “target + useful context”
-- This is slightly clearer, is easier to walk without resorting to
-- list comprehensions, allows us in the future to apply some sensible
-- “optimisations” to checks (exclusive branches, etc.).

-- | @nf@ function is needed to appropriately name some targets which need
-- to be spoonfed (otherwise name appears as "").
--
initTargetAnnotation :: Monoid a =>
        (UnqualComponentName -> a -> a) ->   -- Naming function for targets.
        UnqualComponentName ->
        TargetAnnotation a
initTargetAnnotation nf n = TargetAnnotation (nf n mempty) False

-- | We “build up” target from various slices.
--
updateTargetAnnotation :: Monoid a =>
                a ->                   -- A target (lib, exe, test, …)
                TargetAnnotation a ->
                TargetAnnotation a
updateTargetAnnotation t ta = ta { taTarget = taTarget ta <> t }

-- | Before walking a target 'CondTree', we need to annotate it with
-- information relevant to the checks (read 'TaraAnn' and 'checkCondTarget'
-- doc for more info).
annotateCondTree :: forall a. Monoid a =>
            [PackageFlag] ->                    -- User flags.
            TargetAnnotation a ->
            CondTree ConfVar [Dependency] a ->
            CondTree ConfVar [Dependency] (TargetAnnotation a)
annotateCondTree fs ta (CondNode a c bs) =
        let ta' = updateTargetAnnotation a ta
            bs' = map (annotateBranch ta') bs
        in CondNode ta' c bs'
    where
          annotateBranch :: TargetAnnotation a ->
                            CondBranch ConfVar [Dependency] a ->
                            CondBranch ConfVar [Dependency]
                                               (TargetAnnotation a)
          annotateBranch wta (CondBranch k t mf) =
              let uf = isPkgFlagCond k
                  wta' = wta { taPackageFlag = taPackageFlag wta || uf }
                  atf = annotateCondTree fs
              in CondBranch k (atf wta' t)
                              (atf wta <$> mf)
                              -- Note how we are passing the *old* wta
                              -- in the `else` branch, since we are not
                              -- under that flag.

          -- We only want to pick up variables that are flags and that are
          -- *off* by default.
          isPkgFlagCond :: Condition ConfVar -> Bool
          isPkgFlagCond (Lit _) = False
          isPkgFlagCond (Var (PackageFlag f)) = elem f defOffFlags
          isPkgFlagCond (Var _) = False
          isPkgFlagCond (CNot cn) = not (isPkgFlagCond cn)
          isPkgFlagCond (CAnd ca cb) = isPkgFlagCond ca || isPkgFlagCond cb
          isPkgFlagCond (COr ca cb) = isPkgFlagCond ca && isPkgFlagCond cb

          -- Package flags that are off by default *and* that are manual.
          defOffFlags = map flagName $
                         filter (\f -> not (flagDefault f) &&
                                       flagManual f) fs

-- | A conditional target is a library, exe, benchmark etc., destructured
-- in a CondTree. Traversing method: we render the branches, pass a
-- relevant context, collect checks.
checkCondTarget :: forall m a. (Monad m, Monoid a) =>
                   [PackageFlag] ->           -- User flags.
                   (a -> CheckM m ()) ->      -- Check function (a = target).
                   (UnqualComponentName -> a -> a) ->
                                              -- Naming function (some targets
                                              -- need to have their name
                                              -- spoonfed to them.
                   (UnqualComponentName, CondTree ConfVar [Dependency] a) ->
                                              -- Target name/condtree.
                   CheckM m ()
checkCondTarget fs cf nf (unqualName, ct) =
        wTree $ annotateCondTree fs (initTargetAnnotation nf unqualName) ct
    where
          -- Walking the tree. Remember that CondTree is not a binary
          -- tree but a /rose/tree.
          wTree :: CondTree ConfVar [Dependency] (TargetAnnotation a) ->
                   CheckM m ()
          wTree (CondNode ta _ bs)
                -- There are no branches (and [] == True) *or* every branch
                -- is “simple” (i.e. missing a 'condBranchIfFalse' part).
                -- This is convenient but not necessarily correct in all
                -- cases; a more precise way would be to check incompatibility
                -- among simple branches conditions (or introduce a principled
                -- `cond` construct in `.cabal` files.
              | all isSimple bs = do
                  localCM (initCheckCtx ta) (cf $ taTarget ta)
                  mapM_ wBranch bs
                -- If there are T/F conditions, there is no need to check
                -- the intermediate 'TargetAnnotation' too.
              | otherwise = do
                  mapM_ wBranch bs

          isSimple :: CondBranch ConfVar [Dependency] (TargetAnnotation a)->
                      Bool
          isSimple (CondBranch _ _ Nothing) = True
          isSimple (CondBranch _ _ (Just _)) = False

          wBranch :: CondBranch ConfVar [Dependency] (TargetAnnotation a) ->
                     CheckM m ()
          wBranch (CondBranch k t mf) = do
              checkCondVars k
              wTree t
              maybe (return ()) wTree mf

-- | Condvar checking (misspelled OS in if conditions, etc).
checkCondVars :: Monad m => Condition ConfVar -> CheckM m ()
checkCondVars cond =
        let (_, vs) = simplifyCondition cond (\v -> Left v)
                -- Using simplifyCondition is convenient and correct,
                -- if checks become more complex we can always walk
                -- 'Condition'.
        in mapM_ vcheck vs
    where
          vcheck :: Monad m => ConfVar -> CheckM m ()
          vcheck (OS (OtherOS os)) =
            tellP (PackageDistInexcusable $ UnknownOS [os])
          vcheck (Arch (OtherArch arch)) =
            tellP (PackageDistInexcusable $ UnknownArch [arch])
          vcheck (Impl (OtherCompiler os) _) =
            tellP (PackageDistInexcusable $ UnknownCompiler [os])
          vcheck _ = return ()

-- ------------------------------------------------------------
-- * Targets
-- ------------------------------------------------------------

checkLibrary :: Monad m =>
                Bool ->     -- Is this a sublibrary?
                Library ->
                CheckM m ()
checkLibrary isSub lib@(Library
                          libName_ _exposedModules_ reexportedModules_
                          signatures_ _libExposed_ _libVisibility_
                          libBuildInfo_) = do
        checkP (libName_ == LMainLibName && isSub)
          (PackageBuildImpossible UnnamedInternal)
        -- TODO: bogus if a required-signature was passed through.
        checkP (null (explicitLibModules lib) && null reexportedModules_)
              (PackageDistSuspiciousWarn (NoModulesExposed libName_))
        -- TODO parse-caught check, can safely remove.
        checkSpecVer CabalSpecV2_0 (not . null $ signatures_)
          (PackageDistInexcusable SignaturesCabal2)
          -- autogen/includes checks.
        checkP (not $ all (flip elem (explicitLibModules lib))
                                    (libModulesAutogen lib))
          (PackageBuildImpossible AutogenNotExposed)
          -- check that all autogen-includes appear on includes or
          -- install-includes.
        checkP (not $ all (flip elem (allExplicitIncludes lib))
                                    (view L.autogenIncludes lib)) $
          (PackageBuildImpossible AutogenIncludesNotIncluded)

        -- § Build infos.
        checkBuildInfo BITLib (explicitLibModules lib) libBuildInfo_

        -- Feature checks.
        -- check use of reexported-modules sections
        checkSpecVer CabalSpecV1_22 (not . null $ reexportedModules_)
          (PackageDistInexcusable CVReexported)
    where
          allExplicitIncludes :: L.HasBuildInfo a => a -> [FilePath]
          allExplicitIncludes x = view L.includes x ++
                                  view L.installIncludes x

checkForeignLib :: Monad m => ForeignLib -> CheckM m ()
checkForeignLib (ForeignLib
                   _foreignLibName_ _foreignLibType_ _foreignLibOptions_
                   foreignLibBuildInfo_ _foreignLibVersionInfo_
                   _foreignLibVersionLinux_ _foreignLibModDefFile_) = do
        checkBuildInfo BITLib [] foreignLibBuildInfo_

checkExecutable :: Monad m => PackageId -> Executable -> CheckM m ()
checkExecutable pid exe@(Executable
                           exeName_ modulePath_ _exeScope_ buildInfo_) = do
        -- § Exe specific checks
        checkP (null modulePath_)
          (PackageBuildImpossible (NoMainIs exeName_))
          -- This check does not apply to scripts.
        checkP (pid /= fakePackageId &&
               not (null modulePath_) &&
               not (fileExtensionSupportedLanguage $ modulePath_))
          (PackageBuildImpossible NoHsLhsMain)

        -- § Features check
        checkSpecVer CabalSpecV1_18
          (fileExtensionSupportedLanguage modulePath_ &&
           takeExtension modulePath_ `notElem` [".hs", ".lhs"])
          (PackageDistInexcusable MainCCabal1_18)

        -- Alas exeModules ad exeModulesAutogen (exported from
        -- Distribution.Types.Executable) take `Executable` as a parameter.
        checkP (not $ all (flip elem (exeModules exe)) (exeModulesAutogen exe))
          (PackageBuildImpossible $ AutogenNoOther CETExecutable exeName_)
        checkP (not $ all (flip elem (view L.includes exe))
                                    (view L.autogenIncludes exe))
          (PackageBuildImpossible AutogenIncludesNotIncludedExe)

        -- § Build info checks.
        checkBuildInfo BITOther [] buildInfo_

checkTestSuite :: Monad m => TestSuite -> CheckM m ()
checkTestSuite ts@(TestSuite
                     testName_ testInterface_ testBuildInfo_
                     _testCodeGenerators_) = do
        -- § TS specific checks.
        -- TODO caught by the parser, can remove safely
        case testInterface_ of
          TestSuiteUnsupported tt@(TestTypeUnknown _ _) ->
            tellP (PackageBuildWarning $ TestsuiteTypeNotKnown tt)
          TestSuiteUnsupported tt ->
            tellP (PackageBuildWarning $ TestsuiteNotSupported tt)
          _ -> return ()
        checkP mainIsWrongExt
          (PackageBuildImpossible NoHsLhsMain)
        checkP (not $ all (flip elem (testModules ts))
                         (testModulesAutogen ts))
          (PackageBuildImpossible (AutogenNoOther CETTest $ testName_))
        checkP (not $ all (flip elem (view L.includes ts))
                         (view L.autogenIncludes ts))
          (PackageBuildImpossible AutogenIncludesNotIncludedExe)

        -- § Feature checks.
        checkSpecVer CabalSpecV1_18
          (mainIsNotHsExt && not mainIsWrongExt)
          (PackageDistInexcusable MainCCabal1_18)

        -- § Build info checks.
        checkBuildInfo BITTestBench [] testBuildInfo_
  where
        mainIsWrongExt =
          case testInterface_ of
            TestSuiteExeV10 _ f -> not (fileExtensionSupportedLanguage f)
            _ -> False

        mainIsNotHsExt =
          case testInterface_ of
            TestSuiteExeV10 _ f -> takeExtension f `notElem` [".hs", ".lhs"]
            _ -> False

checkBenchmark :: Monad m => Benchmark -> CheckM m ()
checkBenchmark bm@(Benchmark
                     benchmarkName_ benchmarkInterface_
                     benchmarkBuildInfo_) = do
        -- § Interface & bm specific tests.
        case benchmarkInterface_ of
          BenchmarkUnsupported tt@(BenchmarkTypeUnknown _ _) ->
            tellP (PackageBuildWarning $ BenchmarkTypeNotKnown tt)
          BenchmarkUnsupported tt ->
            tellP (PackageBuildWarning $ BenchmarkNotSupported tt)
          _ -> return ()
        checkP mainIsWrongExt
          (PackageBuildImpossible NoHsLhsMainBench)

        checkP (not $ all (flip elem (benchmarkModules bm))
                         (benchmarkModulesAutogen bm))
          (PackageBuildImpossible $ AutogenNoOther CETBenchmark benchmarkName_)

        checkP (not $ all (flip elem (view L.includes bm))
                         (view L.autogenIncludes bm))
          (PackageBuildImpossible AutogenIncludesNotIncludedExe)

        -- § BuildInfo checks.
        checkBuildInfo BITTestBench [] benchmarkBuildInfo_
    where
          -- Cannot abstract with similar function in checkTestSuite,
          -- they are different.
          mainIsWrongExt =
            case benchmarkInterface_ of
              BenchmarkExeV10 _ f -> takeExtension f `notElem` [".hs", ".lhs"]
              _ -> False


-- ------------------------------------------------------------
-- * Build info
-- ------------------------------------------------------------

-- Target type (library, test/bech, other).
data BITarget = BITLib | BITTestBench | BITOther
    deriving (Eq, Show)

-- Check a great deal of things in buildInfo.
-- With 'checkBuildInfo' we cannot follow the usual “pattern match
-- everything” method, for the number of BuildInfo fields (almost 50)
-- but more importantly because accessing options, etc. is done
-- with functions from 'Distribution.Types.BuildInfo' (e.g. 'hcOptions').
-- Duplicating the effort here means risk of diverging definitions for
-- little gain (most likely if a field is added to BI, the relevant
-- function will be tweaked in Distribution.Types.BuildInfo too).
checkBuildInfo :: Monad m =>
            BITarget ->      -- Target type.
            [ModuleName] ->  -- Additional module names which cannot be
                             -- extracted from BuildInfo (mainly: exposed
                             -- library modules).
            BuildInfo ->
            CheckM m ()
checkBuildInfo t ams bi = do

        -- For the sake of clarity, we split che checks in various
        -- (top level) functions, even if we are not actually going
        -- deeper in the traversal.

        checkBuildInfoOptions t bi
        checkBuildInfoPathsContent bi
        checkBuildInfoPathsWellFormedness bi

        sv <- asksCM ccSpecVersion
        checkBuildInfoFeatures bi sv

        checkAutogenModules ams bi

        -- PVP: we check for base and all other deps.
        (ids, rds) <- partitionDeps ["base"]
                        (mergeDependencies $ targetBuildDepends bi)
        let ick = const (PackageDistInexcusable BaseNoUpperBounds)
            rck = PackageDistSuspiciousWarn . MissingUpperBounds
        mapM_ (checkPVP ick) ids
        checkPVPs rck rds

        -- Custom fields well-formedness (ASCII).
        mapM_ checkCustomField (customFieldsBI bi)

        -- Content.
        mapM_ (checkLocalPathExist "extra-lib-dirs") (extraLibDirs bi)
        mapM_ (checkLocalPathExist "extra-lib-dirs-static")
              (extraLibDirsStatic bi)
        mapM_ (checkLocalPathExist "extra-framework-dirs")
              (extraFrameworkDirs bi)
        mapM_ (checkLocalPathExist "include-dirs") (includeDirs bi)
        mapM_ (checkLocalPathExist "hs-source-dirs")
              (map getSymbolicPath $ hsSourceDirs bi)


-- Well formedness of BI contents (no `Haskell2015`, no deprecated
-- extensions etc).
checkBuildInfoPathsContent :: Monad m => BuildInfo -> CheckM m ()
checkBuildInfoPathsContent bi = do
        mapM_ checkLang (allLanguages bi)
        mapM_ checkExt (allExtensions bi)
        mapM_ checkDep (targetBuildDepends bi) --xxx checdep no va qui
        df <- asksCM ccDesugar
          -- This way we can use the same function for legacy&non exedeps.
        let ds = buildToolDepends bi ++ catMaybes (map df $ buildTools bi)
        mapM_ checkBTDep ds
    where
          checkLang :: Monad m => Language -> CheckM m ()
          checkLang (UnknownLanguage n) =
            tellP (PackageBuildWarning (UnknownLanguages [n]))
          checkLang _ = return ()

          checkExt :: Monad m => Extension -> CheckM m ()
          checkExt (UnknownExtension n)
            | n `elem` map prettyShow knownLanguages =
                tellP (PackageBuildWarning (LanguagesAsExtension [n]))
            | otherwise =
                tellP (PackageBuildWarning (UnknownExtensions [n]))
          checkExt n = do
            let dss = filter (\(a, _) -> a == n) deprecatedExtensions
            checkP (not . null $ dss)
              (PackageDistSuspicious $ DeprecatedExtensions dss)

          checkDep :: Monad m => Dependency -> CheckM m ()
          checkDep d@(Dependency name vrange _) = do
                mpn <- asksCM (packageNameToUnqualComponentName . pkgName .
                               pnPackageId . ccNames)
                lns <- asksCM (pnSubLibs . ccNames)
                pVer <- asksCM (pkgVersion . pnPackageId . ccNames)
                let allLibNs = mpn : lns
                when (packageNameToUnqualComponentName name `elem` allLibNs)
                  (checkP (not $ pVer `withinRange` vrange)
                     (PackageBuildImpossible $ ImpossibleInternalDep [d]))

          checkBTDep :: Monad m => ExeDependency -> CheckM m ()
          checkBTDep ed@(ExeDependency n name vrange) = do
                exns <- asksCM (pnExecs . ccNames)
                pVer <- asksCM (pkgVersion . pnPackageId . ccNames)
                pNam <- asksCM (pkgName . pnPackageId . ccNames)
                checkP (n == pNam &&       -- internal
                       name `notElem`exns) -- not present
                  (PackageBuildImpossible $ MissingInternalExe [ed])
                when (name `elem` exns)
                  (checkP (not $ pVer `withinRange` vrange)
                     (PackageBuildImpossible $ ImpossibleInternalExe [ed]))

-- Paths well-formedness check for BuildInfo.
checkBuildInfoPathsWellFormedness :: Monad m => BuildInfo -> CheckM m ()
checkBuildInfoPathsWellFormedness bi = do
        mapM_ (checkPath False "asm-sources" PathKindFile) (asmSources bi)
        mapM_ (checkPath False "cmm-sources" PathKindFile) (cmmSources bi)
        mapM_ (checkPath False "c-sources" PathKindFile) (cSources bi)
        mapM_ (checkPath False "cxx-sources" PathKindFile) (cxxSources bi)
        mapM_ (checkPath False "js-sources" PathKindFile) (jsSources bi)
        mapM_ (checkPath False "install-includes" PathKindFile)
            (installIncludes bi)
        mapM_ (checkPath False "hs-source-dirs" PathKindDirectory)
            (map getSymbolicPath $ hsSourceDirs bi)
          -- Possibly absolute paths.
        mapM_ (checkPath True "includes" PathKindFile) (includes bi)
        mapM_ (checkPath True "include-dirs" PathKindDirectory)
            (includeDirs bi)
        mapM_ (checkPath True "extra-lib-dirs" PathKindDirectory)
            (extraLibDirs bi)
        mapM_ (checkPath True "extra-lib-dirs-static" PathKindDirectory)
            (extraLibDirsStatic bi)
        mapM_ checkOptionPath (perCompilerFlavorToList $ options bi)
    where
          checkOptionPath :: Monad m => (CompilerFlavor, [FilePath]) ->
                             CheckM m ()
          checkOptionPath (GHC, paths) = mapM_ (\path ->
            checkP (isInsideDist path)
              (PackageDistInexcusable $ DistPoint Nothing path))
            paths
          checkOptionPath _ = return ()

-- Checks for features that can be present in BuildInfo only with certain
-- CabalSpecVersion.
checkBuildInfoFeatures :: Monad m => BuildInfo -> CabalSpecVersion ->
                                     CheckM m ()
checkBuildInfoFeatures bi sv = do

        -- Default language can be used only w/ spec ≥ 1.10
        checkSpecVer CabalSpecV1_10 (isJust $ defaultLanguage bi)
          (PackageBuildWarning CVDefaultLanguage)
        -- CheckSpecVer sv.
        checkP (sv >= CabalSpecV1_10 && sv < CabalSpecV3_4 &&
               isNothing (defaultLanguage bi))
          (PackageBuildWarning CVDefaultLanguageComponent)
        -- Check use of 'extra-framework-dirs' field.
        checkSpecVer CabalSpecV1_24 (not . null $ extraFrameworkDirs bi)
          (PackageDistSuspiciousWarn CVExtraFrameworkDirs)
        -- Check use of default-extensions field don't need to do the
        -- equivalent check for other-extensions.
        checkSpecVer CabalSpecV1_10 (not . null $ defaultExtensions bi)
          (PackageBuildWarning CVDefaultExtensions)
        -- Check use of extensions field
        checkP (sv >= CabalSpecV1_10 && (not . null $ oldExtensions bi))
          (PackageBuildWarning CVExtensionsDeprecated)

        -- asm-sources, cmm-sources and friends only w/ spec ≥ 1.10
        checkCVSources (asmSources bi)
        checkCVSources (cmmSources bi)
        checkCVSources (extraBundledLibs bi)
        checkCVSources (extraLibFlavours bi)

        -- extra-dynamic-library-flavours requires ≥ 3.0
        checkSpecVer CabalSpecV3_0 (not . null $ extraDynLibFlavours bi)
          (PackageDistInexcusable $ CVExtraDynamic [extraDynLibFlavours bi])
        -- virtual-modules requires ≥ 2.2
        checkSpecVer CabalSpecV2_2 (not . null $ virtualModules bi) $
          (PackageDistInexcusable CVVirtualModules)
        -- Check use of thinning and renaming.
        checkSpecVer CabalSpecV2_0 (not . null $ mixins bi)
          (PackageDistInexcusable CVMixins)

        checkBuildInfoExtensions bi
    where
          checkCVSources :: Monad m => [FilePath] -> CheckM m ()
          checkCVSources cvs =
            checkSpecVer CabalSpecV3_0 (not . null $ cvs)
              (PackageDistInexcusable CVSources)

-- Tests for extensions usage which can break Cabal < 1.4.
checkBuildInfoExtensions :: Monad m => BuildInfo -> CheckM m ()
checkBuildInfoExtensions bi = do
        let exts = allExtensions bi
            extCabal1_2 = nub $ filter (`elem` compatExtensionsExtra) exts
            extCabal1_4 = nub $ filter (`notElem` compatExtensions) exts
                -- As of Cabal-1.4 we can add new extensions without worrying
                -- about breaking old versions of cabal.
        checkSpecVer CabalSpecV1_2 (not . null $ extCabal1_2)
          (PackageDistInexcusable $
            CVExtensions CabalSpecV1_2 extCabal1_2)
        checkSpecVer CabalSpecV1_4 (not . null $ extCabal1_4)
          (PackageDistInexcusable $
            CVExtensions CabalSpecV1_4 extCabal1_4)
    where
          -- The known extensions in Cabal-1.2.3
          compatExtensions :: [Extension]
          compatExtensions =
            map EnableExtension
            [OverlappingInstances, UndecidableInstances, IncoherentInstances
            , RecursiveDo, ParallelListComp, MultiParamTypeClasses
            , FunctionalDependencies, Rank2Types
            , RankNTypes, PolymorphicComponents, ExistentialQuantification
            , ScopedTypeVariables, ImplicitParams, FlexibleContexts
            , FlexibleInstances, EmptyDataDecls, CPP, BangPatterns
            , TypeSynonymInstances, TemplateHaskell, ForeignFunctionInterface
            , Arrows, Generics, NamedFieldPuns, PatternGuards
            , GeneralizedNewtypeDeriving, ExtensibleRecords
            , RestrictedTypeSynonyms, HereDocuments] ++
            map DisableExtension
            [MonomorphismRestriction, ImplicitPrelude] ++
            compatExtensionsExtra

          -- The extra known extensions in Cabal-1.2.3 vs Cabal-1.1.6
          -- (Cabal-1.1.6 came with ghc-6.6. Cabal-1.2 came with ghc-6.8)
          compatExtensionsExtra :: [Extension]
          compatExtensionsExtra =
            map EnableExtension
            [KindSignatures, MagicHash, TypeFamilies, StandaloneDeriving
            , UnicodeSyntax, PatternSignatures, UnliftedFFITypes
            , LiberalTypeSynonyms, TypeOperators, RecordWildCards, RecordPuns
            , DisambiguateRecordFields, OverloadedStrings, GADTs
            , RelaxedPolyRec, ExtendedDefaultRules, UnboxedTuples
            , DeriveDataTypeable, ConstrainedClassMethods] ++
            map DisableExtension
            [MonoPatBinds]

-- Autogenerated modules (Paths_, PackageInfo_) checks. We could pass this
-- function something more specific than the whole BuildInfo, but it would be
-- a tuple of [ModuleName] lists, error prone.
checkAutogenModules :: Monad m =>
                       [ModuleName] -> -- Additional modules not present
                                       -- in BuildInfo (e.g. exposed library
                                       -- modules).
                       BuildInfo -> CheckM m ()
checkAutogenModules ams bi = do
        pkgId <- asksCM (pnPackageId . ccNames)
        let -- It is an unfortunate reality that autogenPathsModuleName
            -- and autogenPackageInfoModuleName work on PackageDescription
            -- while not needing it all, but just the `package` bit.
            minimalPD = emptyPackageDescription { package = pkgId }
            autoPathsName = autogenPathsModuleName minimalPD
            autoInfoModuleName = autogenPackageInfoModuleName minimalPD

        -- Autogenerated module + some default extension build failure.
        autogenCheck autoPathsName CVAutogenPaths
        rebindableClashCheck autoPathsName RebindableClashPaths

        -- Paths_* module + some default extension build failure.
        autogenCheck autoInfoModuleName CVAutogenPackageInfo
        rebindableClashCheck autoInfoModuleName RebindableClashPackageInfo
    where
          autogenCheck :: Monad m => ModuleName -> CheckExplanation ->
                                     CheckM m ()
          autogenCheck name warning = do
                sv <- asksCM ccSpecVersion
                let allModsForAuto = ams ++ otherModules bi
                checkP (sv >= CabalSpecV2_0 &&
                        elem name allModsForAuto &&
                        notElem name (autogenModules bi))
                  (PackageDistInexcusable warning)

          rebindableClashCheck :: Monad m => ModuleName -> CheckExplanation ->
                                             CheckM m ()
          rebindableClashCheck name warning = do
                checkSpecVer CabalSpecV2_2
                    ((name `elem` otherModules bi ||
                      name `elem` autogenModules bi) && checkExts)
                  (PackageBuildImpossible warning)

          -- Do we have some peculiar extensions active which would interfere
          -- (cabal-version <2.2) with Paths_modules?
          checkExts :: Bool
          checkExts = let exts = defaultExtensions bi
                      in rebind `elem` exts &&
                         (strings `elem` exts || lists `elem` exts)
            where
              rebind  = EnableExtension RebindableSyntax
              strings = EnableExtension OverloadedStrings
              lists   = EnableExtension OverloadedLists

checkLocalPathExist :: Monad m =>
            String ->     -- .cabal field where we found the error.
            FilePath ->
            CheckM m ()
checkLocalPathExist title dir =
        checkPkg (\ops -> do dn <- not <$> doesDirectoryExist ops dir
                             let rp = not (isAbsoluteOnAnyPlatform dir)
                             return (rp && dn))
          (PackageBuildWarning $ UnknownDirectory title dir)

-- PVP --

-- Convenience function to partition important dependencies by name. To
-- be used together with checkPVP.
partitionDeps :: Monad m =>
                 [String] ->  -- | List of package names ("base", "Cabal"…)
                 [Dependency] ->
                 CheckM m ([Dependency], [Dependency])
partitionDeps ns ds = do
        pId <- asksCM (pnPackageId . ccNames)
        let idName = unPackageName . pkgName $ pId
            -- Do not return dependencies which are package
            -- main library.
            ds' = filter ((/= idName) . depName) ds

        -- February 2022: this is a tricky part of the function. If the
        -- two lists are different in length (hence, we did find a dep-
        -- endency to the package itself), move all dependencies in the
        -- non-critical bucket.
        -- With this pragmatic choice we kill two birds with one stone:
        -- - we still ouptut a warning for naked `base` dependencies in
        --   the target (usually a test, an example exe, etc);
        -- - but we don’t make Hackage refuse the package, which mimics
        --   ante-refactoring behaviour (a soup of all dependencies in
        --   the whole package merged together).
        -- Once the community is positive about upper bounds best-prac-
        -- tices this can be removed.
        if ds /= ds'
          then return ([], ds')
          else return (partition (flip elem ns . depName) ds')
    where
          depName d = unPackageName . depPkgName $ d

-- Sometimes we read (or end up with) “straddle” deps declarations
-- like this:
--
--     build-depends: base > 3, base < 4
--
-- `mergeDependencies` reduces that to base > 3 && < 4, _while_ maintaining
-- dependencies order in the list (better UX).
mergeDependencies :: [Dependency] -> [Dependency]
mergeDependencies [] = []
mergeDependencies l@(d:_) =
        let dName = unPackageName . depPkgName $ d
            (sames, diffs) = partition ((== dName) . depName) l
            merged = Dependency (depPkgName d)
                                (foldl intersectVersionRanges anyVersion $
                                   map depVerRange sames)
                                (depLibraries d)
        in merged : mergeDependencies diffs
    where
          depName wd = unPackageName . depPkgName $ wd

-- PVP dependency check (single dependency).
checkPVP :: Monad m =>
            (String -> PackageCheck) -> -- Warn message dependend on name
                                        -- (e.g. "base", "Cabal").
            Dependency ->
            CheckM m ()
checkPVP ckf (Dependency pname ver _) = do
        checkP ((not . hasUpperBound) ver)
          (ckf . unPackageName $ pname)

-- PVP dependency check for a list of dependencies. Some code duplication
-- is sadly needed to provide more ergonimic error messages.
checkPVPs :: Monad m =>
             ([String] -> PackageCheck) -> -- Grouped error message,
                                           -- depends on a set of names.
             [Dependency] ->
             CheckM m ()
checkPVPs cf ds = do
            let ds' = filter withoutUpper ds
                nds = map (unPackageName . depPkgName) ds'
            unless (null nds)
              (tellP $ cf nds)
        where
              withoutUpper :: Dependency -> Bool
              withoutUpper (Dependency _ ver _) = not . hasUpperBound $ ver


-- ------------------------------------------------------------
-- * Options
-- ------------------------------------------------------------

-- General check on all options (ghc, C, C++, …) for common inaccuracies.
checkBuildInfoOptions :: Monad m => BITarget -> BuildInfo -> CheckM m ()
checkBuildInfoOptions t bi = do
        checkGHCOptions "ghc-options" t (hcOptions GHC bi)
        checkGHCOptions "ghc-prof-options" t (hcProfOptions GHC bi)
        checkGHCOptions "ghc-shared-options" t (hcSharedOptions GHC bi)
        let ldOpts = ldOptions bi
        checkCLikeOptions LangC "cc-options" (ccOptions bi) ldOpts
        checkCLikeOptions LangCPlusPlus "cxx-options" (cxxOptions bi) ldOpts
        checkCPPOptions (cppOptions bi)

-- | Checks GHC options for commonly misused or non-portable flags.
checkGHCOptions :: Monad m =>
            CabalField ->   -- .cabal field name where we found the error.
            BITarget ->     -- Target type.
            [String] ->     -- Options (alas in String form).
            CheckM m ()
checkGHCOptions title t opts = do
        checkGeneral
        case t of
          BITLib -> sequence_ [checkLib, checkNonTestBench]
          BITTestBench -> checkTestBench
          BITOther -> checkNonTestBench
    where
          checkFlags :: Monad m => [String] -> PackageCheck -> CheckM m ()
          checkFlags fs ck = checkP (any (`elem` fs) opts) ck

          checkFlagsP :: Monad m => (String -> Bool) ->
                         (String -> PackageCheck) -> CheckM m ()
          checkFlagsP p ckc =
            case filter p opts of
              []    -> return ()
              (_:_) -> tellP (ckc title)

          checkGeneral = do
            checkFlags ["-fasm"]
              (PackageDistInexcusable $ OptFasm title)
            checkFlags ["-fvia-C"]
              (PackageDistSuspicious $ OptViaC title)
            checkFlags ["-fhpc"]
              (PackageDistInexcusable $ OptHpc title)
            checkFlags ["-prof"]
              (PackageBuildWarning $ OptProf title)
            checkFlags ["-o"]
              (PackageBuildWarning $ OptO title)
            checkFlags ["-hide-package"]
              (PackageBuildWarning $ OptHide title)
            checkFlags ["--make"]
              (PackageBuildWarning $ OptMake title)
            checkFlags [ "-O", "-O1"]
              (PackageDistInexcusable $ OptOOne title)
            checkFlags ["-O2"]
              (PackageDistSuspiciousWarn $ OptOTwo title)
            checkFlags ["-split-sections"]
              (PackageBuildWarning $ OptSplitSections title)
            checkFlags ["-split-objs"]
              (PackageBuildWarning $ OptSplitObjs title)
            checkFlags ["-optl-Wl,-s", "-optl-s"]
              (PackageDistInexcusable $ OptWls title)
            checkFlags ["-fglasgow-exts"]
              (PackageDistSuspicious $ OptExts title)
            let ghcNoRts = rmRtsOpts opts
            checkAlternatives title "extensions"
              [(flag, prettyShow extension)
                | flag <- ghcNoRts
                  , Just extension <- [ghcExtension flag]]
            checkAlternatives title "extensions"
              [(flag, extension)
                | flag@('-':'X':extension) <- ghcNoRts]
            checkAlternatives title "cpp-options"
              ([(flag, flag) | flag@('-':'D':_) <- ghcNoRts] ++
               [(flag, flag) | flag@('-':'U':_) <- ghcNoRts])
            checkAlternatives title "include-dirs"
              [(flag, dir) | flag@('-':'I':dir) <- ghcNoRts]
            checkAlternatives title "extra-libraries"
              [(flag, lib) | flag@('-':'l':lib) <- ghcNoRts]
            checkAlternatives title "extra-libraries-static"
              [(flag, lib) | flag@('-':'l':lib) <- ghcNoRts]
            checkAlternatives title "extra-lib-dirs"
              [(flag, dir) | flag@('-':'L':dir) <- ghcNoRts]
            checkAlternatives title "extra-lib-dirs-static"
              [(flag, dir) | flag@('-':'L':dir) <- ghcNoRts]
            checkAlternatives title "frameworks"
              [(flag, fmwk)
                | (flag@"-framework", fmwk) <-
                  zip ghcNoRts (safeTail ghcNoRts)]
            checkAlternatives title "extra-framework-dirs"
              [(flag, dir)
                | (flag@"-framework-path", dir) <-
                  zip ghcNoRts (safeTail ghcNoRts)]
            -- Old `checkDevelopmentOnlyFlagsOptions` section
            checkFlags ["-Werror"]
              (PackageDistInexcusable $ WErrorUnneeded title)
            checkFlags ["-fdefer-type-errors"]
              (PackageDistInexcusable $ FDeferTypeErrorsUnneeded title)
            checkFlags ["-fprof-auto", "-fprof-auto-top", "-fprof-auto-calls",
                        "-fprof-cafs", "-fno-prof-count-entries", "-auto-all",
                        "-auto", "-caf-all"]
              (PackageDistSuspicious $ ProfilingUnneeded title)
            checkFlagsP (\opt -> "-d" `isPrefixOf` opt &&
                         opt /= "-dynamic")
              (PackageDistInexcusable . DynamicUnneeded)
            checkFlagsP (\opt -> case opt of
                                   "-j" -> True
                                   ('-' : 'j' : d : _) -> isDigit d
                                   _ -> False)
              (PackageDistInexcusable . JUnneeded)

          checkLib = do
            checkP ("-rtsopts" `elem` opts) $
              (PackageBuildWarning $ OptRts title)
            checkP (any (\opt -> "-with-rtsopts" `isPrefixOf` opt) opts)
              (PackageBuildWarning $ OptWithRts title)

          checkTestBench = do
            checkFlags ["-O0", "-Onot"]
              (PackageDistSuspiciousWarn $ OptONot title)

          checkNonTestBench = do
            checkFlags ["-O0", "-Onot"]
              (PackageDistSuspicious $ OptONot title)

          ghcExtension ('-':'f':name) = case name of
            "allow-overlapping-instances"    -> enable  OverlappingInstances
            "no-allow-overlapping-instances" -> disable OverlappingInstances
            "th"                             -> enable  TemplateHaskell
            "no-th"                          -> disable TemplateHaskell
            "ffi"                            -> enable  ForeignFunctionInterface
            "no-ffi"                         -> disable ForeignFunctionInterface
            "fi"                             -> enable  ForeignFunctionInterface
            "no-fi"                          -> disable ForeignFunctionInterface
            "monomorphism-restriction"       -> enable  MonomorphismRestriction
            "no-monomorphism-restriction"    -> disable MonomorphismRestriction
            "mono-pat-binds"                 -> enable  MonoPatBinds
            "no-mono-pat-binds"              -> disable MonoPatBinds
            "allow-undecidable-instances"    -> enable  UndecidableInstances
            "no-allow-undecidable-instances" -> disable UndecidableInstances
            "allow-incoherent-instances"     -> enable  IncoherentInstances
            "no-allow-incoherent-instances"  -> disable IncoherentInstances
            "arrows"                         -> enable  Arrows
            "no-arrows"                      -> disable Arrows
            "generics"                       -> enable  Generics
            "no-generics"                    -> disable Generics
            "implicit-prelude"               -> enable  ImplicitPrelude
            "no-implicit-prelude"            -> disable ImplicitPrelude
            "implicit-params"                -> enable  ImplicitParams
            "no-implicit-params"             -> disable ImplicitParams
            "bang-patterns"                  -> enable  BangPatterns
            "no-bang-patterns"               -> disable BangPatterns
            "scoped-type-variables"          -> enable  ScopedTypeVariables
            "no-scoped-type-variables"       -> disable ScopedTypeVariables
            "extended-default-rules"         -> enable  ExtendedDefaultRules
            "no-extended-default-rules"      -> disable ExtendedDefaultRules
            _                                -> Nothing
          ghcExtension "-cpp"             = enable CPP
          ghcExtension _                  = Nothing

          enable  e = Just (EnableExtension e)
          disable e = Just (DisableExtension e)

          rmRtsOpts :: [String] -> [String]
          rmRtsOpts ("-with-rtsopts":_:xs) = rmRtsOpts xs
          rmRtsOpts (x:xs) = x : rmRtsOpts xs
          rmRtsOpts [] = []

checkCLikeOptions :: Monad m =>
            WarnLang ->     -- Language we are warning about (C or C++).
            CabalField ->   -- Field where we found the error.
            [String] ->     -- Options in string form.
            [String] ->     -- Link options in String form.
                     CheckM m ()
checkCLikeOptions label prefix opts ldOpts = do

        checkAlternatives prefix "include-dirs"
          [(flag, dir) | flag@('-':'I':dir) <- opts]
        checkAlternatives prefix "extra-libraries"
          [(flag, lib) | flag@('-':'l':lib) <- opts]
        checkAlternatives prefix "extra-lib-dirs"
          [(flag, dir) | flag@('-':'L':dir) <- opts]

        checkAlternatives "ld-options" "extra-libraries"
          [(flag, lib) | flag@('-':'l':lib) <- ldOpts]
        checkAlternatives "ld-options" "extra-lib-dirs"
          [(flag, dir) | flag@('-':'L':dir) <- ldOpts]

        checkP (any (`elem` ["-O", "-Os", "-O0", "-O1", "-O2", "-O3"]) opts)
          (PackageDistSuspicious $ COptONumber prefix label)

checkAlternatives :: Monad m =>
            CabalField ->           -- Wrong field.
            CabalField ->           -- Appropriate field.
            [(String, String)] ->   -- List of good and bad flags.
            CheckM m ()
checkAlternatives badField goodField flags = do
        let (badFlags, _) = unzip flags
        checkP (not $ null badFlags)
          (PackageBuildWarning $ OptAlternatives badField goodField flags)

checkCPPOptions :: Monad m =>
            [String] ->   -- Options in String form.
            CheckM m ()
checkCPPOptions opts = do
        checkAlternatives "cpp-options" "include-dirs"
          [(flag, dir) | flag@('-':'I':dir) <- opts]
        mapM_ (\opt -> checkP (not $ any(`isPrefixOf` opt) ["-D", "-U", "-I"])
                        (PackageBuildWarning (COptCPP opt)))
              opts

-- ------------------------------------------------------------
-- * Paths and fields
-- ------------------------------------------------------------

-- Type of path.
data PathKind
    = PathKindFile
    | PathKindDirectory
    | PathKindGlob
  deriving (Eq)

-- Boolean: are absolute paths allowed?
checkPath :: Monad m =>
            Bool ->         -- Can be absolute path?
            CabalField ->   -- .cabal field that we are checking.
            PathKind ->     -- Path type.
            FilePath ->     -- Path.
            CheckM m ()
checkPath isAbs title kind path = do
        checkP (isOutsideTree path)
          (PackageBuildWarning $ RelativeOutside title path)
        checkP (isInsideDist path)
          (PackageDistInexcusable $ DistPoint (Just title) path)
        checkPackageFileNamesWithGlob kind path

        -- Skip if "can be absolute path".
        checkP (not isAbs && isAbsoluteOnAnyPlatform path)
          (PackageDistInexcusable $ AbsolutePath title path)
        case grl path kind of
          Just e -> checkP (not isAbs)
                      (PackageDistInexcusable $ BadRelativePath title path e)
          Nothing -> return ()
        checkWindowsPath (kind == PathKindGlob) path
    where
          isOutsideTree wpath = case splitDirectories wpath of
            "..":_     -> True
            ".":"..":_ -> True
            _          -> False

          -- These are not paths, but globs...
          grl wfp PathKindFile = isGoodRelativeFilePath wfp
          grl wfp PathKindGlob = isGoodRelativeGlob wfp
          grl wfp PathKindDirectory = isGoodRelativeDirectoryPath wfp

-- | Is a 'FilePath' inside `dist`, `dist-newstyle` and friends?
isInsideDist :: FilePath -> Bool
isInsideDist path =
        case map lowercase (splitDirectories path) of
          "dist"    :_ -> True
          ".":"dist":_ -> True
          "dist-newstyle"    :_ -> True
          ".":"dist-newstyle":_ -> True
          _            -> False

checkPackageFileNamesWithGlob :: Monad m =>
            PathKind ->
            FilePath ->  -- Filepath or possibly a glob pattern.
            CheckM m ()
checkPackageFileNamesWithGlob kind fp = do
    checkWindowsPath (kind == PathKindGlob) fp
    checkTarPath fp

checkWindowsPath :: Monad m =>
            Bool ->       -- Is it a glob pattern?
            FilePath ->   -- Path.
            CheckM m ()
checkWindowsPath isGlob path =
        checkP (not . FilePath.Windows.isValid $ escape isGlob path)
          (PackageDistInexcusable $ InvalidOnWin [path])
  where
    -- Force a relative name to catch invalid file names like "f:oo" which
    -- otherwise parse as file "oo" in the current directory on the 'f' drive.
    escape :: Bool -> String -> String
    escape wisGlob wpath = (".\\" ++)
        -- Glob paths will be expanded before being dereferenced, so asterisks
        -- shouldn't count against them.
      $ map (\c -> if c == '*' && wisGlob then 'x' else c) wpath

-- | Check a file name is valid for the portable POSIX tar format.
--
-- The POSIX tar format has a restriction on the length of file names. It is
-- unfortunately not a simple restriction like a maximum length. The exact
-- restriction is that either the whole path be 100 characters or less, or it
-- be possible to split the path on a directory separator such that the first
-- part is 155 characters or less and the second part 100 characters or less.
--
checkTarPath :: Monad m => FilePath -> CheckM m ()
checkTarPath path
  | length path > 255   = tellP longPath
  | otherwise = case pack nameMax (reverse (splitPath path)) of
    Left err           -> tellP err
    Right []           -> return ()
    Right (h:rest) -> case pack prefixMax remainder of
      Left err         -> tellP err
      Right []         -> return ()
      Right (_:_)      -> tellP noSplit
     where
        -- drop the '/' between the name and prefix:
        remainder = safeInit h : rest

  where
    nameMax, prefixMax :: Int
    nameMax   = 100
    prefixMax = 155

    pack _   []     = Left emptyName
    pack maxLen (c:cs)
      | n > maxLen  = Left longName
      | otherwise   = Right (pack' maxLen n cs)
      where n = length c

    pack' maxLen n (c:cs)
      | n' <= maxLen = pack' maxLen n' cs
      where n' = n + length c
    pack' _     _ cs = cs

    longPath = PackageDistInexcusable (FilePathTooLong path)
    longName = PackageDistInexcusable (FilePathNameTooLong path)
    noSplit = PackageDistInexcusable (FilePathSplitTooLong path)
    emptyName = PackageDistInexcusable FilePathEmpty

checkCustomField :: Monad m => (String, String) -> CheckM m ()
checkCustomField (n, _) =
        checkP (any (not . isAscii) n)
          (PackageDistInexcusable $ NonASCIICustomField [n])

-- `checkGlob` checks glob patterns and returns good ones for further
-- processing.
checkGlob :: Monad m =>
                CabalField ->    -- .cabal field we are checking.
                FilePath ->      -- glob filepath pattern
                CheckM m (Maybe Glob)
checkGlob title pat = do
        ver <- asksCM ccSpecVersion

        -- Glob sanity check.
        case parseFileGlob ver pat of
          Left e -> do tellP (PackageDistInexcusable $
                         GlobSyntaxError title (explainGlobSyntaxError pat e))
                       return Nothing
          Right wglob -> do -- * Miscellaneous checks on sane glob.
                            -- Checks for recursive glob in root.
                            checkP (isRecursiveInRoot wglob)
                              (PackageDistSuspiciousWarn $
                               RecursiveGlobInRoot title pat)
                            return (Just wglob)

-- checkMissingDocs will check that we don’t have an interesting file
-- (changes.txt, Changelog.md, NEWS, etc.) in our work-tree which is not
-- present in our .cabal file.
checkMissingDocs :: Monad m =>
                [Glob] ->       -- data-files globs.
                [Glob] ->       -- extra-source-files globs.
                [Glob] ->       -- extra-doc-files globs.
                CheckM m ()
checkMissingDocs dgs esgs edgs = do

    extraDocSupport <- (>= CabalSpecV1_18) <$> asksCM ccSpecVersion

    -- Everything in this block uses CheckPreDistributionOps interface.
    liftInt ciPreDistOps (\ops -> do

        -- 1. Get root files, see if they are interesting to us.
        rootContents <- getDirectoryContentsM ops "."
            -- Recall getDirectoryContentsM arg is relative to root path.
        let des = filter isDesirableExtraDocFile rootContents

        -- 2. Realise Globs.
        let realGlob t = concatMap globMatches <$>
                           mapM (runDirFileGlobM ops "") t
        rgs <- realGlob dgs
        res <- realGlob esgs
        red <- realGlob edgs

        -- 3. Check if anything in 1. is missing in 2.
        let mcs = checkDoc extraDocSupport des (rgs ++ res ++ red)

        -- 4. Check if files are present but in the wrong field.
        let pcsData = checkDocMove extraDocSupport "data-files" des rgs
            pcsSource = if extraDocSupport
                          then checkDocMove extraDocSupport
                                            "extra-source-files" des res
                          else []
            pcs = pcsData ++ pcsSource

        return (mcs ++ pcs))
    where
          -- From Distribution.Simple.Glob.
          globMatches :: [GlobResult a] -> [a]
          globMatches input = [a | GlobMatch a <- input]

          checkDoc :: Bool ->          -- Cabal spec ≥ 1.18?
                      [FilePath] ->    -- Desirables.
                      [FilePath] ->    -- Actuals.
                      [PackageCheck]
          checkDoc b ds as =
                let fds = map ("." </>) $ filter (flip notElem as) ds
                in if null fds then []
                     else [PackageDistSuspiciousWarn $
                             MissingExpectedDocFiles b fds]

          checkDocMove :: Bool ->          -- Cabal spec ≥ 1.18?
                          CabalField ->    -- Name of the field.
                          [FilePath] ->    -- Desirables.
                          [FilePath] ->    -- Actuals.
                          [PackageCheck]
          checkDocMove b field ds as =
                let fds = filter (flip elem as) ds
                in if null fds then []
                     else [PackageDistSuspiciousWarn $
                             WrongFieldForExpectedDocFiles b field fds]

-- Predicate for desirable documentation file on Hackage server.
isDesirableExtraDocFile :: FilePath -> Bool
isDesirableExtraDocFile path = basename `elem` desirableChangeLog &&
                               ext `elem` desirableChangeLogExtensions
  where
        (basename, ext) = splitExtension (map toLower path)

        -- Changelog patterns (basenames & extensions)
        -- Source: hackage-server/src/Distribution/Server/Packages/ChangeLog.hs
        desirableChangeLog = ["news", "changelog", "change_log", "changes"]
        desirableChangeLogExtensions = ["", ".txt", ".md", ".markdown", ".rst"]
        -- [TODO] Check readme. Observations:
        --        • Readme is not necessary if package description is good.
        --        • Some readmes exists only for repository browsing.
        --        • There is currently no reliable way to check what a good
        --          description is; there will be complains if the criterion
        --          is based on the length or number of words (can of worms).
        -- -- Readme patterns
        -- -- Source: hackage-server/src/Distribution/Server/Packages/Readme.hs
        -- desirableReadme = ["readme"]


-- ------------------------------------------------------------
-- * Package and distribution checks
-- ------------------------------------------------------------

-- | Find a package description file in the given directory.  Looks for
-- @.cabal@ files.  Like 'Distribution.Simple.Utils.findPackageDesc',
-- but generalized over monads.
findPackageDesc :: Monad m => CheckPackageContentOps m -> m [FilePath]
findPackageDesc ops = do
        let dir = "."
        files <- getDirectoryContents ops dir
        -- to make sure we do not mistake a ~/.cabal/ dir for a <name>.cabal
        -- file we filter to exclude dirs and null base file names:
        cabalFiles <- filterM (doesFileExist ops)
                         [ dir </> file
                         | file <- files
                         , let (name, ext) = splitExtension file
                         , not (null name) && ext == ".cabal" ]
        return cabalFiles

checkCabalFile :: Monad m => PackageName -> CheckM m ()
checkCabalFile pn = do
        -- liftInt is a bit more messy than stricter interface, but since
        -- each of the following check is exclusive, we can simplify the
        -- condition flow.
        liftInt ciPackageOps (\ops -> do
            -- 1. Get .cabal files.
            ds <- findPackageDesc ops
            case ds of
              [] -> return [PackageBuildImpossible NoDesc]
                -- No .cabal file.
              [d] -> do bc <- bomf ops d
                        return (catMaybes [bc, noMatch d])
                -- BOM + no matching .cabal checks.
              _ -> return [PackageBuildImpossible $ MultiDesc ds])
                -- Multiple .cabal files.
    where
          bomf :: Monad m => CheckPackageContentOps m -> FilePath ->
                             m (Maybe PackageCheck)
          bomf wops wfp = do
              b <- BS.isPrefixOf bomUtf8 <$> getFileContents wops wfp
              if b
                then (return . Just) (PackageDistInexcusable $ BOMStart wfp)
                else return Nothing

          bomUtf8 :: BS.ByteString
          bomUtf8 = BS.pack [0xef,0xbb,0xbf] -- U+FEFF encoded as UTF8

          noMatch :: FilePath -> Maybe PackageCheck
          noMatch wd =
              let expd = unPackageName pn <.> "cabal" in
              if takeFileName wd /= expd
                then Just (PackageDistInexcusable $ NotPackageName wd expd)
                else Nothing

checkLicFileExist :: Monad m => SymbolicPath PackageDir LicenseFile ->
                     CheckM m ()
checkLicFileExist sp = do
          let fp = getSymbolicPath sp
          checkPkg (\ops -> not <$> doesFileExist ops fp)
            (PackageBuildWarning $ UnknownFile "license-file" sp)

checkConfigureExists :: Monad m => BuildType -> CheckM m ()
checkConfigureExists Configure =
        checkPkg (\ops -> not <$> doesFileExist ops "configure")
          (PackageBuildWarning MissingConfigureScript)
checkConfigureExists _ = return ()

checkSetupExists :: Monad m => BuildType -> CheckM m ()
checkSetupExists Simple = return ()
checkSetupExists _ =
        checkPkg (\ops -> do ba <- doesFileExist ops "Setup.hs"
                             bb <- doesFileExist ops "Setup.lhs"
                             return (not $ ba || bb))
          (PackageDistInexcusable MissingSetupFile)

-- The following functions are similar to 'CheckPackageContentOps m' ones,
-- but, as they inspect the files included in the package, but are primarily
-- looking for files in the working tree that may have been missed or other
-- similar problems that can only be detected pre-distribution.
--
-- Because Hackage necessarily checks the uploaded tarball, it is too late to
-- check these on the server; these checks only make sense in the development
-- and package-creation environment.
-- This most likely means we need to use IO, but a dictionary
-- 'CheckPreDistributionOps m' is provided in case in the future such
-- information can come from somewhere else (e.g. VCS filesystem).
--
-- Note: this really shouldn't return any 'Inexcusable' warnings,
-- because that will make us say that Hackage would reject the package.
-- But, because Hackage doesn't yet run these tests, that will be a lie!

checkGlobFile :: Monad m => CabalSpecVersion ->
                 FilePath ->     -- Glob pattern.
                 FilePath ->     -- Folder to check.
                 CabalField ->   -- .cabal field we are checking.
                 CheckM m ()
checkGlobFile cv ddir title fp = do
        let adjDdir = if null ddir then "." else ddir
            dir | title == "data-files" = adjDdir
                | otherwise = "."

        case parseFileGlob cv fp of
          -- We just skip over parse errors here; they're reported elsewhere.
          Left _ -> return ()
          Right parsedGlob -> do
            liftInt ciPreDistOps $ \po -> do
                rs <- runDirFileGlobM po dir parsedGlob
                return $ checkGlobResult title fp rs

-- | Checks for matchless globs and too strict mathching (<2.4 spec).
checkGlobResult ::
                CabalField ->              -- .cabal field we are checking
                FilePath ->                -- Glob pattern (to show the user
                                           -- which pattern is the offending
                                           -- one).
                [GlobResult FilePath] ->   -- List of glob results.
                [PackageCheck]
checkGlobResult title fp rs = dirCheck ++ catMaybes (map getWarning rs)
    where
          dirCheck | all (not . withoutNoMatchesWarning) rs =
                      [PackageDistSuspiciousWarn $ GlobNoMatch title fp]
                   | otherwise = []

          -- If there's a missing directory in play, since our globs don't
          -- (currently) support disjunction, that will always mean there are
          -- no matches. The no matches error in this case is strictly less
          -- informative than the missing directory error.
          withoutNoMatchesWarning (GlobMatch _) = True
          withoutNoMatchesWarning (GlobWarnMultiDot _) = False
          withoutNoMatchesWarning (GlobMissingDirectory _) = True

          getWarning :: GlobResult FilePath -> Maybe PackageCheck
          getWarning (GlobMatch _) = Nothing
          -- Before Cabal 2.4, the extensions of globs had to match the file
          -- exactly. This has been relaxed in 2.4 to allow matching only the
          -- suffix. This warning detects when pre-2.4 package descriptions
          -- are omitting files purely because of the stricter check.
          getWarning (GlobWarnMultiDot file) =
              Just $ PackageDistSuspiciousWarn (GlobExactMatch title fp file)
          getWarning (GlobMissingDirectory dir) =
              Just $ PackageDistSuspiciousWarn (GlobNoDir title fp dir)


-- ------------------------------------------------------------
-- * Other exports and non-traverse checks
-- ------------------------------------------------------------

-- | Wraps `ParseWarning` into `PackageCheck`.
--
wrapParseWarning :: FilePath -> PWarning -> PackageCheck
wrapParseWarning fp pw = PackageDistSuspicious (ParseWarning fp pw)
    -- TODO: as Jul 2022 there is no severity indication attached PWarnType.
    --       Once that is added, we can output something more appropriate
    --       than PackageDistSuspicious for every parse warning.
    --       (see: Cabal-syntax/src/Distribution/Parsec/Warning.hs)

-- Checking duplicated modules cannot unfortunately be done in the
-- “tree checking”. This is because of the monoidal instance in some targets,
-- where e.g. merged dependencies are `nub`’d, hence losing information for
-- this particular check.
checkDuplicateModules :: GenericPackageDescription -> [PackageCheck]
checkDuplicateModules pkg =
  concatMap checkLib (maybe id (:) (condLibrary pkg) . map snd $ condSubLibraries pkg)
    ++ concatMap checkExe (map snd $ condExecutables pkg)
    ++ concatMap checkTest (map snd $ condTestSuites pkg)
    ++ concatMap checkBench (map snd $ condBenchmarks pkg)
  where
    -- the duplicate modules check is has not been thoroughly vetted for backpack
    checkLib = checkDups "library" (\l -> explicitLibModules l ++ map moduleReexportName (reexportedModules l))
    checkExe = checkDups "executable" exeModules
    checkTest = checkDups "test suite" testModules
    checkBench = checkDups "benchmark" benchmarkModules
    checkDups :: String -> (a -> [ModuleName]) -> CondTree v c a -> [PackageCheck]
    checkDups s getModules t =
      let sumPair (x, x') (y, y') = (x + x' :: Int, y + y' :: Int)
          mergePair (x, x') (y, y') = (x + x', max y y')
          maxPair (x, x') (y, y') = (max x x', max y y')
          libMap =
            foldCondTree
              Map.empty
              (\(_, v) -> Map.fromListWith sumPair . map (\x -> (x, (1, 1))) $ getModules v)
              (Map.unionWith mergePair) -- if a module may occur in nonexclusive branches count it twice strictly and once loosely.
              (Map.unionWith maxPair) -- a module occurs the max of times it might appear in exclusive branches
              t
          dupLibsStrict = Map.keys $ Map.filter ((> 1) . fst) libMap
          dupLibsLax = Map.keys $ Map.filter ((> 1) . snd) libMap
       in if not (null dupLibsLax)
            then
              [ PackageBuildImpossible
                  (DuplicateModule s dupLibsLax)
              ]
            else
              if not (null dupLibsStrict)
                then
                  [ PackageDistSuspicious
                      (PotentialDupModule s dupLibsStrict)
                  ]
                else []


-- ------------------------------------------------------------

-- * Utils

-- ------------------------------------------------------------

-- | .cabal field we are referring to. As now it is just a synonym to help
-- reading the code, in the future it might take advantage of typification
-- in Cabal-syntax.
type CabalField = String

-- Remove duplicates from list.
dups :: Ord a => [a] -> [a]
dups xs = [x | (x : _ : _) <- group (sort xs)]

fileExtensionSupportedLanguage :: FilePath -> Bool
fileExtensionSupportedLanguage path =
  isHaskell || isC
  where
    extension = takeExtension path
    isHaskell = extension `elem` [".hs", ".lhs"]
    isC = isJust (filenameCDialect extension)

-- | Whether a path is a good relative path.  We aren't worried about perfect
-- cross-platform compatibility here; this function just checks the paths in
-- the (local) @.cabal@ file, while only Hackage needs the portability.
--
-- >>> let test fp = putStrLn $ show (isGoodRelativeDirectoryPath fp) ++ "; " ++ show (isGoodRelativeFilePath fp)
--
-- Note that "foo./bar.hs" would be invalid on Windows.
--
-- >>> traverse_ test ["foo/bar/quu", "a/b.hs", "foo./bar.hs"]
-- Nothing; Nothing
-- Nothing; Nothing
-- Nothing; Nothing
--
-- Trailing slash is not allowed for files, for directories it is ok.
--
-- >>> test "foo/"
-- Nothing; Just "trailing slash"
--
-- Leading @./@ is fine, but @.@ and @./@ are not valid files.
--
-- >>> traverse_ test [".", "./", "./foo/bar"]
-- Nothing; Just "trailing dot segment"
-- Nothing; Just "trailing slash"
-- Nothing; Nothing
--
-- Lastly, not good file nor directory cases:
--
-- >>> traverse_ test ["", "/tmp/src", "foo//bar", "foo/.", "foo/./bar", "foo/../bar"]
-- Just "empty path"; Just "empty path"
-- Just "posix absolute path"; Just "posix absolute path"
-- Just "empty path segment"; Just "empty path segment"
-- Just "trailing same directory segment: ."; Just "trailing same directory segment: ."
-- Just "same directory segment: ."; Just "same directory segment: ."
-- Just "parent directory segment: .."; Just "parent directory segment: .."
--
-- For the last case, 'isGoodRelativeGlob' doesn't warn:
--
-- >>> traverse_ (print . isGoodRelativeGlob) ["foo/../bar"]
-- Just "parent directory segment: .."
isGoodRelativeFilePath :: FilePath -> Maybe String
isGoodRelativeFilePath = state0
  where
    -- initial state
    state0 [] = Just "empty path"
    state0 (c : cs)
      | c == '.' = state1 cs
      | c == '/' = Just "posix absolute path"
      | otherwise = state5 cs

    -- after initial .
    state1 [] = Just "trailing dot segment"
    state1 (c : cs)
      | c == '.' = state4 cs
      | c == '/' = state2 cs
      | otherwise = state5 cs

    -- after ./ or after / between segments
    state2 [] = Just "trailing slash"
    state2 (c : cs)
      | c == '.' = state3 cs
      | c == '/' = Just "empty path segment"
      | otherwise = state5 cs

    -- after non-first segment's .
    state3 [] = Just "trailing same directory segment: ."
    state3 (c : cs)
      | c == '.' = state4 cs
      | c == '/' = Just "same directory segment: ."
      | otherwise = state5 cs

    -- after ..
    state4 [] = Just "trailing parent directory segment: .."
    state4 (c : cs)
      | c == '.' = state5 cs
      | c == '/' = Just "parent directory segment: .."
      | otherwise = state5 cs

    -- in a segment which is ok.
    state5 [] = Nothing
    state5 (c : cs)
      | c == '.' = state5 cs
      | c == '/' = state2 cs
      | otherwise = state5 cs

-- | See 'isGoodRelativeFilePath'.
--
-- This is barebones function. We check whether the glob is a valid file
-- by replacing stars @*@ with @x@ses.
isGoodRelativeGlob :: FilePath -> Maybe String
isGoodRelativeGlob = isGoodRelativeFilePath . map f
  where
    f '*' = 'x'
    f c = c

-- | See 'isGoodRelativeFilePath'.
isGoodRelativeDirectoryPath :: FilePath -> Maybe String
isGoodRelativeDirectoryPath = state0
  where
    -- initial state
    state0 [] = Just "empty path"
    state0 (c : cs)
      | c == '.' = state5 cs
      | c == '/' = Just "posix absolute path"
      | otherwise = state4 cs

    -- after initial ./ or after / between segments
    state1 [] = Nothing
    state1 (c : cs)
      | c == '.' = state2 cs
      | c == '/' = Just "empty path segment"
      | otherwise = state4 cs

    -- after non-first setgment's .
    state2 [] = Just "trailing same directory segment: ."
    state2 (c : cs)
      | c == '.' = state3 cs
      | c == '/' = Just "same directory segment: ."
      | otherwise = state4 cs

    -- after ..
    state3 [] = Just "trailing parent directory segment: .."
    state3 (c : cs)
      | c == '.' = state4 cs
      | c == '/' = Just "parent directory segment: .."
      | otherwise = state4 cs

    -- in a segment which is ok.
    state4 [] = Nothing
    state4 (c : cs)
      | c == '.' = state4 cs
      | c == '/' = state1 cs
      | otherwise = state4 cs

    -- after initial .
    state5 [] = Nothing -- "."
    state5 (c : cs)
      | c == '.' = state3 cs
      | c == '/' = state1 cs
      | otherwise = state4 cs

-- [Note: Good relative paths]
--
-- Using @kleene@ we can define an extended regex:
--
-- @
-- import Algebra.Lattice
-- import Kleene
-- import Kleene.ERE (ERE (..), intersections)
--
-- data C = CDot | CSlash | CChar
--   deriving (Eq, Ord, Enum, Bounded, Show)
--
-- reservedR :: ERE C
-- reservedR = notChar CSlash
--
-- pathPieceR :: ERE C
-- pathPieceR = intersections
--     [ plus reservedR
--     , ERENot (string [CDot])
--     , ERENot (string [CDot,CDot])
--     ]
--
-- filePathR :: ERE C
-- filePathR = optional (string [CDot, CSlash]) <> pathPieceR <> star (char CSlash <> pathPieceR)
--
-- dirPathR :: ERE C
-- dirPathR = (char CDot \/ filePathR) <> optional (char CSlash)
--
-- plus :: ERE C -> ERE C
-- plus r = r <> star r
--
-- optional :: ERE C -> ERE C
-- optional r = mempty \/ r
-- @
--
-- Results in following state machine for @filePathR@
--
-- @
-- 0 -> \x -> if
--     | x <= CDot           -> 1
--     | otherwise           -> 5
-- 1 -> \x -> if
--     | x <= CDot           -> 4
--     | x <= CSlash         -> 2
--     | otherwise           -> 5
-- 2 -> \x -> if
--     | x <= CDot           -> 3
--     | otherwise           -> 5
-- 3 -> \x -> if
--     | x <= CDot           -> 4
--     | otherwise           -> 5
-- 4 -> \x -> if
--     | x <= CDot           -> 5
--     | otherwise           -> 5
-- 5+ -> \x -> if
--     | x <= CDot           -> 5
--     | x <= CSlash         -> 2
--     | otherwise           -> 5
-- @
--
-- and @dirPathR@:
--
-- @
-- 0 -> \x -> if
--     | x <= CDot           -> 5
--     | otherwise           -> 4
-- 1+ -> \x -> if
--     | x <= CDot           -> 2
--     | otherwise           -> 4
-- 2 -> \x -> if
--     | x <= CDot           -> 3
--     | otherwise           -> 4
-- 3 -> \x -> if
--     | x <= CDot           -> 4
--     | otherwise           -> 4
-- 4+ -> \x -> if
--     | x <= CDot           -> 4
--     | x <= CSlash         -> 1
--     | otherwise           -> 4
-- 5+ -> \x -> if
--     | x <= CDot           -> 3
--     | x <= CSlash         -> 1
--     | otherwise           -> 4
-- @

-- | August 2022: this function is an oddity due to the historical
-- GenericPackageDescription/PackageDescription split (check
-- Distribution.Types.PackageDescription for a description of the relationship
-- between GPD and PD.
-- It is only maintained not to break interface, should be deprecated in the
-- future in favour of `checkPackage` when PD and GPD are refactored sensibly.
pd2gpd :: PackageDescription -> GenericPackageDescription
pd2gpd pd = gpd
    where
          gpd :: GenericPackageDescription
          gpd = emptyGenericPackageDescription {
                    packageDescription = pd,
                    condLibrary = fmap t2c (library pd),
                    condSubLibraries = map (t2cName ln id) (subLibraries pd),
                    condForeignLibs = map (t2cName foreignLibName id)
                                          (foreignLibs pd),
                    condExecutables = map (t2cName exeName id)
                                          (executables pd),
                    condTestSuites = map (t2cName testName remTest)
                                         (testSuites pd),
                    condBenchmarks = map (t2cName benchmarkName remBench)
                                         (benchmarks pd) }

          -- From target to simple, unconditional CondTree.
          t2c :: a -> CondTree ConfVar [Dependency] a
          t2c a = CondNode a [] []

          -- From named target to unconditional CondTree. Notice we have
          -- a function to extract the name *and* a function to modify
          -- the target. This is needed for 'initTargetAnnotation' to work
          -- properly and to contain all the quirks inside 'pd2gpd'.
          t2cName :: (a -> UnqualComponentName) -> (a -> a) -> a ->
                     (UnqualComponentName, CondTree ConfVar [Dependency] a)
          t2cName nf mf a = (nf a, t2c . mf $ a)

          ln :: Library -> UnqualComponentName
          ln wl = case libName wl of
                   (LSubLibName u) -> u
                   LMainLibName -> mkUnqualComponentName "main-library"

          remTest :: TestSuite -> TestSuite
          remTest t = t { testName = mempty }

          remBench :: Benchmark -> Benchmark
          remBench b = b { benchmarkName = mempty }

