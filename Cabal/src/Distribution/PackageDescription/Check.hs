{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  , CheckExplanationID
  , CheckExplanationIDString
  , PackageCheck (..)
  , checkPackage
  , checkConfiguredPackage
  , wrapParseWarning
  , ppPackageCheck
  , ppCheckExplanationId
  , isHackageDistError
  , filterPackageChecksById
  , filterPackageChecksByIdString

    -- ** Checking package contents
  , checkPackageFiles
  , checkPackageFilesGPD
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
import Distribution.Package
import Distribution.PackageDescription
import Distribution.PackageDescription.Check.Common
import Distribution.PackageDescription.Check.Conditional
import Distribution.PackageDescription.Check.Monad
import Distribution.PackageDescription.Check.Paths
import Distribution.PackageDescription.Check.Target
import Distribution.PackageDescription.Check.Warning
import Distribution.Parsec.Warning (PWarning)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.Glob
  ( Glob
  , GlobResult (..)
  , globMatches
  , parseFileGlob
  , runDirFileGlob
  )
import Distribution.Simple.Utils hiding (findPackageDesc, notice)
import Distribution.Utils.Generic (isAscii)
import Distribution.Utils.Path
import Distribution.Verbosity
import Distribution.Version
import System.FilePath (splitExtension, takeFileName)

import qualified Data.ByteString.Lazy as BS
import qualified Distribution.SPDX as SPDX
import qualified System.Directory as System

import qualified System.Directory (getDirectoryContents)
import qualified System.FilePath.Windows as FilePath.Windows (isValid)

import qualified Data.Set as Set
import qualified Distribution.Utils.ShortText as ShortText

import qualified Distribution.Types.GenericPackageDescription.Lens as L

import Control.Monad

-- $setup
-- >>> import Control.Arrow ((&&&))

-- ☞ N.B.
--
-- Part of the tools/scaffold used to perform check is found in
-- Distribution.PackageDescription.Check.Types. Summary of that module (for
-- how we use it here):
-- 1. we work inside a 'CheckM m a' monad (where `m` is an abstraction to
--    run non-pure checks);
-- 2. 'checkP', 'checkPre' functions perform checks (respectively pure and
--    non-pure);
-- 3. 'PackageCheck' and 'CheckExplanation' are types for warning severity
--    and description.

-- ------------------------------------------------------------
-- Checking interface
-- ------------------------------------------------------------

-- | 'checkPackagePrim' is the most general way to invoke package checks.
-- We pass to it two interfaces (one to check contents of packages, the
-- other to inspect working tree for orphan files) and before that a
-- Boolean to indicate whether we want pure checks or not. Based on these
-- parameters, some checks will be performed, some omitted.
-- Generality over @m@ means we could do non pure checks in monads other
-- than IO (e.g. a virtual filesystem, like a zip file, a VCS filesystem,
-- etc).
checkPackagePrim
  :: Monad m
  => Bool -- Perform pure checks?
  -> Maybe (CheckPackageContentOps m) -- Package content interface.
  -> Maybe (CheckPreDistributionOps m) -- Predist checks interface.
  -> GenericPackageDescription -- GPD to check.
  -> m [PackageCheck]
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
checkPackageContent
  :: Monad m
  => CheckPackageContentOps m
  -> GenericPackageDescription
  -> m [PackageCheck]
checkPackageContent pops gpd = checkPackagePrim False (Just pops) Nothing gpd

-- | Sanity checks that require IO. 'checkPackageFiles' looks at the files
-- in the package and expects to find the package unpacked at the given
-- filepath.
checkPackageFilesGPD
  :: Verbosity -- Glob warn message verbosity.
  -> GenericPackageDescription
  -> FilePath -- Package root.
  -> IO [PackageCheck]
checkPackageFilesGPD verbosity gpd root =
  checkPackagePrim False (Just checkFilesIO) (Just checkPreIO) gpd
  where
    checkFilesIO =
      CheckPackageContentOps
        { doesFileExist = System.doesFileExist . relative
        , doesDirectoryExist = System.doesDirectoryExist . relative
        , getDirectoryContents = System.Directory.getDirectoryContents . relative
        , getFileContents = BS.readFile . relative
        }

    checkPreIO =
      CheckPreDistributionOps
        { runDirFileGlobM = \fp g -> runDirFileGlob verbosity (Just . specVersion $ packageDescription gpd) (root </> fp) g
        , getDirectoryContentsM = System.Directory.getDirectoryContents . relative
        }

    relative :: FilePath -> FilePath
    relative path = root </> path

-- | Same as  'checkPackageFilesGPD', but working with 'PackageDescription'.
--
-- This function is included for legacy reasons, use 'checkPackageFilesGPD'
-- if you are working with 'GenericPackageDescription'.
checkPackageFiles
  :: Verbosity -- Glob warn message verbosity.
  -> PackageDescription
  -> FilePath -- Package root.
  -> IO [PackageCheck]
checkPackageFiles verbosity pd oot =
  checkPackageFilesGPD verbosity (pd2gpd pd) oot

-- ------------------------------------------------------------
-- Package description
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
checkGenericPackageDescription
  :: Monad m
  => GenericPackageDescription
  -> CheckM m ()
checkGenericPackageDescription
  gpd@( GenericPackageDescription
          packageDescription_
          _gpdScannedVersion_
          genPackageFlags_
          condLibrary_
          condSubLibraries_
          condForeignLibs_
          condExecutables_
          condTestSuites_
          condBenchmarks_
        ) =
    do
      -- § Description and names.
      checkPackageDescription packageDescription_
      -- Targets should be present...
      let condAllLibraries =
            maybeToList condLibrary_
              ++ (map snd condSubLibraries_)
      checkP
        ( and
            [ null condExecutables_
            , null condTestSuites_
            , null condBenchmarks_
            , null condAllLibraries
            , null condForeignLibs_
            ]
        )
        (PackageBuildImpossible NoTarget)
      -- ... and have unique names (names are not under conditional, it is
      -- appropriate to check here.
      (nsubs, nexes, ntests, nbenchs) <-
        asksCM
          ( ( \n ->
                ( pnSubLibs n
                , pnExecs n
                , pnTests n
                , pnBenchs n
                )
            )
              . ccNames
          )
      let names = concat [nsubs, nexes, ntests, nbenchs]
          dupes = dups names
      checkP
        (not . null $ dups names)
        (PackageBuildImpossible $ DuplicateSections dupes)
      -- PackageDescription checks.
      checkPackageDescription packageDescription_
      -- Flag names.
      mapM_ checkFlagName genPackageFlags_

      -- § Feature checks.
      checkSpecVer
        CabalSpecV2_0
        (not . null $ condSubLibraries_)
        (PackageDistInexcusable CVMultiLib)
      checkSpecVer
        CabalSpecV1_8
        (not . null $ condTestSuites_)
        (PackageDistInexcusable CVTestSuite)

      -- § Conditional targets

      -- Extract dependencies from libraries, to be passed along for
      -- PVP checks purposes.
      pName <-
        asksCM
          ( packageNameToUnqualComponentName
              . pkgName
              . pnPackageId
              . ccNames
          )
      let ads =
            maybe [] ((: []) . extractAssocDeps pName) condLibrary_
              ++ map (uncurry extractAssocDeps) condSubLibraries_

      case condLibrary_ of
        Just cl ->
          checkCondTarget
            genPackageFlags_
            (checkLibrary False ads)
            (const id)
            (mempty, cl)
        Nothing -> return ()
      mapM_
        ( checkCondTarget
            genPackageFlags_
            (checkLibrary False ads)
            (\u l -> l{libName = maybeToLibraryName (Just u)})
        )
        condSubLibraries_
      mapM_
        ( checkCondTarget
            genPackageFlags_
            checkForeignLib
            (const id)
        )
        condForeignLibs_
      mapM_
        ( checkCondTarget
            genPackageFlags_
            (checkExecutable ads)
            (const id)
        )
        condExecutables_
      mapM_
        ( checkCondTarget
            genPackageFlags_
            (checkTestSuite ads)
            (\u l -> l{testName = u})
        )
        condTestSuites_
      mapM_
        ( checkCondTarget
            genPackageFlags_
            (checkBenchmark ads)
            (\u l -> l{benchmarkName = u})
        )
        condBenchmarks_

      -- For unused flags it is clearer and more convenient to fold the
      -- data rather than walk it, an exception to the rule.
      checkP
        (decFlags /= usedFlags)
        (PackageDistSuspicious $ DeclaredUsedFlags decFlags usedFlags)

      -- Duplicate modules.
      mapM_ tellP (checkDuplicateModules gpd)
    where
      -- todo is this caught at parse time?
      checkFlagName :: Monad m => PackageFlag -> CheckM m ()
      checkFlagName pf =
        let fn = unFlagName . flagName $ pf

            invalidFlagName ('-' : _) = True -- starts with dash
            invalidFlagName cs = any (not . isAscii) cs -- non ASCII
         in checkP
              (invalidFlagName fn)
              (PackageDistInexcusable $ SuspiciousFlagName [fn])

      decFlags :: Set.Set FlagName
      decFlags = toSetOf (L.genPackageFlags . traverse . L.flagName) gpd

      usedFlags :: Set.Set FlagName
      usedFlags =
        mconcat
          [ toSetOf (L.condLibrary . traverse . traverseCondTreeV . L._PackageFlag) gpd
          , toSetOf (L.condSubLibraries . traverse . _2 . traverseCondTreeV . L._PackageFlag) gpd
          , toSetOf (L.condForeignLibs . traverse . _2 . traverseCondTreeV . L._PackageFlag) gpd
          , toSetOf (L.condExecutables . traverse . _2 . traverseCondTreeV . L._PackageFlag) gpd
          , toSetOf (L.condTestSuites . traverse . _2 . traverseCondTreeV . L._PackageFlag) gpd
          , toSetOf (L.condBenchmarks . traverse . _2 . traverseCondTreeV . L._PackageFlag) gpd
          ]

checkPackageDescription :: Monad m => PackageDescription -> CheckM m ()
checkPackageDescription
  pkg@( PackageDescription
          specVersion_
          package_
          licenseRaw_
          licenseFiles_
          _copyright_
          maintainer_
          _author_
          _stability_
          testedWith_
          _homepage_
          _pkgUrl_
          _bugReports_
          sourceRepos_
          synopsis_
          description_
          category_
          customFieldsPD_
          buildTypeRaw_
          setupBuildInfo_
          _library_
          _subLibraries_
          _executables_
          _foreignLibs_
          _testSuites_
          _benchmarks_
          dataFiles_
          dataDir_
          extraSrcFiles_
          extraTmpFiles_
          extraDocFiles_
        ) = do
    -- § Sanity checks.
    checkPackageId package_
    -- TODO `name` is caught at parse level, remove this test.
    let pn = packageName package_
    checkP
      (null . unPackageName $ pn)
      (PackageBuildImpossible NoNameField)
    -- TODO `version` is caught at parse level, remove this test.
    checkP
      (nullVersion == packageVersion package_)
      (PackageBuildImpossible NoVersionField)
    -- But it is OK for executables to have the same name.
    nsubs <- asksCM (pnSubLibs . ccNames)
    checkP
      (any (== prettyShow pn) (prettyShow <$> nsubs))
      (PackageBuildImpossible $ IllegalLibraryName pn)

    -- § Fields check.
    checkNull
      category_
      (PackageDistSuspicious MissingFieldCategory)
    checkNull
      maintainer_
      (PackageDistSuspicious MissingFieldMaintainer)
    checkP
      (ShortText.null synopsis_ && not (ShortText.null description_))
      (PackageDistSuspicious MissingFieldSynopsis)
    checkP
      (ShortText.null description_ && not (ShortText.null synopsis_))
      (PackageDistSuspicious MissingFieldDescription)
    checkP
      (all ShortText.null [synopsis_, description_])
      (PackageDistInexcusable MissingFieldSynOrDesc)
    checkP
      (ShortText.length synopsis_ > 80)
      (PackageDistSuspicious SynopsisTooLong)
    checkP
      ( not (ShortText.null description_)
          && ShortText.length description_ <= ShortText.length synopsis_
      )
      (PackageDistSuspicious ShortDesc)

    -- § Paths.
    mapM_ (checkPath False "extra-source-files" PathKindGlob . getSymbolicPath) extraSrcFiles_
    mapM_ (checkPath False "extra-tmp-files" PathKindFile . getSymbolicPath) extraTmpFiles_
    mapM_ (checkPath False "extra-doc-files" PathKindGlob . getSymbolicPath) extraDocFiles_
    mapM_ (checkPath False "data-files" PathKindGlob . getSymbolicPath) dataFiles_
    let rawDataDir = getSymbolicPath dataDir_
    checkPath True "data-dir" PathKindDirectory rawDataDir
    let licPaths = map getSymbolicPath licenseFiles_
    mapM_ (checkPath False "license-file" PathKindFile) licPaths
    mapM_ checkLicFileExist licenseFiles_

    -- § Globs.
    dataGlobs <- mapM (checkGlob "data-files" . getSymbolicPath) dataFiles_
    extraGlobs <- mapM (checkGlob "extra-source-files" . getSymbolicPath) extraSrcFiles_
    docGlobs <- mapM (checkGlob "extra-doc-files" . getSymbolicPath) extraDocFiles_
    -- We collect globs to feed them to checkMissingDocs.

    -- § Missing documentation.
    checkMissingDocs
      (catMaybes dataGlobs)
      (catMaybes extraGlobs)
      (catMaybes docGlobs)

    -- § Datafield checks.
    checkSetupBuildInfo setupBuildInfo_
    mapM_ checkTestedWith testedWith_
    either
      checkNewLicense
      (checkOldLicense $ null licenseFiles_)
      licenseRaw_
    checkSourceRepos sourceRepos_
    mapM_ checkCustomField customFieldsPD_

    -- Feature checks.
    checkSpecVer
      CabalSpecV1_18
      (not . null $ extraDocFiles_)
      (PackageDistInexcusable CVExtraDocFiles)
    checkSpecVer
      CabalSpecV1_6
      (not . null $ sourceRepos_)
      (PackageDistInexcusable CVSourceRepository)
    checkP
      ( specVersion_ >= CabalSpecV1_24
          && isNothing setupBuildInfo_
          && buildTypeRaw_ == Just Custom
      )
      (PackageBuildWarning CVCustomSetup)
    checkSpecVer
      CabalSpecV1_24
      ( isNothing setupBuildInfo_
          && buildTypeRaw_ == Just Custom
      )
      (PackageDistSuspiciousWarn CVExpliticDepsCustomSetup)
    checkP
      (isNothing buildTypeRaw_ && specVersion_ < CabalSpecV2_2)
      (PackageBuildWarning NoBuildType)
    checkP
      (isJust setupBuildInfo_ && buildType pkg /= Custom)
      (PackageBuildWarning NoCustomSetup)

    -- Contents.
    checkConfigureExists (buildType pkg)
    checkSetupExists (buildType pkg)
    checkCabalFile (packageName pkg)
    mapM_ (checkGlobFile specVersion_ "." "extra-source-files" . getSymbolicPath) extraSrcFiles_
    mapM_ (checkGlobFile specVersion_ "." "extra-doc-files" . getSymbolicPath) extraDocFiles_
    mapM_ (checkGlobFile specVersion_ rawDataDir "data-files" . getSymbolicPath) dataFiles_
    where
      checkNull
        :: Monad m
        => ShortText.ShortText
        -> PackageCheck
        -> CheckM m ()
      checkNull st c = checkP (ShortText.null st) c

      checkTestedWith
        :: Monad m
        => (CompilerFlavor, VersionRange)
        -> CheckM m ()
      checkTestedWith (OtherCompiler n, _) =
        tellP (PackageBuildWarning $ UnknownCompilers [n])
      checkTestedWith (compiler, versionRange) =
        checkVersionRange compiler versionRange

      checkVersionRange
        :: Monad m
        => CompilerFlavor
        -> VersionRange
        -> CheckM m ()
      checkVersionRange cmp vr =
        when
          (isNoVersion vr)
          ( let dep =
                  [ Dependency
                      (mkPackageName (prettyShow cmp))
                      vr
                      mainLibSet
                  ]
             in tellP (PackageDistInexcusable (InvalidTestWith dep))
          )

checkSetupBuildInfo :: Monad m => Maybe SetupBuildInfo -> CheckM m ()
checkSetupBuildInfo Nothing = return ()
checkSetupBuildInfo (Just (SetupBuildInfo ds _)) = do
  let uqs = map mkUnqualComponentName ["base", "Cabal"]
  (is, rs) <- partitionDeps [] uqs ds
  let ick = PackageDistInexcusable . UpperBoundSetup
      rck =
        PackageDistSuspiciousWarn
          . MissingUpperBounds CETSetup
  checkPVP ick is
  checkPVPs rck rs

checkPackageId :: Monad m => PackageIdentifier -> CheckM m ()
checkPackageId (PackageIdentifier pkgName_ _pkgVersion_) = do
  checkP
    (not . FilePath.Windows.isValid . prettyShow $ pkgName_)
    (PackageDistInexcusable $ InvalidNameWin pkgName_)
  checkP (isPrefixOf "z-" . prettyShow $ pkgName_) $
    (PackageDistInexcusable ZPrefix)

checkNewLicense :: Monad m => SPDX.License -> CheckM m ()
checkNewLicense lic = do
  checkP
    (lic == SPDX.NONE)
    (PackageDistInexcusable NONELicense)

checkOldLicense
  :: Monad m
  => Bool -- Flag: no license file?
  -> License
  -> CheckM m ()
checkOldLicense nullLicFiles lic = do
  checkP
    (lic == UnspecifiedLicense)
    (PackageDistInexcusable NoLicense)
  checkP
    (lic == AllRightsReserved)
    (PackageDistSuspicious AllRightsReservedLicense)
  checkSpecVer
    CabalSpecV1_4
    (lic `notElem` compatLicenses)
    (PackageDistInexcusable (LicenseMessParse lic))
  checkP
    (lic == BSD4)
    (PackageDistSuspicious UncommonBSD4)
  case lic of
    UnknownLicense l ->
      tellP (PackageBuildWarning (UnrecognisedLicense l))
    _ -> return ()
  checkP
    ( lic
        `notElem` [ AllRightsReserved
                  , UnspecifiedLicense
                  , PublicDomain
                  ]
        &&
        -- AllRightsReserved and PublicDomain are not strictly
        -- licenses so don't need license files.
        nullLicFiles
    )
    $ (PackageDistSuspicious NoLicenseFile)
  case unknownLicenseVersion lic of
    Just knownVersions ->
      tellP
        (PackageDistSuspicious $ UnknownLicenseVersion lic knownVersions)
    _ -> return ()
  where
    compatLicenses =
      [ GPL Nothing
      , LGPL Nothing
      , AGPL Nothing
      , BSD3
      , BSD4
      , PublicDomain
      , AllRightsReserved
      , UnspecifiedLicense
      , OtherLicense
      ]

    unknownLicenseVersion (GPL (Just v))
      | v `notElem` knownVersions = Just knownVersions
      where
        knownVersions = [v' | GPL (Just v') <- knownLicenses]
    unknownLicenseVersion (LGPL (Just v))
      | v `notElem` knownVersions = Just knownVersions
      where
        knownVersions = [v' | LGPL (Just v') <- knownLicenses]
    unknownLicenseVersion (AGPL (Just v))
      | v `notElem` knownVersions = Just knownVersions
      where
        knownVersions = [v' | AGPL (Just v') <- knownLicenses]
    unknownLicenseVersion (Apache (Just v))
      | v `notElem` knownVersions = Just knownVersions
      where
        knownVersions = [v' | Apache (Just v') <- knownLicenses]
    unknownLicenseVersion _ = Nothing

checkSourceRepos :: Monad m => [SourceRepo] -> CheckM m ()
checkSourceRepos rs = do
  mapM_ repoCheck rs
  checkMissingVcsInfo rs
  where
    -- Single repository checks.
    repoCheck :: Monad m => SourceRepo -> CheckM m ()
    repoCheck
      ( SourceRepo
          repoKind_
          repoType_
          repoLocation_
          repoModule_
          _repoBranch_
          repoTag_
          repoSubdir_
        ) = do
        case repoKind_ of
          RepoKindUnknown kind ->
            tellP
              (PackageDistInexcusable $ UnrecognisedSourceRepo kind)
          _ -> return ()
        checkP
          (isNothing repoType_)
          (PackageDistInexcusable MissingType)
        checkP
          (isNothing repoLocation_)
          (PackageDistInexcusable MissingLocation)
        checkP
          ( repoType_ == Just (KnownRepoType CVS)
              && isNothing repoModule_
          )
          (PackageDistInexcusable MissingModule)
        checkP
          (repoKind_ == RepoThis && isNothing repoTag_)
          (PackageDistInexcusable MissingTag)
        checkP
          (any isAbsoluteOnAnyPlatform repoSubdir_)
          (PackageDistInexcusable SubdirRelPath)
        case join . fmap isGoodRelativeDirectoryPath $ repoSubdir_ of
          Just err ->
            tellP
              (PackageDistInexcusable $ SubdirGoodRelPath err)
          Nothing -> return ()

checkMissingVcsInfo :: Monad m => [SourceRepo] -> CheckM m ()
checkMissingVcsInfo rs =
  let rdirs = concatMap repoTypeDirname knownRepoTypes
   in checkPkg
        ( \ops -> do
            us <- or <$> traverse (doesDirectoryExist ops) rdirs
            return (null rs && us)
        )
        (PackageDistSuspicious MissingSourceControl)
  where
    repoTypeDirname :: KnownRepoType -> [FilePath]
    repoTypeDirname Darcs = ["_darcs"]
    repoTypeDirname Git = [".git"]
    repoTypeDirname SVN = [".svn"]
    repoTypeDirname CVS = ["CVS"]
    repoTypeDirname Mercurial = [".hg"]
    repoTypeDirname GnuArch = [".arch-params"]
    repoTypeDirname Bazaar = [".bzr"]
    repoTypeDirname Monotone = ["_MTN"]
    repoTypeDirname Pijul = [".pijul"]

-- ------------------------------------------------------------
-- Package and distribution checks
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
  cabalFiles <-
    filterM
      (doesFileExist ops)
      [ dir </> file
      | file <- files
      , let (name, ext) = splitExtension file
      , not (null name) && ext == ".cabal"
      ]
  return cabalFiles

checkCabalFile :: Monad m => PackageName -> CheckM m ()
checkCabalFile pn = do
  -- liftInt is a bit more messy than stricter interface, but since
  -- each of the following check is exclusive, we can simplify the
  -- condition flow.
  liftInt
    ciPackageOps
    ( \ops -> do
        -- 1. Get .cabal files.
        ds <- findPackageDesc ops
        case ds of
          [] -> return [PackageBuildImpossible NoDesc]
          -- No .cabal file.
          [d] -> do
            bc <- bomf ops d
            return (catMaybes [bc, noMatch d])
          -- BOM + no matching .cabal checks.
          _ -> return [PackageBuildImpossible $ MultiDesc ds]
    )
  where
    -- Multiple .cabal files.

    bomf
      :: Monad m
      => CheckPackageContentOps m
      -> FilePath
      -> m (Maybe PackageCheck)
    bomf wops wfp = do
      b <- BS.isPrefixOf bomUtf8 <$> getFileContents wops wfp
      if b
        then (return . Just) (PackageDistInexcusable $ BOMStart wfp)
        else return Nothing

    bomUtf8 :: BS.ByteString
    bomUtf8 = BS.pack [0xef, 0xbb, 0xbf] -- U+FEFF encoded as UTF8
    noMatch :: FilePath -> Maybe PackageCheck
    noMatch wd =
      let expd = unPackageName pn <.> "cabal"
       in if takeFileName wd /= expd
            then Just (PackageDistInexcusable $ NotPackageName wd expd)
            else Nothing

checkLicFileExist
  :: Monad m
  => RelativePath Pkg File
  -> CheckM m ()
checkLicFileExist sp = do
  let fp = getSymbolicPath sp
  checkPkg
    (\ops -> not <$> doesFileExist ops fp)
    (PackageBuildWarning $ UnknownFile "license-file" sp)

checkConfigureExists :: Monad m => BuildType -> CheckM m ()
checkConfigureExists Configure =
  checkPkg
    (\ops -> not <$> doesFileExist ops "configure")
    (PackageBuildWarning MissingConfigureScript)
checkConfigureExists _ = return ()

checkSetupExists :: Monad m => BuildType -> CheckM m ()
checkSetupExists Simple = return ()
checkSetupExists _ =
  checkPkg
    ( \ops -> do
        ba <- doesFileExist ops "Setup.hs"
        bb <- doesFileExist ops "Setup.lhs"
        return (not $ ba || bb)
    )
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

checkGlobFile
  :: Monad m
  => CabalSpecVersion
  -> FilePath -- Glob pattern.
  -> FilePath -- Folder to check.
  -> CabalField -- .cabal field we are checking.
  -> CheckM m ()
checkGlobFile cv ddir title fp = do
  let adjDdir = if null ddir then "." else ddir
      dir
        | title == "data-files" = adjDdir
        | otherwise = "."

  case parseFileGlob cv fp of
    -- We just skip over parse errors here; they're reported elsewhere.
    Left _ -> return ()
    Right parsedGlob -> do
      liftInt ciPreDistOps $ \po -> do
        rs <- runDirFileGlobM po dir parsedGlob
        return $ checkGlobResult title fp rs

-- | Checks for matchless globs and too strict matching (<2.4 spec).
checkGlobResult
  :: CabalField -- .cabal field we are checking
  -> FilePath -- Glob pattern (to show the user
  -- which pattern is the offending
  -- one).
  -> [GlobResult FilePath] -- List of glob results.
  -> [PackageCheck]
checkGlobResult title fp rs = dirCheck ++ catMaybes (map getWarning rs)
  where
    dirCheck
      | all (not . withoutNoMatchesWarning) rs =
          [PackageDistSuspiciousWarn $ GlobNoMatch title fp]
      | otherwise = []

    -- If there's a missing directory in play, since globs in Cabal packages
    -- don't (currently) support disjunction, that will always mean there are
    -- no matches. The no matches error in this case is strictly less
    -- informative than the missing directory error.
    withoutNoMatchesWarning (GlobMatch _) = True
    withoutNoMatchesWarning (GlobWarnMultiDot _) = False
    withoutNoMatchesWarning (GlobMissingDirectory _) = True
    withoutNoMatchesWarning (GlobMatchesDirectory _) = True

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
    -- GlobMatchesDirectory is handled elsewhere if relevant;
    -- we can discard it here.
    getWarning (GlobMatchesDirectory _) = Nothing

-- ------------------------------------------------------------
-- Other exports
-- ------------------------------------------------------------

-- | Wraps `ParseWarning` into `PackageCheck`.
wrapParseWarning :: FilePath -> PWarning -> PackageCheck
wrapParseWarning fp pw = PackageDistSuspicious (ParseWarning fp pw)

-- TODO: as Jul 2022 there is no severity indication attached PWarnType.
--       Once that is added, we can output something more appropriate
--       than PackageDistSuspicious for every parse warning.
--       (see: Cabal-syntax/src/Distribution/Parsec/Warning.hs)

-- ------------------------------------------------------------
-- Ancillaries
-- ------------------------------------------------------------

-- Gets a list of dependencies from a Library target to pass to PVP related
-- functions. We are not doing checks here: this is not imprecise, as the
-- library itself *will* be checked for PVP errors.
-- Same for branch merging,
-- each of those branch will be checked one by one.
extractAssocDeps
  :: UnqualComponentName -- Name of the target library
  -> CondTree ConfVar [Dependency] Library
  -> AssocDep
extractAssocDeps n ct =
  let a = ignoreConditions ct
   in -- Merging is fine here, remember the specific
      -- library dependencies will be checked branch
      -- by branch.
      (n, snd a)

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
    gpd =
      emptyGenericPackageDescription
        { packageDescription = pd
        , condLibrary = fmap t2c (library pd)
        , condSubLibraries = map (t2cName ln id) (subLibraries pd)
        , condForeignLibs =
            map
              (t2cName foreignLibName id)
              (foreignLibs pd)
        , condExecutables =
            map
              (t2cName exeName id)
              (executables pd)
        , condTestSuites =
            map
              (t2cName testName remTest)
              (testSuites pd)
        , condBenchmarks =
            map
              (t2cName benchmarkName remBench)
              (benchmarks pd)
        }

    -- From target to simple, unconditional CondTree.
    t2c :: a -> CondTree ConfVar [Dependency] a
    t2c a = CondNode a [] []

    -- From named target to unconditional CondTree. Notice we have
    -- a function to extract the name *and* a function to modify
    -- the target. This is needed for 'initTargetAnnotation' to work
    -- properly and to contain all the quirks inside 'pd2gpd'.
    t2cName
      :: (a -> UnqualComponentName)
      -> (a -> a)
      -> a
      -> (UnqualComponentName, CondTree ConfVar [Dependency] a)
    t2cName nf mf a = (nf a, t2c . mf $ a)

    ln :: Library -> UnqualComponentName
    ln wl = case libName wl of
      (LSubLibName u) -> u
      LMainLibName -> mkUnqualComponentName "main-library"

    remTest :: TestSuite -> TestSuite
    remTest t = t{testName = mempty}

    remBench :: Benchmark -> Benchmark
    remBench b = b{benchmarkName = mempty}

-- checkMissingDocs will check that we don’t have an interesting file
-- (changes.txt, Changelog.md, NEWS, etc.) in our work-tree which is not
-- present in our .cabal file.
checkMissingDocs
  :: Monad m
  => [Glob] -- data-files globs.
  -> [Glob] -- extra-source-files globs.
  -> [Glob] -- extra-doc-files globs.
  -> CheckM m ()
checkMissingDocs dgs esgs edgs = do
  extraDocSupport <- (>= CabalSpecV1_18) <$> asksCM ccSpecVersion

  -- Everything in this block uses CheckPreDistributionOps interface.
  liftInt
    ciPreDistOps
    ( \ops -> do
        -- 1. Get root files, see if they are interesting to us.
        rootContents <- getDirectoryContentsM ops "."
        -- Recall getDirectoryContentsM arg is relative to root path.
        let des = filter isDesirableExtraDocFile rootContents

        -- 2. Realise Globs.
        let realGlob t =
              concatMap globMatches
                <$> mapM (runDirFileGlobM ops "") t
        rgs <- realGlob dgs
        res <- realGlob esgs
        red <- realGlob edgs

        -- 3. Check if anything in 1. is missing in 2.
        let mcs = checkDoc extraDocSupport des (rgs ++ res ++ red)

        -- 4. Check if files are present but in the wrong field.
        let pcsData = checkDocMove extraDocSupport "data-files" des rgs
            pcsSource =
              if extraDocSupport
                then
                  checkDocMove
                    extraDocSupport
                    "extra-source-files"
                    des
                    res
                else []
            pcs = pcsData ++ pcsSource

        return (mcs ++ pcs)
    )
  where
    checkDoc
      :: Bool -- Cabal spec ≥ 1.18?
      -> [FilePath] -- Desirables.
      -> [FilePath] -- Actuals.
      -> [PackageCheck]
    checkDoc b ds as =
      let fds = map ("." </>) $ filter (flip notElem as) ds
       in if null fds
            then []
            else
              [ PackageDistSuspiciousWarn $
                  MissingExpectedDocFiles b fds
              ]

    checkDocMove
      :: Bool -- Cabal spec ≥ 1.18?
      -> CabalField -- Name of the field.
      -> [FilePath] -- Desirables.
      -> [FilePath] -- Actuals.
      -> [PackageCheck]
    checkDocMove b field ds as =
      let fds = filter (flip elem as) ds
       in if null fds
            then []
            else
              [ PackageDistSuspiciousWarn $
                  WrongFieldForExpectedDocFiles b field fds
              ]

-- Predicate for desirable documentation file on Hackage server.
isDesirableExtraDocFile :: FilePath -> Bool
isDesirableExtraDocFile path =
  basename `elem` desirableChangeLog
    && ext `elem` desirableChangeLogExtensions
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

-- Remove duplicates from list.
dups :: Ord a => [a] -> [a]
dups xs = [x | (x : _ : _) <- group (sort xs)]
