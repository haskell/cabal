module Distribution.Client.Init.Simple
  ( -- * Project creation
    createProject

    -- * Gen targets
  , genSimplePkgDesc
  , genSimpleLibTarget
  , genSimpleExeTarget
  , genSimpleTestTarget
  ) where

import qualified Data.List.NonEmpty as NEL
import qualified Data.Set as Set
import Distribution.Client.Init.Defaults
import Distribution.Client.Init.FlagExtractors
import Distribution.Client.Init.Types
import Distribution.Client.Init.Utils (currentDirPkgName, fixupDocFiles, mkPackageNameDep)
import Distribution.Client.Types.SourcePackageDb (SourcePackageDb (..))
import Distribution.Simple.Flag (Flag (..), flagElim, fromFlagOrDefault)
import Distribution.Simple.PackageIndex
import Distribution.Types.Dependency
import Distribution.Types.PackageName (unPackageName)
import Distribution.Verbosity

createProject
  :: Interactive m
  => Verbosity
  -> InstalledPackageIndex
  -> SourcePackageDb
  -> InitFlags
  -> m ProjectSettings
createProject v pkgIx _srcDb initFlags = do
  pkgType <- packageTypePrompt initFlags
  isMinimal <- getMinimal initFlags
  doOverwrite <- getOverwrite initFlags
  pkgDir <- getPackageDir initFlags
  pkgDesc <- fixupDocFiles v =<< genSimplePkgDesc initFlags

  let pkgName = _pkgName pkgDesc
      cabalSpec = _pkgCabalVersion pkgDesc
      mkOpts cs =
        WriteOpts
          doOverwrite
          isMinimal
          cs
          v
          pkgDir
          pkgType
          pkgName

  basedFlags <- addBaseDepToFlags pkgIx initFlags

  case pkgType of
    Library -> do
      libTarget <- genSimpleLibTarget basedFlags
      testTarget <- addLibDepToTest pkgName <$> genSimpleTestTarget basedFlags
      return $
        ProjectSettings
          (mkOpts False cabalSpec)
          pkgDesc
          (Just libTarget)
          Nothing
          testTarget
    Executable -> do
      exeTarget <- genSimpleExeTarget basedFlags
      return $
        ProjectSettings
          (mkOpts False cabalSpec)
          pkgDesc
          Nothing
          (Just exeTarget)
          Nothing
    LibraryAndExecutable -> do
      libTarget <- genSimpleLibTarget basedFlags
      testTarget <- addLibDepToTest pkgName <$> genSimpleTestTarget basedFlags
      exeTarget <- addLibDepToExe pkgName <$> genSimpleExeTarget basedFlags
      return $
        ProjectSettings
          (mkOpts False cabalSpec)
          pkgDesc
          (Just libTarget)
          (Just exeTarget)
          testTarget
    TestSuite -> do
      testTarget <- genSimpleTestTarget basedFlags
      return $
        ProjectSettings
          (mkOpts False cabalSpec)
          pkgDesc
          Nothing
          Nothing
          testTarget
  where
    -- Add package name as dependency of test suite
    --
    addLibDepToTest _ Nothing = Nothing
    addLibDepToTest n (Just t) =
      Just $
        t
          { _testDependencies = _testDependencies t ++ [mkPackageNameDep n]
          }

    -- Add package name as dependency of executable
    --
    addLibDepToExe n exe =
      exe
        { _exeDependencies = _exeDependencies exe ++ [mkPackageNameDep n]
        }

genSimplePkgDesc :: Interactive m => InitFlags -> m PkgDescription
genSimplePkgDesc flags = mkPkgDesc <$> currentDirPkgName
  where
    defaultExtraDoc = Just $ Set.singleton defaultChangelog

    extractExtraDoc [] = defaultExtraDoc
    extractExtraDoc fs = Just $ Set.fromList fs

    mkPkgDesc pkgName =
      PkgDescription
        (fromFlagOrDefault defaultCabalVersion (cabalVersion flags))
        pkgName
        (fromFlagOrDefault defaultVersion (version flags))
        (fromFlagOrDefault (defaultLicense $ getCabalVersionNoPrompt flags) (license flags))
        (fromFlagOrDefault "" (author flags))
        (fromFlagOrDefault "" (email flags))
        (fromFlagOrDefault "" (homepage flags))
        (fromFlagOrDefault "" (synopsis flags))
        (fromFlagOrDefault "" (category flags))
        (flagElim mempty Set.fromList (extraSrc flags))
        (flagElim defaultExtraDoc extractExtraDoc (extraDoc flags))

genSimpleLibTarget :: Interactive m => InitFlags -> m LibTarget
genSimpleLibTarget flags = do
  buildToolDeps <- getBuildTools flags
  return $
    LibTarget
      { _libSourceDirs = fromFlagOrDefault [defaultSourceDir] $ sourceDirs flags
      , _libLanguage = fromFlagOrDefault defaultLanguage $ language flags
      , _libExposedModules =
          flagElim (myLibModule NEL.:| []) extractMods $ exposedModules flags
      , _libOtherModules = fromFlagOrDefault [] $ otherModules flags
      , _libOtherExts = fromFlagOrDefault [] $ otherExts flags
      , _libDependencies = fromFlagOrDefault [] $ dependencies flags
      , _libBuildTools = buildToolDeps
      }
  where
    extractMods [] = myLibModule NEL.:| []
    extractMods as = NEL.fromList as

genSimpleExeTarget :: Interactive m => InitFlags -> m ExeTarget
genSimpleExeTarget flags = do
  buildToolDeps <- getBuildTools flags
  return $
    ExeTarget
      { _exeMainIs = flagElim defaultMainIs toHsFilePath $ mainIs flags
      , _exeApplicationDirs =
          fromFlagOrDefault [defaultApplicationDir] $ applicationDirs flags
      , _exeLanguage = fromFlagOrDefault defaultLanguage $ language flags
      , _exeOtherModules = fromFlagOrDefault [] $ otherModules flags
      , _exeOtherExts = fromFlagOrDefault [] $ otherExts flags
      , _exeDependencies = fromFlagOrDefault [] $ dependencies flags
      , _exeBuildTools = buildToolDeps
      }

genSimpleTestTarget :: Interactive m => InitFlags -> m (Maybe TestTarget)
genSimpleTestTarget flags = go =<< initializeTestSuitePrompt flags
  where
    go initialized
      | not initialized = return Nothing
      | otherwise = do
          buildToolDeps <- getBuildTools flags
          return $
            Just $
              TestTarget
                { _testMainIs = flagElim defaultMainIs toHsFilePath $ mainIs flags
                , _testDirs = fromFlagOrDefault [defaultTestDir] $ testDirs flags
                , _testLanguage = fromFlagOrDefault defaultLanguage $ language flags
                , _testOtherModules = fromFlagOrDefault [] $ otherModules flags
                , _testOtherExts = fromFlagOrDefault [] $ otherExts flags
                , _testDependencies = fromFlagOrDefault [] $ dependencies flags
                , _testBuildTools = buildToolDeps
                }

-- -------------------------------------------------------------------- --
-- Utils

-- | If deps are defined, and base is present, we skip the search for base.
-- otherwise, we look up @base@ and add it to the list.
addBaseDepToFlags :: Interactive m => InstalledPackageIndex -> InitFlags -> m InitFlags
addBaseDepToFlags pkgIx initFlags = case dependencies initFlags of
  Flag as
    | any ((==) "base" . unPackageName . depPkgName) as -> return initFlags
    | otherwise -> do
        based <- dependenciesPrompt pkgIx initFlags
        return $
          initFlags
            { dependencies = Flag $ based ++ as
            }
  NoFlag -> do
    based <- dependenciesPrompt pkgIx initFlags
    return initFlags{dependencies = Flag based}
