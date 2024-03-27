{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Distribution.Simple.HaskellSuite where

import Distribution.Compat.Prelude
import Prelude ()

import qualified Data.List.NonEmpty as NE

import Distribution.InstalledPackageInfo hiding (includeDirs)
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Parsec (simpleParsec)
import Distribution.Pretty
import Distribution.Simple.BuildPaths
import Distribution.Simple.Compiler
import Distribution.Simple.Errors
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PackageIndex as PackageIndex
import Distribution.Simple.Program
import Distribution.Simple.Program.Builtin
import Distribution.Simple.Utils
import Distribution.System (Platform)
import Distribution.Verbosity
import Distribution.Version
import Language.Haskell.Extension

configure
  :: Verbosity
  -> Maybe FilePath
  -> Maybe FilePath
  -> ProgramDb
  -> IO (Compiler, Maybe Platform, ProgramDb)
configure verbosity mbHcPath hcPkgPath progdb0 = do
  -- We have no idea how a haskell-suite tool is named, so we require at
  -- least some information from the user.
  hcPath <-
    let msg = "You have to provide name or path of a haskell-suite tool (-w PATH)"
     in maybe (dieWithException verbosity $ ProvideHaskellSuiteTool msg) return mbHcPath

  when (isJust hcPkgPath) $
    warn verbosity "--with-hc-pkg option is ignored for haskell-suite"

  (comp, confdCompiler, progdb1) <- configureCompiler hcPath progdb0

  -- Update our pkg tool. It uses the same executable as the compiler, but
  -- all command start with "pkg"
  (confdPkg, _) <- requireProgram verbosity haskellSuitePkgProgram progdb1
  let progdb2 =
        updateProgram
          confdPkg
            { programLocation = programLocation confdCompiler
            , programDefaultArgs = ["pkg"]
            }
          progdb1

  return (comp, Nothing, progdb2)
  where
    configureCompiler hcPath progdb0' = do
      let
        haskellSuiteProgram' =
          haskellSuiteProgram
            { programFindLocation = \v p -> findProgramOnSearchPath v p hcPath
            }

      -- NB: cannot call requireProgram right away — it'd think that
      -- the program is already configured and won't reconfigure it again.
      -- Instead, call configureProgram directly first.
      progdb1 <- configureProgram verbosity haskellSuiteProgram' progdb0'
      (confdCompiler, progdb2) <- requireProgram verbosity haskellSuiteProgram' progdb1

      extensions <- getExtensions verbosity confdCompiler
      languages <- getLanguages verbosity confdCompiler
      (compName, compVersion) <-
        getCompilerVersion verbosity confdCompiler

      let
        comp =
          Compiler
            { compilerId = CompilerId (HaskellSuite compName) compVersion
            , compilerAbiTag = NoAbiTag
            , compilerCompat = []
            , compilerLanguages = languages
            , compilerExtensions = extensions
            , compilerProperties = mempty
            }

      return (comp, confdCompiler, progdb2)

hstoolVersion :: Verbosity -> FilePath -> IO (Maybe Version)
hstoolVersion = findProgramVersion "--hspkg-version" id

numericVersion :: Verbosity -> FilePath -> IO (Maybe Version)
numericVersion = findProgramVersion "--compiler-version" (fromMaybe "" . safeLast . words)

getCompilerVersion :: Verbosity -> ConfiguredProgram -> IO (String, Version)
getCompilerVersion verbosity prog = do
  output <- rawSystemStdout verbosity (programPath prog) ["--compiler-version"]
  let
    parts = words output
    name = concat $ safeInit parts -- there shouldn't be any spaces in the name anyway
    versionStr = fromMaybe "" $ safeLast parts
  version <-
    maybe
      (dieWithException verbosity CannotDetermineCompilerVersion)
      return
      $ simpleParsec versionStr
  return (name, version)

getExtensions :: Verbosity -> ConfiguredProgram -> IO [(Extension, Maybe CompilerFlag)]
getExtensions verbosity prog = do
  extStrs <-
    lines
      `fmap` rawSystemStdout verbosity (programPath prog) ["--supported-extensions"]
  return
    [(ext, Just $ "-X" ++ prettyShow ext) | Just ext <- map simpleParsec extStrs]

getLanguages :: Verbosity -> ConfiguredProgram -> IO [(Language, CompilerFlag)]
getLanguages verbosity prog = do
  langStrs <-
    lines
      `fmap` rawSystemStdout verbosity (programPath prog) ["--supported-languages"]
  return
    [(ext, "-G" ++ prettyShow ext) | Just ext <- map simpleParsec langStrs]

-- Other compilers do some kind of a packagedb stack check here. Not sure
-- if we need something like that as well.
getInstalledPackages
  :: Verbosity
  -> PackageDBStack
  -> ProgramDb
  -> IO InstalledPackageIndex
getInstalledPackages verbosity packagedbs progdb =
  liftM (PackageIndex.fromList . concat) $ for packagedbs $ \packagedb ->
    do
      str <-
        getDbProgramOutput
          verbosity
          haskellSuitePkgProgram
          progdb
          ["dump", packageDbOpt packagedb]
          `catchExit` \_ -> dieWithException verbosity PkgDumpFailed

      case parsePackages str of
        Right ok -> return ok
        _ -> dieWithException verbosity FailedToParseOutput
  where
    parsePackages str =
      case partitionEithers $ map (parseInstalledPackageInfo . toUTF8BS) (splitPkgs str) of
        ([], ok) -> Right [pkg | (_, pkg) <- ok]
        (msgss, _) -> Left (foldMap NE.toList msgss)

    splitPkgs :: String -> [String]
    splitPkgs = map unlines . splitWith ("---" ==) . lines
      where
        splitWith :: (a -> Bool) -> [a] -> [[a]]
        splitWith p xs =
          ys : case zs of
            [] -> []
            _ : ws -> splitWith p ws
          where
            (ys, zs) = break p xs

buildLib
  :: Verbosity
  -> PackageDescription
  -> LocalBuildInfo
  -> Library
  -> ComponentLocalBuildInfo
  -> IO ()
buildLib verbosity pkg_descr lbi lib clbi = do
  -- In future, there should be a mechanism for the compiler to request any
  -- number of the above parameters (or their parts) — in particular,
  -- pieces of PackageDescription.
  --
  -- For now, we only pass those that we know are used.

  let odir = buildDir lbi
      bi = libBuildInfo lib
      srcDirs = map i (hsSourceDirs bi) ++ [i odir]
      dbStack = withPackageDB lbi
      language = fromMaybe Haskell98 (defaultLanguage bi)
      progdb = withPrograms lbi
      pkgid = packageId pkg_descr
      i = interpretSymbolicPathLBI lbi -- See Note [Symbolic paths] in Distribution.Utils.Path
  runDbProgram verbosity haskellSuiteProgram progdb $
    ["compile", "--build-dir", i odir]
      ++ concat [["-i", d] | d <- srcDirs]
      ++ concat
        [ ["-I", d]
        | d <-
            [ i $ autogenComponentModulesDir lbi clbi
            , i $ autogenPackageModulesDir lbi
            , i odir
            ]
              ++ map i (includeDirs bi)
        ]
      ++ [packageDbOpt pkgDb | pkgDb <- dbStack]
      ++ ["--package-name", prettyShow pkgid]
      ++ concat
        [ ["--package-id", prettyShow ipkgid]
        | (ipkgid, _) <- componentPackageDeps clbi
        ]
      ++ ["-G", prettyShow language]
      ++ concat [["-X", prettyShow ex] | ex <- usedExtensions bi]
      ++ cppOptions (libBuildInfo lib)
      ++ [prettyShow modu | modu <- allLibModules lib clbi]

installLib
  :: Verbosity
  -> LocalBuildInfo
  -> FilePath
  -- ^ install location
  -> FilePath
  -- ^ install location for dynamic libraries
  -> FilePath
  -- ^ Build location
  -> PackageDescription
  -> Library
  -> ComponentLocalBuildInfo
  -> IO ()
installLib verbosity lbi targetDir dynlibTargetDir builtDir pkg lib clbi = do
  let progdb = withPrograms lbi
  runDbProgram verbosity haskellSuitePkgProgram progdb $
    [ "install-library"
    , "--build-dir"
    , builtDir
    , "--target-dir"
    , targetDir
    , "--dynlib-target-dir"
    , dynlibTargetDir
    , "--package-id"
    , prettyShow $ packageId pkg
    ]
      ++ map prettyShow (allLibModules lib clbi)

registerPackage
  :: Verbosity
  -> ProgramDb
  -> PackageDBStack
  -> InstalledPackageInfo
  -> IO ()
registerPackage verbosity progdb packageDbs installedPkgInfo = do
  (hspkg, _) <- requireProgram verbosity haskellSuitePkgProgram progdb

  runProgramInvocation verbosity $
    ( programInvocation
        hspkg
        ["update", packageDbOpt $ registrationPackageDB packageDbs]
    )
      { progInvokeInput = Just $ IODataText $ showInstalledPackageInfo installedPkgInfo
      }

initPackageDB :: Verbosity -> ProgramDb -> FilePath -> IO ()
initPackageDB verbosity progdb dbPath =
  runDbProgram
    verbosity
    haskellSuitePkgProgram
    progdb
    ["init", dbPath]

packageDbOpt :: PackageDB -> String
packageDbOpt GlobalPackageDB = "--global"
packageDbOpt UserPackageDB = "--user"
packageDbOpt (SpecificPackageDB db) = "--package-db=" ++ db
