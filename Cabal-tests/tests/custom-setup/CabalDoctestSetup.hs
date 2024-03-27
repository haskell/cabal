-- This is Distribution.Extra.Doctest module from cabal-doctest-1.0.4
-- This isn't technically a Custom-Setup script, but it /was/.

{-# LANGUAGE FlexibleInstances #-}

{-

Copyright (c) 2017, Oleg Grenrus

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Oleg Grenrus nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

-}

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
-- | The provided 'generateBuildModule' generates 'Build_doctests' module.
-- That module exports enough configuration, so your doctests could be simply
--
-- @
-- module Main where
--
-- import Build_doctests (flags, pkgs, module_sources)
-- import Data.Foldable (traverse_)
-- import Test.Doctest (doctest)
--
-- main :: IO ()
-- main = do
--     traverse_ putStrLn args -- optionally print arguments
--     doctest args
--   where
--     args = flags ++ pkgs ++ module_sources
-- @
--
-- To use this library in the @Setup.hs@, you should specify a @custom-setup@
-- section in the cabal file, for example:
--
-- @
-- custom-setup
--  setup-depends:
--    base >= 4 && <5,
--    cabal-doctest >= 1 && <1.1
-- @
--
-- /Note:/ you don't need to depend on @Cabal@  if you use only
-- 'defaultMainWithDoctests' in the @Setup.hs@.
--
module CabalDoctestSetup (
    defaultMainWithDoctests,
    defaultMainAutoconfWithDoctests,
    addDoctestsUserHook,
    doctestsUserHooks,
    generateBuildModule,
    ) where

-- Hacky way to suppress few deprecation warnings.
#if MIN_VERSION_Cabal(1,24,0)
#define InstalledPackageId UnitId
#endif

import Control.Monad
       (when)
import Data.IORef
       (modifyIORef, newIORef, readIORef)
import Data.List
       (nub)
import Data.Maybe
       (mapMaybe, maybeToList)
import Data.String
       (fromString)
import Distribution.Package
       (InstalledPackageId, Package (..))
import Distribution.PackageDescription
       (BuildInfo (..), Executable (..), GenericPackageDescription,
       Library (..), PackageDescription, TestSuite (..))
import Distribution.Simple
       (UserHooks (..), autoconfUserHooks, defaultMainWithHooks,
       simpleUserHooks)
import Distribution.Simple.Compiler
       (CompilerFlavor (GHC), CompilerId (..), PackageDB (..), compilerId)
import Distribution.Simple.LocalBuildInfo
       (ComponentLocalBuildInfo (componentPackageDeps), LocalBuildInfo,
       compiler, withExeLBI, withLibLBI, withPackageDB, withTestLBI
       )
import Distribution.Simple.Setup
       ( CommonSetupFlags(..)
       , BuildFlags(..)
       , HaddockFlags (..)
       , emptyBuildFlags,
       fromFlag)
import Distribution.Simple.Utils
       (createDirectoryIfMissingVerbose, info)
import Distribution.Text
       (display)
import Distribution.Verbosity
import System.FilePath
       ((</>))

import qualified Data.Foldable    as F
                 (for_)
import qualified Data.Traversable as T
                 (traverse)

#if MIN_VERSION_Cabal(1,25,0)
import Distribution.Simple.BuildPaths
       (autogenComponentModulesDir)
#else
import Distribution.Simple.BuildPaths
       (autogenModulesDir)
#endif

#if MIN_VERSION_Cabal(2,0,0)
import Distribution.Types.MungedPackageId
       (MungedPackageId)
import Distribution.Types.UnqualComponentName
       (unUnqualComponentName)

-- For amendGPD
import Distribution.PackageDescription
       (CondTree (..))
import Distribution.Types.GenericPackageDescription
       (GenericPackageDescription (condTestSuites))

import Distribution.Version
       (mkVersion)
#else
import Data.Version
       (Version (..))
import Distribution.Package
       (PackageId)
#endif

#if MIN_VERSION_Cabal(3,11,0)
import Distribution.Utils.Path
       ( SymbolicPathX
       , makeSymbolicPath
       , makeRelativePathEx )
import qualified Distribution.Utils.Path as Cabal
       (getSymbolicPath)
import Distribution.Simple.Utils
       (findFileEx)
#elif MIN_VERSION_Cabal(3,0,0)
import Distribution.Utils.Path
       (SymbolicPath)
import qualified Distribution.Utils.Path as Cabal
       (getSymbolicPath)
import Distribution.Simple.Utils
       (findFileEx)
#else
import Distribution.Simple.Utils
       (findFile)
#endif

#if MIN_VERSION_Cabal(3,0,0)
import Distribution.Types.LibraryName
       (libraryNameString)
#endif

#if MIN_VERSION_directory(1,2,2)
import System.Directory
       (makeAbsolute)
#else
import System.Directory
       (getCurrentDirectory)
import System.FilePath
       (isAbsolute)

makeAbsolute :: FilePath -> IO FilePath
makeAbsolute p | isAbsolute p = return p
               | otherwise    = do
    cwd <- getCurrentDirectory
    return $ cwd </> p
#endif

findFile' :: Verbosity -> [FilePath] -> FilePath -> IO FilePath
#if MIN_VERSION_Cabal(3,11,0)
findFile' verbosity searchPath fileName
  = toFilePath <$>
    findFileEx verbosity
      (fmap makeSymbolicPath searchPath) (makeRelativePathEx fileName)
#elif MIN_VERSION_Cabal(3,0,0)
findFile' verbosity searchPath fileName
  = findFileEx verbosity searchPath fileName
#else
findFile' _verbosity searchPath fileName
  = findFile searchPath fileName
#endif

#if !MIN_VERSION_Cabal(2,0,0)
mkVersion :: [Int] -> Version
mkVersion ds = Version ds []
#endif

class CompatPath p where
  toFilePath :: p -> FilePath
instance CompatPath FilePath where
  toFilePath = id
#if MIN_VERSION_Cabal(3,11,0)
instance CompatPath (SymbolicPathX allowAbs from to) where
  toFilePath = Cabal.getSymbolicPath
#elif MIN_VERSION_Cabal(3,5,0)
instance CompatPath (SymbolicPath from to) where
  toFilePath = Cabal.getSymbolicPath
#endif

-------------------------------------------------------------------------------
-- Mains
-------------------------------------------------------------------------------

-- | A default main with doctests:
--
-- @
-- import Distribution.Extra.Doctest
--        (defaultMainWithDoctests)
--
-- main :: IO ()
-- main = defaultMainWithDoctests "doctests"
-- @
defaultMainWithDoctests
    :: String  -- ^ doctests test-suite name
    -> IO ()
defaultMainWithDoctests = defaultMainWithHooks . doctestsUserHooks

-- | Like 'defaultMainWithDoctests', for 'build-type: Configure' packages.
--
-- @since 1.0.2
defaultMainAutoconfWithDoctests
    :: String  -- ^ doctests test-suite name
    -> IO ()
defaultMainAutoconfWithDoctests n =
    defaultMainWithHooks (addDoctestsUserHook n autoconfUserHooks)

-- | 'simpleUserHooks' with 'generateBuildModule' prepended to the 'buildHook'.
doctestsUserHooks
    :: String  -- ^ doctests test-suite name
    -> UserHooks
doctestsUserHooks testsuiteName =
    addDoctestsUserHook testsuiteName simpleUserHooks

-- |
--
-- @since 1.0.2
addDoctestsUserHook :: String -> UserHooks -> UserHooks
addDoctestsUserHook testsuiteName uh = uh
    { buildHook = \pkg lbi hooks flags -> do
        generateBuildModule testsuiteName flags pkg lbi
        buildHook uh pkg lbi hooks flags
    -- We use confHook to add "Build_Doctests" to otherModules and autogenModules.
    --
    -- We cannot use HookedBuildInfo as it let's alter only the library and executables.
    , confHook = \(gpd, hbi) flags ->
        confHook uh (amendGPD testsuiteName gpd, hbi) flags
    , haddockHook = \pkg lbi hooks flags -> do
        generateBuildModule testsuiteName (haddockToBuildFlags flags) pkg lbi
        haddockHook uh pkg lbi hooks flags
    }

-- | Convert only flags used by 'generateBuildModule'.
haddockToBuildFlags :: HaddockFlags -> BuildFlags
haddockToBuildFlags f =
#if MIN_VERSION_Cabal(3,11,0)
  emptyBuildFlags
    { buildCommonFlags = haddockCommonFlags f }
#else
   emptyBuildFlags
    { buildVerbosity = haddockVerbosity f
    , buildDistPref  = haddockDistPref f
    }
#endif

data Name = NameLib (Maybe String) | NameExe String deriving (Eq, Show)

nameToString :: Name -> String
nameToString n = case n of
  NameLib x -> maybe "" (("_lib_" ++) . map fixchar) x
  NameExe x -> "_exe_" ++ map fixchar x
  where
    -- Taken from Cabal:
    -- https://github.com/haskell/cabal/blob/20de0bfea72145ba1c37e3f500cee5258cc18e51/Cabal/Distribution/Simple/Build/Macros.hs#L156-L158
    --
    -- Needed to fix component names with hyphens in them, as hyphens aren't
    -- allowed in Haskell identifier names.
    fixchar :: Char -> Char
    fixchar '-' = '_'
    fixchar c   = c

data Component = Component Name [String] [String] [String]
  deriving Show

-- | Generate a build module for the test suite.
--
-- @
-- import Distribution.Simple
--        (defaultMainWithHooks, UserHooks(..), simpleUserHooks)
-- import Distribution.Extra.Doctest
--        (generateBuildModule)
--
-- main :: IO ()
-- main = defaultMainWithHooks simpleUserHooks
--     { buildHook = \pkg lbi hooks flags -> do
--         generateBuildModule "doctests" flags pkg lbi
--         buildHook simpleUserHooks pkg lbi hooks flags
--     }
-- @
generateBuildModule
    :: String -- ^ doctests test-suite name
    -> BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
generateBuildModule testSuiteName flags pkg lbi = do
  let verbosity = fromFlag (buildVerbosity flags)
  let distPref = fromFlag (buildDistPref flags)

  -- Package DBs & environments
  let dbStack = withPackageDB lbi ++ [ SpecificPackageDB $ toFilePath distPref </> "package.conf.inplace" ]
  let dbFlags = "-hide-all-packages" : packageDbArgs dbStack
  let envFlags
        | ghcCanBeToldToIgnorePkgEnvs = [ "-package-env=-" ]
        | otherwise = []

  withTestLBI pkg lbi $ \suite suitecfg -> when (testName suite == fromString testSuiteName) $ do
    let testAutogenDir = toFilePath $
#if MIN_VERSION_Cabal(1,25,0)
          autogenComponentModulesDir lbi suitecfg
#else
          autogenModulesDir lbi
#endif

    createDirectoryIfMissingVerbose verbosity True testAutogenDir

    let buildDoctestsFile = testAutogenDir </> "Build_doctests.hs"

    -- First, we create the autogen'd module Build_doctests.
    -- Initially populate Build_doctests with a simple preamble.
    info verbosity $ "cabal-doctest: writing Build_doctests to " ++ buildDoctestsFile
    writeFile buildDoctestsFile $ unlines
      [ "module Build_doctests where"
      , ""
      , "import Prelude"
      , ""
      , "data Name = NameLib (Maybe String) | NameExe String deriving (Eq, Show)"
      , "data Component = Component Name [String] [String] [String] deriving (Eq, Show)"
      , ""
      ]

    -- we cannot traverse, only traverse_
    -- so we use IORef to collect components
    componentsRef <- newIORef []

    let testBI = testBuildInfo suite

    -- TODO: `words` is not proper parser (no support for quotes)
    let additionalFlags = maybe [] words
          $ lookup "x-doctest-options"
          $ customFieldsBI testBI

    let additionalModules = maybe [] words
          $ lookup "x-doctest-modules"
          $ customFieldsBI testBI

    let additionalDirs' = maybe [] words
          $ lookup "x-doctest-source-dirs"
          $ customFieldsBI testBI

    additionalDirs <- mapM (fmap ("-i" ++) . makeAbsolute) additionalDirs'

    -- Next, for each component (library or executable), we get to Build_doctests
    -- the sets of flags needed to run doctest on that component.
    let getBuildDoctests withCompLBI mbCompName compExposedModules compMainIs compBuildInfo =
         withCompLBI pkg lbi $ \comp compCfg -> do
           let compBI = compBuildInfo comp

           -- modules
           let modules = compExposedModules comp ++ otherModules compBI
           -- it seems that doctest is happy to take in module names, not actual files!
           let module_sources = modules

           -- We need the directory with the component's cabal_macros.h!

           let compAutogenDir =
                 toFilePath $
#if MIN_VERSION_Cabal(1,25,0)
                 autogenComponentModulesDir lbi compCfg
#else
                 autogenModulesDir lbi
#endif

           -- Lib sources and includes
           iArgsNoPrefix
              <- mapM makeAbsolute
               $ compAutogenDir           -- autogenerated files
               : (toFilePath distPref ++ "/build")   -- preprocessed files (.hsc -> .hs); "build" is hardcoded in Cabal.
               : map toFilePath (hsSourceDirs compBI)
           includeArgs <- mapM (fmap ("-I"++) . makeAbsolute . toFilePath) $ includeDirs compBI
           -- We clear all includes, so the CWD isn't used.
           let iArgs' = map ("-i"++) iArgsNoPrefix
               iArgs  = "-i" : iArgs'

           -- default-extensions
           let extensionArgs = map (("-X"++) . display) $ defaultExtensions compBI

           -- CPP includes, i.e. include cabal_macros.h
           let cppFlags = map ("-optP"++) $
                   [ "-include", compAutogenDir ++ "/cabal_macros.h" ]
                   ++ cppOptions compBI

           -- Unlike other modules, the main-is module of an executable is not
           -- guaranteed to share a module name with its filepath name. That is,
           -- even though the main-is module is named Main, its filepath might
           -- actually be Something.hs. To account for this possibility, we simply
           -- pass the full path to the main-is module instead.
           mainIsPath <- T.traverse (findFile' verbosity iArgsNoPrefix) (compMainIs comp)

           let all_sources = map display module_sources
                             ++ additionalModules
                             ++ maybeToList mainIsPath

           let component = Component
                (mbCompName comp)
                (formatDeps $ testDeps compCfg suitecfg)
                (concat
                  [ iArgs
                  , additionalDirs
                  , includeArgs
                  , envFlags
                  , dbFlags
                  , cppFlags
                  , extensionArgs
                  , additionalFlags
                  ])
                all_sources

           -- modify IORef, append component
           modifyIORef componentsRef (\cs -> cs ++ [component])

    -- For now, we only check for doctests in libraries and executables.
    getBuildDoctests withLibLBI mbLibraryName           exposedModules (const Nothing)     libBuildInfo
    getBuildDoctests withExeLBI (NameExe . executableName) (const []) (Just . toFilePath . modulePath) buildInfo

    components <- readIORef componentsRef
    F.for_ components $ \(Component cmpName cmpPkgs cmpFlags cmpSources) -> do
       let compSuffix          = nameToString cmpName
           pkgs_comp           = "pkgs"           ++ compSuffix
           flags_comp          = "flags"          ++ compSuffix
           module_sources_comp = "module_sources" ++ compSuffix

       -- write autogen'd file
       appendFile buildDoctestsFile $ unlines
         [ -- -package-id etc. flags
           pkgs_comp ++ " :: [String]"
         , pkgs_comp ++ " = " ++ show cmpPkgs
         , ""
         , flags_comp ++ " :: [String]"
         , flags_comp ++ " = " ++ show cmpFlags
         , ""
         , module_sources_comp ++ " :: [String]"
         , module_sources_comp ++ " = " ++ show cmpSources
         , ""
         ]

    -- write enabled components, i.e. x-doctest-components
    -- if none enabled, pick library
    let enabledComponents = maybe [NameLib Nothing] (mapMaybe parseComponentName . words)
           $ lookup "x-doctest-components"
           $ customFieldsBI testBI

    let components' =
         filter (\(Component n _ _ _) -> n `elem` enabledComponents) components
    appendFile buildDoctestsFile $ unlines
      [ "-- " ++ show enabledComponents
      , "components :: [Component]"
      , "components = " ++ show components'
      ]

  where
    parseComponentName :: String -> Maybe Name
    parseComponentName "lib"                       = Just (NameLib Nothing)
    parseComponentName ('l' : 'i' : 'b' : ':' : x) = Just (NameLib (Just x))
    parseComponentName ('e' : 'x' : 'e' : ':' : x) = Just (NameExe x)
    parseComponentName _ = Nothing

    -- we do this check in Setup, as then doctests don't need to depend on Cabal
    isNewCompiler = case compilerId $ compiler lbi of
      CompilerId GHC v -> v >= mkVersion [7,6]
      _                -> False

    ghcCanBeToldToIgnorePkgEnvs :: Bool
    ghcCanBeToldToIgnorePkgEnvs = case compilerId $ compiler lbi of
      CompilerId GHC v -> v >= mkVersion [8,4,4]
      _                -> False

    formatDeps = map formatOne
    formatOne (installedPkgId, pkgId)
      -- The problem is how different cabal executables handle package databases
      -- when doctests depend on the library
      --
      -- If the pkgId is current package, we don't output the full package-id
      -- but only the name
      --
      -- Because of MungedPackageId we compare display version of identifiers
      -- not the identifiers themfselves.
      | display (packageId pkg) == display pkgId = "-package=" ++ display pkgId
      | otherwise              = "-package-id=" ++ display installedPkgId

    -- From Distribution.Simple.Program.GHC
    packageDbArgs :: [PackageDB] -> [String]
    packageDbArgs | isNewCompiler = packageDbArgsDb
                  | otherwise     = packageDbArgsConf

    -- GHC <7.6 uses '-package-conf' instead of '-package-db'.
    packageDbArgsConf :: [PackageDB] -> [String]
    packageDbArgsConf dbstack = case dbstack of
      (GlobalPackageDB:UserPackageDB:dbs) -> concatMap specific dbs
      (GlobalPackageDB:dbs)               -> ("-no-user-package-conf")
                                           : concatMap specific dbs
      _ -> ierror
      where
        specific (SpecificPackageDB db) = [ "-package-conf=" ++ db ]
        specific _                      = ierror
        ierror = error $ "internal error: unexpected package db stack: "
                      ++ show dbstack

    -- GHC >= 7.6 uses the '-package-db' flag. See
    -- https://ghc.haskell.org/trac/ghc/ticket/5977.
    packageDbArgsDb :: [PackageDB] -> [String]
    -- special cases to make arguments prettier in common scenarios
    packageDbArgsDb dbstack = case dbstack of
      (GlobalPackageDB:UserPackageDB:dbs)
        | all isSpecific dbs              -> concatMap single dbs
      (GlobalPackageDB:dbs)
        | all isSpecific dbs              -> "-no-user-package-db"
                                           : concatMap single dbs
      dbs                                 -> "-clear-package-db"
                                           : concatMap single dbs
     where
       single (SpecificPackageDB db) = [ "-package-db=" ++ db ]
       single GlobalPackageDB        = [ "-global-package-db" ]
       single UserPackageDB          = [ "-user-package-db" ]
       isSpecific (SpecificPackageDB _) = True
       isSpecific _                     = False

    mbLibraryName :: Library -> Name
#if MIN_VERSION_Cabal(3,0,0)
    mbLibraryName = NameLib . fmap unUnqualComponentName . libraryNameString . libName
#elif MIN_VERSION_Cabal(2,0,0)
    -- Cabal-2.0 introduced internal libraries, which are named.
    mbLibraryName = NameLib . fmap unUnqualComponentName . libName
#else
    -- Before that, there was only ever at most one library per
    -- .cabal file, which has no name.
    mbLibraryName _ = NameLib Nothing
#endif

    executableName :: Executable -> String
#if MIN_VERSION_Cabal(2,0,0)
    executableName = unUnqualComponentName . exeName
#else
    executableName = exeName
#endif

-- | In compat settings it's better to omit the type-signature
testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo
#if MIN_VERSION_Cabal(2,0,0)
         -> [(InstalledPackageId, MungedPackageId)]
#else
         -> [(InstalledPackageId, PackageId)]
#endif
testDeps xs ys = nub $ componentPackageDeps xs ++ componentPackageDeps ys

amendGPD
    :: String -- ^ doctests test-suite name
    -> GenericPackageDescription
    -> GenericPackageDescription
#if !(MIN_VERSION_Cabal(2,0,0))
amendGPD _ gpd = gpd
#else
amendGPD testSuiteName gpd = gpd
    { condTestSuites = map f (condTestSuites gpd)
    }
  where
    f (name, condTree)
        | name == fromString testSuiteName = (name, condTree')
        | otherwise                        = (name, condTree)
      where
        -- I miss 'lens'
        testSuite = condTreeData condTree
        bi = testBuildInfo testSuite
        om = otherModules bi
        am = autogenModules bi

        -- Cons the module to both other-modules and autogen-modules.
        -- At the moment, cabal-spec-2.0 and cabal-spec-2.2 don't have
        -- "all autogen-modules are other-modules if they aren't exposed-modules"
        -- rule. Hopefully cabal-spec-3.0 will have.
        --
        -- Note: we `nub`, because it's unclear if that's ok to have duplicate
        -- modules in the lists.
        om' = nub $ mn : om
        am' = nub $ mn : am

        mn = fromString "Build_doctests"

        bi' = bi { otherModules = om', autogenModules = am' }
        testSuite' = testSuite { testBuildInfo = bi' }
        condTree' = condTree { condTreeData = testSuite' }
#endif
