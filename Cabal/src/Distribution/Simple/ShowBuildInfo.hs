{-# LANGUAGE OverloadedStrings #-}

-- |
-- This module defines a simple JSON-based format for exporting basic
-- information about a Cabal package and the compiler configuration Cabal
-- would use to build it. This can be produced with the
-- @cabal build --enable-build-info@ command.
--
--
-- This format is intended for consumption by external tooling and should
-- therefore be rather stable. Moreover, this allows tooling users to avoid
-- linking against Cabal. This is an important advantage as direct API usage
-- tends to be rather fragile in the presence of user-initiated upgrades of
-- Cabal.
--
-- Below is an example of the output this module produces,
--
-- @
-- { "cabal-lib-version": "1.23.0.0",
--   "compiler": {
--     "flavour": "GHC",
--     "compiler-id": "ghc-7.10.2",
--     "path": "/usr/bin/ghc",
--   },
--   "components": [
--     { "type": "lib",
--       "name": "lib:Cabal",
--       "compiler-args":
--         ["-O", "-XHaskell98", "-Wall",
--          "-package-id", "parallel-3.2.0.6-b79c38c5c25fff77f3ea7271851879eb"]
--       "modules": ["Project.ModA", "Project.ModB", "Paths_project"],
--       "src-files": [],
--       "src-dirs": ["src"]
--     }
--   ]
-- }
-- @
--
-- The output format needs to be validated against 'doc/json-schemas/build-info.schema.json'.
-- If the format changes, update the schema as well!
--
-- The @cabal-lib-version@ property provides the version of the Cabal library
-- which generated the output. The @compiler@ property gives some basic
-- information about the compiler Cabal would use to compile the package.
--
-- The @components@ property gives a list of the Cabal 'Component's defined by
-- the package. Each has,
--
-- * @type@: the type of the component (one of @lib@, @exe@,
--   @test@, @bench@, or @flib@)
-- * @name@: a string serving to uniquely identify the component within the
--   package.
-- * @compiler-args@: the command-line arguments Cabal would pass to the
--   compiler to compile the component
-- * @modules@: the modules belonging to the component
-- * @src-dirs@: a list of directories where the modules might be found
-- * @src-files@: any other Haskell sources needed by the component
--
-- Note: At the moment this is only supported when using the GHC compiler.
module Distribution.Simple.ShowBuildInfo
  ( mkBuildInfo
  , mkBuildInfo'
  , mkCompilerInfo
  , mkComponentInfo
  ) where

import System.FilePath

import Distribution.Compat.Prelude
import Prelude ()

import qualified Distribution.Simple.GHC as GHC
import qualified Distribution.Simple.Program.GHC as GHC

import Distribution.Compiler
import Distribution.PackageDescription
import Distribution.Pretty
import Distribution.Simple.Compiler (Compiler, compilerFlavor, showCompilerId)
import Distribution.Simple.Program
import Distribution.Simple.Setup.Build (BuildFlags)
import Distribution.Simple.Utils (cabalVersion)
import Distribution.Text
import Distribution.Types.Component
import Distribution.Types.ComponentLocalBuildInfo
import Distribution.Types.LocalBuildInfo
import Distribution.Types.TargetInfo
import Distribution.Utils.Json
import Distribution.Verbosity

-- | Construct a JSON document describing the build information for a
-- package.
mkBuildInfo
  :: FilePath
  -- ^ The source directory of the package
  -> PackageDescription
  -- ^ Mostly information from the .cabal file
  -> LocalBuildInfo
  -- ^ Configuration information
  -> BuildFlags
  -- ^ Flags that the user passed to build
  -> (ConfiguredProgram, Compiler)
  -- ^ Compiler information.
  -- Needs to be passed explicitly, as we can't extract that information here
  -- without some partial function.
  -> [TargetInfo]
  -> ([String], Json)
  -- ^ Json representation of buildinfo alongside generated warnings
mkBuildInfo wdir pkg_descr lbi _flags compilerInfo targetsToBuild = (warnings, JsonObject buildInfoFields)
  where
    buildInfoFields = mkBuildInfo' (uncurry mkCompilerInfo compilerInfo) componentInfos
    componentInfosWithWarnings = map (mkComponentInfo wdir pkg_descr lbi . targetCLBI) targetsToBuild
    componentInfos = map snd componentInfosWithWarnings
    warnings = concatMap fst componentInfosWithWarnings

-- | A variant of 'mkBuildInfo' if you need to call 'mkCompilerInfo' and
-- 'mkComponentInfo' yourself.
--
-- If you change the format or any name in the output json, don't forget to update
-- the schema at @\/doc\/json-schemas\/build-info.schema.json@ and the docs of
-- @--enable-build-info@\/@--disable-build-info@.
mkBuildInfo'
  :: Json
  -- ^ The 'Json' from 'mkCompilerInfo'
  -> [Json]
  -- ^ The 'Json' from 'mkComponentInfo'
  -> [(String, Json)]
mkBuildInfo' compilerInfo componentInfos =
  [ "cabal-lib-version" .= JsonString (display cabalVersion)
  , "compiler" .= compilerInfo
  , "components" .= JsonArray componentInfos
  ]

mkCompilerInfo :: ConfiguredProgram -> Compiler -> Json
mkCompilerInfo compilerProgram compilerInfo =
  JsonObject
    [ "flavour" .= JsonString (prettyShow $ compilerFlavor compilerInfo)
    , "compiler-id" .= JsonString (showCompilerId compilerInfo)
    , "path" .= JsonString (programPath compilerProgram)
    ]

mkComponentInfo :: FilePath -> PackageDescription -> LocalBuildInfo -> ComponentLocalBuildInfo -> ([String], Json)
mkComponentInfo wdir pkg_descr lbi clbi =
  ( warnings
  , JsonObject $
      [ "type" .= JsonString compType
      , "name" .= JsonString (prettyShow name)
      , "unit-id" .= JsonString (prettyShow $ componentUnitId clbi)
      , "compiler-args" .= JsonArray (map JsonString compilerArgs)
      , "modules" .= JsonArray (map (JsonString . display) modules)
      , "src-files" .= JsonArray (map JsonString sourceFiles)
      , "hs-src-dirs" .= JsonArray (map (JsonString . prettyShow) $ hsSourceDirs bi)
      , "src-dir" .= JsonString (addTrailingPathSeparator wdir)
      ]
        <> cabalFile
  )
  where
    (warnings, compilerArgs) = getCompilerArgs bi lbi clbi
    name = componentLocalName clbi
    bi = componentBuildInfo comp
    -- If this error happens, a cabal invariant has been violated
    comp = fromMaybe (error $ "mkBuildInfo: no component " ++ prettyShow name) $ lookupComponent pkg_descr name
    compType = case comp of
      CLib _ -> "lib"
      CExe _ -> "exe"
      CTest _ -> "test"
      CBench _ -> "bench"
      CFLib _ -> "flib"
    modules = case comp of
      CLib lib -> explicitLibModules lib
      CExe exe -> exeModules exe
      CTest test ->
        case testInterface test of
          TestSuiteExeV10 _ _ -> []
          TestSuiteLibV09 _ modName -> [modName]
          TestSuiteUnsupported _ -> []
      CBench bench -> benchmarkModules bench
      CFLib flib -> foreignLibModules flib
    sourceFiles = case comp of
      CLib _ -> []
      CExe exe -> [modulePath exe]
      CTest test ->
        case testInterface test of
          TestSuiteExeV10 _ fp -> [fp]
          TestSuiteLibV09 _ _ -> []
          TestSuiteUnsupported _ -> []
      CBench bench -> case benchmarkInterface bench of
        BenchmarkExeV10 _ fp -> [fp]
        BenchmarkUnsupported _ -> []
      CFLib _ -> []
    cabalFile
      | Just fp <- pkgDescrFile lbi = [("cabal-file", JsonString fp)]
      | otherwise = []

-- | Get the command-line arguments that would be passed
-- to the compiler to build the given component.
getCompilerArgs
  :: BuildInfo
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> ([String], [String])
getCompilerArgs bi lbi clbi =
  case compilerFlavor $ compiler lbi of
    GHC -> ([], ghc)
    GHCJS -> ([], ghc)
    c ->
      (
        [ "ShowBuildInfo.getCompilerArgs: Don't know how to get build "
            ++ " arguments for compiler "
            ++ show c
        ]
      , []
      )
  where
    -- This is absolutely awful
    ghc = GHC.renderGhcOptions (compiler lbi) (hostPlatform lbi) baseOpts
      where
        baseOpts = GHC.componentGhcOptions normal lbi bi clbi (buildDir lbi)
