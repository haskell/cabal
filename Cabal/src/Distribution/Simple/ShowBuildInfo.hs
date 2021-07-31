-- |
-- This module defines a simple JSON-based format for exporting basic
-- information about a Cabal package and the compiler configuration Cabal
-- would use to build it. This can be produced with the
-- @cabal new-show-build-info@ command.
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
-- { "cabal-version": "1.23.0.0",
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
-- The @cabal-version@ property provides the version of the Cabal library
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
--

{-# LANGUAGE OverloadedStrings #-}

module Distribution.Simple.ShowBuildInfo
  ( mkBuildInfo, mkBuildInfo', mkCompilerInfo, mkComponentInfo ) where

import Distribution.Compat.Prelude
import Prelude ()

import qualified Distribution.Simple.GHC   as GHC
import qualified Distribution.Simple.Program.GHC as GHC

import Distribution.PackageDescription
import Distribution.Compiler
import Distribution.Verbosity
import Distribution.Simple.Compiler
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Setup
import Distribution.Simple.Utils (cabalVersion)
import Distribution.Utils.Json
import Distribution.Types.TargetInfo
import Distribution.Text
import Distribution.Pretty

-- | Construct a JSON document describing the build information for a
-- package.
mkBuildInfo
  :: FilePath            -- ^ The source directory of the package
  -> PackageDescription  -- ^ Mostly information from the .cabal file
  -> LocalBuildInfo      -- ^ Configuration information
  -> BuildFlags          -- ^ Flags that the user passed to build
  -> [TargetInfo]
  -> Json
mkBuildInfo wdir pkg_descr lbi _flags targetsToBuild =
  JsonObject $
    mkBuildInfo' (mkCompilerInfo (withPrograms lbi) (compiler lbi))
                 (map (mkComponentInfo wdir pkg_descr lbi . targetCLBI) targetsToBuild)

-- | A variant of 'mkBuildInfo' if you need to call 'mkCompilerInfo' and
-- 'mkComponentInfo' yourself.
mkBuildInfo'
  :: Json   -- ^ The 'Json' from 'mkCompilerInfo'
  -> [Json] -- ^ The 'Json' from 'mkComponentInfo'
  -> [(String, Json)]
mkBuildInfo' cmplrInfo componentInfos =
    [ "cabal-version" .= JsonString (display cabalVersion)
    , "compiler"      .= cmplrInfo
    , "components"    .= JsonArray componentInfos
    ]

mkCompilerInfo :: ProgramDb -> Compiler -> Json
mkCompilerInfo programDb cmplr = JsonObject
  [ "flavour"     .= JsonString (prettyShow $ compilerFlavor cmplr)
  , "compiler-id" .= JsonString (showCompilerId cmplr)
  , "path"        .= path
  ]
  where
    path = maybe JsonNull (JsonString . programPath)
            $ (flavorToProgram . compilerFlavor $ cmplr)
            >>= flip lookupProgram programDb

    flavorToProgram :: CompilerFlavor -> Maybe Program
    flavorToProgram GHC   = Just ghcProgram
    flavorToProgram GHCJS = Just ghcjsProgram
    flavorToProgram UHC   = Just uhcProgram
    flavorToProgram JHC   = Just jhcProgram
    flavorToProgram _     = Nothing

mkComponentInfo :: FilePath -> PackageDescription -> LocalBuildInfo -> ComponentLocalBuildInfo -> Json
mkComponentInfo wdir pkg_descr lbi clbi = JsonObject $
  [ "type"          .= JsonString compType
  , "name"          .= JsonString (prettyShow name)
  , "unit-id"       .= JsonString (prettyShow $ componentUnitId clbi)
  , "compiler-args" .= JsonArray (map JsonString $ getCompilerArgs bi lbi clbi)
  , "modules"       .= JsonArray (map (JsonString . display) modules)
  , "src-files"     .= JsonArray (map JsonString sourceFiles)
  , "hs-src-dirs"   .= JsonArray (map (JsonString . prettyShow) $ hsSourceDirs bi)
  , "src-dir"       .= JsonString wdir
  ] <> cabalFile
  where
    name = componentLocalName clbi
    bi = componentBuildInfo comp
    comp = fromMaybe (error $ "mkBuildInfo: no component " ++ prettyShow name) $ lookupComponent pkg_descr name
    compType = case comp of
      CLib _   -> "lib"
      CExe _   -> "exe"
      CTest _  -> "test"
      CBench _ -> "bench"
      CFLib _  -> "flib"
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
      CLib _   -> []
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
      | otherwise                   = []

-- | Get the command-line arguments that would be passed
-- to the compiler to build the given component.
getCompilerArgs
  :: BuildInfo
  -> LocalBuildInfo
  -> ComponentLocalBuildInfo
  -> [String]
getCompilerArgs bi lbi clbi =
  case compilerFlavor $ compiler lbi of
      GHC   -> ghc
      GHCJS -> ghc
      c     -> error $ "ShowBuildInfo.getCompilerArgs: Don't know how to get "++
                       "build arguments for compiler "++show c
  where
    -- This is absolutely awful
    ghc = GHC.renderGhcOptions (compiler lbi) (hostPlatform lbi) baseOpts
      where
        baseOpts = GHC.componentGhcOptions normal lbi bi clbi (buildDir lbi)
