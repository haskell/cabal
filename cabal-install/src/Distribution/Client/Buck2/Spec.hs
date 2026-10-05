-- | The build spec: a description of a package's components as Cabal sees
-- them, from which buck2\/cabal.bzl creates the buck2 rules. See that file
-- for the schema these types correspond to; "Distribution.Client.Buck2.Write"
-- is what turns them into the Starlark it reads.
module Distribution.Client.Buck2.Spec
  ( specSchemaVersion
  , BuildSpec (..)
  , ComponentKind (..)
  , kindName
  , SpecComponent (..)
  , Src (..)
  , SpecDep (..)
  , SpecBuildTool (..)
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

-- | The version of the build spec format; must match @SCHEMA_VERSION@ in
-- buck2\/cabal.bzl.
specSchemaVersion :: Int
specSchemaVersion = 1

-- | A package's build spec: everything the generated rules are built from
-- that comes from Cabal rather than from buck2 conventions.
data BuildSpec = BuildSpec
  { specPackageName :: String
  , specPackageDir :: FilePath
  -- ^ Relative to the buck2 cell root, @.@ at the root.
  , specGhcOptions :: [String]
  -- ^ Supplied by the project, not the @.cabal@ file.
  , specComponents :: [SpecComponent]
  }

data ComponentKind = Library | Executable | TestSuite | Benchmark
  deriving (Eq)

-- | As it appears in the spec and in messages: @library@, @executable@,
-- @test-suite@ or @benchmark@.
kindName :: ComponentKind -> String
kindName Library = "library"
kindName Executable = "executable"
kindName TestSuite = "test-suite"
kindName Benchmark = "benchmark"

-- | One component of a build spec. List fields are empty when the
-- corresponding @.cabal@ field is.
data SpecComponent = SpecComponent
  { scKind :: ComponentKind
  , scName :: String
  -- ^ Also the name of the buck2 target.
  , scMainIs :: Maybe Src
  -- ^ The main module's source; not for a library.
  , scSrcs :: [(String, Src)]
  -- ^ Every other module (by module name) and its source.
  , scTestArgs :: [String]
  -- ^ The project's @test-options@ for a test-suite, with template variables
  -- expanded.
  , scGhcOptions :: [String]
  , scCppOptions :: [String]
  , scLanguage :: Maybe String
  , scExtensions :: [String]
  , scExtraLibraries :: [String]
  , scDeps :: [SpecDep]
  , scBuildTools :: [SpecBuildTool]
  , scCSources :: [FilePath]
  , scCxxSources :: [FilePath]
  , scCxxOptions :: [String]
  , scIncludeDirs :: [FilePath]
  , scPkgconfig :: [String]
  }

-- | A source file of a component: either a real file in the package
-- (relative to its directory), or one generated into the package's
-- @cabal-buck2\/autogen@ directory, named by its @export_file()@ entry there
-- (see 'Distribution.Client.Buck2.Generate.AutogenFile').
data Src = SrcFile FilePath | SrcAutogen String

-- | A library a component depends on.
data SpecDep = SpecDep
  { depPackage :: String
  , depLibrary :: Maybe String
  -- ^ The sub-library, if the main library isn't the one depended on.
  , depDir :: Maybe FilePath
  -- ^ The package's directory (relative to the cell root) if it's built by
  -- this project.
  }

-- | An executable that a component's @build-tool-depends@ needs on @PATH@.
data SpecBuildTool
  = -- | Built by this project, in the given directory.
    LocalTool {toolExe :: String, toolDir :: FilePath}
  | -- | An already-installed binary, exported from @third-party\/haskell@.
    ExternalTool {toolExe :: String}
