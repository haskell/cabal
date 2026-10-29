{-# LANGUAGE OverloadedStrings #-}

module UnitTests.Distribution.Client.ProjectPlanning (tests) where

import Data.List.NonEmpty
import Distribution.Client.ProjectConfig.Types (ProjectConfigShared (..))
import Distribution.Client.ProjectPlanning (ComponentTarget (..), SubComponentTarget (..), nubComponentTargets, projectPackageDbsFor, sameCompiler)
import Distribution.Client.Toolchain (PerMachine (..), Toolchain (..))
import Distribution.Compiler (CompilerFlavor (..))
import Distribution.Simple.Compiler (AbiTag (..), Compiler (..), CompilerId (..), PackageDBX (..))
import Distribution.Simple.Program.Db (defaultProgramDb, userSpecifyPath)
import Distribution.Solver.Types.Machine (Machine (..), toolMachine)
import Distribution.System (Arch (..), OS (..), Platform (..))
import Distribution.Types.ComponentName
import Distribution.Types.LibraryName
import Distribution.Version (mkVersion)
import Test.Tasty
import Test.Tasty.HUnit

tests :: [TestTree]
tests =
  [ testGroup "Build Target Tests" buildTargetTests
  , testGroup "sameCompiler" sameCompilerTests
  , testGroup "projectPackageDbsFor" projectPackageDbsForTests
  ]

-- ----------------------------------------------------------------------------
-- projectPackageDbsFor
--
-- A package database is only readable by the compiler that wrote it, so the
-- project's 'package-dbs' must not reach a distinct build toolchain. The
-- subtle case is the other one: setup scripts are built at 'toolMachine' of
-- the package's own machine, and a non-cross build has no build machine, so
-- 'toolMachine' must keep them at 'Host' and the project's databases must
-- still apply there.
-- ----------------------------------------------------------------------------

projectPackageDbsForTests :: [TestTree]
projectPackageDbsForTests =
  [ testCase "host machine keeps the project databases" $
      projectPackageDbsFor config Host @?= configured
  , testCase "the build machine gets none of them" $
      projectPackageDbsFor config Build @?= []
  , testCase "a distinct build toolchain puts setup scripts on the build machine" $
      toolMachine cross Host @?= Build
  , testCase "tools of build-machine packages stay on the build machine" $
      toolMachine cross Build @?= Build
  , testCase "without a build toolchain setup scripts stay on the host machine" $
      -- This is what a setup script of a host-machine package asks for in an
      -- ordinary build, and it must still see the project's databases.
      projectPackageDbsFor config (toolMachine nonCross Host) @?= configured
  ]
  where
    configured = [Just (SpecificPackageDB "/somewhere/package.db")]

    config = (mempty :: ProjectConfigShared){projectConfigPackageDBs = configured}

    -- 'toolMachine' only looks at whether there is a build machine; which
    -- compilers they are does not matter here.
    nonCross = PerMachine (toolchain ghc912 linuxX86_64) Nothing
    cross = PerMachine (toolchain ghc912 linuxX86_64) (Just (toolchain ghc910 linuxAArch64))

-- ----------------------------------------------------------------------------
-- sameCompiler
--
-- 'configureToolchains' uses 'sameCompiler' to decide whether a requested
-- build compiler is really a second machine. Getting this wrong is expensive in
-- both directions: a false negative solves and builds every shared dependency
-- twice under indistinguishable unit ids, and a false positive silently drops
-- a genuine cross-compilation setup back to a single machine.
-- ----------------------------------------------------------------------------

sameCompilerTests :: [TestTree]
sameCompilerTests =
  [ testCase "a toolchain is the same as itself" $
      sameCompiler (toolchain ghc912 linuxX86_64) (toolchain ghc912 linuxX86_64) @?= True
  , testCase "the same compiler named two ways is the same" $
      -- What GHC's own staged bootstrap does: --with-build-compiler and
      -- --with-compiler given the same compiler, spelled differently. The
      -- ProgramDb differs; nothing that reaches a unit id does.
      sameCompiler
        (toolchain ghc912 linuxX86_64)
        ((toolchain ghc912 linuxX86_64){toolchainProgramDb = userSpecifyPath "ghc" "/usr/bin/ghc" defaultProgramDb})
        @?= True
  , testCase "differing compiler version is not the same" $
      sameCompiler (toolchain ghc912 linuxX86_64) (toolchain ghc910 linuxX86_64) @?= False
  , testCase "differing ABI tag is not the same" $
      sameCompiler
        (toolchain ghc912 linuxX86_64)
        (toolchain ghc912{compilerAbiTag = AbiTag "deadbeef"} linuxX86_64)
        @?= False
  , testCase "differing target platform is not the same" $
      -- The case the store layout also has to separate: one compiler version
      -- targeting two architectures.
      sameCompiler (toolchain ghc912 linuxX86_64) (toolchain ghc912 linuxAArch64) @?= False
  , testCase "differing compiler flavour is not the same" $
      sameCompiler (toolchain ghc912 linuxX86_64) (toolchain ghcjs912 linuxX86_64) @?= False
  ]

toolchain :: Compiler -> Platform -> Toolchain
toolchain c p =
  Toolchain
    { toolchainCompiler = c
    , toolchainPlatform = p
    , toolchainProgramDb = defaultProgramDb
    }

linuxX86_64, linuxAArch64 :: Platform
linuxX86_64 = Platform X86_64 Linux
linuxAArch64 = Platform AArch64 Linux

ghc912, ghc910, ghcjs912 :: Compiler
ghc912 = mkCompiler GHC [9, 12, 2]
ghc910 = mkCompiler GHC [9, 10, 3]
ghcjs912 = mkCompiler GHCJS [9, 12, 2]

mkCompiler :: CompilerFlavor -> [Int] -> Compiler
mkCompiler flavour version =
  Compiler
    { compilerId = CompilerId flavour (mkVersion version)
    , compilerAbiTag = NoAbiTag
    , compilerCompat = []
    , compilerLanguages = []
    , compilerExtensions = []
    , compilerProperties = mempty
    , compilerWiredInUnitIds = Nothing
    }

-- ----------------------------------------------------------------------------
-- Build Target Tests
-- ----------------------------------------------------------------------------

buildTargetTests :: [TestTree]
buildTargetTests =
  [ testGroup "nubComponentTargets" nubComponentTargetsTests
  ]

nubComponentTargetsTests :: [TestTree]
nubComponentTargetsTests =
  [ testCase "Works on empty list" $
      nubComponentTargets [] @?= ([] :: [(ComponentTarget, NonEmpty Int)])
  , testCase "Merges targets to same component" $
      nubComponentTargets
        [ (mainLibModuleTarget, 1 :: Int)
        , (mainLibFileTarget, 2)
        ]
        @?= [(mainLibWholeCompTarget, 1 :| [2])]
  , testCase "Merges whole component targets" $
      nubComponentTargets [(mainLibFileTarget, 2), (mainLibWholeCompTarget, 1 :: Int)]
        @?= [(mainLibWholeCompTarget, 2 :| [1])]
  , testCase "Don't merge unrelated targets" $
      nubComponentTargets
        [ (mainLibWholeCompTarget, 1 :: Int)
        , (exeWholeCompTarget, 2)
        ]
        @?= [(mainLibWholeCompTarget, pure 1), (exeWholeCompTarget, pure 2)]
  , testCase "Merge multiple related targets" $
      nubComponentTargets
        [ (mainLibWholeCompTarget, 1 :: Int)
        , (mainLibModuleTarget, 4)
        , (exeWholeCompTarget, 2)
        , (exeFileTarget, 3)
        ]
        @?= [(mainLibWholeCompTarget, 1 :| [4]), (exeWholeCompTarget, 2 :| [3])]
  , testCase "Merge related targets, don't merge unrelated ones" $
      nubComponentTargets
        [ (mainLibFileTarget, 1 :: Int)
        , (mainLibModuleTarget, 4)
        , (exeWholeCompTarget, 2)
        , (exeFileTarget, 3)
        , (exe2FileTarget, 5)
        ]
        @?= [ (mainLibWholeCompTarget, 1 :| [4])
            , (exeWholeCompTarget, 2 :| [3])
            , (exe2WholeCompTarget, 5 :| [])
            ]
  ]

-- ----------------------------------------------------------------------------
-- Utils
-- ----------------------------------------------------------------------------

mainLibWholeCompTarget :: ComponentTarget
mainLibWholeCompTarget = ComponentTarget (CLibName LMainLibName) WholeComponent

mainLibModuleTarget :: ComponentTarget
mainLibModuleTarget = ComponentTarget (CLibName LMainLibName) (ModuleTarget "Lib")

mainLibFileTarget :: ComponentTarget
mainLibFileTarget = ComponentTarget (CLibName LMainLibName) (FileTarget "./Lib.hs")

exeWholeCompTarget :: ComponentTarget
exeWholeCompTarget = ComponentTarget (CExeName "exe") WholeComponent

exeFileTarget :: ComponentTarget
exeFileTarget = ComponentTarget (CExeName "exe") (FileTarget "./Main.hs")

exe2WholeCompTarget :: ComponentTarget
exe2WholeCompTarget = ComponentTarget (CExeName "exe2") WholeComponent

exe2FileTarget :: ComponentTarget
exe2FileTarget = ComponentTarget (CExeName "exe2") (FileTarget "./Main2.hs")
