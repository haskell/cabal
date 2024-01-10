{-# LANGUAGE BlockArguments #-}
module Distribution.Simple.GHC.Build where

import Distribution.Compat.Prelude
import Prelude ()

import Data.Function
import Control.Monad.IO.Class
import qualified Data.ByteString.Lazy.Char8 as BS
import Distribution.Compat.Binary (encode)
import Distribution.Compat.ResponseFile (escapeArgs)
import qualified Distribution.InstalledPackageInfo as IPI
import Distribution.Package
import Distribution.PackageDescription as PD hiding (buildInfo)
import qualified Distribution.PackageDescription as PD
import Distribution.PackageDescription.Utils (cabalBug)
import Distribution.Pretty
import Distribution.Simple.BuildPaths
import Distribution.Simple.Compiler
import Distribution.Simple.Flag (Flag (..), fromFlag, fromFlagOrDefault)
import Distribution.Simple.GHC.ImplInfo
import qualified Distribution.Simple.GHC.Internal as Internal
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program
import Distribution.Simple.Program.GHC
import Distribution.Simple.Setup.Repl
import Distribution.Simple.Utils
import Distribution.System
import Distribution.Utils.NubList
import Distribution.Utils.Path (getSymbolicPath)
import Distribution.Verbosity
import Distribution.Version
import System.Directory hiding (exeExtension)
import System.FilePath
import Distribution.Simple.Build.Monad
import Distribution.Simple.GHC.Build.ExtraSources
import Distribution.Simple.GHC.Build.Modules
import Distribution.Types.ParStrat
import qualified Distribution.Simple.Hpc as Hpc
import Distribution.Simple.Setup.Common (extraCompilationArtifacts)

-- | The main build phase of building a component.
-- Includes building Haskell modules, extra build sources, and linking.
build :: Flag ParStrat
      -> PackageDescription
      -> BuildM ()
build numJobs pkg_descr = do
  verbosity <- buildVerbosity
  what      <- buildWhat
  component <- buildComponent
  lbi       <- buildLBI
  clbi      <- buildCLBI
  buildInfo <- buildBI
  target    <- buildTarget

  let isLib | CLib{} <- component = True
            | otherwise = False

  {-
    Where to place the build result (targetDir) and the build artifacts (buildTargetDir).

    * For libraries, targetDir == buildTargetDir, where both the library and
    artifacts are put together.
   
    * For executables or foreign libs, buildTargetDir == targetDir/<name-of-target-dir>-tmp, where
        the targetDir is the location where the target (e.g. the executable) is written to
        and buildTargetDir is where the compilation artifacts (e.g. Main.o) will live
      Arguably, this difference should not exist (#9498) (TODO)
   
    For instance, for a component `cabal-benchmarks`:
      targetDir == <buildDir>/cabal-benchmarks
      buildTargetDir == <buildDir>/cabal-benchmarks/cabal-benchmarks-tmp
    
    Or, for a library `Cabal`:
      targetDir == <buildDir>/.
      buildTargetDir == targetDir
    
    Furthermore, we need to account for the limit of characters in ghc
    invocations that different OSes constrain us to. Cabal invocations can
    rapidly reach this limit, in part, due to the long length of cabal v2
    prefixes. To minimize the likelihood, we use
    `makeRelativeToCurrentDirectory` to shorten the paths used in invocations
    (see da6321bb).
  -}
  let targetDir_absolute = componentBuildDir lbi clbi
      buildTargetDir_absolute
        -- Libraries use the target dir for building (see above)
        | isLib = targetDir_absolute

        -- In other cases, use targetDir/<name-of-target-dir>-tmp
        | targetDirName:_ <- reverse $ splitDirectories targetDir_absolute
        = targetDir_absolute </> (targetDirName ++ "-tmp")

        | otherwise = error "GHC.build: targetDir is empty"

  liftIO do
    createDirectoryIfMissingVerbose verbosity True targetDir_absolute
    createDirectoryIfMissingVerbose verbosity True buildTargetDir_absolute
  targetDir <- makeRelativeToCurrentDirectory targetDir_absolute & liftIO
  buildTargetDir <- makeRelativeToCurrentDirectory buildTargetDir_absolute & liftIO

  (ghcProg, _) <- requireProgram verbosity ghcProgram (withPrograms lbi) & liftIO

  -- ensure extra lib dirs exist before passing to ghc
  cleanedExtraLibDirs <- filterM doesDirectoryExist (extraLibDirs buildInfo) & liftIO
  cleanedExtraLibDirsStatic <- filterM doesDirectoryExist (extraLibDirsStatic buildInfo) & liftIO

  buildHaskellModules numJobs ghcProg pkg_descr buildTargetDir
  buildAllExtraSources ghcProg

  -- Now pattern match and call repl or link action for each kind of component
  -- ROMES:TODO: Still a work in progress!
  pure ()
  -- case what of
  --   BuildRepl rflags -> do
  --     -- TODO when (null (allLibModules lib clbi)) $ warn verbosity "No exposed modules"
  --     runReplOrWriteFlags ghcProg lbi rflags replOpts target (pkgName (PD.package pkg_descr)) & liftIO

  --   _build -> linkComponent

--------------------------------------------------------------------------------
-- * Utils, basically.
--------------------------------------------------------------------------------

replNoLoad :: Ord a => ReplOptions -> NubListR a -> NubListR a
replNoLoad replFlags l
  | replOptionsNoLoad replFlags == Flag True = mempty
  | otherwise = l

runReplOrWriteFlags
  :: ConfiguredProgram
  -> LocalBuildInfo
  -> ReplFlags
  -> GhcOptions
  -> TargetInfo
  -> PackageName
  -> IO ()
runReplOrWriteFlags ghcProg lbi rflags ghcOpts target pkg_name =
  let bi = componentBuildInfo $ targetComponent target
      clbi = targetCLBI target
      comp = compiler lbi
      platform = hostPlatform lbi
  in
  case replOptionsFlagOutput (replReplOptions rflags) of
    NoFlag -> runGHC (fromFlag $ replVerbosity rflags) ghcProg comp platform ghcOpts
    Flag out_dir -> do
      src_dir <- getCurrentDirectory
      let uid = componentUnitId clbi
          this_unit = prettyShow uid
          reexported_modules = [mn | LibComponentLocalBuildInfo{} <- [clbi], IPI.ExposedModule mn (Just{}) <- componentExposedModules clbi]
          hidden_modules = otherModules bi
          extra_opts =
            concat $
              [ ["-this-package-name", prettyShow pkg_name]
              , ["-working-dir", src_dir]
              ]
                ++ [ ["-reexported-module", prettyShow m] | m <- reexported_modules
                   ]
                ++ [ ["-hidden-module", prettyShow m] | m <- hidden_modules
                   ]
      -- Create "paths" subdirectory if it doesn't exist. This is where we write
      -- information about how the PATH was augmented.
      createDirectoryIfMissing False (out_dir </> "paths")
      -- Write out the PATH information into `paths` subdirectory.
      writeFileAtomic (out_dir </> "paths" </> this_unit) (encode ghcProg)
      -- Write out options for this component into a file ready for loading into
      -- the multi-repl
      writeFileAtomic (out_dir </> this_unit) $
        BS.pack $
          escapeArgs $
            extra_opts ++ renderGhcOptions comp platform (ghcOpts{ghcOptMode = NoFlag})
