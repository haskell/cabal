module Distribution.Client.CmdEnv.Install
  ( installLibraries
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as Map
import Data.Ord
       ( comparing, Down(..) )
import System.Directory
       ( createDirectoryIfMissing )
import System.FilePath
       ( takeDirectory )

import qualified Distribution.Client.CmdEnv.Internal as EnvInternal
       ( globalPackages )
import Distribution.Client.IndexUtils
       ( getInstalledPackages )
import Distribution.Client.ProjectOrchestration
       ( ComponentName(..), ComponentTarget(..), ProjectBuildContext(..)
       , TargetsMap, TargetSelector )

import Distribution.Package
       ( PackageName )
import Distribution.Simple.Compiler
       ( Compiler(..), PackageDBStack )
import Distribution.Simple.GHC
       ( GhcEnvironmentFileEntry(..), GhcImplInfo(..), getImplInfo
       , renderGhcEnvironmentFile)
import qualified Distribution.Simple.PackageIndex as PI
       ( lookupPackageName )
import Distribution.Simple.Program.Db
       ( ProgramDb )
import Distribution.Simple.Utils
       ( warn, ordNub )
import Distribution.Types.InstalledPackageInfo
       ( InstalledPackageInfo(..) )
import Distribution.Types.UnitId
       ( UnitId )
import Distribution.Utils.Generic
       ( safeHead, writeFileAtomic )
import Distribution.Verbosity
       ( Verbosity )


-- | Install any built library by adding it to the default ghc environment
installLibraries
  :: Verbosity
  -> ProjectBuildContext
  -> Compiler
  -> PackageDBStack
  -> ProgramDb
  -> FilePath -- ^ Environment file
  -> [GhcEnvironmentFileEntry]
  -> IO ()
installLibraries verbosity buildCtx compiler
                 packageDbs programDb envFile envEntries = do
  -- Why do we get it again? If we updated a globalPackage then we need
  -- the new version.
  installedIndex <- getInstalledPackages verbosity compiler packageDbs programDb
  if supportsPkgEnvFiles $ getImplInfo compiler
    then do
      let
        getLatest :: PackageName -> [InstalledPackageInfo]
        getLatest = (=<<) (maybeToList . safeHead . snd) . take 1 . sortBy (comparing (Down . fst))
                  . PI.lookupPackageName installedIndex
        globalLatest = concat (getLatest <$> EnvInternal.globalPackages)

        baseEntries =
          GhcEnvFileClearPackageDbStack : fmap GhcEnvFilePackageDb packageDbs
        globalEntries = GhcEnvFilePackageId . installedUnitId <$> globalLatest
        pkgEntries = ordNub $
              globalEntries
          ++ envEntries
          ++ entriesForLibraryComponents (targetsMap buildCtx)
        contents' = renderGhcEnvironmentFile (baseEntries ++ pkgEntries)
      createDirectoryIfMissing True (takeDirectory envFile)
      writeFileAtomic envFile (BS.pack contents')
    else
      warn verbosity $
          "The current compiler doesn't support safely installing libraries, "
        ++ "so only executables will be available. (Library installation is "
        ++ "supported on GHC 8.0+ only)"

-- | Create 'GhcEnvironmentFileEntry's for packages with exposed libraries.
entriesForLibraryComponents :: TargetsMap -> [GhcEnvironmentFileEntry]
entriesForLibraryComponents = Map.foldrWithKey' (\k v -> mappend (go k v)) []
  where
    hasLib :: (ComponentTarget, [TargetSelector]) -> Bool
    hasLib (ComponentTarget (CLibName _) _, _) = True
    hasLib _                                   = False

    go :: UnitId
       -> [(ComponentTarget, [TargetSelector])]
       -> [GhcEnvironmentFileEntry]
    go unitId targets
      | any hasLib targets = [GhcEnvFilePackageId unitId]
      | otherwise          = []
