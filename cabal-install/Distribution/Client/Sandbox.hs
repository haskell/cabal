-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Sandbox
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- UI for the sandboxing functionality.
-----------------------------------------------------------------------------

module Distribution.Client.Sandbox (
    sandboxInit,
    sandboxDelete,
    sandboxAddSource,
    sandboxAddSourceSnapshot,
    sandboxDeleteSource,
    sandboxListSources,
    sandboxHcPkg,
    dumpPackageEnvironment,
    withSandboxBinDirOnSearchPath,

    loadConfigOrSandboxConfig,
    initPackageDBIfNeeded,
    maybeWithSandboxDirOnSearchPath,

    WereDepsReinstalled(..),
    reinstallAddSourceDeps,
    maybeReinstallAddSourceDeps,

    SandboxPackageInfo(..),
    maybeWithSandboxPackageInfo,

    tryGetIndexFilePath,
    sandboxBuildDir,
    getInstalledPackagesInSandbox,

    -- FIXME: move somewhere else
    configPackageDB', configCompilerAux'
  ) where

import Distribution.Client.Setup
  ( SandboxFlags(..), ConfigFlags(..), ConfigExFlags(..), InstallFlags(..)
  , GlobalFlags(..), defaultConfigExFlags, defaultInstallFlags
  , defaultSandboxLocation, globalRepos )
import Distribution.Client.Sandbox.Timestamp  ( listModifiedDeps
                                              , maybeAddCompilerTimestampRecord
                                              , withAddTimestamps
                                              , withRemoveTimestamps )
import Distribution.Client.Config             ( SavedConfig(..), loadConfig )
import Distribution.Client.Dependency         ( foldProgress )
import Distribution.Client.IndexUtils         ( BuildTreeRefType(..) )
import Distribution.Client.Install            ( InstallArgs,
                                                makeInstallContext,
                                                makeInstallPlan,
                                                processInstallPlan )
import Distribution.Client.Sandbox.PackageEnvironment
  ( PackageEnvironment(..), IncludeComments(..), PackageEnvironmentType(..)
  , createPackageEnvironment, classifyPackageEnvironment
  , tryLoadSandboxPackageEnvironment, loadUserConfig
  , commentPackageEnvironment, showPackageEnvironmentWithComments
  , sandboxPackageEnvironmentFile, userPackageEnvironmentFile )
import Distribution.Client.Sandbox.Types      ( SandboxPackageInfo(..)
                                              , UseSandbox(..) )
import Distribution.Client.Types              ( PackageLocation(..)
                                              , SourcePackage(..) )
import Distribution.Client.Utils              ( inDir, tryCanonicalizePath )
import Distribution.PackageDescription.Configuration
                                              ( flattenPackageDescription )
import Distribution.PackageDescription.Parse  ( readPackageDescription )
import Distribution.Simple.Compiler           ( Compiler(..), PackageDB(..)
                                              , PackageDBStack )
import Distribution.Simple.Configure          ( configCompilerAux
                                              , interpretPackageDbFlags
                                              , getPackageDBContents )
import Distribution.Simple.PreProcess         ( knownSuffixHandlers )
import Distribution.Simple.Program            ( ProgramConfiguration )
import Distribution.Simple.Setup              ( Flag(..), HaddockFlags(..)
                                              , fromFlagOrDefault )
import Distribution.Simple.SrcDist            ( prepareTree )
import Distribution.Simple.Utils              ( die, debug, notice, info, warn
                                              , debugNoWrap, defaultPackageDesc
                                              , findPackageDesc
                                              , intercalate, topHandlerWith
                                              , createDirectoryIfMissingVerbose )
import Distribution.Package                   ( Package(..) )
import Distribution.System                    ( Platform )
import Distribution.Text                      ( display )
import Distribution.Verbosity                 ( Verbosity, lessVerbose )
import Distribution.Compat.Env                ( lookupEnv, setEnv )
import Distribution.Compat.FilePerms          ( setFileHidden )
import qualified Distribution.Client.Sandbox.Index as Index
import qualified Distribution.Simple.PackageIndex  as InstalledPackageIndex
import qualified Distribution.Simple.Register      as Register
import qualified Data.Map                          as M
import qualified Data.Set                          as S
import Control.Exception                      ( assert, bracket_ )
import Control.Monad                          ( forM, liftM2, unless, when )
import Data.Bits                              ( shiftL, shiftR, xor )
import Data.Char                              ( ord )
import Data.IORef                             ( newIORef, writeIORef, readIORef )
import Data.List                              ( delete, foldl' )
import Data.Maybe                             ( fromJust )
import Data.Monoid                            ( mempty, mappend )
import Data.Word                              ( Word32 )
import Numeric                                ( showHex )
import System.Directory                       ( createDirectory
                                              , doesDirectoryExist
                                              , doesFileExist
                                              , getCurrentDirectory
                                              , removeDirectoryRecursive
                                              , removeFile
                                              , renameDirectory )
import System.FilePath                        ( (</>), getSearchPath
                                              , searchPathSeparator )


--
-- * Constants
--

-- | The name of the sandbox subdirectory where we keep snapshots of add-source
-- dependencies.
snapshotDirectoryName :: FilePath
snapshotDirectoryName = "snapshots"

-- | Non-standard build dir that is used for building add-source deps instead of
-- "dist". Fixes surprising behaviour in some cases (see issue #1281).
sandboxBuildDir :: FilePath -> FilePath
sandboxBuildDir sandboxDir = "dist/dist-sandbox-" ++ showHex sandboxDirHash ""
  where
    sandboxDirHash = jenkins sandboxDir

    -- See http://en.wikipedia.org/wiki/Jenkins_hash_function
    jenkins :: String -> Word32
    jenkins str = loop_finish $ foldl' loop 0 str
      where
        loop :: Word32 -> Char -> Word32
        loop hash key_i' = hash'''
          where
            key_i   = toEnum . ord $ key_i'
            hash'   = hash + key_i
            hash''  = hash' + (shiftL hash' 10)
            hash''' = hash'' `xor` (shiftR hash'' 6)

        loop_finish :: Word32 -> Word32
        loop_finish hash = hash'''
          where
            hash'   = hash + (shiftL hash 3)
            hash''  = hash' `xor` (shiftR hash' 11)
            hash''' = hash'' + (shiftL hash'' 15)

--
-- * Basic sandbox functions.
--

-- | Load the default package environment file. In addition to a
-- @PackageEnvironment@, also return a canonical path to the sandbox. Exit with
-- error if the sandbox directory or the package environment file do not exist.
tryLoadSandboxConfig :: Verbosity -> Flag FilePath
                        -> IO (FilePath, PackageEnvironment)
tryLoadSandboxConfig verbosity configFileFlag = do
  pkgEnvDir            <- getCurrentDirectory
  (sandboxDir, pkgEnv) <- tryLoadSandboxPackageEnvironment verbosity pkgEnvDir
                          configFileFlag
  dirExists            <- doesDirectoryExist sandboxDir
  -- TODO: Also check for an initialised package DB?
  unless dirExists $
    die ("No sandbox exists at " ++ sandboxDir)
  info verbosity $ "Using a sandbox located at " ++ sandboxDir
  return (sandboxDir, pkgEnv)

-- | Return the name of the package index file for this package environment.
tryGetIndexFilePath :: SavedConfig -> IO FilePath
tryGetIndexFilePath config = tryGetIndexFilePath' (savedGlobalFlags config)

-- | The same as 'tryGetIndexFilePath', but takes 'GlobalFlags' instead of
-- 'SavedConfig'.
tryGetIndexFilePath' :: GlobalFlags -> IO FilePath
tryGetIndexFilePath' globalFlags = do
  let paths = globalLocalRepos globalFlags
  case paths of
    []  -> die $ "Distribution.Client.Sandbox.tryGetIndexFilePath: " ++
           "no local repos found. " ++ checkConfiguration
    [p] -> return $ p </> Index.defaultIndexFileName
    _   -> die $ "Distribution.Client.Sandbox.tryGetIndexFilePath: " ++
           "too many local repos found. " ++ checkConfiguration

  where
    checkConfiguration = "Please check your configuration ('"
                         ++ userPackageEnvironmentFile ++ "')."

-- | Try to extract a 'PackageDB' from 'ConfigFlags'. Gives a better error
-- message than just pattern-matching.
getSandboxPackageDB :: ConfigFlags -> IO PackageDB
getSandboxPackageDB configFlags = do
  case configPackageDBs configFlags of
    [Just sandboxDB@(SpecificPackageDB _)] -> return sandboxDB
    -- TODO: should we allow multiple package DBs (e.g. with 'inherit')?

    []                                     ->
      die $ "Sandbox package DB is not specified. " ++ sandboxConfigCorrupt
    [_]                                    ->
      die $ "Unexpected contents of the 'package-db' field. "
            ++ sandboxConfigCorrupt
    _                                      ->
      die $ "Too many package DBs provided. " ++ sandboxConfigCorrupt

  where
    sandboxConfigCorrupt = "Your 'cabal.sandbox.config' is probably corrupt."


-- | Which packages are installed in the sandbox package DB?
getInstalledPackagesInSandbox :: Verbosity -> ConfigFlags
                                 -> Compiler -> ProgramConfiguration
                                 -> IO InstalledPackageIndex.PackageIndex
getInstalledPackagesInSandbox verbosity configFlags comp conf = do
    sandboxDB <- getSandboxPackageDB configFlags
    getPackageDBContents verbosity comp sandboxDB conf

-- | Temporarily add $SANDBOX_DIR/bin to $PATH.
withSandboxBinDirOnSearchPath :: FilePath -> IO a -> IO a
withSandboxBinDirOnSearchPath sandboxDir = bracket_ addBinDir rmBinDir
  where
    -- TODO: Instead of modifying the global process state, it'd be better to
    -- set the environment individually for each subprocess invocation. This
    -- will have to wait until the Shell monad is implemented; without it the
    -- required changes are too intrusive.
    addBinDir :: IO ()
    addBinDir = do
      mbOldPath <- lookupEnv "PATH"
      let newPath = maybe sandboxBin ((++) sandboxBin . (:) searchPathSeparator)
                    mbOldPath
      setEnv "PATH" newPath

    rmBinDir :: IO ()
    rmBinDir = do
      oldPath <- getSearchPath
      let newPath = intercalate [searchPathSeparator]
                    (delete sandboxBin oldPath)
      setEnv "PATH" newPath

    sandboxBin = sandboxDir </> "bin"

-- | Initialise a package DB for this compiler if it doesn't exist.
initPackageDBIfNeeded :: Verbosity -> ConfigFlags
                         -> Compiler -> ProgramConfiguration
                         -> IO ()
initPackageDBIfNeeded verbosity configFlags comp conf = do
  SpecificPackageDB dbPath <- getSandboxPackageDB configFlags
  packageDBExists <- doesDirectoryExist dbPath
  unless packageDBExists $
    Register.initPackageDB verbosity comp conf dbPath
  when packageDBExists $
    debug verbosity $ "The package database already exists: " ++ dbPath

-- | Entry point for the 'cabal dump-pkgenv' command.
dumpPackageEnvironment :: Verbosity -> SandboxFlags -> GlobalFlags -> IO ()
dumpPackageEnvironment verbosity _sandboxFlags globalFlags = do
  (sandboxDir, pkgEnv) <- tryLoadSandboxConfig verbosity
                          (globalConfigFile globalFlags)
  commentPkgEnv        <- commentPackageEnvironment sandboxDir
  putStrLn . showPackageEnvironmentWithComments (Just commentPkgEnv) $ pkgEnv

-- | Entry point for the 'cabal sandbox-init' command.
sandboxInit :: Verbosity -> SandboxFlags  -> GlobalFlags -> IO ()
sandboxInit verbosity sandboxFlags globalFlags = do
  -- Check that there is no 'cabal-dev' directory.
  isCabalDevSandbox <- liftM2 (&&) (doesDirectoryExist "cabal-dev")
                       (doesFileExist $ "cabal-dev" </> "cabal.config")
  when isCabalDevSandbox $
    die $
    "You are apparently using a legacy (cabal-dev) sandbox. "
    ++ "To use native cabal sandboxing, please delete the 'cabal-dev' directory "
    ++  "and run 'cabal sandbox init'."

  -- Create the sandbox directory.
  let sandboxDir' = fromFlagOrDefault defaultSandboxLocation
                    (sandboxLocation sandboxFlags)
  createDirectoryIfMissingVerbose verbosity True sandboxDir'
  sandboxDir <- tryCanonicalizePath sandboxDir'
  setFileHidden sandboxDir
  notice verbosity $ "Using a sandbox located at " ++ sandboxDir

  -- Determine which compiler to use (using the value from ~/.cabal/config).
  userConfig <- loadConfig verbosity (globalConfigFile globalFlags) NoFlag
  (comp, platform, _) <- configCompilerAux (savedConfigureFlags userConfig)

  -- Create the package environment file.
  pkgEnvDir   <- getCurrentDirectory
  createPackageEnvironment verbosity sandboxDir pkgEnvDir
    NoComments comp platform
  (_, pkgEnv) <- tryLoadSandboxPackageEnvironment verbosity pkgEnvDir
                 (globalConfigFile globalFlags)

  -- Create the index file if it doesn't exist.
  indexFile <- tryGetIndexFilePath (pkgEnvSavedConfig pkgEnv)
  Index.createEmpty verbosity indexFile

  -- We don't create the package DB for the default compiler here: it's created
  -- by demand in 'install' and 'configure'. This way, if you run 'sandbox init'
  -- and then 'configure -w /path/to/nondefault-ghc', you'll end up with a
  -- package DB for only one compiler instead of two.

-- | Entry point for the 'cabal sandbox-delete' command.
sandboxDelete :: Verbosity -> SandboxFlags -> GlobalFlags -> IO ()
sandboxDelete verbosity _sandboxFlags globalFlags = do
  (useSandbox, _) <- loadConfigOrSandboxConfig verbosity
                     (globalConfigFile globalFlags) mempty
  case useSandbox of
    NoSandbox -> die "Not in a sandbox."
    UseSandbox sandboxDir -> do
      pkgEnvDir  <- getCurrentDirectory

      -- Remove the cabal.sandbox.config file
      removeFile (pkgEnvDir </> sandboxPackageEnvironmentFile)

      -- Remove the sandbox directory, unless we're using a shared sandbox.
      let isNonDefaultLocation = sandboxDir /=
                                 (pkgEnvDir </> defaultSandboxLocation)

      when isNonDefaultLocation $
        die $ "Non-default sandbox location used: '" ++ sandboxDir
        ++ "'\nAssuming a shared sandbox. Please delete '"
        ++ sandboxDir ++ "' manually."

      notice verbosity $ "Deleting the sandbox located at " ++ sandboxDir
      removeDirectoryRecursive sandboxDir

-- Common implementation of 'sandboxAddSource' and 'sandboxAddSourceSnapshot'.
doAddSource :: Verbosity -> [FilePath] -> FilePath -> PackageEnvironment
               -> BuildTreeRefType
               -> IO ()
doAddSource verbosity buildTreeRefs sandboxDir pkgEnv refType = do
  let savedConfig       = pkgEnvSavedConfig pkgEnv
  indexFile            <- tryGetIndexFilePath savedConfig

  -- If we're running 'sandbox add-source' for the first time for this compiler,
  -- we need to create an initial timestamp record.
  (comp, platform, _) <- configCompilerAux . savedConfigureFlags $ savedConfig
  maybeAddCompilerTimestampRecord verbosity sandboxDir indexFile
    (compilerId comp) platform

  withAddTimestamps sandboxDir $ do
    -- FIXME: path canonicalisation is done in addBuildTreeRefs, but we do it
    -- twice because of the timestamps file.
    buildTreeRefs' <- mapM tryCanonicalizePath buildTreeRefs
    Index.addBuildTreeRefs verbosity indexFile buildTreeRefs' refType
    return buildTreeRefs'

-- | Entry point for the 'cabal sandbox add-source' command.
sandboxAddSource :: Verbosity -> [FilePath] -> SandboxFlags -> GlobalFlags
                    -> IO ()
sandboxAddSource verbosity buildTreeRefs sandboxFlags globalFlags = do
  (sandboxDir, pkgEnv) <- tryLoadSandboxConfig verbosity
                          (globalConfigFile globalFlags)

  if fromFlagOrDefault False (sandboxSnapshot sandboxFlags)
    then sandboxAddSourceSnapshot verbosity buildTreeRefs sandboxDir pkgEnv
    else doAddSource verbosity buildTreeRefs sandboxDir pkgEnv LinkRef

-- | Entry point for the 'cabal sandbox add-source --snapshot' command.
sandboxAddSourceSnapshot :: Verbosity -> [FilePath] -> FilePath
                            -> PackageEnvironment
                            -> IO ()
sandboxAddSourceSnapshot verbosity buildTreeRefs sandboxDir pkgEnv = do
  let snapshotDir = sandboxDir </> snapshotDirectoryName

  -- Use 'D.S.SrcDist.prepareTree' to copy each package's files to our private
  -- location.
  createDirectoryIfMissingVerbose verbosity True snapshotDir

  -- Collect the package descriptions first, so that if some path does not refer
  -- to a cabal package, we fail immediately.
  pkgs      <- forM buildTreeRefs $ \buildTreeRef ->
    inDir (Just buildTreeRef) $
    return . flattenPackageDescription
            =<< readPackageDescription verbosity
            =<< defaultPackageDesc     verbosity

  -- Copy the package sources to "snapshots/$PKGNAME-$VERSION-tmp". If
  -- 'prepareTree' throws an error at any point, the old snapshots will still be
  -- in consistent state.
  tmpDirs <- forM (zip buildTreeRefs pkgs) $ \(buildTreeRef, pkg) ->
    inDir (Just buildTreeRef) $ do
      let targetDir    = snapshotDir </> (display . packageId $ pkg)
          targetTmpDir = targetDir ++ "-tmp"
      dirExists <- doesDirectoryExist targetTmpDir
      when dirExists $
        removeDirectoryRecursive targetDir
      createDirectory targetTmpDir
      prepareTree verbosity pkg Nothing targetTmpDir knownSuffixHandlers
      return (targetTmpDir, targetDir)

  -- Now rename the "snapshots/$PKGNAME-$VERSION-tmp" dirs to
  -- "snapshots/$PKGNAME-$VERSION".
  snapshots <- forM tmpDirs $ \(targetTmpDir, targetDir) -> do
    dirExists <- doesDirectoryExist targetDir
    when dirExists $
      removeDirectoryRecursive targetDir
    renameDirectory targetTmpDir targetDir
    return targetDir

  -- Once the packages are copied, just 'add-source' them as usual.
  doAddSource verbosity snapshots sandboxDir pkgEnv SnapshotRef

-- | Entry point for the 'cabal sandbox delete-source' command.
sandboxDeleteSource :: Verbosity -> [FilePath] -> SandboxFlags -> GlobalFlags
                       -> IO ()
sandboxDeleteSource verbosity buildTreeRefs _sandboxFlags globalFlags = do
  (sandboxDir, pkgEnv) <- tryLoadSandboxConfig verbosity
                          (globalConfigFile globalFlags)
  indexFile            <- tryGetIndexFilePath (pkgEnvSavedConfig pkgEnv)

  withRemoveTimestamps sandboxDir $ do
    -- FIXME: path canonicalisation is done in addBuildTreeRefs, but we do it
    -- twice because of the timestamps file.
    buildTreeRefs' <- mapM tryCanonicalizePath buildTreeRefs
    Index.removeBuildTreeRefs verbosity indexFile buildTreeRefs'
    return buildTreeRefs'

-- | Entry point for the 'cabal sandbox list-sources' command.
sandboxListSources :: Verbosity -> SandboxFlags -> GlobalFlags
                      -> IO ()
sandboxListSources verbosity _sandboxFlags globalFlags = do
  (sandboxDir, pkgEnv) <- tryLoadSandboxConfig verbosity
                           (globalConfigFile globalFlags)
  indexFile            <- tryGetIndexFilePath (pkgEnvSavedConfig pkgEnv)

  refs <- Index.listBuildTreeRefs verbosity
          Index.ListIgnored Index.LinksAndSnapshots indexFile
  when (null refs) $
    notice verbosity $ "Index file '" ++ indexFile
    ++ "' has no references to local build trees."
  when (not . null $ refs) $ do
    notice verbosity $ "Source dependencies registered "
      ++ "in the current sandbox ('" ++ sandboxDir ++ "'):\n\n"
    mapM_ putStrLn refs
    notice verbosity $ "\nTo unregister source dependencies, "
                       ++ "use the 'sandbox delete-source' command."

-- | Invoke the @hc-pkg@ tool with provided arguments, restricted to the
-- sandbox.
sandboxHcPkg :: Verbosity -> SandboxFlags -> GlobalFlags -> [String] -> IO ()
sandboxHcPkg verbosity _sandboxFlags globalFlags extraArgs = do
  (_sandboxDir, pkgEnv) <- tryLoadSandboxConfig verbosity
                           (globalConfigFile globalFlags)
  let configFlags = savedConfigureFlags . pkgEnvSavedConfig $ pkgEnv
      dbStack     = configPackageDB' configFlags
  (comp, _platform, conf) <- configCompilerAux' configFlags

  Register.invokeHcPkg verbosity comp conf dbStack extraArgs

-- | Check which type of package environment we're in and return a
-- correctly-initialised @SavedConfig@ and a @UseSandbox@ value that indicates
-- whether we're working in a sandbox.
loadConfigOrSandboxConfig :: Verbosity
                             -> Flag FilePath -- ^ --config-file
                             -> Flag Bool     -- ^ Ignored if we're in a sandbox.
                             -> IO (UseSandbox, SavedConfig)
loadConfigOrSandboxConfig verbosity configFileFlag userInstallFlag = do
  currentDir <- getCurrentDirectory
  pkgEnvType <- classifyPackageEnvironment currentDir
  case pkgEnvType of
    SandboxPackageEnvironment -> do
      (sandboxDir, pkgEnv) <- tryLoadSandboxConfig verbosity configFileFlag
                              -- ^ Prints an error message and exits on error.
      let config = pkgEnvSavedConfig pkgEnv
      return (UseSandbox sandboxDir, config)

    UserPackageEnvironment    -> do
      config <- loadConfig verbosity configFileFlag userInstallFlag
      userConfig <- loadUserConfig verbosity currentDir
      return (NoSandbox, config `mappend` userConfig)

    AmbientPackageEnvironment      -> do
      config <- loadConfig verbosity configFileFlag userInstallFlag
      return (NoSandbox, config)

-- | If we're in a sandbox, call @withSandboxBinDirOnSearchPath@, otherwise do
-- nothing.
maybeWithSandboxDirOnSearchPath :: UseSandbox -> IO a -> IO a
maybeWithSandboxDirOnSearchPath NoSandbox               act = act
maybeWithSandboxDirOnSearchPath (UseSandbox sandboxDir) act =
  withSandboxBinDirOnSearchPath sandboxDir $ act

-- | Had reinstallAddSourceDeps actually reinstalled any dependencies?
data WereDepsReinstalled = ReinstalledSomeDeps | NoDepsReinstalled

-- | Reinstall those add-source dependencies that have been modified since
-- we've last installed them. Assumes that we're working inside a sandbox.
reinstallAddSourceDeps :: Verbosity
                          -> ConfigFlags  -> ConfigExFlags
                          -> InstallFlags -> GlobalFlags
                          -> FilePath
                          -> IO WereDepsReinstalled
reinstallAddSourceDeps verbosity configFlags' configExFlags
                       installFlags globalFlags sandboxDir = topHandler' $ do
  let sandboxDistPref     = sandboxBuildDir sandboxDir
      configFlags         = configFlags'
                            { configDistPref  = Flag sandboxDistPref }
      haddockFlags        = mempty
                            { haddockDistPref = Flag sandboxDistPref }
  (comp, platform, conf) <- configCompilerAux' configFlags
  retVal                 <- newIORef NoDepsReinstalled

  withSandboxPackageInfo verbosity configFlags globalFlags
                         comp platform conf sandboxDir $ \sandboxPkgInfo ->
    unless (null $ modifiedAddSourceDependencies sandboxPkgInfo) $ do

      let args :: InstallArgs
          args = ((configPackageDB' configFlags)
                 ,(globalRepos globalFlags)
                 ,comp, platform, conf
                 ,UseSandbox sandboxDir, Just sandboxPkgInfo
                 ,globalFlags, configFlags, configExFlags, installFlags
                 ,haddockFlags)

      -- This can actually be replaced by a call to 'install', but we use a
      -- lower-level API because of layer separation reasons. Additionally, we
      -- might want to use some lower-level features this in the future.
      withSandboxBinDirOnSearchPath sandboxDir $ do
        installContext <- makeInstallContext verbosity args Nothing
        installPlan    <- foldProgress logMsg die return =<<
                          makeInstallPlan verbosity args installContext

        processInstallPlan verbosity args installContext installPlan
        writeIORef retVal ReinstalledSomeDeps

  readIORef retVal

    where
      logMsg message rest = debugNoWrap verbosity message >> rest

      topHandler' = topHandlerWith $ \_ -> do
        warn verbosity "Couldn't reinstall some add-source dependencies."
        -- Here we can't know whether any deps have been reinstalled, so we have
        -- to be conservative.
        return ReinstalledSomeDeps

-- | Produce a 'SandboxPackageInfo' and feed it to the given action. Note that
-- we don't update the timestamp file here - this is done in
-- 'postInstallActions'.
withSandboxPackageInfo :: Verbosity -> ConfigFlags -> GlobalFlags
                          -> Compiler -> Platform -> ProgramConfiguration
                          -> FilePath
                          -> (SandboxPackageInfo -> IO ())
                          -> IO ()
withSandboxPackageInfo verbosity configFlags globalFlags
                       comp platform conf sandboxDir cont = do
  -- List all add-source deps.
  indexFile              <- tryGetIndexFilePath' globalFlags
  buildTreeRefs          <- Index.listBuildTreeRefs verbosity
                            Index.DontListIgnored Index.OnlyLinks indexFile
  let allAddSourceDepsSet = S.fromList buildTreeRefs

  -- List all packages installed in the sandbox.
  installedPkgIndex <- getInstalledPackagesInSandbox verbosity
                       configFlags comp conf

  -- Get the package descriptions for all add-source deps.
  depsCabalFiles <- mapM findPackageDesc buildTreeRefs
  depsPkgDescs   <- mapM (readPackageDescription verbosity) depsCabalFiles
  let depsMap           = M.fromList (zip buildTreeRefs depsPkgDescs)
      isInstalled pkgid = not . null
        . InstalledPackageIndex.lookupSourcePackageId installedPkgIndex $ pkgid
      installedDepsMap  = M.filter (isInstalled . packageId) depsMap

  -- Get the package ids of modified (and installed) add-source deps.
  modifiedAddSourceDeps <- listModifiedDeps verbosity sandboxDir
                           (compilerId comp) platform installedDepsMap
  -- 'fromJust' here is safe because 'modifiedAddSourceDeps' are guaranteed to
  -- be a subset of the keys of 'depsMap'.
  let modifiedDeps    = [ (modDepPath, fromJust $ M.lookup modDepPath depsMap)
                        | modDepPath <- modifiedAddSourceDeps ]
      modifiedDepsMap = M.fromList modifiedDeps

  assert (all (`S.member` allAddSourceDepsSet) modifiedAddSourceDeps) (return ())
  unless (null modifiedDeps) $
    notice verbosity $ "Some add-source dependencies have been modified. "
                       ++ "They will be reinstalled..."

  -- Get the package ids of the remaining add-source deps (some are possibly not
  -- installed).
  let otherDeps = M.assocs (depsMap `M.difference` modifiedDepsMap)

  -- Finally, assemble a 'SandboxPackageInfo'.
  cont $ SandboxPackageInfo (map toSourcePackage modifiedDeps)
    (map toSourcePackage otherDeps) installedPkgIndex allAddSourceDepsSet

  where
    toSourcePackage (path, pkgDesc) = SourcePackage
      (packageId pkgDesc) pkgDesc (LocalUnpackedPackage path) Nothing

-- | Same as 'withSandboxPackageInfo' if we're inside a sandbox and a no-op
-- otherwise.
maybeWithSandboxPackageInfo :: Verbosity -> ConfigFlags -> GlobalFlags
                               -> Compiler -> Platform -> ProgramConfiguration
                               -> UseSandbox
                               -> (Maybe SandboxPackageInfo -> IO ())
                               -> IO ()
maybeWithSandboxPackageInfo verbosity configFlags globalFlags
                            comp platform conf useSandbox cont =
  case useSandbox of
    NoSandbox             -> cont Nothing
    UseSandbox sandboxDir -> withSandboxPackageInfo verbosity
                             configFlags globalFlags
                             comp platform conf sandboxDir
                             (\spi -> cont (Just spi))

-- | Check if a sandbox is present and call @reinstallAddSourceDeps@ in that
-- case.
maybeReinstallAddSourceDeps :: Verbosity
                               -> Flag (Maybe Int) -- ^ The '-j' flag
                               -> ConfigFlags      -- ^ Saved configure flags
                                                   -- (from dist/setup-config)
                               -> GlobalFlags
                               -> IO (UseSandbox, WereDepsReinstalled)
maybeReinstallAddSourceDeps verbosity numJobsFlag configFlags' globalFlags' = do
  currentDir <- getCurrentDirectory
  pkgEnvType <- classifyPackageEnvironment currentDir
  case pkgEnvType of
    AmbientPackageEnvironment -> return (NoSandbox, NoDepsReinstalled)
    UserPackageEnvironment    -> return (NoSandbox, NoDepsReinstalled)
    SandboxPackageEnvironment -> do
      (useSandbox, config)    <- loadConfigOrSandboxConfig verbosity
                                 (globalConfigFile globalFlags') mempty
      case useSandbox of
        UseSandbox sandboxDir -> do

          -- Actually reinstall the modified add-source deps.
          let configFlags    = mappendSomeSavedFlags configFlags' $
                               savedConfigureFlags config
              configExFlags  = defaultConfigExFlags
                               `mappend` savedConfigureExFlags config
              installFlags'  = defaultInstallFlags
                               `mappend` savedInstallFlags config
              installFlags   = installFlags' {
                installNumJobs    = installNumJobs installFlags'
                                    `mappend` numJobsFlag
                }
              globalFlags    = savedGlobalFlags config
              -- This makes it possible to override things like
              -- 'remote-repo-cache' from the command line. These options are
              -- hidden, and are only useful for debugging, so this should be
              -- fine.
                               `mappend` globalFlags'
          depsReinstalled <- reinstallAddSourceDeps verbosity
                             configFlags configExFlags installFlags globalFlags
                             sandboxDir
          return (useSandbox, depsReinstalled)

        NoSandbox -> error $
                     "Distribution.Client.Sandbox.maybeReinstallAddSourceDeps: "
                     ++ "can't happen."

  where

    -- NOTE: we can't simply `mappend` configFlags' because we don't want to
    -- auto-enable things like 'library-profiling' for all add-source
    -- dependencies even if the user has passed '--enable-library-profiling' to
    -- 'cabal configure'. These options are supposed to be set in
    -- 'cabal.config'.
    mappendSomeSavedFlags :: ConfigFlags -> ConfigFlags -> ConfigFlags
    mappendSomeSavedFlags savedFlags configFlags =
      configFlags {
        configHcFlavor   = configHcFlavor configFlags
                           `mappend` configHcFlavor savedFlags,
        configHcPath     = configHcPath configFlags
                           `mappend` configHcPath savedFlags,
        configHcPkg      = configHcPkg configFlags
                           `mappend` configHcPkg savedFlags,
        -- NOTE: since configPackageDBs is a list instead of a flag, we override
        -- instead of mappending. We know that the list is not empty since it
        -- was passed to us from 'configure' (see 'setPackageDB' in 'Main.hs').
        configPackageDBs = configPackageDBs savedFlags
        }

--
-- Utils (transitionary)
--
-- FIXME: configPackageDB' and configCompilerAux' don't really belong in this
-- module
--

configPackageDB' :: ConfigFlags -> PackageDBStack
configPackageDB' cfg =
    interpretPackageDbFlags userInstall (configPackageDBs cfg)
  where
    userInstall = fromFlagOrDefault True (configUserInstall cfg)

configCompilerAux' :: ConfigFlags
                   -> IO (Compiler, Platform, ProgramConfiguration)
configCompilerAux' configFlags =
  configCompilerAux configFlags
    --FIXME: make configCompilerAux use a sensible verbosity
    { configVerbosity = fmap lessVerbose (configVerbosity configFlags) }
