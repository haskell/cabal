{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}

-- | cabal-install CLI command: vendor
module Distribution.Client.CmdVendor
  ( vendorCommand
  , vendorAction
  , VendorFlags (..)
  , defaultVendorFlags
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.DistDirLayout (DistDirLayout (..))
import Distribution.Client.Errors
import Distribution.Client.FetchUtils (fetchRepoTarball)
import Distribution.Client.GlobalFlags (RepoContext)
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , cfgVerbosity
  , defaultNixStyleFlags
  , nixStyleOptions
  )
import Distribution.Client.ProjectConfig
  ( projectConfigWithBuilderRepoContext
  )
import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectPlanning
import Distribution.Client.Setup (GlobalFlags)
import Distribution.Client.Types
  ( PackageLocation (..)
  , Repo (..)
  , asPosixPath
  )
import Distribution.Client.Types.SourceRepo
  ( SourceRepoMaybe
  , SourceRepositoryPackage (..)
  )
import Distribution.Package
  ( PackageName
  , packageId
  , packageName
  )
import Distribution.ReadE (succeedReadE)
import Distribution.Simple.Command
  ( CommandUI (..)
  , OptionField
  , ShowOrParseArgs
  , option
  , reqArg
  , usageAlternatives
  )
import Distribution.Simple.Flag
  ( Flag
  , flagToList
  , fromFlagOrDefault
  , pattern Flag
  )
import Distribution.Simple.Utils
  ( copyFileVerbose
  , dieWithException
  , notice
  , warn
  , wrapText
  , writeFileAtomic
  )
import Distribution.System (OS (Windows), buildOS)
import Distribution.Verbosity (normal)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import System.Directory
  ( canonicalizePath
  , createDirectoryIfMissing
  , doesFileExist
  , makeAbsolute
  , removeFile
  )
import System.FilePath
  ( isRelative
  , makeRelative
  , (<.>)
  , (</>)
  )

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

vendorCommand :: CommandUI (NixStyleFlags VendorFlags)
vendorCommand =
  CommandUI
    { commandName = "v2-vendor"
    , commandSynopsis = "Vendor dependencies into a local repository."
    , commandUsage = usageAlternatives "v2-vendor" ["[FLAGS]", "[PACKAGES] [FLAGS]"]
    , commandDescription = Just $ \_ ->
        wrapText $
          "Copy the source tarballs of the project's dependencies into a "
            ++ "directory laid out as a 'file+noindex' package repository, so "
            ++ "that the project can later be built without network access.\n\n"
            ++ "The dependencies are those selected by the solver for the current "
            ++ "project configuration, including build tools and setup "
            ++ "dependencies. Each is written as '<pkgid>.tar.gz', with the "
            ++ "revised '.cabal' file from the package index next to it as "
            ++ "'<pkgid>.cabal'. Dependencies from 'source-repository-package' "
            ++ "stanzas are vendored as source distributions of their checkouts. "
            ++ "Local packages of the project are not vendored.\n\n"
            ++ "The directory defaults to 'vendor' in the project root and can be "
            ++ "changed with '--output-directory'. After vendoring, the command "
            ++ "prints the 'repository' stanza to add to the project file so that "
            ++ "the vendored packages are used. When package names are given, "
            ++ "only those packages are vendored.\n"
    , commandNotes = Just $ \pname ->
        "Examples:\n"
          ++ "  "
          ++ pname
          ++ " v2-vendor\n"
          ++ "    Vendor all dependencies of the project into ./vendor\n"
          ++ "  "
          ++ pname
          ++ " v2-vendor --output-directory=deps\n"
          ++ "    Vendor all dependencies of the project into ./deps\n"
          ++ "  "
          ++ pname
          ++ " v2-vendor aeson text\n"
          ++ "    Vendor only the aeson and text packages\n"
          ++ "  "
          ++ pname
          ++ " v2-vendor --dry-run\n"
          ++ "    Show which packages would be vendored\n"
    , commandDefaultFlags = defaultNixStyleFlags defaultVendorFlags
    , commandOptions = nixStyleOptions vendorOptions
    }

-------------------------------------------------------------------------------
-- Flags
-------------------------------------------------------------------------------

newtype VendorFlags = VendorFlags
  { vendorOutputDir :: Flag FilePath
  }
  deriving (Eq, Show)

defaultVendorFlags :: VendorFlags
defaultVendorFlags =
  VendorFlags
    { vendorOutputDir = mempty
    }

vendorOptions :: ShowOrParseArgs -> [OptionField VendorFlags]
vendorOptions _ =
  [ option
      ['o']
      ["output-directory", "outputdir"]
      "The directory to vendor the packages into (default: 'vendor' in the project root)"
      vendorOutputDir
      (\v flags -> flags{vendorOutputDir = v})
      (reqArg "PATH" (succeedReadE Flag) flagToList)
  ]

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

-- | Where a vendored package comes from.
data VendorSource
  = -- | A package from a package repository, with the revised @.cabal@ file
    -- from the repository's index if it has one.
    FromRepo Repo (Maybe LBS.ByteString)
  | -- | A package from a @source-repository-package@ stanza, as the source
    -- distribution tarball that cabal made from its checkout.
    FromSourceRepo SourceRepoMaybe FilePath

-- | The name of the repository in the stanza printed for the user.
vendoredRepoName :: String
vendoredRepoName = "vendored"

-- | To a first approximation, the @vendor@ command runs the first phase of
-- the @build@ command where we bring the install plan up to date, and then
-- copies the source tarball of every dependency in the plan into a directory
-- that cabal can use as a @file+noindex@ package repository.
vendorAction :: NixStyleFlags VendorFlags -> [String] -> GlobalFlags -> IO ()
vendorAction flags@NixStyleFlags{extraFlags = VendorFlags{vendorOutputDir}} pkgArgs globalFlags = do
  pkgNames <- for pkgArgs $ \arg ->
    case simpleParsec arg of
      Just name -> return (name :: PackageName)
      Nothing -> dieWithException verbosity (VendorInvalidPackageName arg)

  ProjectBaseContext
    { distDirLayout
    , cabalDirLayout
    , projectConfig
    , localPackages
    , buildSettings
    } <-
    establishProjectBaseContext verbosity cliConfig OtherCommand

  (_, elaboratedPlan, _, _, _) <-
    rebuildInstallPlan
      verbosity
      distDirLayout
      cabalDirLayout
      projectConfig
      localPackages
      Nothing

  let projectRoot = distProjectRootDirectory distDirLayout
  vendorDir <-
    makeAbsolute $
      fromFlagOrDefault (projectRoot </> "vendor") vendorOutputDir

  let elabPkgs =
        [ elab
        | pkg <- InstallPlan.toList elaboratedPlan
        , elab <- case pkg of
            InstallPlan.Configured elab -> [elab]
            InstallPlan.Installed elab -> [elab]
            InstallPlan.PreExisting _ -> []
        ]
      localNames =
        Set.fromList
          [packageName elab | elab <- elabPkgs, elabLocalToProject elab]

      -- One package can appear once per component in the plan, so collect
      -- by package id.
      (sources, notVendored) =
        foldr classify (Map.empty, []) elabPkgs
      classify elab (vendorable, skipped)
        | elabLocalToProject elab = (vendorable, skipped)
        | otherwise =
            case elabPkgSourceLocation elab of
              RepoTarballPackage repo _ _ ->
                ( Map.insert pkgid (FromRepo repo (elabPkgDescriptionOverride elab)) vendorable
                , skipped
                )
              RemoteSourceRepoPackage srp (Just tarball) ->
                (Map.insert pkgid (FromSourceRepo srp tarball) vendorable, skipped)
              RemoteSourceRepoPackage srp Nothing ->
                (vendorable, (pkgid, "source repository " ++ Text.unpack (srpLocation srp) ++ " has not been fetched") : skipped)
              RemoteTarballPackage uri _ ->
                (vendorable, (pkgid, "remote tarball " ++ show uri ++ " is listed in the project's packages") : skipped)
              LocalTarballPackage path ->
                (vendorable, (pkgid, "local tarball " ++ path ++ " is listed in the project's packages") : skipped)
              LocalUnpackedPackage path ->
                (vendorable, (pkgid, "local directory " ++ path ++ " is listed in the project's packages") : skipped)
        where
          pkgid = packageId elab

      -- Restrict to the requested package names, if any.
      wanted = Set.fromList pkgNames
      unknown = Set.toList (wanted `Set.difference` Set.fromList (map packageName elabPkgs))
      local = Set.toList (wanted `Set.intersection` localNames)
      selected
        | null pkgNames = sources
        | otherwise = Map.filterWithKey (\pkgid _ -> packageName pkgid `Set.member` wanted) sources
      partial = Map.size selected < Map.size sources

      dryRun =
        buildSettingDryRun buildSettings
          || buildSettingOnlyDownload buildSettings

  unless (null unknown) $
    dieWithException verbosity (VendorUnknownPackages unknown)
  unless (null local) $
    dieWithException verbosity (VendorLocalPackages local)

  for_ notVendored $ \(pkgid, why) ->
    warn verbosity $ "Not vendoring " ++ prettyShow pkgid ++ ": " ++ why ++ "."

  if dryRun
    then
      notice verbosity $
        unlines $
          ("Would vendor the following packages into " ++ vendorDir ++ ":")
            : [" - " ++ prettyShow pkgid | pkgid <- Map.keys selected]
    else do
      createDirectoryIfMissing True vendorDir
      projectConfigWithBuilderRepoContext verbosity buildSettings $ \repoCtxt ->
        for_ (Map.toList selected) $ \(pkgid, source) ->
          vendorPackage verbosity repoCtxt vendorDir pkgid source

      -- The index cache of a file+noindex repository is never invalidated by
      -- cabal itself, so drop it; it is rebuilt on first use.
      let cacheFile = vendorDir </> "noindex.cache"
      cacheExists <- doesFileExist cacheFile
      when cacheExists $ removeFile cacheFile

      notice verbosity $
        vendorReport projectRoot vendorDir partial (Map.size sources) (Map.toList selected)
  where
    verbosity = cfgVerbosity normal flags
    cliConfig =
      commandLineFlagsToProjectConfig
        globalFlags
        flags
        mempty -- ClientInstallFlags, not needed here

-- | Copy one package into the vendor directory: the tarball as
-- @\<pkgid\>.tar.gz@ and, for repository packages with a revised @.cabal@
-- file, that file as @\<pkgid\>.cabal@ next to it. That is the layout
-- "Distribution.Client.IndexUtils" reads a @file+noindex@ repository from.
vendorPackage :: Verbosity -> RepoContext -> FilePath -> PackageId -> VendorSource -> IO ()
vendorPackage verbosity repoCtxt vendorDir pkgid source = do
  src <- case source of
    FromRepo repo _ -> fetchRepoTarball verbosity repoCtxt repo pkgid
    FromSourceRepo _ tarball -> return tarball
  let dest = vendorDir </> prettyShow pkgid <.> "tar.gz"
  -- When the project already takes this package from the vendor directory,
  -- the source is the destination.
  sameFile <- do
    destExists <- doesFileExist dest
    if destExists
      then (==) <$> canonicalizePath src <*> canonicalizePath dest
      else return False
  unless sameFile $ copyFileVerbose verbosity src dest
  case source of
    FromRepo _ (Just cabalFile) ->
      writeFileAtomic (vendorDir </> prettyShow pkgid <.> "cabal") cabalFile
    _ -> return ()

-- | What was vendored, and the project configuration needed to use it.
vendorReport :: FilePath -> FilePath -> Bool -> Int -> [(PackageId, VendorSource)] -> String
vendorReport projectRoot vendorDir partial total selected =
  unlines $
    [ summary
    , ""
    , "To build using the vendored packages, add the following to the project file:"
    , ""
    , "    repository " ++ vendoredRepoName
    , "      url: " ++ vendorUrl
    , ""
    , "    active-repositories: " ++ activeRepos
    ]
      ++ sourceRepoNotes
  where
    summary
      | partial =
          "Vendored " ++ show (length selected) ++ " of " ++ show total ++ " dependencies into " ++ vendorDir
      | otherwise =
          "Vendored " ++ show (length selected) ++ " " ++ plural (length selected) "package" "packages" ++ " into " ++ vendorDir
    plural n singular pluralForm = if n == 1 then singular else pluralForm

    -- Prefer a path relative to the project root, so that the vendored
    -- repository can be committed along with the project.
    relativeDir = makeRelative projectRoot vendorDir
    vendorUrl
      | isRelative relativeDir = "file+noindex:" ++ asPosixPath relativeDir
      | Windows <- buildOS = "file+noindex:" ++ asPosixPath vendorDir
      | otherwise = "file+noindex://" ++ vendorDir

    -- With only some dependencies vendored, the other repositories have to
    -- stay active; the vendored one then overrides them for its packages.
    activeRepos
      | partial = ":rest, " ++ vendoredRepoName ++ ":override"
      | otherwise = vendoredRepoName

    sourceRepoNotes =
      case [(pkgid, srp) | (pkgid, FromSourceRepo srp _) <- selected] of
        [] -> []
        srps ->
          [ ""
          , "The vendored repository now provides these source-repository-package"
          , "dependencies, whose stanzas can be removed from the project file:"
          ]
            ++ [ "  - " ++ prettyShow pkgid ++ " (" ++ describeSourceRepo srp ++ ")"
               | (pkgid, srp) <- srps
               ]

    describeSourceRepo SourceRepositoryPackage{srpType, srpLocation, srpTag, srpBranch, srpSubdir} =
      intercalate ", " $
        [prettyShow srpType ++ " " ++ Text.unpack srpLocation]
          ++ ["tag " ++ Text.unpack tag | Just tag <- [srpTag]]
          ++ ["branch " ++ Text.unpack branch | Just branch <- [srpBranch]]
          ++ ["subdir " ++ subdir | Just subdir <- [srpSubdir]]
