{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

-- | cabal-install CLI command: vendor
module Distribution.Client.CmdVendor
  ( vendorCommand
  , vendorAction
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.DistDirLayout (DistDirLayout (..))
import Distribution.Client.Errors
import Distribution.Client.FetchUtils (fetchRepoTarball)
import Distribution.Client.Get (unpackPackage)
import Distribution.Client.GlobalFlags (RepoContext)
import Distribution.Client.InstallPlan (foldPlanPackage)
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.NixStyleOptions (NixStyleFlags (..), cfgVerbosity, defaultNixStyleFlags, nixStyleOptions)
import Distribution.Client.ProjectConfig (projectConfigWithBuilderRepoContext)
import Distribution.Client.ProjectOrchestration
import Distribution.Client.ProjectPlanning
import Distribution.Client.Setup (GlobalFlags)
import Distribution.Client.Types (PackageLocation (..), Repo (..), asPosixPath)
import Distribution.Client.Types.SourceRepo (SourceRepoMaybe, SourceRepositoryPackage (..))
import Distribution.Package (PackageName, packageId, packageName)
import Distribution.ReadE (succeedReadE)
import Distribution.Simple.Command
  ( CommandUI (..)
  , OptionField
  , ShowOrParseArgs
  , option
  , reqArg
  , usageAlternatives
  )
import Distribution.Simple.Flag (Flag, flagToList, fromFlagOrDefault, pattern Flag)
import Distribution.Simple.Setup (trueArg)
import Distribution.Simple.Utils
  ( copyFileVerbose
  , dieWithException
  , info
  , noticeDoc
  , warn
  , wrapText
  , writeFileAtomic
  )
import Distribution.System (OS (Windows), buildOS)
import Distribution.Verbosity (normal)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import System.Directory
  ( canonicalizePath
  , createDirectoryIfMissing
  , doesDirectoryExist
  , listDirectory
  , makeAbsolute
  , removeFile
  , removePathForcibly
  )
import System.FilePath (isRelative, makeRelative, stripExtension, (<.>), (</>))
import Text.PrettyPrint hiding ((<>))

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

vendorCommand :: CommandUI (NixStyleFlags VendorFlags)
vendorCommand =
  CommandUI
    { commandName = "v2-vendor"
    , commandSynopsis = "Vendor dependencies into a local repository."
    , commandUsage = usageAlternatives "v2-vendor" ["[FLAGS]", "[PACKAGES] [FLAGS]"]
    , commandDescription =
        Just . const . render . vcat $
          paragraph <$> [intro, selection, directory, unpacking]
    , commandNotes = Just $ \pname -> render (examples pname) ++ "\n"
    , commandDefaultFlags = defaultNixStyleFlags defaultVendorFlags
    , commandOptions = nixStyleOptions vendorOptions
    }
  where
    -- A wrapped paragraph ends with a newline, so stacking paragraphs with
    -- 'vcat' leaves a blank line between them.
    paragraph = text . wrapText

    intro =
      "Copy the source tarballs of the project's dependencies into a "
        ++ "directory laid out as a 'file+noindex' package repository, so "
        ++ "that the project can later be built without network access. "
        ++ "Tarballs not already in the download cache are downloaded first."

    selection =
      "The dependencies are those selected by the solver for the current "
        ++ "project configuration, including build tools and setup "
        ++ "dependencies. Each is written as '<pkgid>.tar.gz', with its "
        ++ "'.cabal' file from the package index, which carries any "
        ++ "revision, next to it as '<pkgid>.cabal'. Dependencies from "
        ++ "'source-repository-package' stanzas are vendored as source "
        ++ "distributions of their checkouts. Local packages of the project "
        ++ "are not vendored."

    directory =
      "The directory defaults to 'vendor' in the project root and can be "
        ++ "changed with '--output-directory'. After vendoring, the command "
        ++ "prints the 'repository' stanza to add to the project file so that "
        ++ "the vendored packages are used. When package names are given, "
        ++ "only those packages are vendored."

    unpacking =
      "With '--unpack', the named packages are also unpacked into "
        ++ "'src/' under the vendor directory, with the '.cabal' file from "
        ++ "the package index applied, so that they can be worked on as "
        ++ "local packages listed in 'packages:'."

    examples pname =
      vcat
        [ text "Examples" <> colon
        , nest 2 . vcat $
            [ example [] "Vendor all dependencies of the project into ./vendor"
            , example ["--output-directory=deps"] "Vendor all dependencies of the project into ./deps"
            , example ["aeson", "text"] "Vendor only the aeson and text packages"
            , example ["--dry-run"] "Show which packages would be vendored"
            , example ["--prune"] "Vendor all dependencies and remove packages no longer needed"
            , example ["--unpack", "aeson"] "Vendor aeson and unpack it into vendor/src/ to work on it"
            ]
        ]
      where
        example args effect = vcat [hsep $ text <$> pname : "v2-vendor" : args, nest 2 $ text effect]

-------------------------------------------------------------------------------
-- Flags
-------------------------------------------------------------------------------

data VendorFlags = VendorFlags
  { vendorOutputDir :: Flag FilePath
  , vendorPrune :: Flag Bool
  , vendorUnpack :: Flag Bool
  }
  deriving (Eq, Show)

defaultVendorFlags :: VendorFlags
defaultVendorFlags = VendorFlags mempty mempty mempty

vendorOptions :: ShowOrParseArgs -> [OptionField VendorFlags]
vendorOptions _ =
  [ option
      ['o']
      ["output-directory", "outputdir"]
      "The directory to vendor the packages into (default: 'vendor' in the project root)"
      vendorOutputDir
      (\v flags -> flags{vendorOutputDir = v})
      (reqArg "PATH" (succeedReadE Flag) flagToList)
  , option
      []
      ["prune"]
      "Remove package files that are not dependencies in the current plan from the directory"
      vendorPrune
      (\v flags -> flags{vendorPrune = v})
      trueArg
  , option
      []
      ["unpack"]
      "Also unpack the named packages into 'src/' under the directory, to work on them as local packages"
      vendorUnpack
      (\v flags -> flags{vendorUnpack = v})
      trueArg
  ]

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

-- | Where a vendored package comes from.
data VendorSource
  = -- | A package from a package repository, with the @.cabal@ file from
    -- the repository's index, which carries any revision, if it has one.
    FromRepo Repo (Maybe CabalFileText)
  | -- | A package from a @source-repository-package@ stanza, as the source
    -- distribution tarball that cabal made from its checkout.
    FromSourceRepo SourceRepoMaybe FilePath

-- | The @.cabal@ file to go with a vendored package, if its source has one.
sourceCabalFile :: VendorSource -> Maybe CabalFileText
sourceCabalFile (FromRepo _ cabalFile) = cabalFile
sourceCabalFile FromSourceRepo{} = Nothing

-- | The name of the repository in the stanza printed for the user.
vendoredRepoName :: String
vendoredRepoName = "vendored"

-- | To a first approximation, the @vendor@ command runs the first phase of
-- the @build@ command where we bring the install plan up to date, and then
-- copies the source tarball of every dependency in the plan into a directory
-- that cabal can use as a @file+noindex@ package repository.
vendorAction :: NixStyleFlags VendorFlags -> [String] -> GlobalFlags -> IO ()
vendorAction flags@NixStyleFlags{extraFlags = VendorFlags{vendorOutputDir, vendorPrune, vendorUnpack}} pkgArgs globalFlags = do
  pkgNames <- for pkgArgs $ \arg ->
    case simpleParsec arg of
      Just pkgName -> return (pkgName :: PackageName)
      Nothing -> dieWithException verbosity (VendorInvalidPackageName arg)
  let unpack = fromFlagOrDefault False vendorUnpack
  when (unpack && null pkgNames) $ dieWithException verbosity VendorUnpackNeedsPackages

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
  vendorDir <- makeAbsolute $ fromFlagOrDefault (projectRoot </> "vendor") vendorOutputDir

  let elabPkgs = concatMap (foldPlanPackage (const []) pure) (InstallPlan.toList elaboratedPlan)
      localNames = Set.fromList [packageName elab | elab <- elabPkgs, elabLocalToProject elab]

      -- One package can appear once per component in the plan, so collect
      -- by package id.
      (sources, notVendored) = foldr classify (Map.empty, Map.empty) elabPkgs
      classify elab@(packageId -> pkgid) acc@(vendorable, skipped)
        | elabLocalToProject elab = acc
        | otherwise =
            case elabPkgSourceLocation elab of
              RepoTarballPackage repo _ _ -> vendor (FromRepo repo (elabPkgDescriptionOverride elab))
              RemoteSourceRepoPackage srp (Just tarball) -> vendor (FromSourceRepo srp tarball)
              RemoteSourceRepoPackage srp Nothing ->
                skip $ "source repository " ++ Text.unpack (srpLocation srp) ++ " has not been fetched"
              RemoteTarballPackage uri _ ->
                skip $ "remote tarball " ++ show uri ++ " is listed in the project's packages"
              LocalTarballPackage path ->
                skip $ "local tarball " ++ path ++ " is listed in the project's packages"
              LocalUnpackedPackage path ->
                skip $ "local directory " ++ path ++ " is listed in the project's packages"
        where
          vendor source = (Map.insert pkgid source vendorable, skipped)
          skip why = (vendorable, Map.insert pkgid why skipped)

      -- Restrict to the requested package names, if any.
      wanted = Set.fromList pkgNames
      unknown = Set.toList (wanted `Set.difference` Set.fromList (map packageName elabPkgs))
      local = Set.toList (wanted `Set.intersection` localNames)
      selected
        | null pkgNames = sources
        | otherwise = Map.filterWithKey (\pkgid _ -> packageName pkgid `Set.member` wanted) sources

      dryRun = buildSettingDryRun buildSettings
      prune = fromFlagOrDefault False vendorPrune
      displayDir = displayVendorDir projectRoot vendorDir

  unless (null unknown) $ dieWithException verbosity (VendorUnknownPackages unknown)
  unless (null local) $ dieWithException verbosity (VendorLocalPackages local)

  for_ (Map.toList notVendored) $ \(prettyShow -> pkgid, why) ->
    warn verbosity $ "Not vendoring " ++ pkgid ++ ": " ++ why ++ "."

  if dryRun
    then do
      noticeDoc verbosity . listing ("Would vendor the following packages into " ++ displayDir) $
        pretty <$> Map.keys selected
      when prune $ do
        stale <- staleVendorFiles vendorDir (Map.keysSet sources)
        unless (null stale) . noticeDoc verbosity . listing "Would remove the following files not in the current plan" $
          text <$> stale
      when unpack . noticeDoc verbosity . listing ("Would unpack the following packages into " ++ displayDir ++ "/src") $
        pretty <$> Map.keys selected
    else do
      createDirectoryIfMissing True vendorDir
      projectConfigWithBuilderRepoContext verbosity buildSettings $ \repoCtxt ->
        for_ (Map.toList selected) $ uncurry (vendorPackage verbosity repoCtxt vendorDir)

      when prune $ do
        removed@(length -> count) <- pruneVendorDir verbosity vendorDir (Map.keysSet sources)
        let heading = "Removed " ++ show count ++ " " ++ plural count "file" "files" ++ " not in the current plan"
        unless (null removed) . noticeDoc verbosity . listing heading $ text <$> removed

      -- The index cache of a file+noindex repository is never invalidated by
      -- cabal itself, so drop it if there is one; it is rebuilt on first use.
      removePathForcibly $ vendorDir </> "noindex.cache"

      unpacked <-
        if unpack
          then fmap catMaybes . for (Map.toList selected) $ uncurry (unpackVendored verbosity vendorDir)
          else return []

      noticeDoc verbosity $ vendorReport displayDir (Map.size sources) (Map.toList selected) unpacked
  where
    verbosity = cfgVerbosity normal flags
    cliConfig =
      commandLineFlagsToProjectConfig
        globalFlags
        flags
        mempty -- ClientInstallFlags, not needed here

-- | Copy one package into the vendor directory: the tarball as
-- @\<pkgid\>.tar.gz@ and, for repository packages, the @.cabal@ file from
-- the index as @\<pkgid\>.cabal@ next to it. That is the layout
-- "Distribution.Client.IndexUtils" reads a @file+noindex@ repository from.
vendorPackage :: Verbosity -> RepoContext -> FilePath -> PackageId -> VendorSource -> IO ()
vendorPackage verbosity repoCtxt vendorDir pkgid@(prettyShow -> pkgName) source = do
  src <- case source of
    FromRepo repo _ -> fetchRepoTarball verbosity repoCtxt repo pkgid
    FromSourceRepo _ tarball -> return tarball
  let dest = vendorDir </> pkgName <.> "tar.gz"
  -- When the project already takes this package from the vendor directory,
  -- the source is the destination. The destination need not exist yet:
  -- canonicalizePath resolves as much of a path as is there.
  sameFile <- (==) <$> canonicalizePath src <*> canonicalizePath dest
  unless sameFile $ do
    copyFileVerbose verbosity src dest
    for_ (sourceCabalFile source) $ writeFileAtomic (vendorDir </> pkgName <.> "cabal")

-- | Unpack a vendored package into @src/@ under the vendor directory, with
-- the @.cabal@ file from the index applied as @cabal get@ does, so that it can be
-- listed in @packages:@ and worked on. A directory that is already there is
-- left alone, as it may hold edits, and then nothing is returned.
unpackVendored :: Verbosity -> FilePath -> PackageId -> VendorSource -> IO (Maybe PackageId)
unpackVendored verbosity vendorDir pkgid@(prettyShow -> pkgName) source = do
  let unpackDir = vendorDir </> "src"
      pkgDir = unpackDir </> pkgName
      tarball = vendorDir </> pkgName <.> "tar.gz"
  exists <- doesDirectoryExist pkgDir
  if exists
    then do
      warn verbosity $
        "Not unpacking " ++ pkgName ++ ": " ++ pkgDir ++ " already exists and may hold changes; remove it to unpack again."
      return Nothing
    else do
      createDirectoryIfMissing True unpackDir
      unpackPackage verbosity unpackDir pkgid (sourceCabalFile source) tarball
      return (Just pkgid)

-- | The package a file in the vendor directory belongs to, for the file
-- names that the @file+noindex@ reader understands: @\<pkgid\>.tar.gz@ and
-- the @\<pkgid\>.cabal@ sidecar. Anything else is not a package file.
vendorFilePackageId :: FilePath -> Maybe PackageId
vendorFilePackageId file = simpleParsec =<< stripExtension "tar.gz" file <|> stripExtension "cabal" file

-- | The package files in the vendor directory that belong to packages not in
-- the given set.
staleVendorFiles :: FilePath -> Set PackageId -> IO [FilePath]
staleVendorFiles vendorDir keep = do
  exists <- doesDirectoryExist vendorDir
  if not exists
    then return []
    else do
      entries <- listDirectory vendorDir
      return $
        sort
          [ file
          | file <- entries
          , Just pkgid <- [vendorFilePackageId file]
          , pkgid `Set.notMember` keep
          ]

-- | Remove the package files that belong to packages not in the given set,
-- returning what was removed.
pruneVendorDir :: Verbosity -> FilePath -> Set PackageId -> IO [FilePath]
pruneVendorDir verbosity vendorDir keep = do
  stale <- staleVendorFiles vendorDir keep
  for_ stale $ \file -> do
    info verbosity $ "Removing " ++ vendorDir </> file
    removeFile (vendorDir </> file)
  return stale

plural :: Int -> String -> String -> String
plural n singular pluralForm = if n == 1 then singular else pluralForm

-- | A line of text with an empty line above it, as a @<br>@ would leave
-- between two blocks. The empty line is a 'text', which 'vcat' keeps where
-- it would drop 'Text.PrettyPrint.empty'.
brText :: String -> Doc
brText line = text "" $+$ text line

-- | A heading, closed with a colon, over a dashed list of items.
listing :: String -> [Doc] -> Doc
listing heading items = vcat [text heading <> colon, nest 1 $ vcat [char '-' <+> item | item <- items]]

-- | The vendor directory as shown to the user: relative to the project root
-- when it lies inside the project, so that it reads the same as the @url:@
-- line of the printed stanza. A directory outside the project stays absolute.
displayVendorDir :: FilePath -> FilePath -> FilePath
displayVendorDir projectRoot vendorDir@(makeRelative projectRoot -> relativeDir)
  | isRelative relativeDir = asPosixPath relativeDir
  | otherwise = vendorDir

-- | What was vendored, and the project configuration needed to use it.
vendorReport :: FilePath -> Int -> [(PackageId, VendorSource)] -> [PackageId] -> Doc
vendorReport displayDir total selected@(length -> count) unpacked =
  vcat $
    [ summary
    , brText "To build using the vendored packages, add the following to the project file"
    , text "(the 'url' line must stay indented under 'repository'):"
    , brText "repository" <+> text vendoredRepoName
    , nest 2 $ (text "url" <> colon) <+> text vendorUrl
    , (brText "active-repositories" <> colon) <+> text activeRepos
    ]
      ++ sourceRepoNotes
      ++ unpackNotes
  where
    summary
      | partial =
          hsep [text "Vendored", int count, text "of", int total, text "dependencies into", text displayDir]
      | otherwise =
          hsep [text "Vendored", int count, text (plural count "package" "packages"), text "into", text displayDir]

    partial = count < total

    -- A directory inside the project is given relative to the project root,
    -- so that the vendored repository can be committed along with the project.
    vendorUrl
      | isRelative displayDir = "file+noindex:" ++ displayDir
      | Windows <- buildOS = "file+noindex:" ++ asPosixPath displayDir
      | otherwise = "file+noindex://" ++ displayDir

    -- With only some dependencies vendored, the other repositories have to
    -- stay active; the vendored one then overrides them for its packages.
    activeRepos
      | partial = ":rest, " ++ vendoredRepoName ++ ":override"
      | otherwise = vendoredRepoName

    sourceRepoNotes =
      case [(pkgid, srp) | (pkgid, FromSourceRepo srp _) <- selected] of
        [] -> []
        srps ->
          [ brText "The vendored repository now provides these source-repository-package"
          , text "dependencies, whose stanzas can be removed from the project file:"
          , nest 2 $ vcat [char '-' <+> pretty pkgid <+> parens (describeSourceRepo srp) | (pkgid, srp) <- srps]
          ]

    unpackNotes = case unpacked of
      [] -> []
      pkgids ->
        [ brText $ "Unpacked into " ++ displayDir ++ "/src. To work on these as local packages,"
        , text "which take precedence over the vendored ones, add to the project file:"
        , brText "packages" <> colon
        , nest 2 $ vcat [text (displayDir ++ "/src/" ++ prettyShow pkgid) | pkgid <- pkgids]
        ]

    describeSourceRepo SourceRepositoryPackage{srpType, srpLocation, srpTag, srpBranch, srpSubdir} =
      hsep . punctuate comma $
        [pretty srpType <+> text (Text.unpack srpLocation)]
          ++ [text "tag" <+> text (Text.unpack tag) | Just tag <- [srpTag]]
          ++ [text "branch" <+> text (Text.unpack branch) | Just branch <- [srpBranch]]
          ++ [text "subdir" <+> text subdir | Just subdir <- [srpSubdir]]
