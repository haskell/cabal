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
import Distribution.Client.Get (unpackPackage)
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

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import System.Directory
  ( canonicalizePath
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , listDirectory
  , makeAbsolute
  , removeFile
  )
import System.FilePath
  ( isRelative
  , makeRelative
  , (<.>)
  , (</>)
  )
import Text.PrettyPrint
import qualified Text.PrettyPrint as Pretty

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
        Just . const . render $
          vcat [intro, selection, directory, unpacking]
    , commandNotes = Just $ \pname -> render (examples pname) ++ "\n"
    , commandDefaultFlags = defaultNixStyleFlags defaultVendorFlags
    , commandOptions = nixStyleOptions vendorOptions
    }
  where
    -- A wrapped paragraph ends with a newline, so stacking paragraphs with
    -- 'vcat' leaves a blank line between them.
    paragraph = text . wrapText

    intro =
      paragraph $
        "Copy the source tarballs of the project's dependencies into a "
          ++ "directory laid out as a 'file+noindex' package repository, so "
          ++ "that the project can later be built without network access. "
          ++ "Tarballs not already in the download cache are downloaded first."

    selection =
      paragraph $
        "The dependencies are those selected by the solver for the current "
          ++ "project configuration, including build tools and setup "
          ++ "dependencies. Each is written as '<pkgid>.tar.gz', with its "
          ++ "'.cabal' file from the package index, which carries any "
          ++ "revision, next to it as '<pkgid>.cabal'. Dependencies from "
          ++ "'source-repository-package' stanzas are vendored as source "
          ++ "distributions of their checkouts. Local packages of the project "
          ++ "are not vendored."

    directory =
      paragraph $
        "The directory defaults to 'vendor' in the project root and can be "
          ++ "changed with '--output-directory'. After vendoring, the command "
          ++ "prints the 'repository' stanza to add to the project file so that "
          ++ "the vendored packages are used. When package names are given, "
          ++ "only those packages are vendored."

    unpacking =
      paragraph $
        "With '--unpack', the named packages are also unpacked into "
          ++ "'src/' under the vendor directory, with the '.cabal' file from "
          ++ "the package index applied, so that they can be worked on as "
          ++ "local packages listed in 'packages:'."

    examples pname =
      vcat
        [ text "Examples" Pretty.<> colon
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
        example args what =
          vcat
            [ hsep $ text <$> pname : "v2-vendor" : args
            , nest 2 $ text what
            ]

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
defaultVendorFlags =
  VendorFlags
    { vendorOutputDir = mempty
    , vendorPrune = mempty
    , vendorUnpack = mempty
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
vendorAction flags@NixStyleFlags{extraFlags = VendorFlags{vendorOutputDir, vendorPrune, vendorUnpack}} pkgArgs globalFlags = do
  pkgNames <- for pkgArgs $ \arg ->
    case simpleParsec arg of
      Just name -> return (name :: PackageName)
      Nothing -> dieWithException verbosity (VendorInvalidPackageName arg)
  let unpack = fromFlagOrDefault False vendorUnpack
  when (unpack && null pkgNames) $
    dieWithException verbosity VendorUnpackNeedsPackages

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
      prune = fromFlagOrDefault False vendorPrune
      unpackDir = vendorDir </> "src"

  unless (null unknown) $
    dieWithException verbosity (VendorUnknownPackages unknown)
  unless (null local) $
    dieWithException verbosity (VendorLocalPackages local)

  for_ notVendored $ \(pkgid, why) ->
    warn verbosity $ "Not vendoring " ++ prettyShow pkgid ++ ": " ++ why ++ "."

  if dryRun
    then do
      noticeDoc verbosity . listing ("Would vendor the following packages into " ++ displayVendorDir projectRoot vendorDir) $
        pretty <$> Map.keys selected
      when prune $ do
        stale <- staleVendorFiles vendorDir (Map.keysSet sources)
        unless (null stale) $
          noticeDoc verbosity . listing "Would remove the following files not in the current plan" $
            text <$> stale
      when unpack $
        noticeDoc verbosity . listing ("Would unpack the following packages into " ++ unpackDir) $
          pretty <$> Map.keys selected
    else do
      createDirectoryIfMissing True vendorDir
      projectConfigWithBuilderRepoContext verbosity buildSettings $ \repoCtxt ->
        for_ (Map.toList selected) $ uncurry (vendorPackage verbosity repoCtxt vendorDir)

      when prune $ do
        removed <- pruneVendorDir verbosity vendorDir (Map.keysSet sources)
        unless (null removed) $
          noticeDoc verbosity $
            listing ("Removed " ++ show (length removed) ++ " " ++ plural (length removed) "file" "files" ++ " not in the current plan") $
              text <$> removed

      -- The index cache of a file+noindex repository is never invalidated by
      -- cabal itself, so drop it; it is rebuilt on first use.
      let cacheFile = vendorDir </> "noindex.cache"
      cacheExists <- doesFileExist cacheFile
      when cacheExists $ removeFile cacheFile

      unpacked <-
        if unpack
          then fmap concat . for (Map.toList selected) $ uncurry (unpackVendored verbosity vendorDir)
          else return []

      noticeDoc verbosity $
        vendorReport projectRoot vendorDir partial (Map.size sources) (Map.toList selected) unpacked
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
  unless sameFile $ do
    copyFileVerbose verbosity src dest
    case source of
      FromRepo _ (Just cabalFile) ->
        writeFileAtomic (vendorDir </> prettyShow pkgid <.> "cabal") cabalFile
      _ -> return ()

-- | Unpack a vendored package into @src/@ under the vendor directory, with
-- the @.cabal@ file from the index applied as @cabal get@ does, so that it can be
-- listed in @packages:@ and worked on. A directory that is already there is
-- left alone: it may hold edits.
unpackVendored :: Verbosity -> FilePath -> PackageId -> VendorSource -> IO [PackageId]
unpackVendored verbosity vendorDir pkgid source = do
  let unpackDir = vendorDir </> "src"
      pkgDir = unpackDir </> prettyShow pkgid
      tarball = vendorDir </> prettyShow pkgid <.> "tar.gz"
      override = case source of
        FromRepo _ cabalFile -> cabalFile
        FromSourceRepo _ _ -> Nothing
  exists <- doesDirectoryExist pkgDir
  if exists
    then do
      warn verbosity $
        "Not unpacking " ++ prettyShow pkgid ++ ": " ++ pkgDir ++ " already exists and may hold changes; remove it to unpack again."
      return []
    else do
      createDirectoryIfMissing True unpackDir
      unpackPackage verbosity unpackDir pkgid override tarball
      return [pkgid]

-- | The package a file in the vendor directory belongs to, for the file
-- names that the @file+noindex@ reader understands: @\<pkgid\>.tar.gz@ and
-- the @\<pkgid\>.cabal@ sidecar. Anything else is not a package file.
vendorFilePackageId :: FilePath -> Maybe PackageId
vendorFilePackageId file =
  listToMaybe
    [ pkgid
    | suffix <- [".tar.gz", ".cabal"]
    , suffix `isSuffixOf` file
    , Just pkgid <- [simpleParsec (take (length file - length suffix) file)]
    ]

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
-- it would drop 'Pretty.empty'.
brText :: String -> Doc
brText line = text "" $+$ text line

-- | A heading, closed with a colon, over a dashed list of items.
listing :: String -> [Doc] -> Doc
listing heading items = vcat [text heading Pretty.<> colon, nest 1 $ vcat [char '-' <+> item | item <- items]]

-- | The vendor directory as shown to the user: relative to the project root
-- when it lies inside the project, so that it reads the same as the @url:@
-- line of the printed stanza. A directory outside the project stays absolute.
displayVendorDir :: FilePath -> FilePath -> FilePath
displayVendorDir projectRoot vendorDir
  | isRelative relativeDir = asPosixPath relativeDir
  | otherwise = vendorDir
  where
    relativeDir = makeRelative projectRoot vendorDir

-- | What was vendored, and the project configuration needed to use it.
vendorReport :: FilePath -> FilePath -> Bool -> Int -> [(PackageId, VendorSource)] -> [PackageId] -> Doc
vendorReport projectRoot vendorDir partial total selected unpacked =
  vcat $
    [ summary
    , brText "To build using the vendored packages, add the following to the project file"
    , text "(the 'url' line must stay indented under 'repository'):"
    , brText "repository" <+> text vendoredRepoName
    , nest 2 $ text "url" Pretty.<> colon <+> text vendorUrl
    , brText "active-repositories" Pretty.<> colon <+> text activeRepos
    ]
      ++ sourceRepoNotes
      ++ unpackNotes
  where
    summary
      | partial =
          hsep [text "Vendored", int (length selected), text "of", int total, text "dependencies into", text displayDir]
      | otherwise =
          hsep [text "Vendored", int (length selected), text (plural (length selected) "package" "packages"), text "into", text displayDir]

    -- Prefer a path relative to the project root, so that the vendored
    -- repository can be committed along with the project.
    relativeDir = makeRelative projectRoot vendorDir
    displayDir = displayVendorDir projectRoot vendorDir
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
          [ brText "The vendored repository now provides these source-repository-package"
          , text "dependencies, whose stanzas can be removed from the project file:"
          , nest 2 $ vcat [char '-' <+> pretty pkgid <+> parens (describeSourceRepo srp) | (pkgid, srp) <- srps]
          ]

    unpackNotes = case unpacked of
      [] -> []
      pkgids ->
        [ brText $ "Unpacked into " ++ displayDir ++ "/src. To work on these as local packages,"
        , text "which take precedence over the vendored ones, add to the project file:"
        , brText "packages" Pretty.<> colon
        , nest 2 $ vcat [text (displayDir ++ "/src/" ++ prettyShow pkgid) | pkgid <- pkgids]
        ]

    describeSourceRepo SourceRepositoryPackage{srpType, srpLocation, srpTag, srpBranch, srpSubdir} =
      hsep . punctuate comma $
        [pretty srpType <+> text (Text.unpack srpLocation)]
          ++ [text "tag" <+> text (Text.unpack tag) | Just tag <- [srpTag]]
          ++ [text "branch" <+> text (Text.unpack branch) | Just branch <- [srpBranch]]
          ++ [text "subdir" <+> text subdir | Just subdir <- [srpSubdir]]
