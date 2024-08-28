{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

module Distribution.Client.CmdSdist
  ( sdistCommand
  , sdistAction
  , packageToSdist
  , OutputFormat (..)
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.CmdErrorMessages
  ( Plural (..)
  , renderComponentKind
  )
import Distribution.Client.DistDirLayout
  ( DistDirLayout (..)
  , ProjectRoot (..)
  )
import Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , defaultNixStyleFlags
  , updNixStyleCommonSetupFlags
  )
import Distribution.Client.ProjectConfig
  ( ProjectConfig
  , commandLineFlagsToProjectConfig
  , projectConfigConfigFile
  , projectConfigShared
  , withGlobalConfig
  , withProjectOrGlobalConfig
  )
import Distribution.Client.ProjectFlags
  ( ProjectFlags (..)
  , defaultProjectFlags
  , projectFlagsOptions
  )
import Distribution.Client.ProjectOrchestration
  ( CurrentCommand (..)
  , ProjectBaseContext (..)
  , establishProjectBaseContext
  , establishProjectBaseContextWithRoot
  )
import Distribution.Client.Setup
  ( CommonSetupFlags (..)
  , GlobalFlags (..)
  )
import Distribution.Client.TargetSelector
  ( ComponentKind
  , TargetSelector (..)
  , readTargetSelectors
  , reportTargetSelectorProblems
  )
import Distribution.Client.Types
  ( PackageLocation (..)
  , PackageSpecifier (..)
  , UnresolvedSourcePackage
  )
import Distribution.Solver.Types.SourcePackage
  ( SourcePackage (..)
  )
import Distribution.Utils.Path hiding
  ( (<.>)
  , (</>)
  )

import Distribution.Client.Errors
import Distribution.Client.SrcDist
  ( packageDirToSdist
  )
import Distribution.Compat.Lens
  ( _1
  , _2
  )
import Distribution.Package
  ( Package (packageId)
  )
import Distribution.PackageDescription.Configuration
  ( flattenPackageDescription
  )
import Distribution.ReadE
  ( succeedReadE
  )
import Distribution.Simple.Command
  ( CommandUI (..)
  , OptionField
  , ShowOrParseArgs
  , liftOptionL
  , option
  , reqArg
  )
import Distribution.Simple.PreProcess
  ( knownSuffixHandlers
  )
import Distribution.Simple.Setup
  ( Flag (..)
  , flagToList
  , flagToMaybe
  , fromFlagOrDefault
  , optionDistPref
  , optionVerbosity
  , toFlag
  , trueArg
  )
import Distribution.Simple.SrcDist
  ( listPackageSourcesWithDie
  )
import Distribution.Simple.Utils
  ( dieWithException
  , notice
  , withOutputMarker
  , wrapText
  )
import Distribution.Types.ComponentName
  ( ComponentName
  , showComponentName
  )
import Distribution.Types.GenericPackageDescription (GenericPackageDescription)
import Distribution.Types.PackageName
  ( PackageName
  , unPackageName
  )
import Distribution.Verbosity
  ( normal
  )

import qualified Data.ByteString.Lazy.Char8 as BSL
import System.Directory
  ( createDirectoryIfMissing
  , getCurrentDirectory
  , makeAbsolute
  )
import System.FilePath
  ( makeRelative
  , normalise
  , (<.>)
  , (</>)
  )

-------------------------------------------------------------------------------
-- Command
-------------------------------------------------------------------------------

sdistCommand :: CommandUI (ProjectFlags, SdistFlags)
sdistCommand =
  CommandUI
    { commandName = "v2-sdist"
    , commandSynopsis = "Generate a source distribution file (.tar.gz)."
    , commandUsage = \pname ->
        "Usage: " ++ pname ++ " v2-sdist [FLAGS] [PACKAGES]\n"
    , commandDescription = Just $ \_ ->
        wrapText
          "Generates tarballs of project packages suitable for upload to Hackage."
    , commandNotes = Nothing
    , commandDefaultFlags = (defaultProjectFlags, defaultSdistFlags)
    , commandOptions = \showOrParseArgs ->
        map (liftOptionL _1) (projectFlagsOptions showOrParseArgs)
          ++ map (liftOptionL _2) (sdistOptions showOrParseArgs)
    }

-------------------------------------------------------------------------------
-- Flags
-------------------------------------------------------------------------------

data SdistFlags = SdistFlags
  { sdistVerbosity :: Flag Verbosity
  , sdistDistDir :: Flag (SymbolicPath Pkg (Dir Dist))
  , sdistListSources :: Flag Bool
  , sdistNulSeparated :: Flag Bool
  , sdistOutputPath :: Flag FilePath
  }

defaultSdistFlags :: SdistFlags
defaultSdistFlags =
  SdistFlags
    { sdistVerbosity = toFlag normal
    , sdistDistDir = mempty
    , sdistListSources = toFlag False
    , sdistNulSeparated = toFlag False
    , sdistOutputPath = mempty
    }

sdistOptions :: ShowOrParseArgs -> [OptionField SdistFlags]
sdistOptions showOrParseArgs =
  [ optionVerbosity
      sdistVerbosity
      (\v flags -> flags{sdistVerbosity = v})
  , optionDistPref
      sdistDistDir
      (\dd flags -> flags{sdistDistDir = dd})
      showOrParseArgs
  , option
      ['l']
      ["list-only"]
      "Just list the sources, do not make a tarball"
      sdistListSources
      (\v flags -> flags{sdistListSources = v})
      trueArg
  , option
      []
      ["null-sep"]
      "Separate the source files with NUL bytes rather than newlines."
      sdistNulSeparated
      (\v flags -> flags{sdistNulSeparated = v})
      trueArg
  , option
      ['o']
      ["output-directory", "outputdir"]
      "Choose the output directory of this command. '-' sends all output to stdout"
      sdistOutputPath
      (\o flags -> flags{sdistOutputPath = o})
      (reqArg "PATH" (succeedReadE Flag) flagToList)
  ]

-------------------------------------------------------------------------------
-- Action
-------------------------------------------------------------------------------

sdistAction :: (ProjectFlags, SdistFlags) -> [String] -> GlobalFlags -> IO ()
sdistAction (pf@ProjectFlags{..}, SdistFlags{..}) targetStrings globalFlags = do
  (baseCtx, distDirLayout) <-
    withProjectOrGlobalConfig
      flagIgnoreProject
      withProject
      (withGlobalConfig verbosity globalConfigFlag withoutProject)

  let localPkgs = localPackages baseCtx

  targetSelectors <-
    either (reportTargetSelectorProblems verbosity) return
      =<< readTargetSelectors localPkgs Nothing targetStrings

  -- elaborate path, create target directory
  mOutputPath' <- case mOutputPath of
    Just "-" -> return (Just "-")
    Just path -> do
      abspath <- makeAbsolute path
      createDirectoryIfMissing True abspath
      return (Just abspath)
    Nothing -> do
      createDirectoryIfMissing True (distSdistDirectory distDirLayout)
      return Nothing

  let format :: OutputFormat
      format =
        if
            | listSources, nulSeparated -> SourceList '\0'
            | listSources -> SourceList '\n'
            | otherwise -> TarGzArchive

      ext = case format of
        SourceList _ -> "list"
        TarGzArchive -> "tar.gz"

      outputPath pkg = case mOutputPath' of
        Just path
          | path == "-" -> "-"
          | otherwise -> path </> prettyShow (packageId pkg) <.> ext
        Nothing
          | listSources -> "-"
          | otherwise -> distSdistFile distDirLayout (packageId pkg)

  case reifyTargetSelectors localPkgs targetSelectors of
    Left errs -> dieWithException verbosity $ SdistActionException . fmap renderTargetProblem $ errs
    Right pkgs
      | length pkgs > 1
      , not listSources
      , Just "-" <- mOutputPath' ->
          dieWithException verbosity Can'tWriteMultipleTarballs
      | otherwise ->
          for_ pkgs $ \pkg ->
            packageToSdist
              verbosity
              (distProjectRootDirectory distDirLayout)
              format
              (outputPath pkg)
              pkg
  where
    verbosity = fromFlagOrDefault normal sdistVerbosity
    listSources = fromFlagOrDefault False sdistListSources
    nulSeparated = fromFlagOrDefault False sdistNulSeparated
    mOutputPath = flagToMaybe sdistOutputPath

    prjConfig :: ProjectConfig
    prjConfig =
      commandLineFlagsToProjectConfig
        globalFlags
        (updNixStyleCommonSetupFlags (const commonFlags) $ defaultNixStyleFlags ())
          { projectFlags = pf
          }
        mempty
    commonFlags =
      mempty
        { setupVerbosity = sdistVerbosity
        , setupDistPref = sdistDistDir
        }

    globalConfigFlag = projectConfigConfigFile (projectConfigShared prjConfig)

    withProject :: IO (ProjectBaseContext, DistDirLayout)
    withProject = do
      baseCtx <- establishProjectBaseContext verbosity prjConfig OtherCommand
      return (baseCtx, distDirLayout baseCtx)

    withoutProject :: ProjectConfig -> IO (ProjectBaseContext, DistDirLayout)
    withoutProject config = do
      cwd <- getCurrentDirectory
      baseCtx <- establishProjectBaseContextWithRoot verbosity (config <> prjConfig) (ProjectRootImplicit cwd) OtherCommand
      return (baseCtx, distDirLayout baseCtx)

data OutputFormat
  = SourceList Char
  | TarGzArchive
  deriving (Show, Eq)

packageToSdist :: Verbosity -> FilePath -> OutputFormat -> FilePath -> UnresolvedSourcePackage -> IO ()
packageToSdist verbosity projectRootDir format outputFile pkg = do
  let death = dieWithException verbosity $ ImpossibleHappened (show pkg)
  dir0 <- case srcpkgSource pkg of
    LocalUnpackedPackage path -> pure (Right path)
    RemoteSourceRepoPackage _ (Just tgz) -> pure (Left tgz)
    RemoteSourceRepoPackage{} -> death
    LocalTarballPackage tgz -> pure (Left tgz)
    RemoteTarballPackage _ (Just tgz) -> pure (Left tgz)
    RemoteTarballPackage{} -> death
    RepoTarballPackage{} -> death

  let
    -- Write String to stdout or file, using the default TextEncoding.
    write str
      | outputFile == "-" = putStr (withOutputMarker verbosity str)
      | otherwise = do
          writeFile outputFile str
          notice verbosity $ "Wrote source list to " ++ outputFile ++ "\n"
    -- Write raw ByteString to stdout or file as it is, without encoding.
    writeLBS lbs
      | outputFile == "-" = BSL.putStr lbs
      | otherwise = do
          BSL.writeFile outputFile lbs
          notice verbosity $ "Wrote tarball sdist to " ++ outputFile ++ "\n"

  case dir0 of
    Left tgz -> do
      case format of
        TarGzArchive -> do
          writeLBS =<< BSL.readFile tgz
        _ -> dieWithException verbosity $ CannotConvertTarballPackage (show format)
    Right dir -> do
      case format of
        SourceList nulSep -> do
          let gpd :: GenericPackageDescription
              gpd = srcpkgDescription pkg

          files' <- listPackageSourcesWithDie verbosity dieWithException (Just $ makeSymbolicPath dir) (flattenPackageDescription gpd) knownSuffixHandlers
          let files = nub $ sort $ map (normalise . getSymbolicPath) files'
          let prefix = makeRelative (normalise projectRootDir) dir
          write $ concat [prefix </> i ++ [nulSep] | i <- files]
        TarGzArchive -> do
          packageDirToSdist verbosity (srcpkgDescription pkg) dir >>= writeLBS

--

reifyTargetSelectors :: [PackageSpecifier UnresolvedSourcePackage] -> [TargetSelector] -> Either [TargetProblem] [UnresolvedSourcePackage]
reifyTargetSelectors pkgs sels =
  case partitionEithers (foldMap go sels) of
    ([], sels') -> Right sels'
    (errs, _) -> Left errs
  where
    -- there can be pkgs which are in extra-packages:
    -- these are not SpecificSourcePackage
    --
    -- Why these packages are in localPkgs, it's confusing.
    -- Anyhow, better to be lenient here.
    --
    flatten (SpecificSourcePackage pkg@SourcePackage{}) = Just pkg
    flatten _ = Nothing

    pkgs' = mapMaybe flatten pkgs

    getPkg pid = case find ((== pid) . packageId) pkgs' of
      Just pkg -> Right pkg
      Nothing -> error "The impossible happened: we have a reference to a local package that isn't in localPackages."

    go :: TargetSelector -> [Either TargetProblem UnresolvedSourcePackage]
    go (TargetPackage _ pids Nothing) = fmap getPkg pids
    go (TargetAllPackages Nothing) = Right <$> pkgs'
    go (TargetPackage _ _ (Just kind)) = [Left (AllComponentsOnly kind)]
    go (TargetAllPackages (Just kind)) = [Left (AllComponentsOnly kind)]
    go (TargetPackageNamed pname _) = [Left (NonlocalPackageNotAllowed pname)]
    go (TargetComponentUnknown pname _ _) = [Left (NonlocalPackageNotAllowed pname)]
    go (TargetComponent _ cname _) = [Left (ComponentsNotAllowed cname)]

data TargetProblem
  = AllComponentsOnly ComponentKind
  | NonlocalPackageNotAllowed PackageName
  | ComponentsNotAllowed ComponentName

renderTargetProblem :: TargetProblem -> String
renderTargetProblem (AllComponentsOnly kind) =
  "It is not possible to package only the "
    ++ renderComponentKind Plural kind
    ++ " from a package "
    ++ "for distribution. Only entire packages may be packaged for distribution."
renderTargetProblem (ComponentsNotAllowed cname) =
  "The component "
    ++ showComponentName cname
    ++ " cannot be packaged for distribution on its own. "
    ++ "Only entire packages may be packaged for distribution."
renderTargetProblem (NonlocalPackageNotAllowed pname) =
  "The package "
    ++ unPackageName pname
    ++ " cannot be packaged for distribution, because it is not "
    ++ "local to this project."
