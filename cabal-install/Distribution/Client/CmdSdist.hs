{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE ViewPatterns      #-}
module Distribution.Client.CmdSdist
    ( sdistCommand, sdistAction, packageToSdist
    , SdistFlags(..), defaultSdistFlags
    , OutputFormat(..)) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Client.CmdErrorMessages
    ( Plural(..), renderComponentKind )
import Distribution.Client.ProjectOrchestration
    ( ProjectBaseContext(..), CurrentCommand(..), establishProjectBaseContext )
import Distribution.Client.TargetSelector
    ( TargetSelector(..), ComponentKind
    , readTargetSelectors, reportTargetSelectorProblems )
import Distribution.Client.RebuildMonad
    ( runRebuild )
import Distribution.Client.Setup
    ( GlobalFlags(..) )
import Distribution.Solver.Types.SourcePackage
    ( SourcePackage(..) )
import Distribution.Client.Types
    ( PackageSpecifier(..), PackageLocation(..), UnresolvedSourcePackage )
import Distribution.Client.DistDirLayout
    ( DistDirLayout(..), defaultDistDirLayout )
import Distribution.Client.ProjectConfig
    ( findProjectRoot, readProjectConfig )

import Distribution.Package
    ( Package(packageId) )
import Distribution.PackageDescription.Configuration
    ( flattenPackageDescription )
import Distribution.Pretty
    ( prettyShow )
import Distribution.ReadE
    ( succeedReadE )
import Distribution.Simple.Command
    ( CommandUI(..), option, reqArg )
import Distribution.Simple.PreProcess
    ( knownSuffixHandlers )
import Distribution.Simple.Setup
    ( Flag(..), toFlag, fromFlagOrDefault, flagToList, flagToMaybe
    , optionVerbosity, optionDistPref, trueArg
    )
import Distribution.Simple.SrcDist
    ( listPackageSources )
import Distribution.Simple.Utils
    ( die', notice, withOutputMarker, wrapText )
import Distribution.Types.ComponentName
    ( ComponentName, showComponentName )
import Distribution.Types.PackageName
    ( PackageName, unPackageName )
import Distribution.Verbosity
    ( Verbosity, normal )

import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Compression.GZip  as GZip
import Control.Exception
    ( throwIO )
import Control.Monad.Trans
    ( liftIO )
import Control.Monad.State.Lazy
    ( StateT, modify, gets, evalStateT )
import Control.Monad.Writer.Lazy
    ( WriterT, tell, execWriterT )
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Either
    ( partitionEithers )
import Data.List
    ( sortOn )
import qualified Data.Set as Set
import System.Directory
    ( getCurrentDirectory, setCurrentDirectory
    , createDirectoryIfMissing, makeAbsolute )
import System.FilePath
    ( (</>), (<.>), makeRelative, normalise, takeDirectory )

sdistCommand :: CommandUI SdistFlags
sdistCommand = CommandUI
    { commandName = "v2-sdist"
    , commandSynopsis = "Generate a source distribution file (.tar.gz)."
    , commandUsage = \pname ->
        "Usage: " ++ pname ++ " v2-sdist [FLAGS] [PACKAGES]\n"
    , commandDescription  = Just $ \_ -> wrapText
        "Generates tarballs of project packages suitable for upload to Hackage."
    , commandNotes = Nothing
    , commandDefaultFlags = defaultSdistFlags
    , commandOptions = \showOrParseArgs ->
        [ optionVerbosity
            sdistVerbosity (\v flags -> flags { sdistVerbosity = v })
        , optionDistPref
            sdistDistDir (\dd flags -> flags { sdistDistDir = dd })
            showOrParseArgs
        , option [] ["project-file"]
            "Set the name of the cabal.project file to search for in parent directories"
            sdistProjectFile (\pf flags -> flags { sdistProjectFile = pf })
            (reqArg "FILE" (succeedReadE Flag) flagToList)
        , option ['l'] ["list-only"]
            "Just list the sources, do not make a tarball"
            sdistListSources (\v flags -> flags { sdistListSources = v })
            trueArg
        , option ['z'] ["null-sep"]
            "Separate the source files with NUL bytes rather than newlines."
            sdistNulSeparated (\v flags -> flags { sdistNulSeparated = v })
            trueArg
        , option ['o'] ["output-dir", "outputdir"]
            "Choose the output directory of this command. '-' sends all output to stdout"
            sdistOutputPath (\o flags -> flags { sdistOutputPath = o })
            (reqArg "PATH" (succeedReadE Flag) flagToList)
        ]
    }

data SdistFlags = SdistFlags
    { sdistVerbosity     :: Flag Verbosity
    , sdistDistDir       :: Flag FilePath
    , sdistProjectFile   :: Flag FilePath
    , sdistListSources   :: Flag Bool
    , sdistNulSeparated  :: Flag Bool
    , sdistOutputPath    :: Flag FilePath
    }

defaultSdistFlags :: SdistFlags
defaultSdistFlags = SdistFlags
    { sdistVerbosity     = toFlag normal
    , sdistDistDir       = mempty
    , sdistProjectFile   = mempty
    , sdistListSources   = toFlag False
    , sdistNulSeparated  = toFlag False
    , sdistOutputPath    = mempty
    }

--

sdistAction :: SdistFlags -> [String] -> GlobalFlags -> IO ()
sdistAction SdistFlags{..} targetStrings globalFlags = do
    let verbosity = fromFlagOrDefault normal sdistVerbosity
        mDistDirectory = flagToMaybe sdistDistDir
        mProjectFile = flagToMaybe sdistProjectFile
        globalConfig = globalConfigFile globalFlags
        listSources = fromFlagOrDefault False sdistListSources
        nulSeparated = fromFlagOrDefault False sdistNulSeparated
        mOutputPath = flagToMaybe sdistOutputPath

    projectRoot <- either throwIO return =<< findProjectRoot Nothing mProjectFile
    let distLayout = defaultDistDirLayout projectRoot mDistDirectory
    dir <- getCurrentDirectory
    projectConfig <- runRebuild dir $ readProjectConfig verbosity globalConfig distLayout
    baseCtx <- establishProjectBaseContext verbosity projectConfig OtherCommand
    let localPkgs = localPackages baseCtx

    targetSelectors <- either (reportTargetSelectorProblems verbosity) return
        =<< readTargetSelectors localPkgs Nothing targetStrings

    mOutputPath' <- case mOutputPath of
        Just "-"  -> return (Just "-")
        Just path -> Just <$> makeAbsolute path
        Nothing   -> return Nothing

    let
        format =
            if | listSources, nulSeparated -> SourceList '\0'
               | listSources               -> SourceList '\n'
               | otherwise                 -> TarGzArchive

        ext = case format of
                SourceList _  -> "list"
                TarGzArchive  -> "tar.gz"

        outputPath pkg = case mOutputPath' of
            Just path
                | path == "-" -> "-"
                | otherwise   -> path </> prettyShow (packageId pkg) <.> ext
            Nothing
                | listSources -> "-"
                | otherwise   -> distSdistFile distLayout (packageId pkg)

    createDirectoryIfMissing True (distSdistDirectory distLayout)

    case reifyTargetSelectors localPkgs targetSelectors of
        Left errs -> die' verbosity . unlines . fmap renderTargetProblem $ errs
        Right pkgs
            | length pkgs > 1, not listSources, Just "-" <- mOutputPath' ->
                die' verbosity "Can't write multiple tarballs to standard output!"
            | otherwise ->
                traverse_ (\pkg -> packageToSdist verbosity (distProjectRootDirectory distLayout) format (outputPath pkg) pkg) pkgs

data IsExec = Exec | NoExec
            deriving (Show, Eq)

data OutputFormat = SourceList Char
                  | TarGzArchive
                  deriving (Show, Eq)

packageToSdist :: Verbosity -> FilePath -> OutputFormat -> FilePath -> UnresolvedSourcePackage -> IO ()
packageToSdist verbosity projectRootDir format outputFile pkg = do
    let death = die' verbosity ("The impossible happened: a local package isn't local" <> (show pkg))
    dir0 <- case packageSource pkg of
             LocalUnpackedPackage path             -> pure (Right path)
             RemoteSourceRepoPackage _ (Just path) -> pure (Right path)
             RemoteSourceRepoPackage {}            -> death
             LocalTarballPackage tgz               -> pure (Left tgz)
             RemoteTarballPackage _ (Just tgz)     -> pure (Left tgz)
             RemoteTarballPackage {}               -> death
             RepoTarballPackage {}                 -> death

    let -- Write String to stdout or file, using the default TextEncoding.
        write
          | outputFile == "-" = putStr . withOutputMarker verbosity
          | otherwise = writeFile outputFile
        -- Write raw ByteString to stdout or file as it is, without encoding.
        writeLBS
          | outputFile == "-" = BSL.putStr
          | otherwise = BSL.writeFile outputFile

    case dir0 of
      Left tgz -> do
        case format of
          TarGzArchive -> do
            writeLBS =<< BSL.readFile tgz
            when (outputFile /= "-") $
              notice verbosity $ "Wrote tarball sdist to " ++ outputFile ++ "\n"
          _ -> die' verbosity ("cannot convert tarball package to " ++ show format)

      Right dir -> do
        oldPwd <- getCurrentDirectory
        setCurrentDirectory dir

        let norm flag = fmap ((flag, ) . normalise)
        (norm NoExec -> nonexec, norm Exec -> exec) <-
           listPackageSources verbosity (flattenPackageDescription $ packageDescription pkg) knownSuffixHandlers

        let files =  nub . sortOn snd $ nonexec ++ exec

        case format of
            SourceList nulSep -> do
                let prefix = makeRelative projectRootDir dir
                write $ concat [prefix </> i ++ [nulSep] | (_, i) <- files]
                when (outputFile /= "-") $
                    notice verbosity $ "Wrote source list to " ++ outputFile ++ "\n"
            TarGzArchive -> do
                let entriesM :: StateT (Set.Set FilePath) (WriterT [Tar.Entry] IO) ()
                    entriesM = do
                        let prefix = prettyShow (packageId pkg)
                        modify (Set.insert prefix)
                        case Tar.toTarPath True prefix of
                            Left err -> liftIO $ die' verbosity ("Error packing sdist: " ++ err)
                            Right path -> tell [Tar.directoryEntry path]

                        for_ files $ \(perm, file) -> do
                            let fileDir = takeDirectory (prefix </> file)
                                perm' = case perm of
                                    Exec -> Tar.executableFilePermissions
                                    NoExec -> Tar.ordinaryFilePermissions
                            needsEntry <- gets (Set.notMember fileDir)

                            when needsEntry $ do
                                modify (Set.insert fileDir)
                                case Tar.toTarPath True fileDir of
                                    Left err -> liftIO $ die' verbosity ("Error packing sdist: " ++ err)
                                    Right path -> tell [Tar.directoryEntry path]

                            contents <- liftIO . fmap BSL.fromStrict . BS.readFile $ file
                            case Tar.toTarPath False (prefix </> file) of
                                Left err -> liftIO $ die' verbosity ("Error packing sdist: " ++ err)
                                Right path -> tell [(Tar.fileEntry path contents) { Tar.entryPermissions = perm' }]

                entries <- execWriterT (evalStateT entriesM mempty)
                let -- Pretend our GZip file is made on Unix.
                    normalize bs = BSL.concat [pfx, "\x03", rest']
                        where
                            (pfx, rest) = BSL.splitAt 9 bs
                            rest' = BSL.tail rest
                    -- The Unix epoch, which is the default value, is
                    -- unsuitable because it causes unpacking problems on
                    -- Windows; we need a post-1980 date. One gigasecond
                    -- after the epoch is during 2001-09-09, so that does
                    -- nicely. See #5596.
                    setModTime entry = entry { Tar.entryTime = 1000000000 }
                writeLBS . normalize . GZip.compress . Tar.write $ fmap setModTime entries
                when (outputFile /= "-") $
                    notice verbosity $ "Wrote tarball sdist to " ++ outputFile ++ "\n"

        setCurrentDirectory oldPwd

--

reifyTargetSelectors :: [PackageSpecifier UnresolvedSourcePackage] -> [TargetSelector] -> Either [TargetProblem] [UnresolvedSourcePackage]
reifyTargetSelectors pkgs sels =
    case partitionEithers (foldMap go sels) of
        ([], sels') -> Right sels'
        (errs, _)   -> Left errs
    where
        flatten (SpecificSourcePackage pkg@SourcePackage{}) = pkg
        flatten _ = error "The impossible happened: how do we not know about a local package?"
        pkgs' = fmap flatten pkgs

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

data TargetProblem = AllComponentsOnly ComponentKind
                   | NonlocalPackageNotAllowed PackageName
                   | ComponentsNotAllowed ComponentName

renderTargetProblem :: TargetProblem -> String
renderTargetProblem (AllComponentsOnly kind) =
    "It is not possible to package only the " ++ renderComponentKind Plural kind ++ " from a package "
    ++ "for distribution. Only entire packages may be packaged for distribution."
renderTargetProblem (ComponentsNotAllowed cname) =
    "The component " ++ showComponentName cname ++ " cannot be packaged for distribution on its own. "
    ++ "Only entire packages may be packaged for distribution."
renderTargetProblem (NonlocalPackageNotAllowed pname) =
    "The package " ++ unPackageName pname ++ " cannot be packaged for distribution, because it is not "
    ++ "local to this project."
