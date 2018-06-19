{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
module Distribution.Client.CmdSdist ( sdistCommand, sdistAction, packageToSdist ) where

import Distribution.Client.CmdErrorMessages
    ( Plural(..), renderComponentKind )
import Distribution.Client.ProjectOrchestration
    ( ProjectBaseContext(..), establishProjectBaseContext )
import Distribution.Client.TargetSelector
    ( TargetSelector(..), ComponentKind
    , readTargetSelectors, reportTargetSelectorProblems )
import Distribution.Client.RebuildMonad
    ( runRebuild )
import Distribution.Client.Setup
    ( ArchiveFormat(..), GlobalFlags(..) )
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
    ( CommandUI(..), option, choiceOpt, reqArg )
import Distribution.Simple.PreProcess
    ( knownSuffixHandlers )
import Distribution.Simple.Setup
    ( Flag(..), toFlag, fromFlagOrDefault, flagToList, flagToMaybe
    , optionVerbosity, optionDistPref, trueArg
    )
import Distribution.Simple.SrcDist
    ( listPackageSources )
import Distribution.Simple.Utils
    ( die', notice )
import Distribution.Types.ComponentName
    ( ComponentName, showComponentName )
import Distribution.Types.PackageName
    ( PackageName, unPackageName )
import Distribution.Verbosity
    ( Verbosity, normal )

import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import qualified Codec.Archive.Zip       as Zip
import qualified Codec.Compression.GZip  as GZip
import Control.Exception
    ( throwIO )
import Control.Monad
    ( when, forM, forM_ )
import Control.Monad.IO.Class
    ( liftIO )
import Control.Monad.State.Lazy
    ( StateT, modify, gets, evalStateT )
import Control.Monad.Writer.Lazy
    ( WriterT, tell, execWriterT )
import Data.Bits
    ( shiftL )
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Either
    ( partitionEithers )
import Data.List
    ( find, sortBy, nub )
import System.Directory
    ( getCurrentDirectory, setCurrentDirectory
    , createDirectoryIfMissing )
import System.FilePath
    ( (</>), (<.>), normalise, takeDirectory )

sdistCommand :: CommandUI SdistFlags
sdistCommand = CommandUI
    { commandName = "new-sdist"
    , commandSynopsis = "Generate a source distribution file (.tar.gz)."
    , commandUsage = \pname ->
        "Usage: " ++ pname ++ " new-sdist [FLAGS]\n"
    , commandDescription  = Just $ \_ ->
        "Generates a tarball of a package suitable for upload to Hackage."
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
        , option [] ["list-only"]
            "Just list the sources, do not make a tarball"
            sdistListSources (\v flags -> flags { sdistListSources = v })
            trueArg
        , option [] ["archive-format"] 
            "Choose what type of archive to create. No effect if given with '--list-only'"
                sdistArchiveFormat (\v flags -> flags { sdistArchiveFormat = v })
            (choiceOpt
                [ (Flag TargzFormat, ([], ["targz"]),
                        "Produce a '.tar.gz' format archive (default and required for uploading to hackage)")
                , (Flag ZipFormat,   ([], ["zip"]),
                        "Produce a '.zip' format archive")
                ]
            )
        , option ['o'] ["output-dir"]
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
    , sdistArchiveFormat :: Flag ArchiveFormat
    , sdistOutputPath    :: Flag FilePath
    }

defaultSdistFlags :: SdistFlags
defaultSdistFlags = SdistFlags
    { sdistVerbosity     = toFlag normal
    , sdistDistDir       = mempty
    , sdistProjectFile   = mempty
    , sdistListSources   = toFlag False
    , sdistArchiveFormat = toFlag TargzFormat
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
        archiveFormat = fromFlagOrDefault TargzFormat sdistArchiveFormat
        mOutputPath = flagToMaybe sdistOutputPath
  
    projectRoot <- either throwIO return =<< findProjectRoot Nothing mProjectFile
    let distLayout = defaultDistDirLayout projectRoot mDistDirectory
    dir <- getCurrentDirectory
    projectConfig <- runRebuild dir $ readProjectConfig verbosity globalConfig distLayout
    baseCtx <- establishProjectBaseContext verbosity projectConfig
    let localPkgs = localPackages baseCtx

    targetSelectors <- either (reportTargetSelectorProblems verbosity) return
        =<< readTargetSelectors localPkgs targetStrings
    
    let 
        ext = case archiveFormat of
            TargzFormat -> "tar.gz"
            ZipFormat -> "zip"
        
        outputPath pkg = case mOutputPath of
            Just path -> path </> prettyShow (packageId pkg) <.> ext
            Nothing
                | listSources -> "-"
                | otherwise   -> distSdistFile distLayout (packageId pkg) archiveFormat

    createDirectoryIfMissing True (distSdistDirectory distLayout)
    
    case reifyTargetSelectors localPkgs targetSelectors of
        Left errs -> die' verbosity . unlines . fmap renderTargetProblem $ errs
        Right pkgs -> mapM_ (\pkg -> packageToSdist verbosity listSources archiveFormat (outputPath pkg) pkg) pkgs

data IsExec = Exec | NoExec
            deriving (Show, Eq)

packageToSdist :: Verbosity -> Bool -> ArchiveFormat -> FilePath -> UnresolvedSourcePackage -> IO ()
packageToSdist verbosity listSources archiveFormat outputFile pkg = do
    dir <- case packageSource pkg of
        LocalUnpackedPackage path -> return path
        _ -> die' verbosity "The impossible happened: a local package isn't local"
    setCurrentDirectory dir

    let norm flag = fmap ((flag, ) . normalise)
    (norm NoExec -> nonexec, norm Exec -> exec) <- 
        listPackageSources verbosity (flattenPackageDescription $ packageDescription pkg) knownSuffixHandlers

    let write = if outputFile == "-" then BSL.putStrLn else BSL.writeFile outputFile
        files =  nub . sortBy (\(_, a) (_, b) -> compare a b) $ nonexec ++ exec

    if 
        | listSources -> do
            notice verbosity $ "File manifest for package " ++ prettyShow (packageId pkg) ++ ":\n"
            write (BSL.pack . unlines . fmap snd $ files)
        | archiveFormat == TargzFormat -> do
            let entriesM :: StateT [FilePath] (WriterT [Tar.Entry] IO) ()
                entriesM = forM_ files $ \(perm, file) -> do
                    let fileDir = takeDirectory file
                        perm' = case perm of
                            Exec -> Tar.executableFilePermissions
                            NoExec -> Tar.ordinaryFilePermissions
                    needsEntry <- gets (notElem fileDir)

                    when needsEntry $ do
                        modify (fileDir:)
                        case Tar.toTarPath True fileDir of
                            Left err -> liftIO $ die' verbosity ("Error packing sdist: " ++ err)
                            Right path -> tell [Tar.directoryEntry path]
                        
                    contents <- liftIO $ BSL.readFile file
                    case Tar.toTarPath False file of
                        Left err -> liftIO $ die' verbosity ("Error packing sdist: " ++ err)
                        Right path -> tell [(Tar.fileEntry path contents) { Tar.entryPermissions = perm' }]
            
            entries <- execWriterT (evalStateT entriesM [])
            write . GZip.compress . Tar.write $ entries
            notice verbosity $ "Wrote tarball sdist to " ++ outputFile ++ "\n"
        | archiveFormat == ZipFormat -> do
            entries <- forM files $ \(perm, file) -> do
                let perm' = case perm of
                        -- -rwxr-xr-x
                        Exec   -> 0o010755 `shiftL` 16
                        -- -rw-r--r--
                        NoExec -> 0o010644 `shiftL` 16
                contents <- BSL.readFile file
                return $ (Zip.toEntry file 0 contents) { Zip.eExternalFileAttributes = perm' }
            let archive = foldr Zip.addEntryToArchive Zip.emptyArchive entries
            write (Zip.fromArchive archive)
            notice verbosity $ "Wrote zip sdist to " ++ outputFile ++ "\n"

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
    "It is not possible to package only the " ++ renderComponentKind Plural kind ++ "from a package "
    ++ "for distribution. Only entire packages may be packaged for distribution."
renderTargetProblem (ComponentsNotAllowed cname) =
    "The component " ++ showComponentName cname ++ " cannot be packaged for distribution on its own. "
    ++ "Only entire packages may be packaged for distribution."
renderTargetProblem (NonlocalPackageNotAllowed pname) =
    "The package " ++ unPackageName pname ++ " cannot be packaged for distribution, because it is not "
    ++ "local to this project."

