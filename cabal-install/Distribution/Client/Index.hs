-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Index
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Querying and modifying the package index.
--
--
-----------------------------------------------------------------------------

module Distribution.Client.Index (index)
       where

import qualified Distribution.Client.Tar as Tar
import Distribution.Client.IndexUtils ( getSourcePackages )
import Distribution.Client.PackageIndex ( allPackages )
import Distribution.Client.Setup ( IndexFlags(..) )
import Distribution.Client.Types ( Repo(..), LocalRepo(..)
                                 , SourcePackageDb(..)
                                 , SourcePackage(..), PackageLocation(..) )
import Distribution.Client.Utils ( byteStringToFilePath, filePathToByteString
                                 , makeAbsoluteToCwd )

import Distribution.Simple.Setup ( fromFlagOrDefault )
import Distribution.Simple.Utils ( die, debug, notice, findPackageDesc )
import Distribution.Verbosity    ( Verbosity )

import qualified Data.ByteString.Lazy as BS
import Control.Exception         ( evaluate )
import Control.Monad             ( liftM, when, unless )
import Data.List                 ( (\\), nub )
import Data.Maybe                ( catMaybes )
import System.Directory          ( canonicalizePath,
                                   doesDirectoryExist, doesFileExist,
                                   renameFile )
import System.FilePath           ( (</>), (<.>), takeDirectory, takeExtension )
import System.IO                 ( IOMode(..), SeekMode(..)
                                 , hSeek, withBinaryFile )

-- | A reference to a local build tree.
newtype BuildTreeRef = BuildTreeRef {
  buildTreePath :: FilePath
  }

-- | Entry point for the 'cabal index' command.
index :: Verbosity -> IndexFlags -> FilePath -> IO ()
index verbosity indexFlags path' = do
  let runInit = fromFlagOrDefault False (indexInit indexFlags)
  let linkSource = indexLinkSource indexFlags
  let runLinkSource = not . null $ linkSource
  let removeSource = indexRemoveSource indexFlags
  let runRemoveSource = not . null $ removeSource
  let runList = fromFlagOrDefault False (indexList indexFlags)

  unless (or [runInit, runLinkSource, runRemoveSource, runList]) $ do
    die "no arguments passed to the 'index' command"

  path <- validateIndexPath path'

  when runInit $ do
    indexExists <- doesFileExist path
    if indexExists
      then die $ "index already exists: '" ++ path ++ "'"
      else doInit verbosity path

  indexExists <- doesFileExist path
  when (not indexExists) $ do
    die $ "index does not exist: '" ++ path ++ "'"

  when runLinkSource $ do
    doLinkSource verbosity path linkSource

  when runRemoveSource $ do
    doRemoveSource verbosity path removeSource

  when runList $ do
    doList verbosity path

-- | Given a path, ensure that it refers to a local build tree.
buildTreeRefFromPath :: FilePath -> IO (Maybe BuildTreeRef)
buildTreeRefFromPath dir = do
  dirExists <- doesDirectoryExist dir
  when (not dirExists) $ do
    die $ "directory '" ++ dir ++ "' does not exist"
  _ <- findPackageDesc dir
  return . Just $ BuildTreeRef { buildTreePath = dir }

-- | Given a tar archive entry, try to parse it as a local build tree reference.
readBuildTreePath :: Tar.Entry -> Maybe FilePath
readBuildTreePath entry = case Tar.entryContent entry of
  (Tar.OtherEntryType typeCode bs size)
    | (typeCode == Tar.buildTreeRefTypeCode)
      && (size == BS.length bs) -> Just $ byteStringToFilePath bs
    | otherwise                 -> Nothing
  _ -> Nothing

-- | Given a sequence of tar archive entries, extract all references to local
-- build trees.
readBuildTreePaths :: Tar.Entries -> [FilePath]
readBuildTreePaths =
  catMaybes
  . Tar.foldrEntries (\e r -> (readBuildTreePath e):r)
  [] error

-- | Given a path to a tar archive, extract all references to local build trees.
readBuildTreePathsFromFile :: FilePath -> IO [FilePath]
readBuildTreePathsFromFile = liftM (readBuildTreePaths . Tar.read)
                                  . BS.readFile

-- | Given a local build tree, serialise it to a tar archive entry.
writeBuildTreeRef :: BuildTreeRef -> Tar.Entry
writeBuildTreeRef lbt = Tar.simpleEntry tarPath content
  where
    bs       = filePathToByteString path
    path     = buildTreePath lbt
    -- fromRight can't fail because the path is shorter than 255 characters.
    tarPath  = fromRight $ Tar.toTarPath True tarPath'
    -- Provide a filename for tools that treat custom entries as ordinary files.
    tarPath' = "local-build-tree-reference"
    content  = Tar.OtherEntryType Tar.buildTreeRefTypeCode bs (BS.length bs)

    -- TODO: Move this to D.C.Utils?
    fromRight (Left err) = error err
    fromRight (Right a)  = a

-- | Check that the provided path is either an existing directory, or a tar
-- archive in an existing directory.
validateIndexPath :: FilePath -> IO FilePath
validateIndexPath path' = do
   path <- makeAbsoluteToCwd path'
   if (== ".tar") . takeExtension $ path
     then return path
     else do dirExists <- doesDirectoryExist path
             unless dirExists $ do
               die $ "directory does not exist: '" ++ path ++ "'"
             return $ path </> "00-index.tar"

-- | Create an empty index file (index --init).
doInit :: Verbosity -> FilePath -> IO ()
doInit verbosity path = do
  debug verbosity $ "Creating the index file '" ++ path ++ "'"
  -- Equivalent to 'tar cvf empty.tar --files-from /dev/null'.
  let zeros = BS.replicate (512*20) 0
  BS.writeFile path zeros

-- | Add a reference to a local build tree to the index.
doLinkSource :: Verbosity -> FilePath -> [FilePath] -> IO ()
doLinkSource _         _   [] =
  error "Distribution.Client.Index.doLinkSource: unexpected"
doLinkSource verbosity path l' = do
  l <- liftM nub . mapM canonicalizePath $ l'
  treesInIndex <- readBuildTreePathsFromFile path
  -- Add only those paths that aren't already in the index.
  treesToAdd <- mapM buildTreeRefFromPath (l \\ treesInIndex)
  let entries = map writeBuildTreeRef (catMaybes treesToAdd)
  when (not . null $ entries) $ do
    offset <-
      fmap (Tar.foldrEntries (\e acc -> Tar.entrySizeInBytes e + acc) 0 error
            . Tar.read) $ BS.readFile path
    _ <- evaluate offset
    debug verbosity $ "Writing at offset: " ++ show offset
    withBinaryFile path ReadWriteMode $ \h -> do
      hSeek h AbsoluteSeek (fromIntegral offset)
      BS.hPut h (Tar.write entries)
      debug verbosity $ "Successfully appended to '" ++ path ++ "'"

-- | Remove a reference to a local build tree from the index.
doRemoveSource :: Verbosity -> FilePath -> [FilePath] -> IO ()
doRemoveSource _         _   [] =
  error "Distribution.Client.Index.doRemoveSource: unexpected"
doRemoveSource verbosity path l' = do
  l <- mapM canonicalizePath l'
  let tmpFile = path <.> "tmp"
  -- Performance note: on my system, it takes 'index --remove-source'
  -- approx. 3,5s to filter a 65M file. Real-life indices are expected to be
  -- much smaller.
  BS.writeFile tmpFile . Tar.writeEntries . Tar.filterEntries (p l) . Tar.read
    =<< BS.readFile path
  -- This invalidates the cache, so we don't have to update it explicitly.
  renameFile tmpFile path
  debug verbosity $ "Successfully renamed '" ++ tmpFile
    ++ "' to '" ++ path ++ "'"
    where
      p l entry = case readBuildTreePath entry of
        Nothing    -> True
        (Just pth) -> not $ any (== pth) l

-- | List the local build trees that are referred to from the index.
doList :: Verbosity -> FilePath -> IO ()
doList verbosity path = do
  let repo = Repo { repoKind = Right LocalRepo
                  , repoLocalDir = takeDirectory path }
  pkgIndex <- fmap packageIndex . getSourcePackages verbosity $ [repo]
  let buildTreeRefs = [ pkgPath | (LocalUnpackedPackage pkgPath) <-
                           map packageSource . allPackages $ pkgIndex ]
  when (null buildTreeRefs) $ do
    notice verbosity $ "Index file '" ++ path
      ++ "' has no references to local build trees."
  mapM_ putStrLn buildTreeRefs
