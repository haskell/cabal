-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Index
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Querying and modifying local build tree references in the package index.
-----------------------------------------------------------------------------

module Distribution.Client.Index (
    index,

    createEmpty,
    addBuildTreeRefs,
    removeBuildTreeRefs,
    listBuildTreeRefs,

    defaultIndexFileName
  ) where

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
import Distribution.Simple.Utils ( die, debug, info, findPackageDesc )
import Distribution.Verbosity    ( Verbosity )

import qualified Data.ByteString.Lazy as BS
import Control.Exception         ( evaluate )
import Control.Monad             ( liftM, when, unless )
import Data.List                 ( (\\), nub )
import Data.Maybe                ( catMaybes )
import System.Directory          ( canonicalizePath, createDirectoryIfMissing,
                                   doesDirectoryExist, doesFileExist,
                                   renameFile )
import System.FilePath           ( (</>), (<.>), takeDirectory, takeExtension )
import System.IO                 ( IOMode(..), SeekMode(..)
                                 , hSeek, withBinaryFile )

-- | A reference to a local build tree.
newtype BuildTreeRef = BuildTreeRef {
  buildTreePath :: FilePath
  }

defaultIndexFileName :: FilePath
defaultIndexFileName = "00-index.tar"

-- | Entry point for the 'cabal index' command.
index :: Verbosity -> IndexFlags -> FilePath -> IO ()
index verbosity indexFlags path' = do
  let runInit         = fromFlagOrDefault False (indexInit indexFlags)
  let refsToAdd       = indexLinkSource indexFlags
  let runLinkSource   = not . null $ refsToAdd
  let refsToRemove    = indexRemoveSource indexFlags
  let runRemoveSource = not . null $ refsToRemove
  let runList         = fromFlagOrDefault False (indexList indexFlags)

  unless (or [runInit, runLinkSource, runRemoveSource, runList]) $
    die "no arguments passed to the 'index' command"

  path <- validateIndexPath path'

  when runInit $
    createEmpty verbosity path

  when runLinkSource $
    addBuildTreeRefs verbosity path refsToAdd

  when runRemoveSource $
    removeBuildTreeRefs verbosity path refsToRemove

  when runList $ do
    refs <- listBuildTreeRefs verbosity path
    mapM_ putStrLn refs

-- | Given a path, ensure that it refers to a local build tree.
buildTreeRefFromPath :: FilePath -> IO (Maybe BuildTreeRef)
buildTreeRefFromPath dir = do
  dirExists <- doesDirectoryExist dir
  unless dirExists $
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
  . Tar.foldrEntries (\e r -> readBuildTreePath e : r)
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
             unless dirExists $
               die $ "directory does not exist: '" ++ path ++ "'"
             return $ path </> defaultIndexFileName

-- | Create an empty index file.
createEmpty :: Verbosity -> FilePath -> IO ()
createEmpty verbosity path = do
  indexExists <- doesFileExist path
  if indexExists
    then debug verbosity $ "Package index already exists: " ++ path
    else do
    debug verbosity $ "Creating the index file '" ++ path ++ "'"
    createDirectoryIfMissing True (takeDirectory path)
    -- Equivalent to 'tar cvf empty.tar --files-from /dev/null'.
    let zeros = BS.replicate (512*20) 0
    BS.writeFile path zeros

-- | Add given local build tree references to the index.
addBuildTreeRefs :: Verbosity -> FilePath -> [FilePath] -> IO ()
addBuildTreeRefs _         _   [] =
  error "Distribution.Client.Index.addBuildTreeRefs: unexpected"
addBuildTreeRefs verbosity path l' = do
  checkIndexExists path
  l <- liftM nub . mapM canonicalizePath $ l'
  treesInIndex <- readBuildTreePathsFromFile path
  -- Add only those paths that aren't already in the index.
  treesToAdd <- mapM buildTreeRefFromPath (l \\ treesInIndex)
  let entries = map writeBuildTreeRef (catMaybes treesToAdd)
  unless (null entries) $ do
    offset <-
      fmap (Tar.foldrEntries (\e acc -> Tar.entrySizeInBytes e + acc) 0 error
            . Tar.read) $ BS.readFile path
    _ <- evaluate offset
    debug verbosity $ "Writing at offset: " ++ show offset
    withBinaryFile path ReadWriteMode $ \h -> do
      hSeek h AbsoluteSeek (fromIntegral offset)
      BS.hPut h (Tar.write entries)
      debug verbosity $ "Successfully appended to '" ++ path ++ "'"

-- | Remove given local build tree references from the index.
removeBuildTreeRefs :: Verbosity -> FilePath -> [FilePath] -> IO ()
removeBuildTreeRefs _         _   [] =
  error "Distribution.Client.Index.removeBuildTreeRefs: unexpected"
removeBuildTreeRefs verbosity path l' = do
  checkIndexExists path
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
        (Just pth) -> pth `notElem` l

-- | List the local build trees that are referred to from the index.
listBuildTreeRefs :: Verbosity -> FilePath -> IO [FilePath]
listBuildTreeRefs verbosity path = do
  checkIndexExists path
  let repo = Repo { repoKind = Right LocalRepo
                  , repoLocalDir = takeDirectory path }
  pkgIndex <- fmap packageIndex . getSourcePackages verbosity $ [repo]
  let buildTreeRefs = [ pkgPath | (LocalUnpackedPackage pkgPath) <-
                           map packageSource . allPackages $ pkgIndex ]
  when (null buildTreeRefs) $
    info verbosity $ "Index file '" ++ path
      ++ "' has no references to local build trees."
  return buildTreeRefs

-- | Check that the package index file exists and exit with error if it does not.
checkIndexExists :: FilePath -> IO ()
checkIndexExists path = do
  indexExists <- doesFileExist path
  unless indexExists $
    die $ "index does not exist: '" ++ path ++ "'"
