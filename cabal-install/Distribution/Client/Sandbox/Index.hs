-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Sandbox.Index
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Querying and modifying local build tree references in the package index.
-----------------------------------------------------------------------------

module Distribution.Client.Sandbox.Index (
    createEmpty,
    addBuildTreeRefs,
    removeBuildTreeRefs,
    ListIgnoredBuildTreeRefs(..), RefTypesToList(..),
    DeleteSourceError(..),
    listBuildTreeRefs,
    validateIndexPath,

    defaultIndexFileName
  ) where

import qualified Distribution.Client.Tar as Tar
import Distribution.Client.IndexUtils ( BuildTreeRefType(..)
                                      , refTypeFromTypeCode
                                      , typeCodeFromRefType
                                      , updatePackageIndexCacheFile
                                      , getSourcePackagesStrict )
import Distribution.Client.PackageIndex ( allPackages )
import Distribution.Client.Types ( Repo(..), LocalRepo(..)
                                 , SourcePackageDb(..)
                                 , SourcePackage(..), PackageLocation(..) )
import Distribution.Client.Utils ( byteStringToFilePath, filePathToByteString
                                 , makeAbsoluteToCwd, tryCanonicalizePath
                                 , tryFindAddSourcePackageDesc  )

import Distribution.Simple.Utils ( die, debug )
import Distribution.Compat.Exception   ( tryIO )
import Distribution.Verbosity    ( Verbosity )

import qualified Data.ByteString.Lazy as BS
import Control.Exception         ( evaluate )
import Control.Monad             ( liftM, unless )
import Control.Monad.Writer.Lazy (WriterT(..), runWriterT, tell)
import Data.List                 ( (\\), intersect, nub, find )
import Data.Maybe                ( catMaybes, fromMaybe )
import Data.Either               (partitionEithers)
import System.Directory          ( createDirectoryIfMissing,
                                   doesDirectoryExist, doesFileExist,
                                   renameFile, canonicalizePath)
import System.FilePath           ( (</>), (<.>), takeDirectory, takeExtension
                                 , replaceExtension )
import System.IO                 ( IOMode(..), SeekMode(..)
                                 , hSeek, withBinaryFile )

-- | A reference to a local build tree.
data BuildTreeRef = BuildTreeRef {
  buildTreeRefType :: !BuildTreeRefType,
  buildTreePath     :: !FilePath
  }

defaultIndexFileName :: FilePath
defaultIndexFileName = "00-index.tar"

-- | Given a path, ensure that it refers to a local build tree.
buildTreeRefFromPath :: BuildTreeRefType -> FilePath -> IO (Maybe BuildTreeRef)
buildTreeRefFromPath refType dir = do
  dirExists <- doesDirectoryExist dir
  unless dirExists $
    die $ "directory '" ++ dir ++ "' does not exist"
  _ <- tryFindAddSourcePackageDesc dir "Error adding source reference."
  return . Just $ BuildTreeRef refType dir

-- | Given a tar archive entry, try to parse it as a local build tree reference.
readBuildTreeRef :: Tar.Entry -> Maybe BuildTreeRef
readBuildTreeRef entry = case Tar.entryContent entry of
  (Tar.OtherEntryType typeCode bs size)
    | (Tar.isBuildTreeRefTypeCode typeCode)
      && (size == BS.length bs) -> Just $! BuildTreeRef
                                   (refTypeFromTypeCode typeCode)
                                   (byteStringToFilePath bs)
    | otherwise                 -> Nothing
  _ -> Nothing

-- | Given a sequence of tar archive entries, extract all references to local
-- build trees.
readBuildTreeRefs :: Tar.Entries -> [BuildTreeRef]
readBuildTreeRefs =
  catMaybes
  . Tar.foldrEntries (\e r -> readBuildTreeRef e : r)
  [] error

-- | Given a path to a tar archive, extract all references to local build trees.
readBuildTreeRefsFromFile :: FilePath -> IO [BuildTreeRef]
readBuildTreeRefsFromFile = liftM (readBuildTreeRefs . Tar.read) . BS.readFile

-- | Given a local build tree ref, serialise it to a tar archive entry.
writeBuildTreeRef :: BuildTreeRef -> Tar.Entry
writeBuildTreeRef (BuildTreeRef refType path) = Tar.simpleEntry tarPath content
  where
    bs       = filePathToByteString path
    -- Provide a filename for tools that treat custom entries as ordinary files.
    tarPath' = "local-build-tree-reference"
    -- fromRight can't fail because the path is shorter than 255 characters.
    tarPath  = fromRight $ Tar.toTarPath True tarPath'
    content  = Tar.OtherEntryType (typeCodeFromRefType refType) bs (BS.length bs)

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
addBuildTreeRefs :: Verbosity -> FilePath -> [FilePath] -> BuildTreeRefType
                    -> IO ()
addBuildTreeRefs _         _   []  _ =
  error "Distribution.Client.Sandbox.Index.addBuildTreeRefs: unexpected"
addBuildTreeRefs verbosity path l' refType = do
  checkIndexExists path
  l <- liftM nub . mapM tryCanonicalizePath $ l'
  treesInIndex <- fmap (map buildTreePath) (readBuildTreeRefsFromFile path)
  -- Add only those paths that aren't already in the index.
  treesToAdd <- mapM (buildTreeRefFromPath refType) (l \\ treesInIndex)
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
    updatePackageIndexCacheFile verbosity path
      (path `replaceExtension` "cache")

data DeleteSourceError = ErrNonregisteredSource { nrPath :: FilePath }
                       | ErrNonexistentSource   { nePath :: FilePath } deriving Show

-- | Remove given local build tree references from the index.
--
-- Returns a tuple consisting of removed build tree refs and mappings from
-- provided build tree refs to corresponding full directory paths).
removeBuildTreeRefs :: Verbosity -> FilePath -> [FilePath]
                       -> IO ([Either DeleteSourceError FilePath],
                              (FilePath -> FilePath)) -- ^ A tuple consisting of:
                                                      -- * either removed build tree refs or errors
                                                      -- * a functio that converts from a provided
                                                      -- build tree ref to corresponding
                                                      -- full directory path)
removeBuildTreeRefs _         _   [] =
  error "Distribution.Client.Sandbox.Index.removeBuildTreeRefs: unexpected"
removeBuildTreeRefs verbosity indexPath l = do
  checkIndexExists indexPath
  let tmpFile = indexPath <.> "tmp"

  canonRes <- mapM (\btr -> do res <- tryIO $ canonicalizePath btr
                               return $ case res of
                                 Right pth -> Right (btr, pth)
                                 Left _ -> Left $ ErrNonexistentSource btr) l
  let (failures, convDict) = partitionEithers canonRes
      allRefs = fmap snd convDict

  -- Performance note: on my system, it takes 'index --remove-source'
  -- approx. 3,5s to filter a 65M file. Real-life indices are expected to be
  -- much smaller.
  removedRefs <- doRemove allRefs tmpFile

  renameFile tmpFile indexPath
  debug verbosity $ "Successfully renamed '" ++ tmpFile
    ++ "' to '" ++ indexPath ++ "'"

  unless (null removedRefs) $
    updatePackageIndexCacheFile verbosity indexPath (indexPath `replaceExtension` "cache")

  let results = fmap Right removedRefs
                ++ fmap Left failures
                ++ fmap (Left . ErrNonregisteredSource)
                        (fmap (convertWith convDict) (allRefs \\ removedRefs))
  return (results, convertWith convDict)

    where
      doRemove :: [FilePath] -> FilePath -> IO [FilePath]
      doRemove srcRefs tmpFile = do
        (newIdx, changedPaths) <- Tar.read `fmap` BS.readFile indexPath
                                          >>= runWriterT . Tar.filterEntriesM (p srcRefs)
        BS.writeFile tmpFile $ Tar.writeEntries newIdx
        return changedPaths

      p :: [FilePath] -> Tar.Entry -> WriterT [FilePath] IO Bool
      p refs entry = case readBuildTreeRef entry of
        Nothing -> return True
        -- FIXME: removing snapshot deps is done with `delete-source
        -- .cabal-sandbox/snapshots/$SNAPSHOT_NAME`. Perhaps we also want to
        -- support removing snapshots by providing the original path.
        (Just (BuildTreeRef _ pth)) -> if pth `elem` refs
                                       then tell [pth] >> return False
                                       else return True

      convertWith dict pth = fromMaybe pth $ fmap fst $ find ((==pth) . snd) dict

-- | A build tree ref can become ignored if the user later adds a build tree ref
-- with the same package ID. We display ignored build tree refs when the user
-- runs 'cabal sandbox list-sources', but do not look at their timestamps in
-- 'reinstallAddSourceDeps'.
data ListIgnoredBuildTreeRefs = ListIgnored | DontListIgnored

-- | Which types of build tree refs should be listed?
data RefTypesToList = OnlySnapshots | OnlyLinks | LinksAndSnapshots

-- | List the local build trees that are referred to from the index.
listBuildTreeRefs :: Verbosity -> ListIgnoredBuildTreeRefs -> RefTypesToList
                     -> FilePath
                     -> IO [FilePath]
listBuildTreeRefs verbosity listIgnored refTypesToList path = do
  checkIndexExists path
  buildTreeRefs <-
    case listIgnored of
      DontListIgnored -> do
        paths <- listWithoutIgnored
        case refTypesToList of
          LinksAndSnapshots -> return paths
          _                 -> do
            allPathsFiltered <- fmap (map buildTreePath . filter predicate)
                                listWithIgnored
            _ <- evaluate (length allPathsFiltered)
            return (paths `intersect` allPathsFiltered)

      ListIgnored -> fmap (map buildTreePath . filter predicate) listWithIgnored

  _ <- evaluate (length buildTreeRefs)
  return buildTreeRefs

    where
      predicate :: BuildTreeRef -> Bool
      predicate = case refTypesToList of
        OnlySnapshots     -> (==) SnapshotRef . buildTreeRefType
        OnlyLinks         -> (==) LinkRef     . buildTreeRefType
        LinksAndSnapshots -> const True

      listWithIgnored :: IO [BuildTreeRef]
      listWithIgnored = readBuildTreeRefsFromFile $ path

      listWithoutIgnored :: IO [FilePath]
      listWithoutIgnored = do
        let repo = Repo { repoKind = Right LocalRepo
                        , repoLocalDir = takeDirectory path }
        pkgIndex <- fmap packageIndex
                    . getSourcePackagesStrict verbosity $ [repo]
        return [ pkgPath | (LocalUnpackedPackage pkgPath) <-
                    map packageSource . allPackages $ pkgIndex ]


-- | Check that the package index file exists and exit with error if it does not.
checkIndexExists :: FilePath -> IO ()
checkIndexExists path = do
  indexExists <- doesFileExist path
  unless indexExists $
    die $ "index does not exist: '" ++ path ++ "'"
