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
import Distribution.Client.Setup ( IndexFlags(..) )
import Distribution.Client.Utils ( byteStringToFilePath, filePathToByteString
                                 , makeAbsoluteToCwd )

import Distribution.Simple.Setup ( fromFlagOrDefault )
import Distribution.Simple.Utils ( die, debug, notice )
import Distribution.Verbosity    ( Verbosity )

import qualified Data.ByteString.Lazy as BS
import Control.Monad             ( liftM, when, unless )
import Data.List                 ( (\\), nub )
import Data.Maybe                ( catMaybes )
import System.Directory          ( canonicalizePath,
                                   doesDirectoryExist, doesFileExist,
                                   getDirectoryContents, renameFile )
import System.FilePath           ( (</>), (<.>), takeExtension )


-- | A reference to a local build tree.
data LocalBuildTree = LocalBuildTree {
  localBuildTreePath :: FilePath
  }

-- | Type code of the corresponding tar entry type.
localBuildTreeTypeCode :: Tar.TypeCode
localBuildTreeTypeCode = 'C'

-- | Given a path, ensure that it refers to a local build tree.
localBuildTreeFromPath :: FilePath -> IO (Maybe LocalBuildTree)
localBuildTreeFromPath dir = do
  dirExists <- doesDirectoryExist dir
  if dirExists then
    do fns <- getDirectoryContents dir
       case filter ((== ".cabal") . takeExtension) fns of
         [_] -> return . Just $ LocalBuildTree { localBuildTreePath = dir }
         []  -> die $ "directory '" ++ dir
                ++ "' doesn't contain a .cabal file"
         _   -> die $ "directory '" ++ dir
                ++ "' contains more than one .cabal file"
    else die $ "directory '" ++ dir ++ "' does not exist"

-- | Given a tar archive entry, try to parse it as a reference to a local build
-- tree.
readLocalBuildTree :: Tar.Entry -> Maybe FilePath
readLocalBuildTree entry = case Tar.entryContent entry of
  (Tar.OtherEntryType typeCode bs size)
    | (typeCode == localBuildTreeTypeCode)
      && (size == BS.length bs) -> Just $ byteStringToFilePath bs
    | otherwise                 -> Nothing
  _ -> Nothing

readLocalBuildTrees :: Tar.Entries -> [FilePath]
readLocalBuildTrees = catMaybes
                      . Tar.foldrEntries (\e r -> (readLocalBuildTree e):r)
                      [] error

readLocalBuildTreesFromFile :: FilePath -> IO [FilePath]
readLocalBuildTreesFromFile = liftM (readLocalBuildTrees . Tar.read)
                              . BS.readFile

-- | Given a local build tree, serialise it to a tar archive entry.
writeLocalBuildTree :: LocalBuildTree -> Tar.Entry
writeLocalBuildTree lbt = Tar.simpleEntry tarPath content
  where
    bs       = filePathToByteString path
    path     = localBuildTreePath lbt
    -- fromRight can't fail because the path is shorter than 255 characters.
    tarPath  = fromRight $ Tar.toTarPath True tarPath'
    -- Provide a filename for tools that treat custom entries as ordinary files.
    tarPath' = "local-build-tree-reference"
    content  = Tar.OtherEntryType localBuildTreeTypeCode bs (BS.length bs)

    -- TODO: Move this to D.C.Utils?
    fromRight (Left err) = error err
    fromRight (Right a)  = a


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
  treesInIndex <- readLocalBuildTreesFromFile path
  -- Add only those paths that aren't already in the index.
  treesToAdd <- mapM localBuildTreeFromPath (l \\ treesInIndex)
  let entries = map writeLocalBuildTree (catMaybes treesToAdd)
  when (not . null $ entries) $ do
    let tmpFile = path <.> "tmp"
    -- TODO: Calculate the offset and append instead of rewriting. Complicated
    -- by the fact that a tar archive can have a nondeterministic number of
    -- trailing zeros after two obligatory zero blocks, so searching for the
    -- last entry from the end is problematic.
    BS.writeFile tmpFile . Tar.appendEntries entries. Tar.read
      =<< BS.readFile path
    renameFile tmpFile path
    debug verbosity $ "Successfully renamed '" ++ tmpFile
      ++ "' to '" ++ path ++ "'"


-- | Remove a reference to a local build tree to the index.
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
  renameFile tmpFile path
  debug verbosity $ "Successfully renamed '" ++ tmpFile
    ++ "' to '" ++ path ++ "'"
    where
      p l entry = case readLocalBuildTree entry of
        Nothing    -> True
        (Just pth) -> not $ any (== pth) l

-- | List the local build trees that are referred to from the index.
doList :: Verbosity -> FilePath -> IO ()
doList verbosity path = do
  localTrees <- readLocalBuildTreesFromFile path
  when (null localTrees) $ do
    notice verbosity $ "Index file '" ++ path
      ++ "' has no references to local build trees."
  mapM_ putStrLn localTrees
