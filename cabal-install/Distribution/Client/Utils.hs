module Distribution.Client.Utils where

import Data.List
         ( sortBy, groupBy )
import qualified Data.ByteString.Lazy as BS
import System.FilePath
         ( (<.>), splitFileName )
import Control.Monad
        ( unless )
import System.IO
         ( openBinaryTempFile, hClose )
import System.IO.Error
         ( isDoesNotExistError )
import System.Directory
         ( removeFile, renameFile, doesFileExist, getModificationTime
         , getCurrentDirectory, setCurrentDirectory, removeDirectoryRecursive )
import Distribution.Compat.TempFile
         ( createTempDirectory )
import qualified Control.Exception as Exception
         ( handle, throwIO, evaluate, finally, bracket )

-- | Generic merging utility. For sorted input lists this is a full outer join.
--
-- * The result list never contains @(Nothing, Nothing)@.
--
mergeBy :: (a -> b -> Ordering) -> [a] -> [b] -> [MergeResult a b]
mergeBy cmp = merge
  where
    merge []     ys     = [ OnlyInRight y | y <- ys]
    merge xs     []     = [ OnlyInLeft  x | x <- xs]
    merge (x:xs) (y:ys) =
      case x `cmp` y of
        GT -> OnlyInRight   y : merge (x:xs) ys
        EQ -> InBoth      x y : merge xs     ys
        LT -> OnlyInLeft  x   : merge xs  (y:ys)

data MergeResult a b = OnlyInLeft a | InBoth a b | OnlyInRight b

duplicates :: Ord a => [a] -> [[a]]
duplicates = duplicatesBy compare

duplicatesBy :: (a -> a -> Ordering) -> [a] -> [[a]]
duplicatesBy cmp = filter moreThanOne . groupBy eq . sortBy cmp
  where
    eq a b = case cmp a b of
               EQ -> True
               _  -> False
    moreThanOne (_:_:_) = True
    moreThanOne _       = False

writeFileAtomic :: FilePath -> BS.ByteString -> IO ()
writeFileAtomic targetFile content = do
  (tmpFile, tmpHandle) <- openBinaryTempFile targetDir template
  Exception.handle (\err -> do hClose tmpHandle
                               removeFile tmpFile
                               Exception.throwIO err) $ do
      BS.hPut tmpHandle content
      hClose tmpHandle
      renameFile tmpFile targetFile
  where
    template = targetName <.> "tmp"
    targetDir | null targetDir_ = "."
              | otherwise       = targetDir_
    --TODO: remove this when takeDirectory/splitFileName is fixed
    --      to always return a valid dir
    (targetDir_,targetName) = splitFileName targetFile

-- | Compare the modification times of two files to see if the first is newer
-- than the second. The first file must exist but the second need not.
-- The expected use case is when the second file is generated using the first.
-- In this use case, if the result is True then the second file is out of date.
--
moreRecentFile :: FilePath -> FilePath -> IO Bool
moreRecentFile a b = do
  exists <- doesFileExist b
  if not exists
    then return True
    else do tb <- getModificationTime b
            ta <- getModificationTime a
            return (ta > tb)

-- | Write a file but only if it would have new content. If we would be writing
-- the same as the existing content then leave the file as is so that we do not
-- update the file's modification time.
--
rewriteFile :: FilePath -> String -> IO ()
rewriteFile path newContent =
  flip catch mightNotExist $ do
    existingContent <- readFile path
    Exception.evaluate (length existingContent)
    unless (existingContent == newContent) $
      writeFile path newContent
  where
    mightNotExist e | isDoesNotExistError e = writeFile path newContent
                    | otherwise             = ioError e

--TODO: replace with function from Cabal utils in next version
withTempDirectory :: FilePath -> String -> (FilePath -> IO a) -> IO a
withTempDirectory targetDir template =
  Exception.bracket
    (createTempDirectory targetDir template)
    (removeDirectoryRecursive)

-- | Executes the action in the specified directory.
inDir :: Maybe FilePath -> IO () -> IO ()
inDir Nothing m = m
inDir (Just d) m = do
  old <- getCurrentDirectory
  setCurrentDirectory d
  m `Exception.finally` setCurrentDirectory old
