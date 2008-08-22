module Distribution.Client.Utils where

import Data.List
         ( sortBy, groupBy )
import qualified Data.ByteString.Lazy as BS
import System.FilePath
         ( (<.>), splitFileName )
import System.IO
         ( openBinaryTempFile, hClose )
import System.Directory
         ( removeFile, renameFile )
import qualified Control.Exception as Exception
         ( handle, throwIO )

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
