module Distribution.Client.Utils where

import Data.List
         ( sortBy, groupBy )
import System.Directory
         ( doesFileExist, getModificationTime
         , getCurrentDirectory, setCurrentDirectory )
import qualified Control.Exception as Exception
         ( finally )

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

-- | Executes the action in the specified directory.
inDir :: Maybe FilePath -> IO () -> IO ()
inDir Nothing m = m
inDir (Just d) m = do
  old <- getCurrentDirectory
  setCurrentDirectory d
  m `Exception.finally` setCurrentDirectory old
