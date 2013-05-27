module Distribution.Simple.BoundedResource
  ( BoundedResource
  , newBoundedResource
  , takeResource
  , putResource
  , withResource
  ) where

import Control.Concurrent.Chan
import Control.Exception (bracket_)
import Control.Monad


-- | Manages sparse resources in IO, allowing you to use at maximum a certain
-- number of them.
--
-- The number of available resources can be adjusted at run-time with
-- `takeResource` and `putResource`.
--
-- Exception-safe use-and-free access is provided via `withResource`.
--
-- Uses O(n) memory in the number of free slots.
newtype BoundedResource =
    BoundedResource
      (Chan ()) -- ^ a "slot" for each available resource (>= 0, <= N)


-- | Create a new resource that can provide at maximum n slots.
newBoundedResource :: Int -> IO BoundedResource
newBoundedResource n = do
  when (n < 0) $ error $ "newBoundedResource: negative size: " ++ show n
  chan <- newChan
  replicateM_ n $ writeChan chan () -- put n slots in
  return $ BoundedResource chan


-- | Take one slot away from the resource.
-- If no slot is free, blocks until one becomes free.
--
-- Use `withResource` for use-free combinations.
takeResource :: BoundedResource -> IO ()
takeResource (BoundedResource slotChan) = readChan slotChan


-- | Add a free slot to the resource.
--
-- Use `withResource` for use-free combinations.
putResource :: BoundedResource -> IO ()
putResource (BoundedResource slotChan) = writeChan slotChan ()


-- | Perform an action, requiring one free slot from the resource.
--
-- Ensures that the action failing with an exception cannot break the Resource.
withResource :: BoundedResource -> IO a -> IO a
withResource res io = bracket_ (takeResource res) (putResource res) io
