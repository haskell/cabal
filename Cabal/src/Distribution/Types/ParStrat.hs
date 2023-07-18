module Distribution.Types.ParStrat where

-- | How to control parallelism, e.g. a fixed number of jobs or by using a system semaphore.
data ParStratX sem
  = -- | Compile in parallel with the given number of jobs (`-jN` or `-j`).
    NumJobs (Maybe Int)
  | -- | `--semaphore`: use a system semaphore to control parallelism.
    UseSem sem
  | -- | No parallelism (neither `-jN` nor `--semaphore`, but could be `-j1`).
    Serial
  deriving (Show)

-- | Used by Cabal to indicate that we want to use this specific semaphore (created by cabal-install)
type ParStrat = ParStratX String

-- | Used by cabal-install to say we want to create a semaphore with N slots.
type ParStratInstall = ParStratX Int

-- | Determine if the parallelism strategy enables parallel builds.
isParallelBuild :: ParStratX n -> Bool
isParallelBuild Serial = False
isParallelBuild (NumJobs (Just 1)) = False
isParallelBuild (NumJobs _) = True
isParallelBuild UseSem{} = True
