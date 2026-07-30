-- | Regression test for #11329
--
-- When several cabal processes share one --store-dir and that store is cold,
-- they all race 'createPackageDBIfMissing'.
--
-- This test builds N independent trivial projects concurrently against one
-- shared store. Each project gets its own build dir so the only shared state
-- is the store package DB.

import Control.Concurrent
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (forM, forM_, unless)
import Data.List (isInfixOf)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.Exit (ExitCode (..))
import Test.Cabal.Prelude

main = cabalTest $ do
  env <- getTestEnv
  cabalPath <- programPath <$> requireProgramM cabalProgram
  -- The condition we need to test is the small window where multiple cabal
  -- calls concurrently attempt to create the shared store. Use a sufficiently
  -- high number to increase the probability of hitting that window.
  let n = 10
      attempts = 3 :: Int
      ids = [1 .. n] :: [Int]
      root = testCurrentDir env
      store = testWorkDir env </> "shared-store"
      projDir i = root </> ("p" ++ show i)
      build i =
        run
          (Just (projDir i))
          (testEnvironment env)
          cabalPath
          ["--store-dir=" ++ store, "build", "-v2", "--builddir=" ++ projDir i </> "dist"]
          Nothing

  -- Generate N independent trivial projects.
  liftIO $ forM_ ids $ \i -> do
    let p = projDir i
    createDirectoryIfMissing True p
    writeFile (p </> ("p" ++ show i ++ ".cabal")) $
      unlines
        [ "cabal-version: 2.4"
        , "name: p" ++ show i
        , "version: 0.1"
        , "executable p" ++ show i
        , "  main-is: Main.hs"
        , "  build-depends: base"
        , "  default-language: Haskell2010"
        ]

    writeFile (p </> "Main.hs") "main :: IO ()\nmain = return ()\n"
    writeFile (p </> "cabal.project") "packages: .\n"

  let -- Run the cabal command that exercises the race window.
      burst = liftIO $ do
        -- Make sure all package DBs are cold right before the burst.
        removePathForcibly store
        forM_ ids $ \i -> removePathForcibly (projDir i </> "dist")
        slots <- forM ids $ \i -> do
          mv <- newEmptyMVar
          _ <- forkIO $ do
            r <- try (build i) :: IO (Either SomeException Result)
            putMVar mv (i, r)
          return mv
        forM slots $ \mv -> do
          (i, r) <- takeMVar mv
          either throwIO (\res -> pure (i, res)) r

      -- Only a process that blocked on the lock logs this.
      contended (_, r) =
        "Waiting for file lock on package database" `isInfixOf` resultOutput r

      -- Try the test some times, if they all manage to miss the window. Emit a
      -- `skip`, as the test is inconclusive.
      go attempt = do
        results <- burst
        liftIO $ forM_ results $ \(i, r) -> do
          let out = resultOutput r
          assertBool
            ("cabal build for p" ++ show i ++ " hit the store package.db init race:\n" ++ out)
            (not ("already exists" `isInfixOf` out))
          assertEqual
            ("cabal build for p" ++ show i ++ " failed:\n" ++ out)
            ExitSuccess
            (resultExitCode r)
        unless (any contended results) $
          if attempt < attempts
            then go (attempt + 1)
            else
              skip $
                "no build contended for the store package db lock in "
                  ++ show attempts
                  ++ " attempts"

  go 1
