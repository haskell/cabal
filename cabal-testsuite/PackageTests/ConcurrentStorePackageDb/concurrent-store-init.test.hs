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
import Control.Monad (forM, forM_)
import Data.List (isInfixOf)
import System.Directory (createDirectoryIfMissing, removePathForcibly)
import System.Exit (ExitCode (..))
import Test.Cabal.Prelude

main = cabalTest $ do
  env <- getTestEnv
  cabalPath <- programPath <$> requireProgramM cabalProgram
  let n = 10
      ids = [1 .. n] :: [Int]
      root = testCurrentDir env
      store = testWorkDir env </> "shared-store"
      projDir i = root </> ("p" ++ show i)
      build i =
        run
          (Just (projDir i))
          (testEnvironment env)
          cabalPath
          ["--store-dir=" ++ store, "build", "--builddir=" ++ projDir i </> "dist"]
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

  -- Make sure the shared store package DB is cold right before the burst.
  liftIO $ removePathForcibly store

  -- Build all projects concurrently against the cold shared store.
  results <- liftIO $ do
    slots <- forM ids $ \i -> do
      mv <- newEmptyMVar
      _ <- forkIO $ do
        r <- try (build i) :: IO (Either SomeException Result)
        putMVar mv (i, r)
      return mv
    forM slots $ \mv -> do
      (i, r) <- takeMVar mv
      either throwIO (\res -> pure (i, res)) r

  liftIO $ forM_ results $ \(i, r) -> do
    let out = resultOutput r
    assertBool
      ("cabal build for p" ++ show i ++ " hit the store package.db init race:\n" ++ out)
      (not ("already exists" `isInfixOf` out))
    assertEqual
      ("cabal build for p" ++ show i ++ " failed:\n" ++ out)
      ExitSuccess
      (resultExitCode r)
