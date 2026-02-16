import qualified Control.Exception as E (IOException, catch)
import Control.Monad (unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Reader (ask)
import Data.Maybe (catMaybes)

import qualified Distribution.Verbosity as Verbosity

import Test.Cabal.Prelude

main = do
  skipIfCIAndOSX 4902
  cabalTest $ do
    hasShared   <- hasSharedLibraries
    hasProfiled <- hasProfiledLibraries
    hpcOk       <- correctHpcVersion

    forM_ (choose4 [True, False]) $ \(libProf, exeProf, exeDyn, shared) ->
      do
        let
          opts = catMaybes
              [ enable libProf "library-profiling"
              , enable exeProf "profiling"
              , enable exeDyn "executable-dynamic"
              , enable shared "shared"
              ]
            where
              enable cond flag
                | cond = Just $ "--enable-" ++ flag
                | otherwise = Nothing
          args = "test-Short" : "--enable-coverage" : opts
        recordMode DoNotRecord $ do
          let
            skip =
                not hpcOk
                || (not hasShared && (exeDyn || shared))
                || (not hasProfiled && (libProf || exeProf))
          unless skip $ cabal "v2-test" args
  where
    choose4 :: [a] -> [(a, a, a, a)]
    choose4 xs = liftM4 (,,,) xs xs xs xs

-- | Checks for a suitable HPC version for testing.
correctHpcVersion :: TestM Bool
correctHpcVersion = do
    let verbosity = Verbosity.mkVerbosity Verbosity.defaultVerbosityHandles Verbosity.normal
        verRange  = orLaterVersion (mkVersion [0,7])
    progDB <- testProgramDb `fmap` ask
    liftIO $ (requireProgramVersion verbosity hpcProgram verRange progDB
              >> return True) `catchIO` (\_ -> return False)
  where
    -- Distribution.Compat.Exception is hidden.
    catchIO :: IO a -> (E.IOException -> IO a) -> IO a
    catchIO = E.catch
