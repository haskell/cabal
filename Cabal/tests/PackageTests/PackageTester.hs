module PackageTests.PackageTester (
        PackageSpec(..),
        Success(..),
        Result(..),
        cabal_configure,
        cabal_build,
        cabal_test,
        cabal_bench,
        cabal_install,
        unregister,
        run
    ) where

import qualified Control.Exception.Extensible as E
import System.Directory
import System.FilePath
import System.IO
import System.Posix.IO
import System.Process
import System.Exit
import Control.Concurrent.Chan
import Control.Concurrent.MVar
import Control.Concurrent
import Control.Monad
import Data.List
import Data.Maybe
import qualified Data.ByteString.Char8 as C


data PackageSpec =
    PackageSpec {
        directory  :: FilePath,
        configOpts :: [String]
    }

data Success = Failure
             | ConfigureSuccess
             | BuildSuccess
             | InstallSuccess
             | TestSuccess
             | BenchSuccess
             deriving (Eq, Show)

data Result = Result {
        successful :: Bool,
        success    :: Success,
        outputText :: String
    }
    deriving Show

nullResult :: Result
nullResult = Result True Failure ""

recordRun :: (String, ExitCode, String) -> Success -> Result -> Result
recordRun (cmd, exitCode, exeOutput) thisSucc res =
    res {
        successful = successful res && exitCode == ExitSuccess,
        success = if exitCode == ExitSuccess then thisSucc
                                             else success res,
        outputText =
            (if null $ outputText res then "" else outputText res ++ "\n") ++
                cmd ++ "\n" ++ exeOutput
    }

cabal_configure :: PackageSpec -> IO Result
cabal_configure spec = do
    res <- doCabalConfigure spec
    record spec res
    return res

doCabalConfigure :: PackageSpec -> IO Result
doCabalConfigure spec = do
    cleanResult@(_, _, cleanOutput) <- cabal spec ["clean"]
    requireSuccess cleanResult
    res <- cabal spec $ ["configure", "--user"] ++ configOpts spec
    return $ recordRun res ConfigureSuccess nullResult

doCabalBuild :: PackageSpec -> IO Result
doCabalBuild spec = do
    configResult <- doCabalConfigure spec
    if successful configResult
        then do
            res <- cabal spec ["build"]
            return $ recordRun res BuildSuccess configResult
        else
            return configResult

cabal_build :: PackageSpec -> IO Result
cabal_build spec = do
    res <- doCabalBuild spec
    record spec res
    return res

unregister :: String -> IO ()
unregister libraryName = do
    res@(_, _, output) <- run Nothing "ghc-pkg" ["unregister", "--user", libraryName]
    if "cannot find package" `isInfixOf` output
        then return ()
        else requireSuccess res

-- | Install this library in the user area
cabal_install :: PackageSpec -> IO Result
cabal_install spec = do
    buildResult <- doCabalBuild spec
    res <- if successful buildResult
        then do
            res <- cabal spec ["install"]
            return $ recordRun res InstallSuccess buildResult
        else
            return buildResult
    record spec res
    return res

cabal_test :: PackageSpec -> [String] -> IO Result
cabal_test spec extraArgs = do
    res <- cabal spec $ "test" : extraArgs
    let r = recordRun res TestSuccess nullResult
    record spec r
    return r

cabal_bench :: PackageSpec -> [String] -> IO Result
cabal_bench spec extraArgs = do
    res <- cabal spec $ "bench" : extraArgs
    let r = recordRun res BenchSuccess nullResult
    record spec r
    return r

-- | Returns the command that was issued, the return code, and hte output text
cabal :: PackageSpec -> [String] -> IO (String, ExitCode, String)
cabal spec cabalArgs = do
    wd <- getCurrentDirectory
    r <- run (Just $ directory spec) "ghc"
             [ "--make"
             , "-fhpc"
             , "-package-conf " ++ wd </> "../dist/package.conf.inplace"
             , "Setup.hs"
             ]
    requireSuccess r
    run (Just $ directory spec) (wd </> directory spec </> "Setup") cabalArgs

-- | Returns the command that was issued, the return code, and hte output text
run :: Maybe FilePath -> String -> [String] -> IO (String, ExitCode, String)
run cwd cmd args = do
    -- Posix-specific
    (outf, outf0) <- createPipe
    (errf, errf0) <- createPipe
    outh <- fdToHandle outf
    outh0 <- fdToHandle outf0
    errh <- fdToHandle errf
    errh0 <- fdToHandle errf0
    pid <- runProcess cmd args cwd Nothing Nothing (Just outh0) (Just errh0)

    {-
    -- ghc-6.10.1 specific
    (Just inh, Just outh, Just errh, pid) <-
        createProcess (proc cmd args){ std_in  = CreatePipe,
                                       std_out = CreatePipe,
                                       std_err = CreatePipe,
                                       cwd     = cwd }
    hClose inh -- done with stdin
    -}

    -- fork off a thread to start consuming the output
    outChan <- newChan
    forkIO $ suckH outChan outh
    forkIO $ suckH outChan errh

    output <- suckChan outChan

    hClose outh
    hClose errh

    -- wait on the process
    ex <- waitForProcess pid
    let fullCmd = intercalate " " $ cmd:args
    return ("\"" ++ fullCmd ++ "\" in " ++ fromMaybe "" cwd,
        ex, output)
  where
    suckH chan h = do
        eof <- hIsEOF h
        if eof
            then writeChan chan Nothing
            else do
                c <- hGetChar h
                writeChan chan $ Just c
                suckH chan h
    suckChan chan = sc' chan 2 []
      where
        sc' _ 0 acc = return $ reverse acc
        sc' chan eofs acc = do
            mC <- readChan chan
            case mC of
                Just c -> sc' chan eofs (c:acc)
                Nothing -> sc' chan (eofs-1) acc

requireSuccess :: (String, ExitCode, String) -> IO ()
requireSuccess (cmd, exitCode, output) = do
    case exitCode of
        ExitSuccess -> return ()
        ExitFailure r -> do
            ioError $ userError $ "Command " ++ cmd ++ " failed."

record :: PackageSpec -> Result -> IO ()
record spec res = do
    C.writeFile (directory spec </> "test-log.txt") (C.pack $ outputText res)

