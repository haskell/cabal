import qualified Distribution.ModuleName               as ModuleName
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parse
                 (ParseResult (..), parseGenericPackageDescription)
import           Distribution.Verbosity                (silent)

import Control.Monad      (liftM, filterM)
import Data.List          (isPrefixOf, isSuffixOf, sort)
import System.Directory   (canonicalizePath, doesFileExist, setCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.FilePath    ((</>), takeDirectory, takeExtension, takeFileName)
import System.Process     (readProcess)

import qualified System.IO as IO

main' :: FilePath -> IO ()
main' fp' = do
    fp <- canonicalizePath fp'
    setCurrentDirectory (takeDirectory fp)

    -- Read cabal file, so we can determine test modules
    contents <- strictReadFile fp
    cabal <- case parseGenericPackageDescription contents of
        ParseOk _ x      -> pure x
        ParseFailed errs -> fail (show errs)

    -- We skip some files
    testModuleFiles    <- getOtherModulesFiles cabal
    let skipPredicates' = skipPredicates ++ map (==) testModuleFiles

    -- Read all files git knows about under "tests"
    files0 <- lines <$> readProcess "git" ["ls-files", "tests"] ""

    -- Filter
    let files1 = filter (\f -> takeExtension f `elem` whitelistedExtensionss ||
                               takeFileName f `elem` whitelistedFiles)
                        files0
    let files2 = filter (\f -> not $ any ($ f) skipPredicates') files1
    let files3 = sort files2
    let files = files3

    -- Read current file
    let inputLines  = lines contents
        linesBefore = takeWhile (/= topLine) inputLines
        linesAfter  = dropWhile (/= bottomLine) inputLines

    -- Output
    let outputLines = linesBefore ++ [topLine] ++ map ("  " ++) files ++ linesAfter
    writeFile fp (unlines outputLines)


topLine, bottomLine :: String
topLine = "  -- BEGIN gen-extra-source-files"
bottomLine = "  -- END gen-extra-source-files"

whitelistedFiles :: [FilePath]
whitelistedFiles = [ "ghc", "ghc-pkg", "ghc-7.10"
                   , "ghc-pkg-7.10", "ghc-pkg-ghc-7.10" ]

whitelistedExtensionss :: [String]
whitelistedExtensionss = map ('.' : )
    [ "hs", "lhs", "c", "h", "sh", "cabal", "hsc"
    , "err", "out", "in", "project", "format", "errors", "expr"
    , "check"
    ]

getOtherModulesFiles :: GenericPackageDescription -> IO [FilePath]
getOtherModulesFiles gpd = do
  mainModules   <- liftM concat . mapM findMainModules  $ testSuites
  otherModules' <- liftM concat . mapM findOtherModules $ testSuites

  return $ mainModules ++ otherModules'
  where
    testSuites :: [TestSuite]
    testSuites = map (foldMap id . snd) (condTestSuites gpd)

    findMainModules, findOtherModules :: TestSuite -> IO [FilePath]
    findMainModules  ts = findModules (mainModule . testInterface $ ts) ts
    findOtherModules ts =
      findModules (map fromModuleName . otherModules . testBuildInfo $ ts) ts

    findModules :: [FilePath] -> TestSuite -> IO [FilePath]
    findModules filenames ts = filterM doesFileExist
                               [ d </> f | d <- locations, f <- filenames ]
      where locations = hsSourceDirs . testBuildInfo $ ts

    fromModuleName mn = ModuleName.toFilePath mn ++ ".hs"

    mainModule (TestSuiteLibV09 _ mn) = [fromModuleName mn]
    mainModule (TestSuiteExeV10 _ fp) = [fp]
    mainModule _                      = []

skipPredicates :: [FilePath -> Bool]
skipPredicates =
    [ isSuffixOf "register.sh"
    ]

main :: IO ()
main = do
    args <- getArgs
    case args of
        [fp] -> main' fp
        _    -> do
            progName <- getProgName
            putStrLn "Error too few arguments!"
            putStrLn $ "Usage: " ++ progName ++ " FILE"
            putStrLn $ "  where FILE is Cabal.cabal, cabal-testsuite.cabal, "
              ++ "or cabal-install.cabal"

strictReadFile :: FilePath -> IO String
strictReadFile fp = do
    handle <- IO.openFile fp IO.ReadMode
    contents <- get handle
    IO.hClose handle
    return contents
  where
    get h = IO.hGetContents h >>= \s -> length s `seq` return s
