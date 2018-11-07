{-# LANGUAGE OverloadedStrings #-}

import qualified Distribution.ModuleName               as ModuleName
import           Distribution.PackageDescription
import           Distribution.PackageDescription.Parsec
                 (parseGenericPackageDescription, runParseResult)
import           Distribution.Verbosity                (silent)

import Control.Monad      (liftM, filterM)
import Data.List          (isPrefixOf, isSuffixOf, sort)
import System.Directory   (canonicalizePath, doesFileExist, setCurrentDirectory)
import System.Environment (getArgs, getProgName)
import System.FilePath    ((</>), takeDirectory, takeExtension, takeFileName)
import System.Process     (readProcess)


import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified System.IO             as IO

main' :: FilePath -> FilePath -> IO ()
main' templateFp fp' = do
    fp <- canonicalizePath fp'
    setCurrentDirectory (takeDirectory fp)
    print $ takeDirectory fp

    -- Read cabal file, so we can determine test modules
    contents <- BS.readFile fp
    cabal <-
      case snd . runParseResult . parseGenericPackageDescription $ contents of
        Right x            -> pure x
        Left (_mver, errs) -> fail (show errs)

    -- We skip some files
    testModuleFiles    <- getOtherModulesFiles cabal
    let skipPredicates' = skipPredicates ++ map (==) testModuleFiles
    print testModuleFiles

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
    templateContents <- BS.readFile templateFp
    let topLine'    = BS8.pack topLine
        bottomLine' = BS8.pack bottomLine
        inputLines  = BS8.lines templateContents
        linesBefore = takeWhile (/= topLine')    inputLines
        linesAfter  = dropWhile (/= bottomLine') inputLines

    -- Output
    let outputLines = linesBefore ++ [topLine']
                      ++ map ((<>) "  " . BS8.pack) files ++ linesAfter
    BS.writeFile templateFp (BS8.unlines outputLines)


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
        [fp]     -> main' fp fp
        [fp,fp'] -> main' fp fp'
        _        -> do
            progName <- getProgName
            putStrLn "Error too few arguments!"
            putStrLn $ "Usage: " ++ progName ++ " <FILE | FILE CABAL>"
            putStrLn $ "  where FILE is Cabal.cabal, cabal-testsuite.cabal, "
              ++ "or cabal-install.cabal"
