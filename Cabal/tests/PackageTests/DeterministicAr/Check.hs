{-# LANGUAGE OverloadedStrings #-}

module PackageTests.DeterministicAr.Check where

import Control.Monad
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Data.Char (isSpace)
import Data.List
import Data.Traversable
import PackageTests.PackageTester
import System.Exit
import System.FilePath
import System.IO
import Test.HUnit (Assertion, Test (TestCase), assertFailure)

-- Perhaps these should live in PackageTester.

-- For a polymorphic @IO a@ rather than @Assertion = IO ()@.
assertFailure' :: String -> IO a
assertFailure' msg = assertFailure msg >> return {-unpossible!-}undefined

ghcPkg_field :: String -> String -> FilePath -> IO [FilePath]
ghcPkg_field libraryName fieldName ghcPkgPath = do
    (cmd, exitCode, raw) <- run Nothing ghcPkgPath
        ["--user", "field", libraryName, fieldName]
    let output = filter ('\r' /=) raw -- Windows
    -- copypasta of PackageTester.requireSuccess
    unless (exitCode == ExitSuccess) . assertFailure $
        "Command " ++ cmd ++ " failed.\n" ++ "output: " ++ output

    let prefix = fieldName ++ ": "
    case traverse (stripPrefix prefix) (lines output) of
        Nothing -> assertFailure' $ "Command " ++ cmd ++ " failed: expected "
            ++ show prefix ++ " prefix on every line.\noutput: " ++ output
        Just fields -> return fields

ghcPkg_field1 :: String -> String -> FilePath -> IO FilePath
ghcPkg_field1 libraryName fieldName ghcPkgPath = do
    fields <- ghcPkg_field libraryName fieldName ghcPkgPath
    case fields of
        [field] -> return field
        _ -> assertFailure' $ "Command ghc-pkg field failed: "
            ++ "output not a single line.\noutput: " ++ show fields

------------------------------------------------------------------------

this :: String
this = "DeterministicAr"

suite :: FilePath -> FilePath -> Test
suite ghcPath ghcPkgPath = TestCase $ do
    let dir = "PackageTests" </> this
    let spec = PackageSpec dir []

    unregister this ghcPkgPath
    iResult <- cabal_install spec ghcPath
    assertInstallSucceeded iResult

    let distBuild = dir </> "dist" </> "build"
    libdir <- ghcPkg_field1 this "library-dirs" ghcPkgPath
    mapM_ checkMetadata [distBuild, libdir]
    unregister this ghcPkgPath

-- Almost a copypasta of Distribution.Simple.Program.Ar.wipeMetadata
checkMetadata :: FilePath -> Assertion
checkMetadata dir = withBinaryFile path ReadMode $ \ h -> do
    hFileSize h >>= checkArchive h
  where
    path = dir </> "libHS" ++ this ++ "-0.a"

    checkError msg = assertFailure' $
        "PackageTests.DeterministicAr.checkMetadata: " ++ msg ++
        " in " ++ path
    archLF = "!<arch>\x0a" -- global magic, 8 bytes
    x60LF = "\x60\x0a" -- header magic, 2 bytes
    metadata = BS.concat
        [ "0           " -- mtime, 12 bytes
        , "0     " -- UID, 6 bytes
        , "0     " -- GID, 6 bytes
        , "0644    " -- mode, 8 bytes
        ]
    headerSize = 60

    -- http://en.wikipedia.org/wiki/Ar_(Unix)#File_format_details
    checkArchive :: Handle -> Integer -> IO ()
    checkArchive h archiveSize = do
        global <- BS.hGet h (BS.length archLF)
        unless (global == archLF) $ checkError "Bad global header"
        checkHeader (toInteger $ BS.length archLF)

      where
        checkHeader :: Integer -> IO ()
        checkHeader offset = case compare offset archiveSize of
            EQ -> return ()
            GT -> checkError (atOffset "Archive truncated")
            LT -> do
                header <- BS.hGet h headerSize
                unless (BS.length header == headerSize) $
                    checkError (atOffset "Short header")
                let magic = BS.drop 58 header
                unless (magic == x60LF) . checkError . atOffset $
                    "Bad magic " ++ show magic ++ " in header"

                unless (metadata == BS.take 32 (BS.drop 16 header))
                    . checkError . atOffset $ "Metadata has changed"

                let size = BS.take 10 $ BS.drop 48 header
                objSize <- case reads (BS8.unpack size) of
                    [(n, s)] | all isSpace s -> return n
                    _ -> checkError (atOffset "Bad file size in header")

                let nextHeader = offset + toInteger headerSize +
                        -- Odd objects are padded with an extra '\x0a'
                        if odd objSize then objSize + 1 else objSize
                hSeek h AbsoluteSeek nextHeader
                checkHeader nextHeader

          where
            atOffset msg = msg ++ " at offset " ++ show offset
