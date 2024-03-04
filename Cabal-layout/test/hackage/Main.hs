{-# LANGUAGE OverloadedStrings
           , ScopedTypeVariables #-}

module Main where

import           Codec.Manifest.Cabal.Internal.Parse
import           Codec.Manifest.Cabal.Internal.Render
import           Codec.Manifest.Cabal.Internal.Version
import           Hackage.Cabal.Patches

import           Test.Strictness.Layout ()

import           Data.ByteString.Builder
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Unsafe as BS
import           Data.Foldable
import           Data.Function
import qualified Data.List as List
import           Data.Maybe
import           Data.Ord
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Encoding as Lazy
import           Foreign.Ptr
import           GHC.Fingerprint
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO
import           Text.Parsec
import           NoThunks.Class



main :: IO ()
main = do
  as <- getArgs
  case as of
    []     -> fail "Expecting path to the Hackage index as an argument"
    [path] -> hackage path
    _:_:_  -> fail "Too many arguments provided"



-- | Convert "\r\n" to "\n", and '\t' and '\160' to ' '.
normalize :: Lazy.Text -> Lazy.Text
normalize = go . Lazy.toChunks
  where
    go (b:bs) =
      let -- Whether or not the next chunk starts with a newline
          new
            | c:_ <- bs, Just ('\n', _) <- Text.uncons c = True
            | otherwise                                 = False

      in squeeze new b <> go bs

    go []     = Lazy.empty


    squeeze new b =
      let (before, after) = Text.break (\c -> c == '\t' || c == '\r' || c == '\160') b
      in case Text.uncons after of
           Just (c, rest) ->
             case c of
               '\t'   -> Lazy.fromStrict before <> " " <> squeeze new rest
               '\r'   ->
                 case Text.uncons rest of
                   Just (d, _) ->
                     case d of
                       '\n' -> Lazy.fromStrict before <> squeeze new rest
                       _    ->
                         Lazy.fromStrict before <> "\r" <> squeeze new rest

                   Nothing        ->
                     if new
                       then Lazy.fromStrict before
                       else Lazy.fromStrict before <> "\r"

               '\160' -> Lazy.fromStrict before <> " " <> squeeze new rest

               _      -> Lazy.fromStrict before <> Lazy.cons c (squeeze new rest)

           Nothing        -> Lazy.fromStrict b



-- | Fold two lists into one, combining elements from the second list
--   with matching entries from the first list.
--
--   Used over dumb dictionary lookups for performance: O(n) instead of O(n * log n).
arrange
  :: (a -> b -> Ordering)
  -> [a]
  -> [(b, [c])]
  -> [Either b (a, [c])]
arrange cmp = go
  where
    go as bs =
      case bs of
        (b, c) : bt -> align as
          where
            align xs =
              case xs of
                x:ys ->
                  case cmp x b of
                    LT -> Right (x, []) : go ys bs
                    EQ -> Right (x, c) : go ys bt
                    GT -> Left b : align ys

                []   -> []

        []          -> fmap (\x -> Right (x, [])) as



arrangePackages
  :: [String]
  -> [Either Package (String, [(Version, [(Revision, Patch)])])]
arrangePackages manifests =
  arrange
    (\name (Package pkg) -> Text.pack name `compare` Text.decodeASCII pkg)
    (List.sort manifests)
    (List.sortOn ((\(Package pkg) -> pkg) . fst) patches)

arrangeVersions
  :: [Version]
  -> [(Version, [(Revision, Patch)])]
  -> [Either Version (Version, [(Revision, Patch)])]
arrangeVersions versions verPatches =
  arrange
    (\(Version v0) (Version v1) -> compare v0 v1)
    (List.sortOn (\(Version ver) -> ver) versions)
    (List.sortOn ((\(Version ver) -> ver) . fst) verPatches)



-- | Check whether the file is a directory and then whether it's a proper Version.
--
--   Nothing is returned if it's not a directory.
checkVersion :: FilePath -> String -> String -> IO (Maybe Version)
checkVersion path manifest version = do
  exists <- doesDirectoryExist $ path </> manifest </> version
  if exists
    then case parse versionP "" $ Lazy.pack version of
           Left err  -> fail $ "Version " <> version <> " of package " <> manifest
                                                     <> " is unparseable:\n" <> show err
           Right ver -> pure $ Just ver

    else pure Nothing



checkMD5 :: BSC.ByteString -> IO Fingerprint
checkMD5 bs = BS.unsafeUseAsCStringLen bs $ \(ptr, len) ->
                fingerprintData (castPtr ptr) len



-- | Go through patches in order of descending revision, apply if MD5 hashes match
--   both for the original file and for its modified version respectively.
applyPatches
  :: [(Revision, Patch)] -> BSLC.ByteString -> IO BSLC.ByteString
applyPatches patches_ bs =
  go $ List.sortBy (compare `on` \(Revision rev, _) -> Down rev) patches_
  where
    go ((Revision rev, Patch origRef modRef patch):xs) = do
      ref0 <- checkMD5 $ BSLC.toStrict bs
      if ref0 /= origRef
        then do
          putStrLn $ "Revision " <> show rev <> " patch does not match the original file"
          go xs

        else do
          let modified = patch bs
          ref1 <- checkMD5 $ BSLC.toStrict modified
          if ref1 /= modRef
            then do
              putStrLn $ "Revision " <> show rev <> " patch matches the original file, "
                                                 <> "however the modified file does not"
              go xs

            else do
              putStrLn $ "Using patched version of the manifest under revision "
                                                              <> show rev
              pure modified

    go [] = pure bs



hackage :: FilePath -> IO ()
hackage path = do
  manifests <- listDirectory path
  for_ (arrangePackages manifests) $ \pkgpoint ->
    case pkgpoint of
      Left (Package pkg) ->
        putStrLn $ "Package " <> BSC.unpack pkg <> " is not a part of the index"

      Right (manifest, verPatches) -> do
        rawVersions <- listDirectory $ path </> manifest
        mayVersions <- traverse (checkVersion path manifest) rawVersions
        for_ (arrangeVersions (catMaybes mayVersions) verPatches) $ \verpoint ->
          case verpoint of
            Left version ->
              putStrLn $ "Version " <> show version <> " of package " <> manifest
                                                    <> " is not a part of the index"

            Right (version, patches_) -> do
              let rawVersion = BSLC.unpack . toLazyByteString $ versionB version
                  filepath = path </> manifest </> rawVersion </> manifest <.> "cabal"

              putStrLn filepath

              withBinaryFile filepath ReadMode $ \h -> do
                file <- BSLC.hGetContents h

                file' <- applyPatches patches_ file

                let utf8File = normalize $ Lazy.decodeUtf8 file'

                case parse layoutP "" utf8File of
                  Left err -> do
                    fail $ "Layout could not be parsed:\n" <> show err

                  Right layout -> do
                    mayThunks <- wNoThunks [] layout
                    case mayThunks of
                      Just (ThunkInfo ctx) -> do
                        fail $ "Layout is not fully evaluated: " <> show (reverse ctx)

                      Nothing  ->
                        if utf8File == Lazy.decodeUtf8 (toLazyByteString $ layoutB layout)
                          then putStrLn "Correct"
                          else fail "Rendered layout is not the same as the original file"
