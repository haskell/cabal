{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Codec.Manifest.Cabal.Internal.Parse
import           Codec.Manifest.Cabal.Internal.Render

import           Test.Strictness.Layout ()

import           Data.ByteString.Builder
import           Data.Foldable
import qualified Data.Text.Lazy as Lazy
import           Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy.IO as Lazy
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



withCabalFile :: FilePath -> (Lazy.Text -> IO a) -> IO a
withCabalFile path f =
  withFile path ReadMode $ \h -> do
    hSetEncoding h utf8
    hSetNewlineMode h universalNewlineMode
    file <- Lazy.hGetContents h

    let detab c = case c of
                    '\t'   -> ' '
                    '\160' -> ' ' -- non-breaking space
                    _      -> c

    f $ Lazy.map detab file



hackage :: FilePath -> IO ()
hackage path = do
  manifests <- listDirectory path
  for_ manifests $ \manifest -> do
    versions <- listDirectory $ path </> manifest
    for_ versions $ \version ->
      if version == "preferred-versions"
        then pure ()
        else do
          let filepath = (path </> manifest) </> version </> manifest <.> "cabal"

          putStr filepath
          hFlush stdout

          withCabalFile filepath $ \file -> do
            case parse layoutP "" file of
              Left err -> do
                putStrLn " ✗"
                fail $ show err

              Right layout -> do
                mayThunks <- wNoThunks [] layout
                case mayThunks of
                  Just (ThunkInfo ctx) -> do
                    putStrLn " ✗"
                    fail $ "Not fully evaluated: " <> show (reverse ctx)

                  Nothing  ->
                    if file == decodeUtf8 (toLazyByteString $ layoutB layout)
                      then putStrLn " ✓"
                      else do
                        putStrLn " ✗"
                        fail "Layout mismatch"
