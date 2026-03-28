{-# LANGUAGE LambdaCase #-}

-- | A test program to check that ghc has got all of its extensions registered
-- with `KnownExtension` of Cabal-syntax.
module Main where

import Distribution.Compat.Prelude
import Distribution.Pretty (prettyShow)
import Distribution.Simple.Utils (rawSystemStdout)
import Distribution.Text (display, simpleParse)
import Distribution.Verbosity (Verbosity (..), defaultVerbosityHandles, normal)
import Language.Haskell.Extension (Extension (..), knownLanguages)

import Data.List ((\\))
import System.Environment (getArgs, getProgName)

-- | Language editions as Extensions.
--
-- >>> langsAsExts
-- [UnknownExtension "Haskell98",UnknownExtension "Haskell2010",UnknownExtension "GHC2021",UnknownExtension "GHC2024"]
--
-- Both of the following calls to @ghc@ return the same set of results but we
-- want to separate `Language` editions from other extensions (both enabled and
-- disabled) so we need a list of `knownLanguages` as unknown extensions that we
-- can then use to filter out those languages.
--
-- @

-- $ ghc --supported-languages
-- Haskell98
-- Haskell2010
-- GHC2021
-- GHC2024
-- Unsafe
-- Trustworthy
-- Safe
-- CPP
-- NoCPP
-- ...

-- $ ghc --supported-extensions
-- Haskell98
-- Haskell2010
-- GHC2021
-- GHC2024
-- Unsafe
-- Trustworthy
-- Safe
-- CPP
-- NoCPP
-- ...
-- @
--
-- If we're missing a language edition from `knownLanguages` then we'll notice
-- this omission as it will appear in the unregistered list.
langsAsExts :: [Extension]
langsAsExts = map (readExtension . prettyShow) knownLanguages

checkProblems :: [Extension] -> [String]
checkProblems implemented =
  -- Extensions that ghc knows about but that are not registered except for the known languages.
  let unregistered = [ext | ext <- implemented, not (registered ext), ext `notElem` langsAsExts]

      -- check if someone has forgotten to update the `langsAsExts` exceptions list...
      badExceptions = langsAsExts \\ implemented

      -- exceptions that are now registered
      badExceptions' = filter registered langsAsExts
   in catMaybes
        [ check unregistered $
            unlines
              [ "The following extensions are known to GHC but are not in the "
              , "extension registry in Language.Haskell.Extension."
              , "  " ++ intercalate "\n  " (map display unregistered)
              , "All extensions should be registered, even experimental extensions."
              ]
        , check badExceptions $
            unlines
              [ "Error in the extension exception list. The following extensions"
              , "are listed as exceptions but are not even implemented by GHC:"
              , "  " ++ intercalate "\n  " (map display badExceptions)
              , "Please fix this test program by correcting the list of"
              , "exceptions."
              ]
        , check badExceptions' $
            unlines
              [ "Error in the extension exception list. The following extensions"
              , "are listed as exceptions to registration but they are in fact"
              , "now registered in Language.Haskell.Extension:"
              , "  " ++ intercalate "\n  " (map display badExceptions')
              , "Please fix this test program by correcting the list of"
              , "exceptions."
              ]
        ]
  where
    registered UnknownExtension{} = False
    registered EnableExtension{} = True
    registered DisableExtension{} = True

    check [] _ = Nothing
    check _ i = Just i

main :: IO a
main = do
  getArgs >>= \case
    [ghcPath] -> do
      exts <- getExtensions ghcPath
      let problems = checkProblems exts
      putStrLn (intercalate "\n" problems)
      if null problems
        then exitSuccess
        else exitFailure
    args -> do
      n <- getProgName
      putStrLn $ "Error: Got " ++ show (length args) ++ " arguments" ++ if null args then "." else ": " ++ show args ++ "."
      putStrLn $ "Usage: Supply the path to ghc as a single argument to " ++ n ++ "."
      exitFailure

getExtensions :: FilePath -> IO [Extension]
getExtensions ghcPath =
  map readExtension . lines
    <$> rawSystemStdout (Verbosity normal defaultVerbosityHandles) ghcPath ["--supported-languages"]

-- | Reads extensions. Anything unknown becomes an `UnknownExtension`.
--
-- >>> readExtension "Haskell98"
-- UnknownExtension "Haskell98"
readExtension :: String -> Extension
readExtension str = handleNoParse $ do
  -- GHC defines extensions in a positive way, Cabal defines them
  -- relative to H98 so we try parsing ("No" ++ extName) first
  ext <- simpleParse ("No" ++ str)
  case ext of
    UnknownExtension _ -> simpleParse str
    _ -> return ext
  where
    handleNoParse :: Maybe Extension -> Extension
    handleNoParse = fromMaybe (error $ "unparsable extension " ++ show str)
