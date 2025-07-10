{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Client.Utils.Parsec
  ( renderParseError
  , renderParseErrorCabalFile
  , renderCabalParserSourceMsgs
  , renderParseErrorFile
  , remoteRepoGrammar

    -- ** Flag
  , alaFlag
  , Flag'

    -- ** NubList
  , alaNubList
  , alaNubList'
  , NubList'

    -- ** Newtype wrappers
  , module Distribution.Client.Utils.Newtypes
  ) where

import Distribution.Client.Compat.Prelude
import Distribution.Compat.Newtype
import System.FilePath (normalise)
import Prelude ()

import qualified Data.ByteString.Char8 as BS8
import Distribution.Client.Types.Repo
import Distribution.Client.Types.RepoName
import Distribution.Client.Utils.Newtypes
import Distribution.FieldGrammar
import Distribution.Parsec
import Distribution.Parsec.Source
import Distribution.Simple.Flag
import Distribution.Simple.Utils (fromUTF8BS)
import Distribution.Utils.NubList (NubList (..))
import qualified Distribution.Utils.NubList as NubList
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Map

renderParseErrorCabalFile :: NonEmpty (PErrorWithSource CabalParserSource) -> [PWarningWithSource CabalParserSource] -> String
renderParseErrorCabalFile errors warnings =
  renderParseError renderCabalParserSourceMsgs errors warnings
-- | Render parse error highlighting the part of the input file.
renderParseError
  :: forall src
  .  (Ord src, Eq src)
  => (src -> ([PError], [PWarning]) -> String)
  -> NonEmpty (PErrorWithSource src)
  -> [PWarningWithSource src]
  -> String
renderParseError display errors warnings =
  unlines [renderParseErrorsWarnings (fmap display source) ws | (source, ws) <- joinedErrorsWarningsList]
  where
    mkErrorGroup :: NonEmpty (PErrorWithSource src) -> (PSource src, [PError])
    mkErrorGroup (x :| xs) = (perrorSource x, perror x : map perror xs)

    mkWarningGroup :: NonEmpty (PWarningWithSource src) -> (PSource src, [PWarning])
    mkWarningGroup (x :| xs) = (pwarningSource x, pwarning x : map pwarning xs)

    groupedErrors =
      Map.fromListWith (++) $
      map mkErrorGroup $
      NE.groupBy (\a b -> perrorSource a == perrorSource b)
      errors
    groupedWarnings =
      Map.fromListWith (++) $
      map mkWarningGroup $
      NE.groupBy (\a b -> pwarningSource a == pwarningSource b)
      warnings

    joinedErrorsWarnings :: Map.Map (PSource src) ([PError], [PWarning])
    joinedErrorsWarnings = Map.merge (Map.mapMissing (\_ es -> (es, []))) (Map.mapMissing (\_ ps -> ([], ps))) (Map.zipWithMatched (\_ es ps -> (es, ps))) groupedErrors groupedWarnings

    joinedErrorsWarningsList = Map.toList joinedErrorsWarnings

renderParseErrorsWarnings :: PSource (([PError], [PWarning]) -> String) -> ([PError], [PWarning]) -> String
renderParseErrorsWarnings source (errors, warnings) =
  case source of
    PKnownSource src -> src (errors, warnings)
    PUnknownSource -> renderParseErrorNoFile "" errors warnings

renderCabalParserSourceMsgs :: CabalParserSource -> ([PError], [PWarning]) -> String
renderCabalParserSourceMsgs PInstalledPackageInfo (errors, warnings) =
  renderParseErrorNoFile "installed package info" errors warnings
renderCabalParserSourceMsgs (PCabalFile (fpath, contents)) (errors, warnings) =
  renderParseErrorFile "cabal" fpath Nothing contents (errors, warnings)

  --PProjectFile (fpath, contents) -> renderParseErrorFile "project" fpath contents errors warnings
  --PCabalFile (fpath, contents) -> renderParseErrorFile "cabal" fpath contents errors warnings
  --PInstalledPackageInfo -> renderParseErrorNoFile "installed package info" errors warnings

renderParseErrorNoFile :: String -> [PError] -> [PWarning] -> String
renderParseErrorNoFile herald errors warnings =
  renderParseErrorGeneral herald "" Nothing (const []) errors warnings



-- | Render a parse error which resulted from a file on disk
renderParseErrorFile :: String  -- ^ Human name for the kind of file (i.e. cabal, project "file")
                     -> FilePath  -- ^ Path to the file
                     -> Maybe String  -- ^ Provenance, any additional contextual info to print
                     -> BS8.ByteString  -- ^ Contents of the file
                     -> ([PError], [PWarning]) -> String
renderParseErrorFile herald filepath provenance contents (errors, warnings) =
  renderParseErrorGeneral (herald <> " file " <> filepath) (filepath' <> ":") provenance formatInput errors warnings
  where
    filepath' = normalise filepath

    -- lines of the input file. 'lines' is taken, so they are called rows
    -- contents, line number, whether it's empty line
    rows :: [(String, Int, Bool)]
    rows = zipWith f (BS8.lines contents) [1 ..]
      where
        f bs i = let s = fromUTF8BS bs in (s, i, isEmptyOrComment s)

    rowsZipper = listToZipper rows

    isEmptyOrComment :: String -> Bool
    isEmptyOrComment s = case dropWhile (== ' ') s of
      "" -> True -- empty
      ('-' : '-' : _) -> True -- comment
      _ -> False

    -- format line: prepend the given line number
    formatInput :: Position -> [String]
    formatInput (Position row col) = case advance (row - 1) rowsZipper of
      Zipper xs ys -> before ++ after
        where
          before = case span (\(_, _, b) -> b) xs of
            (_, []) -> []
            (zs, z : _) -> map formatInputLine $ z : reverse zs

          after = case ys of
            [] -> []
            (z : _zs) ->
              [ formatInputLine z -- error line
              , "      | " ++ replicate (col - 1) ' ' ++ "^" -- pointer: ^
              ]
    -- do we need rows after?
    -- ++ map formatInputLine (take 1 zs)           -- one row after

    formatInputLine :: (String, Int, Bool) -> String
    formatInputLine (str, row, _) = leftPadShow row ++ " | " ++ str

    -- hopefully we don't need to work with over 99999 lines .cabal files
    -- at that point small glitches in error messages are hopefully fine.
    leftPadShow :: Int -> String
    leftPadShow n = let s = show n in replicate (5 - length s) ' ' ++ s


-- | A generic rendering function which can render from many sources.
renderParseErrorGeneral :: String -> String -> Maybe String
                        -> (Position -> [String]) -- ^ Extra information to render based on the position
                        -> [PError] -> [PWarning] -> String
renderParseErrorGeneral header err_header provenance extra_info errors warnings =
  unlines $
    [ warningsOrErrors <> " encountered when parsing" <> header' <> ":"
    ] ++ [p | Just p <- [provenance]]
      ++ renderedErrors
      ++ renderedWarnings
  where
    warningsOrErrors = case errors of
      [] -> case warnings of
              [_] -> "Warning"
              _ -> "Warnings"
      [_] -> "Error"
      _ -> "Errors"

    header' = if null header then "" else (" " <> header)


    renderedErrors = concatMap renderError errors
    renderedWarnings = concatMap renderWarning warnings

    renderError :: PError -> [String]
    renderError (PError _ pos msg)
      -- if position is 0:0, then it doesn't make sense to show input
      -- looks like, Parsec errors have line-feed in them
      | pos == zeroPos = msgs
      | otherwise = msgs ++ extra_info pos
      where
        msgs = ["", err_header ++ showPos pos ++ ": error:", trimLF msg, ""]

    renderWarning :: PWarning -> [String]
    renderWarning (PWarning _ pos msg)
      | pos == zeroPos = msgs
      | otherwise = msgs ++ extra_info pos
      where
        msgs = ["", err_header ++ showPos pos ++ ": warning:", trimLF msg, ""]

    -- sometimes there are (especially trailing) newlines.
    trimLF :: String -> String
    trimLF = dropWhile (== '\n') . reverse . dropWhile (== '\n') . reverse


data Zipper a = Zipper [a] [a]

listToZipper :: [a] -> Zipper a
listToZipper = Zipper []

advance :: Int -> Zipper a -> Zipper a
advance n z@(Zipper xs ys)
  | n <= 0 = z
  | otherwise = case ys of
      [] -> z
      (y : ys') -> advance (n - 1) $ Zipper (y : xs) ys'

-- | Like 'List' for usage with a 'FieldGrammar', but for 'Flag'.
-- This enables to parse type aliases such as 'FilePath' that do not have 'Parsec' instances
-- by using newtype variants such as 'FilePathNT'.
-- For example, if you need to parse a 'Flag FilePath', you can use 'alaFlag' FilePathNT'.
newtype Flag' b a = Flag' {_getFlag :: Flag a}

-- | 'Flag'' constructor, with additional phantom argument to constrain the resulting type
alaFlag :: (a -> b) -> Flag a -> Flag' b a
alaFlag _ = Flag'

instance Newtype (Flag a) (Flag' wrapper a)

instance (Newtype a b, Parsec b) => Parsec (Flag' b a) where
  parsec = pack . toFlag . (unpack :: b -> a) <$> parsec

instance (Newtype a b, Pretty b) => Pretty (Flag' b a) where
  pretty = pretty . (pack :: a -> b) . fromFlag . unpack

-- | Like 'List' for usage with a 'FieldGrammar', but for 'NubList'.
newtype NubList' sep b a = NubList' {_getNubList :: NubList a}

-- | 'alaNubList' and 'alaNubList'' are simply 'NubList'' constructor, with additional phantom
-- arguments to constrain the resulting type
--
-- >>> :t alaNubList VCat
-- alaNubList VCat :: NubList a -> NubList' VCat (Identity a) a
--
-- >>> :t alaNubList' FSep Token
-- alaNubList' FSep Token
--   :: NubList String -> NubList' FSep Token String
--
-- >>> unpack' (alaNubList' FSep Token) <$> eitherParsec "foo bar foo"
-- Right ["foo","bar"]
alaNubList :: sep -> NubList a -> NubList' sep (Identity a) a
alaNubList _ = NubList'

-- | More general version of 'alaNubList'.
alaNubList' :: sep -> (a -> b) -> NubList a -> NubList' sep b a
alaNubList' _ _ = NubList'

instance Newtype (NubList a) (NubList' sep wrapper a)

instance (Newtype a b, Ord a, Sep sep, Parsec b) => Parsec (NubList' sep b a) where
  parsec = pack . NubList.toNubList . map (unpack :: b -> a) <$> parseSep (Proxy :: Proxy sep) parsec

instance (Newtype a b, Sep sep, Pretty b) => Pretty (NubList' sep b a) where
  pretty = prettySep (Proxy :: Proxy sep) . map (pretty . (pack :: a -> b)) . NubList.fromNubList . unpack

remoteRepoGrammar :: RepoName -> ParsecFieldGrammar src RemoteRepo RemoteRepo
remoteRepoGrammar name =
  RemoteRepo
    <$> pure name
    <*> uniqueFieldAla "url" URI_NT remoteRepoURILens
    <*> optionalField "secure" remoteRepoSecureLens
    <*> monoidalFieldAla "root-keys" (alaList' FSep Token) remoteRepoRootKeysLens
    <*> optionalFieldDefAla "key-threshold" KeyThreshold remoteRepoKeyThresholdLens 0
    <*> pure False -- we don't parse remoteRepoShouldTryHttps
