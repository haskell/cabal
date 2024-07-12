{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Client.Utils.Parsec
  ( renderParseError
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

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Distribution.Client.Types.Repo
import Distribution.Client.Types.RepoName
import Distribution.Client.Utils.Newtypes
import Distribution.FieldGrammar
import Distribution.Parsec
import Distribution.Simple.Flag
import Distribution.Simple.Utils (fromUTF8BS)
import Distribution.Utils.NubList (NubList (..))
import qualified Distribution.Utils.NubList as NubList

-- | Render parse error highlighting the part of the input file.
renderParseError
  :: FilePath
  -> BS.ByteString
  -> NonEmpty PError
  -> [PWarning]
  -> String
renderParseError filepath contents errors warnings =
  unlines $
    [ "Errors encountered when parsing cabal file " <> filepath <> ":"
    ]
      ++ renderedErrors
      ++ renderedWarnings
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

    renderedErrors = concatMap renderError errors
    renderedWarnings = concatMap renderWarning warnings

    renderError :: PError -> [String]
    renderError (PError pos@(Position row col) msg)
      -- if position is 0:0, then it doesn't make sense to show input
      -- looks like, Parsec errors have line-feed in them
      | pos == zeroPos = msgs
      | otherwise = msgs ++ formatInput row col
      where
        msgs = ["", filepath' ++ ":" ++ showPos pos ++ ": error:", trimLF msg, ""]

    renderWarning :: PWarning -> [String]
    renderWarning (PWarning _ pos@(Position row col) msg)
      | pos == zeroPos = msgs
      | otherwise = msgs ++ formatInput row col
      where
        msgs = ["", filepath' ++ ":" ++ showPos pos ++ ": warning:", trimLF msg, ""]

    -- sometimes there are (especially trailing) newlines.
    trimLF :: String -> String
    trimLF = dropWhile (== '\n') . reverse . dropWhile (== '\n') . reverse

    -- format line: prepend the given line number
    formatInput :: Int -> Int -> [String]
    formatInput row col = case advance (row - 1) rowsZipper of
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

remoteRepoGrammar :: RepoName -> ParsecFieldGrammar RemoteRepo RemoteRepo
remoteRepoGrammar name =
  RemoteRepo
    <$> pure name
    -- <*> uniqueFieldAla "url" URI_NT undefined -- remoteRepoURI "url"
    <*> undefined
    <*> undefined -- remoteRepoSecure "secure"
    <*> undefined -- remoteRepoRootKeys "root-keys"
    <*> undefined -- remoteRepoKeyThreshold "key-threshold"
    <*> undefined -- remoteRepoShouldTryHttps --nope
