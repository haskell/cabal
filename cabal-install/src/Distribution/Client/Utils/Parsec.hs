{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Client.Utils.Parsec
  ( renderParseError

    -- ** Flag
  , alaFlag
  , Flag'

    -- ** NubList
  , alaNubList
  , alaNubList'
  , NubList'

    -- ** Newtype wrappers
  , NumJobs (..)
  , PackageDBNT (..)
  , ProjectConstraints (..)
  , MaxBackjumps (..)
  ) where

import Distribution.Client.Compat.Prelude
import Distribution.Client.Targets (UserConstraint)
import Distribution.Compat.Newtype
import Distribution.Solver.Types.ConstraintSource (ConstraintSource (..))
import System.FilePath (normalise)
import Prelude ()

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import Distribution.Compat.CharParsing
import Distribution.FieldGrammar.Newtypes
import Distribution.Parsec (PError (..), PWarnType (..), PWarning (..), Position (..), parsecToken, parsecWarning, showPos, zeroPos)
import Distribution.Simple.Compiler (PackageDB (..), readPackageDb)
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

-- | We can't write a Parsec instance for Maybe PackageDB. We need to wrap it in a newtype and define the instance.
newtype PackageDBNT = PackageDBNT {getPackageDBNT :: Maybe PackageDB}

instance Newtype (Maybe PackageDB) PackageDBNT

instance Parsec PackageDBNT where
  parsec = parsecPackageDB

parsecPackageDB :: CabalParsing m => m PackageDBNT
parsecPackageDB = PackageDBNT . readPackageDb <$> parsecToken

-- | We can't write a Parsec instance for Maybe Int. We need to wrap it in a newtype and define the instance.
newtype NumJobs = NumJobs {getNumJobs :: Maybe Int}

instance Newtype (Maybe Int) NumJobs

instance Parsec NumJobs where
  parsec = parsecNumJobs

parsecNumJobs :: CabalParsing m => m NumJobs
parsecNumJobs = ncpus <|> numJobs
  where
    ncpus = string "$ncpus" >> return (NumJobs Nothing)
    numJobs = do
      num <- integral
      if num < (1 :: Int)
        then do
          parsecWarning PWTOther "The number of jobs should be 1 or more."
          return (NumJobs Nothing)
        else return (NumJobs $ Just num)

newtype ProjectConstraints = ProjectConstraints {getProjectConstraints :: (UserConstraint, ConstraintSource)}

instance Newtype (UserConstraint, ConstraintSource) ProjectConstraints

instance Parsec ProjectConstraints where
  parsec = parsecProjectConstraints

-- | Parse 'ProjectConstraints'. As the 'CabalParsing' class does not have access to the file we parse,
-- ConstraintSource is first unknown and we set it afterwards
parsecProjectConstraints :: CabalParsing m => m ProjectConstraints
parsecProjectConstraints = do
  userConstraint <- parsec
  return $ ProjectConstraints (userConstraint, ConstraintSourceUnknown)

newtype MaxBackjumps = MaxBackjumps {getMaxBackjumps :: Int}

instance Newtype Int MaxBackjumps

instance Parsec MaxBackjumps where
  parsec = parseMaxBackjumps

parseMaxBackjumps :: CabalParsing m => m MaxBackjumps
parseMaxBackjumps = MaxBackjumps <$> integral
