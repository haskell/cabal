{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
module Distribution.Utils.Path (
    -- * Symbolic path
    SymbolicPath,
    getSymbolicPath,
    sameDirectory,
    unsafeMakeSymbolicPath,
    parsecSymbolicPath,
    -- * Path ends
    PackageDir,
    SourceDir,
    LicenseFile,
    IsDir,
    -- * Utilities
    isGoodRelativeFilePath,
    isGoodRelativeDirectoryPath,
) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty

import qualified Distribution.Compat.CharParsing as P
-- import qualified Text.PrettyPrint                as Disp

-------------------------------------------------------------------------------
-- * SymbolicPath
-------------------------------------------------------------------------------

-- | Symbolic paths.
--
-- These paths are system independent and relative.
-- They are *symbolic* which means we cannot perform any 'IO'
-- until we interpret them.
--
newtype SymbolicPath from to = SymbolicPath FilePath
  deriving (Generic, Show, Read, Eq, Typeable, Data)

instance Binary (SymbolicPath from to)
instance (Typeable from, Typeable to) => Structured (SymbolicPath from to)
instance NFData (SymbolicPath from to) where rnf = genericRnf

-- | Extract underlying 'FilePath'.
--
-- Avoid using this in new code.
--
getSymbolicPath :: SymbolicPath from to -> FilePath
getSymbolicPath (SymbolicPath p) = p

sameDirectory :: (IsDir from, IsDir to) => SymbolicPath from to
sameDirectory = SymbolicPath "."

-- | Make 'SymbolicPath' without performing any checks.
unsafeMakeSymbolicPath :: FilePath -> SymbolicPath from to
unsafeMakeSymbolicPath = SymbolicPath

-------------------------------------------------------------------------------
-- ** Parsing and pretty printing
-------------------------------------------------------------------------------

instance KnownPathEndKind (PathEndKindFam to) => Parsec (SymbolicPath from to) where
    parsec = do
        token <- parsecToken
        parsecSymbolicPath token

instance Pretty (SymbolicPath from to) where
    pretty = showFilePath . getSymbolicPath

parsecSymbolicPath
    :: forall from to m. (KnownPathEndKind (PathEndKindFam to), CabalParsing m)
    => String
    -> m (SymbolicPath from to)
parsecSymbolicPath token
    | null token = P.unexpected "empty FilePath"
    | otherwise =  case pathEndKind :: SPathEndKind (PathEndKindFam to) of
        SKindDir  -> maybe (return (SymbolicPath token)) P.unexpected $ isGoodRelativeDirectoryPath token
        SKindFile -> maybe (return (SymbolicPath token)) P.unexpected $ isGoodRelativeFilePath token

-------------------------------------------------------------------------------
-- * Composition
-------------------------------------------------------------------------------

-- TODO
-- infixr 5 <//>
--
-- -- | Path composition
-- --
-- -- We don't reuse @</>@ name to not clash with "System.FilePath".
-- --
-- (<//>) :: path a b -> path b c -> path a c

-------------------------------------------------------------------------------
-- * Path ends
-------------------------------------------------------------------------------

data PathEndKind = KindDir | KindFile
  deriving (Eq, Show)

data SPathEndKind (k :: PathEndKind) where
    SKindDir  :: SPathEndKind 'KindDir
    SKindFile :: SPathEndKind 'KindFile

class    KnownPathEndKind (k :: PathEndKind) where pathEndKind :: SPathEndKind k
instance KnownPathEndKind 'KindDir           where pathEndKind = SKindDir
instance KnownPathEndKind 'KindFile          where pathEndKind = SKindFile

type family PathEndKindFam e :: PathEndKind

type IsDir e = PathEndKindFam e ~ 'KindDir

-------------------------------------------------------------------------------
-- * Actual path ends
-------------------------------------------------------------------------------

data PackageDir deriving (Typeable)
data SourceDir  deriving (Typeable)
data LicenseFile deriving (Typeable)

-- These instances needs to be derived standalone at least on GHC-7.6
deriving instance Data PackageDir
deriving instance Data SourceDir
deriving instance Data LicenseFile

type instance PathEndKindFam PackageDir = 'KindDir
type instance PathEndKindFam SourceDir = 'KindDir
type instance PathEndKindFam LicenseFile = 'KindFile

-------------------------------------------------------------------------------
--
-------------------------------------------------------------------------------

-- | Whether a path is a good relative path.
--
-- >>> let test fp = putStrLn $ show (isGoodRelativeDirectoryPath fp) ++ "; " ++ show (isGoodRelativeFilePath fp)
--
-- >>> test "foo/bar/quu"
-- Nothing; Nothing
--
-- Trailing slash is not allowed for files, for directories it is ok.
--
-- >>> test "foo/"
-- Nothing; Just "trailing slash"
--
-- Leading @./@ is fine, but @.@ and @./@ are not valid files.
--
-- >>> traverse_ test [".", "./", "./foo/bar"]
-- Nothing; Just "trailing dot segment"
-- Nothing; Just "trailing slash"
-- Nothing; Nothing
--
-- Lastly, not good file nor directory cases:
--
-- >>> traverse_ test ["", "/tmp/src", "foo//bar", "foo/.", "foo/./bar", "foo/../bar", "foo*bar"]
-- Just "empty path"; Just "empty path"
-- Just "posix absolute path"; Just "posix absolute path"
-- Just "empty path segment"; Just "empty path segment"
-- Just "trailing same directory segment: ."; Just "trailing same directory segment: ."
-- Just "same directory segment: ."; Just "same directory segment: .."
-- Just "parent directory segment: .."; Just "parent directory segment: .."
-- Just "reserved character '*'"; Just "reserved character '*'"
--
-- For the last case, 'isGoodRelativeGlob' doesn't warn:
--
-- >>> traverse_ (print . isGoodRelativeGlob) ["foo/../bar", "foo*bar"]
-- Just "parent directory segment: .."
-- Nothing
--
isGoodRelativeFilePath :: FilePath -> Maybe String
isGoodRelativeFilePath = state0
  where
    -- Reserved characters
    -- https://docs.microsoft.com/en-us/windows/win32/fileio/naming-a-file
    isReserved c = c `elem` "<>:\"\\/|?*"

    -- initial state
    state0 []                    = Just "empty path"
    state0 (c:cs) | c == '.'     = state1 cs
                  | c == '/'     = Just "posix absolute path"
                  | isReserved c = Just ("reserved character " ++ show c)
                  | otherwise    = state3 cs

    -- after .
    state1 []                    = Just "trailing dot segment"
    state1 (c:cs) | c == '.'     = state4 cs
                  | c == '/'     = state2 cs
                  | isReserved c = Just ("reserved character " ++ show c)
                  | otherwise    = state5 cs

    -- after ./
    state2 []                    = Just "trailing slash"
    state2 (c:cs) | c == '.'     = state3 cs
                  | c == '/'     = Just "empty path segment"
                  | isReserved c = Just ("reserved character " ++ show c)
                  | otherwise    = state5 cs

    -- after non-first segment's .
    state3 []                    = Just "trailing same directory segment: ."
    state3 (c:cs) | c == '.'     = state4 cs
                  | c == '/'     = Just "same directory segment: .."
                  | isReserved c = Just ("reserved character " ++ show c)
                  | otherwise    = state5 cs

    -- after non-first segment's ..
    state4 []                    = Just "trailing parent directory segment: .."
    state4 (c:cs) | c == '.'     = state5 cs
                  | c == '/'     = Just "parent directory segment: .."
                  | isReserved c = Just ("reserved character " ++ show c)
                  | otherwise    = state5 cs

    -- in a segment which is ok.
    state5 []                    = Nothing
    state5 (c:cs) | c == '.'     = state3 cs
                  | c == '/'     = state2 cs
                  | isReserved c = Just ("reserved character " ++ show c)
                  | otherwise    = state5 cs

-- | See 'isGoodRelativeFilePath'.
isGoodRelativeDirectoryPath :: FilePath -> Maybe String
isGoodRelativeDirectoryPath = state0
  where
    -- Reserved characters
    -- https://docs.microsoft.com/en-us/windows/win32/fileio/naming-a-file
    isReserved c = c `elem` "<>:\"\\/|?*"

    -- initial state
    state0 []                    = Just "empty path"
    state0 (c:cs) | c == '.'     = state5 cs
                  | c == '/'     = Just "posix absolute path"
                  | isReserved c = Just ("reserved character " ++ show c)
                  | otherwise    = state4 cs

    -- after ./
    state1 []                    = Nothing -- "./"
    state1 (c:cs) | c == '.'     = state2 cs
                  | c == '/'     = Just "empty path segment"
                  | isReserved c = Just ("reserved character " ++ show c)
                  | otherwise    = state4 cs

    -- after non-first setgment's .
    state2 []                    = Just "trailing same directory segment: ."
    state2 (c:cs) | c == '.'     = state3 cs
                  | c == '/'     = Just "same directory segment: ."
                  | isReserved c = Just ("reserved character " ++ show c)
                  | otherwise    = state4 cs

    -- after non-first segment's ..
    state3 []                    = Just "trailing parent directory segment: ."
    state3 (c:cs) | c == '.'     = state4 cs
                  | c == '/'     = Just "parent directory segment: .."
                  | isReserved c = Just ("reserved character " ++ show c)
                  | otherwise    = state4 cs

    -- in a segment which is ok.
    state4 []                    = Nothing
    state4 (c:cs) | c == '.'     = state4 cs
                  | c == '/'     = state1 cs
                  | isReserved c = Just ("reserved character " ++ show c)
                  | otherwise    = state4 cs

    -- after .
    state5 []                    = Nothing -- "."
    state5 (c:cs) | c == '.'     = state3 cs
                  | c == '/'     = state1 cs
                  | isReserved c = Just ("reserved character " ++ show c)
                  | otherwise    = state4 cs

-- [Note: Good relative paths]
--
-- Using @kleene@ we can define an extended regex:
--
-- @
-- import Algebra.Lattice
-- import Kleene
-- import Kleene.ERE (ERE (..), intersections)
--
-- data C = CDot | CSlash | COtherReserved | CChar
--   deriving (Eq, Ord, Enum, Bounded, Show)
--
-- reservedR :: ERE C
-- reservedR = notChar CSlash /\ notChar COtherReserved
--
-- pathPieceR :: ERE C
-- pathPieceR = intersections
--     [ plus reservedR
--     , ERENot (string [CDot])
--     , ERENot (string [CDot,CDot])
--     ]
--
-- filePathR :: ERE C
-- filePathR = optional (string [CDot, CSlash]) <> pathPieceR <> star (char CSlash <> pathPieceR)
--
-- dirPathR :: ERE C
-- dirPathR = (char CDot \/ filePathR) <> optional (char CSlash)
--
-- plus :: ERE C -> ERE C
-- plus r = r <> star r
--
-- optional :: ERE C -> ERE C
-- optional r = mempty \/ r
-- @
--
-- Results in following state machine for @filePathR@
--
-- @
-- 0 -> \x -> if
--     | x <= CDot           -> 1
--     | x <= COtherReserved -> 6
--     | otherwise           -> 5
-- 1 -> \x -> if
--     | x <= CDot           -> 4
--     | x <= CSlash         -> 2
--     | x <= COtherReserved -> 6
--     | otherwise           -> 5
-- 2 -> \x -> if
--     | x <= CDot           -> 3
--     | x <= COtherReserved -> 6
--     | otherwise           -> 5
-- 3 -> \x -> if
--     | x <= CDot           -> 4
--     | x <= COtherReserved -> 6
--     | otherwise           -> 5
-- 4 -> \x -> if
--     | x <= CDot           -> 5
--     | x <= COtherReserved -> 6
--     | otherwise           -> 5
-- 5+ -> \x -> if
--     | x <= CDot           -> 5
--     | x <= CSlash         -> 2
--     | x <= COtherReserved -> 6
--     | otherwise           -> 5
-- 6 -> \_ -> 6 -- black hole
-- @
--
-- and @dirPathR@:
--
-- @
-- 0 -> \x -> if
--     | x <= CDot           -> 5
--     | x <= COtherReserved -> 6
--     | otherwise           -> 4
-- 1+ -> \x -> if
--     | x <= CDot           -> 2
--     | x <= COtherReserved -> 6
--     | otherwise           -> 4
-- 2 -> \x -> if
--     | x <= CDot           -> 3
--     | x <= COtherReserved -> 6
--     | otherwise           -> 4
-- 3 -> \x -> if
--     | x <= CDot           -> 4
--     | x <= COtherReserved -> 6
--     | otherwise           -> 4
-- 4+ -> \x -> if
--     | x <= CDot           -> 4
--     | x <= CSlash         -> 1
--     | x <= COtherReserved -> 6
--     | otherwise           -> 4
-- 5+ -> \x -> if
--     | x <= CDot           -> 3
--     | x <= CSlash         -> 1
--     | x <= COtherReserved -> 6
--     | otherwise           -> 4
-- 6 -> \_ -> 6 -- black hole
-- @
