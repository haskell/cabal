{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Distribution.Utils.Path
  ( -- * Symbolic path
    SymbolicPath
  , getSymbolicPath
  , sameDirectory
  , unsafeMakeSymbolicPath

    -- * Path ends
  , PackageDir
  , SourceDir
  , LicenseFile
  , IsDir
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty
import Distribution.Utils.Generic (isAbsoluteOnAnyPlatform)

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
newtype SymbolicPath from to = SymbolicPath FilePath
  deriving (Generic, Show, Read, Eq, Ord, Typeable, Data)

instance Binary (SymbolicPath from to)
instance (Typeable from, Typeable to) => Structured (SymbolicPath from to)
instance NFData (SymbolicPath from to) where rnf = genericRnf

-- | Extract underlying 'FilePath'.
--
-- Avoid using this in new code.
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

instance Parsec (SymbolicPath from to) where
  parsec = do
    token <- parsecToken
    if null token
      then P.unexpected "empty FilePath"
      else
        if isAbsoluteOnAnyPlatform token
          then P.unexpected "absolute FilePath"
          else return (SymbolicPath token) -- TODO: normalise

instance Pretty (SymbolicPath from to) where
  pretty = showFilePath . getSymbolicPath

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

-- | Class telling that index is for directories.
class IsDir dir

data PackageDir deriving (Typeable)
data SourceDir deriving (Typeable)

data LicenseFile deriving (Typeable)

-- These instances needs to be derived standalone at least on GHC-7.6
deriving instance Data PackageDir
deriving instance Data SourceDir
deriving instance Data LicenseFile

instance IsDir PackageDir
instance IsDir SourceDir
