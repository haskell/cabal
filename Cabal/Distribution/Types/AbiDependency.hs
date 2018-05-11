{-# LANGUAGE DeriveGeneric #-}
module Distribution.Types.AbiDependency where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec.Class
import Distribution.Pretty
import Distribution.Text

import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.Compat.ReadP       as Parse
import qualified Distribution.Package            as Package
import qualified Text.PrettyPrint                as Disp

-- | An ABI dependency is a dependency on a library which also
-- records the ABI hash ('abiHash') of the library it depends
-- on.
--
-- The primary utility of this is to enable an extra sanity when
-- GHC loads libraries: it can check if the dependency has a matching
-- ABI and if not, refuse to load this library.  This information
-- is critical if we are shadowing libraries; differences in the
-- ABI hash let us know what packages get shadowed by the new version
-- of a package.
data AbiDependency = AbiDependency {
        depUnitId  :: Package.UnitId,
        depAbiHash :: Package.AbiHash
    }
  deriving (Eq, Generic, Read, Show)

instance Pretty AbiDependency where
    pretty (AbiDependency uid abi) =
        disp uid <<>> Disp.char '=' <<>> disp abi

instance  Parsec AbiDependency where
    parsec = do
        uid <- parsec
        _ <- P.char '='
        abi <- parsec
        return (AbiDependency uid abi)

instance Text AbiDependency where
    parse = do
        uid <- parse
        _ <- Parse.char '='
        abi <- parse
        return (AbiDependency uid abi)

instance Binary AbiDependency

instance NFData AbiDependency where rnf = genericRnf
