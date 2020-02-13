{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Types.AbiDependency where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Parsec
import Distribution.Pretty
import Distribution.FieldGrammar.Described

import qualified Distribution.Compat.CharParsing as P
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
  deriving (Eq, Generic, Read, Show, Typeable)

instance Pretty AbiDependency where
    pretty (AbiDependency uid abi) =
        pretty uid <<>> Disp.char '=' <<>> pretty abi

instance  Parsec AbiDependency where
    parsec = do
        uid <- parsec
        _ <- P.char '='
        abi <- parsec
        return (AbiDependency uid abi)

instance Described AbiDependency where
    describe _ =
        describe (Proxy :: Proxy Package.UnitId) <> 
        reChar '=' <>
        describe (Proxy :: Proxy Package.AbiHash)

instance Binary AbiDependency
instance Structured AbiDependency
instance NFData AbiDependency where rnf = genericRnf
