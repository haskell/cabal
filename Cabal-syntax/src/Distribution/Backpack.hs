{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}

-- | This module defines the core data types for Backpack.  For more
-- details, see:
--
--  <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>
module Distribution.Backpack
  ( -- * OpenUnitId
    OpenUnitId (..)
  , openUnitIdFreeHoles
  , mkOpenUnitId

    -- * DefUnitId
  , DefUnitId
  , unDefUnitId
  , mkDefUnitId

    -- * OpenModule
  , OpenModule (..)
  , openModuleFreeHoles

    -- * OpenModuleSubst
  , OpenModuleSubst
  , dispOpenModuleSubst
  , dispOpenModuleSubstEntry
  , parsecOpenModuleSubst
  , parsecOpenModuleSubstEntry
  , openModuleSubstFreeHoles

    -- * Conversions to 'UnitId'
  , abstractUnitId
  , hashModuleSubst
  ) where

import Distribution.Compat.Prelude hiding (mod)
import Distribution.Parsec
import Distribution.Pretty
import Text.PrettyPrint (hcat)
import Prelude ()

import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp

import Distribution.ModuleName
import Distribution.Types.ComponentId
import Distribution.Types.Module
import Distribution.Types.UnitId
import Distribution.Utils.Base62

import qualified Data.Map as Map
import qualified Data.Set as Set

-----------------------------------------------------------------------
-- OpenUnitId

-- | An 'OpenUnitId' describes a (possibly partially) instantiated
-- Backpack component, with a description of how the holes are filled
-- in.  Unlike 'OpenUnitId', the 'ModuleSubst' is kept in a structured
-- form that allows for substitution (which fills in holes.) This form
-- of unit cannot be installed. It must first be converted to a
-- 'UnitId'.
--
-- In the absence of Backpack, there are no holes to fill, so any such
-- component always has an empty module substitution; thus we can lossily
-- represent it as a 'DefiniteUnitId uid'.
--
-- For a source component using Backpack, however, there is more
-- structure as components may be parametrized over some signatures, and
-- these \"holes\" may be partially or wholly filled.
--
-- OpenUnitId plays an important role when we are mix-in linking,
-- and is recorded to the installed packaged database for indefinite
-- packages; however, for compiled packages that are fully instantiated,
-- we instantiate 'OpenUnitId' into 'UnitId'.
--
-- For more details see the Backpack spec
-- <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>
data OpenUnitId
  = -- | Identifies a component which may have some unfilled holes;
    -- specifying its 'ComponentId' and its 'OpenModuleSubst'.
    -- TODO: Invariant that 'OpenModuleSubst' is non-empty?
    -- See also the Text instance.
    IndefFullUnitId ComponentId OpenModuleSubst
  | -- | Identifies a fully instantiated component, which has
    -- been compiled and abbreviated as a hash.  The embedded 'UnitId'
    -- MUST NOT be for an indefinite component; an 'OpenUnitId'
    -- is guaranteed not to have any holes.
    DefiniteUnitId DefUnitId
  deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

-- TODO: cache holes?

instance Binary OpenUnitId
instance Structured OpenUnitId
instance NFData OpenUnitId where
  rnf (IndefFullUnitId cid subst) = rnf cid `seq` rnf subst
  rnf (DefiniteUnitId uid) = rnf uid

instance Pretty OpenUnitId where
  pretty (IndefFullUnitId cid insts)
    -- TODO: arguably a smart constructor to enforce invariant would be
    -- better
    | Map.null insts = pretty cid
    | otherwise = pretty cid <<>> Disp.brackets (dispOpenModuleSubst insts)
  pretty (DefiniteUnitId uid) = pretty uid

-- |
--
-- >>> eitherParsec "foobar" :: Either String OpenUnitId
-- Right (DefiniteUnitId (DefUnitId {unDefUnitId = UnitId "foobar"}))
--
-- >>> eitherParsec "foo[Str=text-1.2.3:Data.Text.Text]" :: Either String OpenUnitId
-- Right (IndefFullUnitId (ComponentId "foo") (fromList [(ModuleName "Str",OpenModule (DefiniteUnitId (DefUnitId {unDefUnitId = UnitId "text-1.2.3"})) (ModuleName "Data.Text.Text"))]))
instance Parsec OpenUnitId where
  parsec = P.try parseOpenUnitId <|> fmap DefiniteUnitId parsec
    where
      parseOpenUnitId = do
        cid <- parsec
        insts <-
          P.between
            (P.char '[')
            (P.char ']')
            parsecOpenModuleSubst
        return (IndefFullUnitId cid insts)

-- | Get the set of holes ('ModuleVar') embedded in a 'UnitId'.
openUnitIdFreeHoles :: OpenUnitId -> Set ModuleName
openUnitIdFreeHoles (IndefFullUnitId _ insts) = openModuleSubstFreeHoles insts
openUnitIdFreeHoles _ = Set.empty

-- | Safe constructor from a UnitId.  The only way to do this safely
-- is if the instantiation is provided.
mkOpenUnitId :: UnitId -> ComponentId -> OpenModuleSubst -> OpenUnitId
mkOpenUnitId uid cid insts =
  if Set.null (openModuleSubstFreeHoles insts)
    then DefiniteUnitId (unsafeMkDefUnitId uid) -- invariant holds!
    else IndefFullUnitId cid insts

-----------------------------------------------------------------------
-- DefUnitId

-- | Create a 'DefUnitId' from a 'ComponentId' and an instantiation
-- with no holes.
mkDefUnitId :: ComponentId -> Map ModuleName Module -> DefUnitId
mkDefUnitId cid insts =
  unsafeMkDefUnitId
    ( mkUnitId
        (unComponentId cid ++ maybe "" ("+" ++) (hashModuleSubst insts))
    )

-- impose invariant!

-----------------------------------------------------------------------
-- OpenModule

-- | Unlike a 'Module', an 'OpenModule' is either an ordinary
-- module from some unit, OR an 'OpenModuleVar', representing a
-- hole that needs to be filled in.  Substitutions are over
-- module variables.
data OpenModule
  = OpenModule OpenUnitId ModuleName
  | OpenModuleVar ModuleName
  deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

instance Binary OpenModule
instance Structured OpenModule

instance NFData OpenModule where
  rnf (OpenModule uid mod_name) = rnf uid `seq` rnf mod_name
  rnf (OpenModuleVar mod_name) = rnf mod_name

instance Pretty OpenModule where
  pretty (OpenModule uid mod_name) =
    hcat [pretty uid, Disp.text ":", pretty mod_name]
  pretty (OpenModuleVar mod_name) =
    hcat [Disp.char '<', pretty mod_name, Disp.char '>']

-- |
--
-- >>> eitherParsec "Includes2-0.1.0.0-inplace-mysql:Database.MySQL" :: Either String OpenModule
-- Right (OpenModule (DefiniteUnitId (DefUnitId {unDefUnitId = UnitId "Includes2-0.1.0.0-inplace-mysql"})) (ModuleName "Database.MySQL"))
instance Parsec OpenModule where
  parsec = parsecModuleVar <|> parsecOpenModule
    where
      parsecOpenModule = do
        uid <- parsec
        _ <- P.char ':'
        mod_name <- parsec
        return (OpenModule uid mod_name)

      parsecModuleVar = do
        _ <- P.char '<'
        mod_name <- parsec
        _ <- P.char '>'
        return (OpenModuleVar mod_name)

-- | Get the set of holes ('ModuleVar') embedded in a 'Module'.
openModuleFreeHoles :: OpenModule -> Set ModuleName
openModuleFreeHoles (OpenModuleVar mod_name) = Set.singleton mod_name
openModuleFreeHoles (OpenModule uid _n) = openUnitIdFreeHoles uid

-----------------------------------------------------------------------
-- OpenModuleSubst

-- | An explicit substitution on modules.
--
-- NB: These substitutions are NOT idempotent, for example, a
-- valid substitution is (A -> B, B -> A).
type OpenModuleSubst = Map ModuleName OpenModule

-- | Pretty-print the entries of a module substitution, suitable
-- for embedding into a 'OpenUnitId' or passing to GHC via @--instantiate-with@.
dispOpenModuleSubst :: OpenModuleSubst -> Disp.Doc
dispOpenModuleSubst subst =
  Disp.hcat
    . Disp.punctuate Disp.comma
    $ map dispOpenModuleSubstEntry (Map.toAscList subst)

-- | Pretty-print a single entry of a module substitution.
dispOpenModuleSubstEntry :: (ModuleName, OpenModule) -> Disp.Doc
dispOpenModuleSubstEntry (k, v) = pretty k <<>> Disp.char '=' <<>> pretty v

-- | Inverse to 'dispModSubst'.
--
-- @since 2.2
parsecOpenModuleSubst :: CabalParsing m => m OpenModuleSubst
parsecOpenModuleSubst =
  fmap Map.fromList
    . flip P.sepBy (P.char ',')
    $ parsecOpenModuleSubstEntry

-- | Inverse to 'dispModSubstEntry'.
--
-- @since 2.2
parsecOpenModuleSubstEntry :: CabalParsing m => m (ModuleName, OpenModule)
parsecOpenModuleSubstEntry =
  do
    k <- parsec
    _ <- P.char '='
    v <- parsec
    return (k, v)

-- | Get the set of holes ('ModuleVar') embedded in a 'OpenModuleSubst'.
-- This is NOT the domain of the substitution.
openModuleSubstFreeHoles :: OpenModuleSubst -> Set ModuleName
openModuleSubstFreeHoles insts = Set.unions (map openModuleFreeHoles (Map.elems insts))

-----------------------------------------------------------------------
-- Conversions to UnitId

-- | When typechecking, we don't demand that a freshly instantiated
-- 'IndefFullUnitId' be compiled; instead, we just depend on the
-- installed indefinite unit installed at the 'ComponentId'.
abstractUnitId :: OpenUnitId -> UnitId
abstractUnitId (DefiniteUnitId def_uid) = unDefUnitId def_uid
abstractUnitId (IndefFullUnitId cid _) = newSimpleUnitId cid

-- | Take a module substitution and hash it into a string suitable for
-- 'UnitId'.  Note that since this takes 'Module', not 'OpenModule',
-- you are responsible for recursively converting 'OpenModule'
-- into 'Module'.  See also "Distribution.Backpack.ReadyComponent".
hashModuleSubst :: Map ModuleName Module -> Maybe String
hashModuleSubst subst
  | Map.null subst = Nothing
  | otherwise =
      Just . hashToBase62 $
        concat
          [ prettyShow mod_name ++ "=" ++ prettyShow m ++ "\n"
          | (mod_name, m) <- Map.toList subst
          ]
