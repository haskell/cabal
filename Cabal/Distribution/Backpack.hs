{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | This module defines the core data types for Backpack.  For more
-- details, see:
--
--  <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>

module Distribution.Backpack (
    -- * OpenUnitId
    OpenUnitId(..),
    openUnitIdComponentId,
    openUnitIdFreeHoles,

    -- * IndefModule
    IndefModule(..),
    indefModuleFreeHoles,

    -- * IndefModuleSubst
    IndefModuleSubst,
    dispIndefModuleSubst,
    dispIndefModuleSubstEntry,
    parseIndefModuleSubst,
    parseIndefModuleSubstEntry,
    indefModuleSubstFreeHoles,

    -- * Conversions to 'UnitId'
    abstractUnitId,
    hashModuleSubst,
) where

import Prelude ()
import Distribution.Compat.Prelude hiding (mod)
import Distribution.Compat.ReadP
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint (hcat)

import Distribution.ModuleName
import Distribution.Package
import Distribution.Text
import Distribution.Utils.Base62

import qualified Data.Map as Map
import Data.Set (Set)
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
-- component always has an empty module substitution; thus we can lossly
-- represent it as an 'OpenUnitId uid'.
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
--

data OpenUnitId
    -- | Identifies a component which may have some unfilled holes;
    -- specifying its 'ComponentId' and its 'IndefModuleSubst'.
    -- TODO: Invariant that 'IndefModuleSubst' is non-empty?
    -- See also the Text instance.
    = IndefFullUnitId ComponentId IndefModuleSubst
    -- | Identifies a fully instantiated component, which has
    -- been compiled and abbreviated as a hash.  The embedded 'UnitId'
    -- MUST NOT be for an indefinite component; an 'OpenUnitId'
    -- is guaranteed not to have any holes.
    | DefiniteUnitId UnitId
  deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)
-- TODO: cache holes?

instance Binary OpenUnitId

instance NFData OpenUnitId where
    rnf (IndefFullUnitId cid subst) = rnf cid `seq` rnf subst
    rnf (DefiniteUnitId uid) = rnf uid

instance Text OpenUnitId where
    disp (IndefFullUnitId cid insts)
        -- TODO: arguably a smart constructor to enforce invariant would be
        -- better
        | Map.null insts = disp cid
        | otherwise      = disp cid <<>> Disp.brackets (dispIndefModuleSubst insts)
    disp (DefiniteUnitId uid) = disp uid
    parse = parseOpenUnitId <++ fmap DefiniteUnitId parse
      where
        parseOpenUnitId = do
            cid <- parse
            insts <- Parse.between (Parse.char '[') (Parse.char ']')
                       parseIndefModuleSubst
            return (IndefFullUnitId cid insts)

-- | Get the 'ComponentId' of an 'OpenUnitId'.
openUnitIdComponentId :: OpenUnitId -> ComponentId
openUnitIdComponentId (IndefFullUnitId cid _) = cid
openUnitIdComponentId (DefiniteUnitId uid) = unitIdComponentId uid

-- | Get the set of holes ('ModuleVar') embedded in a 'UnitId'.
openUnitIdFreeHoles :: OpenUnitId -> Set ModuleName
openUnitIdFreeHoles (IndefFullUnitId _ insts) = indefModuleSubstFreeHoles insts
openUnitIdFreeHoles _ = Set.empty

-----------------------------------------------------------------------
-- IndefModule

-- | Unlike a 'Module', an 'IndefModule' is either an ordinary
-- module from some unit, OR an 'IndefModuleVar', representing a
-- hole that needs to be filled in.  Substitutions are over
-- module variables.
data IndefModule
    = IndefModule OpenUnitId ModuleName
    | IndefModuleVar ModuleName
  deriving (Generic, Read, Show, Eq, Ord, Typeable, Data)

instance Binary IndefModule

instance NFData IndefModule where
    rnf (IndefModule uid mod_name) = rnf uid `seq` rnf mod_name
    rnf (IndefModuleVar mod_name) = rnf mod_name

instance Text IndefModule where
    disp (IndefModule uid mod_name) =
        hcat [disp uid, Disp.text ":", disp mod_name]
    disp (IndefModuleVar mod_name) =
        hcat [Disp.char '<', disp mod_name, Disp.char '>']
    parse = parseModuleVar <++ parseIndefModule
      where
        parseIndefModule = do
            uid <- parse
            _ <- Parse.char ':'
            mod_name <- parse
            return (IndefModule uid mod_name)
        parseModuleVar = do
            _ <- Parse.char '<'
            mod_name <- parse
            _ <- Parse.char '>'
            return (IndefModuleVar mod_name)

-- | Get the set of holes ('ModuleVar') embedded in a 'Module'.
indefModuleFreeHoles :: IndefModule -> Set ModuleName
indefModuleFreeHoles (IndefModuleVar mod_name) = Set.singleton mod_name
indefModuleFreeHoles (IndefModule uid _n) = openUnitIdFreeHoles uid

-----------------------------------------------------------------------
-- IndefModuleSubst

-- | An explicit substitution on modules.
--
-- NB: These substitutions are NOT idempotent, for example, a
-- valid substitution is (A -> B, B -> A).
type IndefModuleSubst = Map ModuleName IndefModule

-- | Pretty-print the entries of a module substitution, suitable
-- for embedding into a 'OpenUnitId' or passing to GHC via @--instantiate-with@.
dispIndefModuleSubst :: IndefModuleSubst -> Disp.Doc
dispIndefModuleSubst subst
    = Disp.hcat
    . Disp.punctuate Disp.comma
    $ map dispIndefModuleSubstEntry (Map.toAscList subst)

-- | Pretty-print a single entry of a module substitution.
dispIndefModuleSubstEntry :: (ModuleName, IndefModule) -> Disp.Doc
dispIndefModuleSubstEntry (k, v) = disp k <<>> Disp.char '=' <<>> disp v

-- | Inverse to 'dispModSubst'.
parseIndefModuleSubst :: ReadP r IndefModuleSubst
parseIndefModuleSubst = fmap Map.fromList
      . flip Parse.sepBy (Parse.char ',')
      $ parseIndefModuleSubstEntry

-- | Inverse to 'dispModSubstEntry'.
parseIndefModuleSubstEntry :: ReadP r (ModuleName, IndefModule)
parseIndefModuleSubstEntry =
    do k <- parse
       _ <- Parse.char '='
       v <- parse
       return (k, v)

-- | Get the set of holes ('ModuleVar') embedded in a 'IndefModuleSubst'.
-- This is NOT the domain of the substitution.
indefModuleSubstFreeHoles :: IndefModuleSubst -> Set ModuleName
indefModuleSubstFreeHoles insts = Set.unions (map indefModuleFreeHoles (Map.elems insts))

-----------------------------------------------------------------------
-- Conversions to UnitId

-- | When typechecking, we don't demand that a freshly instantiated
-- 'IndefFullUnitId' be compiled; instead, we just depend on the
-- installed indefinite unit installed at the 'ComponentId'.
abstractUnitId :: OpenUnitId -> UnitId
abstractUnitId (DefiniteUnitId uid) = uid
abstractUnitId (IndefFullUnitId cid _) = newSimpleUnitId cid

-- | Take a module substitution and hash it into a string suitable for
-- 'UnitId'.  Note that since this takes 'Module', not 'IndefModule',
-- you are responsible for recursively converting 'IndefModule'
-- into 'Module'.  See also "Distribution.Backpack.ReadyComponent".
hashModuleSubst :: Map ModuleName Module -> Maybe String
hashModuleSubst subst
  | Map.null subst = Nothing
  | otherwise =
      Just . hashToBase62 $
        concat [ display mod_name ++ "=" ++ display m ++ "\n"
               | (mod_name, m) <- Map.toList subst]
