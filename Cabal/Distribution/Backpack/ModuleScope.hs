-- | See <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>
module Distribution.Backpack.ModuleScope (
    -- * Module scopes
    ModuleScope(..),
    ModuleProvides,
    ModuleRequires,
    ModuleSource(..),
    emptyModuleScope,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.ModuleName
import Distribution.Package
import Distribution.Types.IncludeRenaming

import Distribution.Backpack
import Distribution.Backpack.ModSubst

import qualified Data.Map as Map


-----------------------------------------------------------------------
-- Module scopes

-- Why is ModuleProvides so complicated?  The basic problem is that
-- we want to support this:
--
--  package p where
--      include q (A)
--      include r (A)
--      module B where
--          import "q" A
--          import "r" A
--
-- Specifically, in Cabal today it is NOT an error have two modules in
-- scope with the same identifier.  So we need to preserve this for
-- Backpack.  The modification is that an ambiguous module name is
-- OK... as long as it is NOT used to fill a requirement!
--
-- So as a first try, we might try deferring unifying provisions that
-- are being glommed together, and check for equality after the fact.
-- But this doesn't work, because what if a multi-module provision
-- is used to fill a requirement?!  So you do the equality test
-- IMMEDIATELY before a requirement fill happens... or never at all.
--
-- Alternate strategy: go ahead and unify, and then if it is revealed
-- that some requirements got filled "out-of-thin-air", error.


-- | A 'ModuleScope' describes the modules and requirements that
-- are in-scope as we are processing a Cabal package.  Unlike
-- a 'ModuleShape', there may be multiple modules in scope at
-- the same 'ModuleName'; this is only an error if we attempt
-- to use those modules to fill a requirement.  A 'ModuleScope'
-- can influence the 'ModuleShape' via a reexport.
data ModuleScope = ModuleScope {
    modScopeProvides :: ModuleProvides,
    modScopeRequires :: ModuleRequires
    }

-- | Every 'Module' in scope at a 'ModuleName' is annotated with
-- the 'PackageName' it comes from.
type ModuleProvides = Map ModuleName [ModuleSource]
-- | INVARIANT: entries for ModuleName m, have msrc_module is OpenModuleVar m
type ModuleRequires = Map ModuleName [ModuleSource]
-- TODO: consider newtping the two types above.

data ModuleSource =
    ModuleSource {
        -- We don't have line numbers, but if we did the
        -- package name and renaming could be associated
        -- with that as well
        msrc_pkgname :: PackageName,
        msrc_renaming :: IncludeRenaming,
        msrc_module :: OpenModule,
        msrc_implicit :: Bool
    }

-- | An empty 'ModuleScope'.
emptyModuleScope :: ModuleScope
emptyModuleScope = ModuleScope Map.empty Map.empty

instance ModSubst ModuleSource where
    modSubst subst src = src { msrc_module = modSubst subst (msrc_module src) }
