module Distribution.Solver.Types.PackagePath
    ( PackagePath(..)
    , Namespace(..)
    , Qualifier(..)
    , Qualified(..)
    , QPN
    , dispNamespace
    , dispQPN
    , showQPN
    ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()
import Distribution.Package (PackageName, PrivateAlias)
import Distribution.Pretty (Pretty, pretty, flatStyle)
import qualified Text.PrettyPrint as Disp
import Distribution.Solver.Types.ComponentDeps

-- | A package path consists of a namespace and a package path inside that
-- namespace.
data PackagePath = PackagePath Namespace Qualifier
  deriving (Eq, Ord, Show)

-- | Top-level namespace
--
-- Package choices in different namespaces are considered completely independent
-- by the solver.
data Namespace =
    -- | A goal which is solved
    DefaultNamespace

    -- | A goal which is solved per-package
    -- `--independent-goals`
  | Independent PackageName


  | IndependentComponent PackageName Component
  -- Build-tools are solved per-package so there are consistent build-tools across
  -- components.
  | IndependentBuildTool PackageName PackageName
  deriving (Eq, Ord, Show)

-- | Pretty-prints a namespace. The result is either empty or
-- ends in a period, so it can be prepended onto a qualifier.
dispNamespace :: Namespace -> Disp.Doc
dispNamespace DefaultNamespace = Disp.empty
dispNamespace (Independent i) = pretty i <<>> Disp.text "."
dispNamespace (IndependentBuildTool pn btp) = pretty pn <<>> Disp.text ":" <<>>
                                              pretty btp <<>> Disp.text ":exe."
dispNamespace (IndependentComponent pn c) = pretty pn <<>> Disp.text ":" <<>> pretty c <<>> Disp.text "."

-- | Qualifier of a package within a namespace (see 'PackagePath')
data Qualifier =
    -- | Top-level dependency in this namespace
    QualToplevel

    -- | Any dependency on base is considered independent
    --
    -- This makes it possible to have base shims.
  | QualBase PackageName

  -- A goal which is solved per-component
  | QualAlias PackageName Component PrivateAlias

  deriving (Eq, Ord, Show)

-- | Pretty-prints a qualifier. The result is either empty or
-- ends in a period, so it can be prepended onto a package name.
--
-- NOTE: the base qualifier is for a dependency _on_ base; the qualifier is
-- there to make sure different dependencies on base are all independent.
-- So we want to print something like @"A.base"@, where the @"A."@ part
-- is the qualifier and @"base"@ is the actual dependency (which, for the
-- 'Base' qualifier, will always be @base@).
dispQualifier :: Qualifier -> Disp.Doc
dispQualifier QualToplevel = Disp.empty
dispQualifier (QualBase pn)  = pretty pn <<>> Disp.text ".bb."
dispQualifier (QualAlias pn c alias) = pretty pn <<>> Disp.text ":" <<>> pretty c <<>> Disp.text ":" <<>> pretty alias <<>> Disp.text "."

instance Pretty Qualifier where
  pretty = dispQualifier

-- | A qualified entity. Pairs a package path with the entity.
data Qualified a = Q PackagePath a
  deriving (Eq, Ord, Show)

-- | Qualified package name.
type QPN = Qualified PackageName

-- | Pretty-prints a qualified package name.
dispQPN :: QPN -> Disp.Doc
dispQPN (Q (PackagePath ns qual) pn) =
  dispNamespace ns <<>> dispQualifier qual <<>> pretty pn

-- | String representation of a qualified package name.
showQPN :: QPN -> String
showQPN = Disp.renderStyle flatStyle . dispQPN
