{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
module Distribution.Solver.Types.PackagePath
    ( PackagePath(..)
    , Qualifier(..)
    , dispQualifier
    , Qualified(..)
    , QPN
    , dispQPN
    , showQPN
    ) where

import Distribution.Solver.Compat.Prelude
import Prelude ()
import Distribution.Package (PackageName)
import Distribution.Pretty (pretty, flatStyle, Pretty)
import qualified Text.PrettyPrint as Disp
import Distribution.Solver.Types.Stage (Stage)

data PackagePath = PackagePath Stage Qualifier
  deriving (Eq, Ord, Show, Generic)

instance Binary PackagePath
instance Structured PackagePath

instance Pretty PackagePath where
  pretty (PackagePath stage qualifier) =
    pretty stage <<>> Disp.text ":" <<>> pretty qualifier

-- | Qualifier of a package within a namespace (see 'PackagePath')
data Qualifier =
    -- | Top-level dependency in this namespace
    QualToplevel

    -- | Setup dependency
    --
    -- By rights setup dependencies ought to be nestable; after all, the setup
    -- dependencies of a package might themselves have setup dependencies, which
    -- are independent from everything else. However, this very quickly leads to
    -- infinite search trees in the solver. Therefore we limit ourselves to
    -- a single qualifier (within a given namespace).
  | QualSetup PackageName

    -- | If we depend on an executable from a package (via
    -- @build-tools@), we should solve for the dependencies of that
    -- package separately (since we're not going to actually try to
    -- link it.)  We qualify for EACH package separately; e.g.,
    -- @'Exe' pn1 pn2@ qualifies the @build-tools@ dependency on
    -- @pn2@ from package @pn1@.  (If we tracked only @pn1@, that
    -- would require a consistent dependency resolution for all
    -- of the depended upon executables from a package; if we
    -- tracked only @pn2@, that would require us to pick only one
    -- version of an executable over the entire install plan.)
  | QualExe PackageName PackageName
  deriving (Eq, Ord, Show, Generic)

instance Binary Qualifier
instance Structured Qualifier

instance Pretty Qualifier where
  pretty QualToplevel = Disp.text "toplevel"
  pretty (QualSetup pn) = pretty pn <<>> Disp.text ":setup"
  pretty (QualExe pn pn2) = pretty pn <<>> Disp.text ":" <<>>
                            pretty pn2 <<>> Disp.text ":exe"

-- | Pretty-prints a qualifier. The result is either empty or
-- ends in a period, so it can be prepended onto a package name.
dispQualifier :: Qualifier -> Disp.Doc
dispQualifier QualToplevel = mempty
dispQualifier (QualSetup pn) = pretty pn <> Disp.text ":setup."
dispQualifier (QualExe pn pn2) =
  pretty pn
    <> Disp.text ":"
    <> pretty pn2
    <> Disp.text ":exe."

-- | A qualified entity. Pairs a package path with the entity.
data Qualified a = Q PackagePath a
  deriving (Eq, Ord, Show, Generic)

instance (Binary a) => Binary (Qualified a)
instance (Structured a) => Structured (Qualified a)

-- | Qualified package name.
type QPN = Qualified PackageName

instance Pretty (Qualified PackageName) where
  pretty (Q (PackagePath stage qual) pn) =
    pretty stage <<>> Disp.colon <<>> dispQualifier qual <<>> pretty pn

-- | Pretty-prints a qualified package name.
dispQPN :: QPN -> Disp.Doc
dispQPN = pretty

-- | String representation of a qualified package name.
showQPN :: QPN -> String
showQPN = Disp.renderStyle flatStyle . dispQPN
