module Distribution.Solver.Types.PackagePath
    ( PackagePath(..)
    , Namespace(..)
    , Qualifier(..)
    , QPN
    , Qualified(..)
    , showQPN
    ) where

import Distribution.Package
import Distribution.Text

-- | A package path consists of a namespace and a package path inside that
-- namespace.
data PackagePath = PackagePath Namespace Qualifier
  deriving (Eq, Ord, Show)

-- | Top-level namespace
--
-- Package choices in different namespaces are considered completely independent
-- by the solver.
data Namespace =
    -- | The default namespace
    DefaultNamespace

    -- | Independent namespace
    --
    -- For now we just number these (rather than giving them more structure).
  | Independent Int
  deriving (Eq, Ord, Show)

-- | Qualifier of a package within a namespace (see 'PackagePath')
data Qualifier =
    -- | Top-level dependency in this namespace
    Unqualified

    -- | Any dependency on base is considered independent
    --
    -- This makes it possible to have base shims.
  | Base PackageName

    -- | Setup dependency
    --
    -- By rights setup dependencies ought to be nestable; after all, the setup
    -- dependencies of a package might themselves have setup dependencies, which
    -- are independent from everything else. However, this very quickly leads to
    -- infinite search trees in the solver. Therefore we limit ourselves to
    -- a single qualifier (within a given namespace).
  | Setup PackageName
  deriving (Eq, Ord, Show)

-- | String representation of a package path.
--
-- NOTE: The result of 'showPP' is either empty or results in a period, so that
-- it can be prepended to a package name.
showPP :: PackagePath -> String
showPP (PackagePath ns q) =
    case ns of
      DefaultNamespace -> go q
      Independent i    -> show i ++ "." ++ go q
  where
    -- Print the qualifier
    --
    -- NOTE: the base qualifier is for a dependency _on_ base; the qualifier is
    -- there to make sure different dependencies on base are all independent.
    -- So we want to print something like @"A.base"@, where the @"A."@ part
    -- is the qualifier and @"base"@ is the actual dependency (which, for the
    -- 'Base' qualifier, will always be @base@).
    go Unqualified = ""
    go (Setup pn)  = display pn ++ "-setup."
    go (Base  pn)  = display pn ++ "."

-- | A qualified entity. Pairs a package path with the entity.
data Qualified a = Q PackagePath a
  deriving (Eq, Ord, Show)

-- | Standard string representation of a qualified entity.
showQ :: (a -> String) -> (Qualified a -> String)
showQ showa (Q pp x) = showPP pp ++ showa x

-- | Qualified package name.
type QPN = Qualified PackageName

-- | String representation of a qualified package path.
showQPN :: QPN -> String
showQPN = showQ display
