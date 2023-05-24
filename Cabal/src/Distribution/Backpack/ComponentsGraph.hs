-- | See <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>
module Distribution.Backpack.ComponentsGraph
  ( ComponentsGraph
  , ComponentsWithDeps
  , mkComponentsGraph
  , componentsGraphToList
  , dispComponentsWithDeps
  , componentCycleMsg
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Compat.Graph (Graph, Node (..))
import qualified Distribution.Compat.Graph as Graph
import qualified Distribution.Compat.NonEmptySet as NES
import Distribution.Package
import Distribution.PackageDescription
import Distribution.Simple.BuildToolDepends
import Distribution.Simple.LocalBuildInfo
import Distribution.Types.ComponentRequestedSpec
import Distribution.Utils.Generic

import Distribution.Pretty (pretty)
import Text.PrettyPrint

------------------------------------------------------------------------------
-- Components graph
------------------------------------------------------------------------------

-- | A graph of source-level components by their source-level
-- dependencies
type ComponentsGraph = Graph (Node ComponentName Component)

-- | A list of components associated with the source level
-- dependencies between them.
type ComponentsWithDeps = [(Component, [ComponentName])]

-- | Pretty-print 'ComponentsWithDeps'.
dispComponentsWithDeps :: ComponentsWithDeps -> Doc
dispComponentsWithDeps graph =
  vcat
    [ hang
      (text "component" <+> pretty (componentName c))
      4
      (vcat [text "dependency" <+> pretty cdep | cdep <- cdeps])
    | (c, cdeps) <- graph
    ]

-- | Create a 'Graph' of 'Component', or report a cycle if there is a
-- problem.
mkComponentsGraph
  :: ComponentRequestedSpec
  -> PackageDescription
  -> Either [ComponentName] ComponentsGraph
mkComponentsGraph enabled pkg_descr =
  let g =
        Graph.fromDistinctList
          [ N c (componentName c) (componentDeps c)
          | c <- pkgBuildableComponents pkg_descr
          , componentEnabled enabled c
          ]
   in case Graph.cycles g of
        [] -> Right g
        ccycles -> Left [componentName c | N c _ _ <- concat ccycles]
  where
    -- The dependencies for the given component
    componentDeps component =
      toolDependencies ++ libDependencies
      where
        bi = componentBuildInfo component

        toolDependencies = CExeName <$> getAllInternalToolDependencies pkg_descr bi

        libDependencies = do
          Dependency pkgname _ lns <- targetBuildDepends bi
          guard (pkgname == packageName pkg_descr)

          ln <- NES.toList lns
          return (CLibName ln)

-- | Given the package description and a 'PackageDescription' (used
-- to determine if a package name is internal or not), sort the
-- components in dependency order (fewest dependencies first).  This is
-- NOT necessarily the build order (although it is in the absence of
-- Backpack.)
componentsGraphToList
  :: ComponentsGraph
  -> ComponentsWithDeps
componentsGraphToList =
  map (\(N c _ cs) -> (c, cs)) . Graph.revTopSort

-- | Error message when there is a cycle; takes the SCC of components.
componentCycleMsg :: PackageIdentifier -> [ComponentName] -> Doc
componentCycleMsg pn cnames =
  text "Components in the package"
    <+> pretty pn
    <+> text "depend on each other in a cyclic way:"
    $$ text
      ( intercalate
          " depends on "
          [ "'" ++ showComponentName cname ++ "'"
          | cname <- cnames ++ maybeToList (safeHead cnames)
          ]
      )
