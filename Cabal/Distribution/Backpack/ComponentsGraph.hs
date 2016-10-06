-- | See <https://github.com/ezyang/ghc-proposals/blob/backpack/proposals/0000-backpack.rst>

module Distribution.Backpack.ComponentsGraph (
    ComponentsGraph,
    dispComponentsGraph,
    toComponentsGraph,
    componentCycleMsg
) where

import Distribution.Package
import Distribution.PackageDescription as PD hiding (Flag)
import Distribution.Simple.LocalBuildInfo
import Distribution.Types.ComponentRequestedSpec
import Distribution.Simple.Utils
import Distribution.Compat.Graph (Node(..))
import qualified Distribution.Compat.Graph as Graph

import Distribution.Text
    ( Text(disp) )
import Text.PrettyPrint

------------------------------------------------------------------------------
-- Components graph
------------------------------------------------------------------------------

-- | A components graph is a source level graph tracking the
-- dependencies between components in a package.
type ComponentsGraph = [(Component, [ComponentName])]

-- | Pretty-print a 'ComponentsGraph'.
dispComponentsGraph :: ComponentsGraph -> Doc
dispComponentsGraph graph =
    vcat [ hang (text "component" <+> disp (componentName c)) 4
                (vcat [ text "dependency" <+> disp cdep | cdep <- cdeps ])
         | (c, cdeps) <- graph ]

-- | Given the package description and a 'PackageDescription' (used
-- to determine if a package name is internal or not), create a graph of
-- dependencies between the components.  This is NOT necessarily the
-- build order (although it is in the absence of Backpack.)
toComponentsGraph :: ComponentRequestedSpec
                  -> PackageDescription
                  -> Either [ComponentName] ComponentsGraph
toComponentsGraph enabled pkg_descr =
    let g = Graph.fromList [ N c (componentName c) (componentDeps c)
                           | c <- pkgBuildableComponents pkg_descr
                           , componentEnabled enabled c ]
    in case Graph.cycles g of
          []     -> Right (map (\(N c _ cs) -> (c, cs)) (Graph.revTopSort g))
          ccycles -> Left  [ componentName c | N c _ _ <- concat ccycles ]
  where
    -- The dependencies for the given component
    componentDeps component =
         [ CExeName toolname | Dependency pkgname _
                               <- buildTools bi
                             , let toolname = unPackageName pkgname
                             , toolname `elem` map exeName
                               (executables pkg_descr) ]

      ++ [ if pkgname == packageName pkg_descr
            then CLibName
            else CSubLibName toolname
            | Dependency pkgname _
                               <- targetBuildDepends bi
                             , pkgname `elem` internalPkgDeps
            , let toolname = unPackageName pkgname ]
      where
        bi = componentBuildInfo component
        internalPkgDeps = map (conv . libName) (allLibraries pkg_descr)
        conv Nothing = packageName pkg_descr
        conv (Just s) = mkPackageName s

-- | Error message when there is a cycle; takes the SCC of components.
componentCycleMsg :: [ComponentName] -> Doc
componentCycleMsg cnames =
    text $ "Components in the package depend on each other in a cyclic way:\n  "
       ++ intercalate " depends on "
            [ "'" ++ showComponentName cname ++ "'"
            | cname <- cnames ++ [head cnames] ]
