{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Utilities to help format error messages for the various CLI commands.
module Distribution.Client.CmdErrorMessages
  ( module Distribution.Client.CmdErrorMessages
  , module Distribution.Client.TargetSelector
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.ProjectPlanning
  ( AvailableTarget (..)
  , AvailableTargetStatus (..)
  , CannotPruneDependencies (..)
  , TargetRequested (..)
  )
import Distribution.Client.TargetProblem
  ( TargetProblem (..)
  , TargetProblem'
  )
import Distribution.Client.TargetSelector
  ( ComponentKind (..)
  , ComponentKindFilter
  , SubComponentTarget (..)
  , TargetSelector (..)
  , componentKind
  , showTargetSelector
  )

import Distribution.Package
  ( PackageId
  , PackageName
  , packageId
  , packageName
  )
import Distribution.Simple.Utils
  ( die'
  )
import Distribution.Solver.Types.OptionalStanza
  ( OptionalStanza (..)
  )
import Distribution.Types.ComponentName
  ( ComponentName (..)
  , showComponentName
  )
import Distribution.Types.LibraryName
  ( LibraryName (..)
  )

import qualified Data.List.NonEmpty as NE

-----------------------
-- Singular or plural
--

-- | A tag used in rendering messages to distinguish singular or plural.
data Plural = Singular | Plural

-- | Used to render a singular or plural version of something
--
-- > plural (listPlural theThings) "it is" "they are"
plural :: Plural -> a -> a -> a
plural Singular si _pl = si
plural Plural _si pl = pl

-- | Singular for singleton lists and plural otherwise.
listPlural :: [a] -> Plural
listPlural [_] = Singular
listPlural _ = Plural

--------------------
-- Rendering lists
--

-- | Render a list of things in the style @foo, bar and baz@
renderListCommaAnd :: [String] -> String
renderListCommaAnd [] = ""
renderListCommaAnd [x] = x
renderListCommaAnd [x, x'] = x ++ " and " ++ x'
renderListCommaAnd (x : xs) = x ++ ", " ++ renderListCommaAnd xs

renderListTabular :: [String] -> String
renderListTabular = ("\n" ++) . unlines . map ("| * " ++)

renderListPretty :: [String] -> String
renderListPretty xs =
  if length xs > 5
    then renderListTabular xs
    else renderListCommaAnd xs

-- | Render a list of things in the style @blah blah; this that; and the other@
renderListSemiAnd :: [String] -> String
renderListSemiAnd [] = ""
renderListSemiAnd [x] = x
renderListSemiAnd [x, x'] = x ++ "; and " ++ x'
renderListSemiAnd (x : xs) = x ++ "; " ++ renderListSemiAnd xs

-- | When rendering lists of things it often reads better to group related
-- things, e.g. grouping components by package name
--
-- > renderListSemiAnd
-- >   [     "the package " ++ prettyShow pkgname ++ " components "
-- >      ++ renderListCommaAnd showComponentName components
-- >   | (pkgname, components) <- sortGroupOn packageName allcomponents ]
sortGroupOn :: Ord b => (a -> b) -> [a] -> [(b, [a])]
sortGroupOn key =
  map (\(x :| xs) -> (key x, x : xs))
    . NE.groupBy ((==) `on` key)
    . sortBy (compare `on` key)

----------------------------------------------------
-- Rendering for a few project and package types
--

renderTargetSelector :: TargetSelector -> String
renderTargetSelector (TargetPackage _ pkgids Nothing) =
  "the "
    ++ plural (listPlural pkgids) "package" "packages"
    ++ " "
    ++ renderListCommaAnd (map prettyShow pkgids)
renderTargetSelector (TargetPackage _ pkgids (Just kfilter)) =
  "the "
    ++ renderComponentKind Plural kfilter
    ++ " in the "
    ++ plural (listPlural pkgids) "package" "packages"
    ++ " "
    ++ renderListCommaAnd (map prettyShow pkgids)
renderTargetSelector (TargetPackageNamed pkgname Nothing) =
  "the package " ++ prettyShow pkgname
renderTargetSelector (TargetPackageNamed pkgname (Just kfilter)) =
  "the "
    ++ renderComponentKind Plural kfilter
    ++ " in the package "
    ++ prettyShow pkgname
renderTargetSelector (TargetAllPackages Nothing) =
  "all the packages in the project"
renderTargetSelector (TargetAllPackages (Just kfilter)) =
  "all the "
    ++ renderComponentKind Plural kfilter
    ++ " in the project"
renderTargetSelector (TargetComponent pkgid cname subtarget) =
  renderSubComponentTarget subtarget
    ++ "the "
    ++ renderComponentName (packageName pkgid) cname
renderTargetSelector (TargetComponentUnknown pkgname (Left ucname) subtarget) =
  renderSubComponentTarget subtarget
    ++ "the component "
    ++ prettyShow ucname
    ++ " in the package "
    ++ prettyShow pkgname
renderTargetSelector (TargetComponentUnknown pkgname (Right cname) subtarget) =
  renderSubComponentTarget subtarget
    ++ "the "
    ++ renderComponentName pkgname cname

renderSubComponentTarget :: SubComponentTarget -> String
renderSubComponentTarget WholeComponent = ""
renderSubComponentTarget (FileTarget filename) =
  "the file " ++ filename ++ " in "
renderSubComponentTarget (ModuleTarget modname) =
  "the module " ++ prettyShow modname ++ " in "

renderOptionalStanza :: Plural -> OptionalStanza -> String
renderOptionalStanza Singular TestStanzas = "test suite"
renderOptionalStanza Plural TestStanzas = "test suites"
renderOptionalStanza Singular BenchStanzas = "benchmark"
renderOptionalStanza Plural BenchStanzas = "benchmarks"

-- | The optional stanza type (test suite or benchmark), if it is one.
optionalStanza :: ComponentName -> Maybe OptionalStanza
optionalStanza (CTestName _) = Just TestStanzas
optionalStanza (CBenchName _) = Just BenchStanzas
optionalStanza _ = Nothing

-- | Does the 'TargetSelector' potentially refer to one package or many?
targetSelectorPluralPkgs :: TargetSelector -> Plural
targetSelectorPluralPkgs (TargetAllPackages _) = Plural
targetSelectorPluralPkgs (TargetPackage _ pids _) = listPlural pids
targetSelectorPluralPkgs (TargetPackageNamed _ _) = Singular
targetSelectorPluralPkgs TargetComponent{} = Singular
targetSelectorPluralPkgs TargetComponentUnknown{} = Singular

-- | Does the 'TargetSelector' refer to packages or to components?
targetSelectorRefersToPkgs :: TargetSelector -> Bool
targetSelectorRefersToPkgs (TargetAllPackages mkfilter) = isNothing mkfilter
targetSelectorRefersToPkgs (TargetPackage _ _ mkfilter) = isNothing mkfilter
targetSelectorRefersToPkgs (TargetPackageNamed _ mkfilter) = isNothing mkfilter
targetSelectorRefersToPkgs TargetComponent{} = False
targetSelectorRefersToPkgs TargetComponentUnknown{} = False

targetSelectorFilter :: TargetSelector -> Maybe ComponentKindFilter
targetSelectorFilter (TargetPackage _ _ mkfilter) = mkfilter
targetSelectorFilter (TargetPackageNamed _ mkfilter) = mkfilter
targetSelectorFilter (TargetAllPackages mkfilter) = mkfilter
targetSelectorFilter TargetComponent{} = Nothing
targetSelectorFilter TargetComponentUnknown{} = Nothing

renderComponentName :: PackageName -> ComponentName -> String
renderComponentName pkgname (CLibName LMainLibName) = "library " ++ prettyShow pkgname
renderComponentName _ (CLibName (LSubLibName name)) = "library " ++ prettyShow name
renderComponentName _ (CFLibName name) = "foreign library " ++ prettyShow name
renderComponentName _ (CExeName name) = "executable " ++ prettyShow name
renderComponentName _ (CTestName name) = "test suite " ++ prettyShow name
renderComponentName _ (CBenchName name) = "benchmark " ++ prettyShow name

renderComponentKind :: Plural -> ComponentKind -> String
renderComponentKind Singular ckind = case ckind of
  LibKind -> "library" -- internal/sub libs?
  FLibKind -> "foreign library"
  ExeKind -> "executable"
  TestKind -> "test suite"
  BenchKind -> "benchmark"
renderComponentKind Plural ckind = case ckind of
  LibKind -> "libraries" -- internal/sub libs?
  FLibKind -> "foreign libraries"
  ExeKind -> "executables"
  TestKind -> "test suites"
  BenchKind -> "benchmarks"

-------------------------------------------------------
-- Rendering error messages for TargetProblem
--

-- | Default implementation of 'reportTargetProblems' simply renders one problem per line.
reportTargetProblems :: Verbosity -> String -> [TargetProblem'] -> IO a
reportTargetProblems verbosity verb =
  die' verbosity . unlines . map (renderTargetProblem verb absurd)

-- | Default implementation of 'renderTargetProblem'.
renderTargetProblem
  :: String
  -- ^ verb
  -> (a -> String)
  -- ^ how to render custom problems
  -> TargetProblem a
  -> String
renderTargetProblem _verb f (CustomTargetProblem x) = f x
renderTargetProblem verb _ (TargetProblemNoneEnabled targetSelector targets) =
  renderTargetProblemNoneEnabled verb targetSelector targets
renderTargetProblem verb _ (TargetProblemNoTargets targetSelector) =
  renderTargetProblemNoTargets verb targetSelector
renderTargetProblem verb _ (TargetNotInProject pkgname) =
  "Cannot "
    ++ verb
    ++ " the package "
    ++ prettyShow pkgname
    ++ ", it is not "
    ++ "in this project (either directly or indirectly). If you want to add it "
    ++ "to the project then edit the cabal.project file."
renderTargetProblem verb _ (TargetAvailableInIndex pkgname) =
  "Cannot "
    ++ verb
    ++ " the package "
    ++ prettyShow pkgname
    ++ ", it is not "
    ++ "in this project (either directly or indirectly), but it is in the current "
    ++ "package index. If you want to add it to the project then edit the "
    ++ "cabal.project file."
renderTargetProblem verb _ (TargetComponentNotProjectLocal pkgid cname _) =
  "Cannot "
    ++ verb
    ++ " the "
    ++ showComponentName cname
    ++ " because the "
    ++ "package "
    ++ prettyShow pkgid
    ++ " is not local to the project, and cabal "
    ++ "does not currently support building test suites or benchmarks of "
    ++ "non-local dependencies. To run test suites or benchmarks from "
    ++ "dependencies you can unpack the package locally and adjust the "
    ++ "cabal.project file to include that package directory."
renderTargetProblem verb _ (TargetComponentNotBuildable pkgid cname _) =
  "Cannot "
    ++ verb
    ++ " the "
    ++ showComponentName cname
    ++ " because it is "
    ++ "marked as 'buildable: False' within the '"
    ++ prettyShow (packageName pkgid)
    ++ ".cabal' file (at least for the current configuration). If you believe it "
    ++ "should be buildable then check the .cabal file to see if the buildable "
    ++ "property is conditional on flags. Alternatively you may simply have to "
    ++ "edit the .cabal file to declare it as buildable and fix any resulting "
    ++ "build problems."
renderTargetProblem verb _ (TargetOptionalStanzaDisabledByUser _ cname _) =
  "Cannot "
    ++ verb
    ++ " the "
    ++ showComponentName cname
    ++ " because "
    ++ "building "
    ++ compkinds
    ++ " has been explicitly disabled in the "
    ++ "configuration. You can adjust this configuration in the "
    ++ "cabal.project{.local} file either for all packages in the project or on "
    ++ "a per-package basis. Note that if you do not explicitly disable "
    ++ compkinds
    ++ " then the solver will merely try to make a plan with "
    ++ "them available, so you may wish to explicitly enable them which will "
    ++ "require the solver to find a plan with them available or to fail with an "
    ++ "explanation."
  where
    compkinds = renderComponentKind Plural (componentKind cname)
renderTargetProblem verb _ (TargetOptionalStanzaDisabledBySolver pkgid cname _) =
  "Cannot "
    ++ verb
    ++ " the "
    ++ showComponentName cname
    ++ " because the "
    ++ "solver did not find a plan that included the "
    ++ compkinds
    ++ " for "
    ++ prettyShow pkgid
    ++ ". It is probably worth trying again with "
    ++ compkinds
    ++ " explicitly enabled in the configuration in the "
    ++ "cabal.project{.local} file. This will ask the solver to find a plan with "
    ++ "the "
    ++ compkinds
    ++ " available. It will either fail with an "
    ++ "explanation or find a different plan that uses different versions of some "
    ++ "other packages. Use the '--dry-run' flag to see package versions and "
    ++ "check that you are happy with the choices."
  where
    compkinds = renderComponentKind Plural (componentKind cname)
renderTargetProblem verb _ (TargetProblemUnknownComponent pkgname ecname) =
  "Cannot "
    ++ verb
    ++ " the "
    ++ ( case ecname of
          Left ucname -> "component " ++ prettyShow ucname
          Right cname -> renderComponentName pkgname cname
       )
    ++ " from the package "
    ++ prettyShow pkgname
    ++ ", because the package does not contain a "
    ++ ( case ecname of
          Left _ -> "component"
          Right cname -> renderComponentKind Singular (componentKind cname)
       )
    ++ " with that name."
renderTargetProblem verb _ (TargetProblemNoSuchPackage pkgid) =
  "Internal error when trying to "
    ++ verb
    ++ " the package "
    ++ prettyShow pkgid
    ++ ". The package is not in the set of available targets "
    ++ "for the project plan, which would suggest an inconsistency "
    ++ "between readTargetSelectors and resolveTargets."
renderTargetProblem verb _ (TargetProblemNoSuchComponent pkgid cname) =
  "Internal error when trying to "
    ++ verb
    ++ " the "
    ++ showComponentName cname
    ++ " from the package "
    ++ prettyShow pkgid
    ++ ". The package,component pair is not in the set of available targets "
    ++ "for the project plan, which would suggest an inconsistency "
    ++ "between readTargetSelectors and resolveTargets."

------------------------------------------------------------
-- Rendering error messages for TargetProblemNoneEnabled
--

-- | Several commands have a @TargetProblemNoneEnabled@ problem constructor.
-- This renders an error message for those cases.
renderTargetProblemNoneEnabled
  :: String
  -> TargetSelector
  -> [AvailableTarget ()]
  -> String
renderTargetProblemNoneEnabled verb targetSelector targets =
  "Cannot "
    ++ verb
    ++ " "
    ++ renderTargetSelector targetSelector
    ++ " because none of the components are available to build: "
    ++ renderListSemiAnd
      [ case (status, mstanza) of
        (TargetDisabledByUser, Just stanza) ->
          renderListCommaAnd
            [ "the " ++ showComponentName availableTargetComponentName
            | AvailableTarget{availableTargetComponentName} <- targets'
            ]
            ++ plural (listPlural targets') " is " " are "
            ++ " not available because building "
            ++ renderOptionalStanza Plural stanza
            ++ " has been disabled in the configuration"
        (TargetDisabledBySolver, Just stanza) ->
          renderListCommaAnd
            [ "the " ++ showComponentName availableTargetComponentName
            | AvailableTarget{availableTargetComponentName} <- targets'
            ]
            ++ plural (listPlural targets') " is " " are "
            ++ "not available because the solver picked a plan that does not "
            ++ "include the "
            ++ renderOptionalStanza Plural stanza
            ++ ", perhaps because no such plan exists. To see the error message "
            ++ "explaining the problems with such plans, force the solver to "
            ++ "include the "
            ++ renderOptionalStanza Plural stanza
            ++ " for all "
            ++ "packages, by adding the line 'tests: True' to the "
            ++ "'cabal.project.local' file."
        (TargetNotBuildable, _) ->
          renderListCommaAnd
            [ "the " ++ showComponentName availableTargetComponentName
            | AvailableTarget{availableTargetComponentName} <- targets'
            ]
            ++ plural (listPlural targets') " is " " are all "
            ++ "marked as 'buildable: False'"
        (TargetNotLocal, _) ->
          renderListCommaAnd
            [ "the " ++ showComponentName availableTargetComponentName
            | AvailableTarget{availableTargetComponentName} <- targets'
            ]
            ++ " cannot be built because cabal does not currently support "
            ++ "building test suites or benchmarks of non-local dependencies"
        (TargetBuildable () TargetNotRequestedByDefault, Just stanza) ->
          renderListCommaAnd
            [ "the " ++ showComponentName availableTargetComponentName
            | AvailableTarget{availableTargetComponentName} <- targets'
            ]
            ++ " will not be built because "
            ++ renderOptionalStanza Plural stanza
            ++ " are not built by default in the current configuration (but you "
            ++ "can still build them specifically)" -- TODO: say how
        _ ->
          error $
            "renderBuildTargetProblem: unexpected status "
              ++ show (status, mstanza)
      | ((status, mstanza), targets') <- sortGroupOn groupingKey targets
      ]
  where
    groupingKey t =
      ( availableTargetStatus t
      , case availableTargetStatus t of
          TargetNotBuildable -> Nothing
          TargetNotLocal -> Nothing
          _ -> optionalStanza (availableTargetComponentName t)
      )

------------------------------------------------------------
-- Rendering error messages for TargetProblemNoneEnabled
--

-- | Several commands have a @TargetProblemNoTargets@ problem constructor.
-- This renders an error message for those cases.
renderTargetProblemNoTargets :: String -> TargetSelector -> String
renderTargetProblemNoTargets verb targetSelector =
  "Cannot "
    ++ verb
    ++ " "
    ++ renderTargetSelector targetSelector
    ++ " because "
    ++ reason targetSelector
    ++ ". "
    ++ "Check the .cabal "
    ++ plural
      (targetSelectorPluralPkgs targetSelector)
      "file for the package and make sure that it properly declares "
      "files for the packages and make sure that they properly declare "
    ++ "the components that you expect."
  where
    reason (TargetPackage _ _ Nothing) =
      "it does not contain any components at all"
    reason (TargetPackage _ _ (Just kfilter)) =
      "it does not contain any " ++ renderComponentKind Plural kfilter
    reason (TargetPackageNamed _ Nothing) =
      "it does not contain any components at all"
    reason (TargetPackageNamed _ (Just kfilter)) =
      "it does not contain any " ++ renderComponentKind Plural kfilter
    reason (TargetAllPackages Nothing) =
      "none of them contain any components at all"
    reason (TargetAllPackages (Just kfilter)) =
      "none of the packages contain any "
        ++ renderComponentKind Plural kfilter
    reason ts@TargetComponent{} =
      error $ "renderTargetProblemNoTargets: " ++ show ts
    reason ts@TargetComponentUnknown{} =
      error $ "renderTargetProblemNoTargets: " ++ show ts

-----------------------------------------------------------
-- Rendering error messages for CannotPruneDependencies
--

renderCannotPruneDependencies :: CannotPruneDependencies -> String
renderCannotPruneDependencies (CannotPruneDependencies brokenPackages) =
  "Cannot select only the dependencies (as requested by the "
    ++ "'--only-dependencies' flag), "
    ++ ( case pkgids of
          [pkgid] -> "the package " ++ prettyShow pkgid ++ " is "
          _ ->
            "the packages "
              ++ renderListCommaAnd (map prettyShow pkgids)
              ++ " are "
       )
    ++ "required by a dependency of one of the other targets."
  where
    -- throw away the details and just list the deps that are needed
    pkgids :: [PackageId]
    pkgids = nub . map packageId . concatMap snd $ brokenPackages

{-
           ++ "Syntax:\n"
           ++ " - build [package]\n"
           ++ " - build [package:]component\n"
           ++ " - build [package:][component:]module\n"
           ++ " - build [package:][component:]file\n"
           ++ " where\n"
           ++ "  package is a package name, package dir or .cabal file\n\n"
           ++ "Examples:\n"
           ++ " - build foo            -- package name\n"
           ++ " - build tests          -- component name\n"
           ++ "    (name of library, executable, test-suite or benchmark)\n"
           ++ " - build Data.Foo       -- module name\n"
           ++ " - build Data/Foo.hsc   -- file name\n\n"
           ++ "An ambiguous target can be qualified by package, component\n"
           ++ "and/or component kind (lib|exe|test|bench|flib)\n"
           ++ " - build foo:tests      -- component qualified by package\n"
           ++ " - build tests:Data.Foo -- module qualified by component\n"
           ++ " - build lib:foo        -- component qualified by kind"
-}
