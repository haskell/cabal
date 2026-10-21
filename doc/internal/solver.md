# The modular dependency solver

This note explains how the dependency solver in `cabal-install-solver` works:
what problem it solves, how the code is organised, and why it is organised that
way. It is for people who want to change the solver or debug its output. For
the user-facing options (`--reorder-goals`, `--max-backjumps`,
`--independent-goals`, …) see the `cabal.project` reference.

The solver lives in `cabal-install-solver/src/Distribution/Solver/Modular/`.
Shared types that `cabal-install` also uses are in
`cabal-install-solver/src/Distribution/Solver/Types/`.

## Background and sources

Andres Löh and Duncan Coutts wrote the solver at Well-Typed, in a project
funded by the Industrial Haskell Group. They presented it at the Haskell
Implementors' Workshop in 2011. It replaced an older solver that never
backtracked and that tied flag resolution to package selection. The design is
inspired by Nordin and Tolmach, *Modular Lazy Search for Constraint
Satisfaction Problems* (JFP, 2001).

- Andres Löh, Duncan Coutts, *A new, modular dependency solver for
  cabal-install*, HIW 2011.
  <https://www.haskell.org/wikiupload/b/b4/HIW2011-Talk-Loeh.pdf>
- Edsko de Vries, *Qualified Goals in the Cabal Solver*, Well-Typed blog,
  March 2015. <https://well-typed.com/blog/2015/03/qualified-goals/>
- Duncan Coutts, *Dependencies for Cabal Setup.hs files and other goodies*,
  Well-Typed blog, July 2015.
  <https://well-typed.com/blog/2015/07/cabal-setup-deps/>

The basic architecture in these sources still holds. Many details have changed,
though, and the section [How the design has moved on](#how-the-design-has-moved-on)
lists the main differences. When this note and the sources disagree, trust this
note, and trust the code over both.

## The problem

`cabal-install` gives the solver:

- the **targets** (package names to solve for);
- the **installed packages** of each build stage, and the **source packages**
  available from repositories and the project;
- **constraints** (from the user, the project and `cabal-install` itself) and
  **preferences** (preferred versions, prefer installed/latest/oldest);
- the **compiler** and **pkg-config database** of each build stage.

It must choose:

- one **instance** for each package goal it reaches. An instance (`I` in
  `Modular/Package.hs`) is a version plus a location: either `InRepo` (to be
  built) or `Inst uid` (already installed, with a fixed unit id);
- a value for each **flag** of each chosen package;
- whether to enable each **optional stanza** (test suites, benchmarks).

These choices must satisfy every dependency, constraint and environment
requirement. Installed instances have fixed dependencies. Source instances have
flexible ones, which depend on the flags chosen. The solver returns the first
solution it finds. It does not look for the best solution. Preferences only
decide the order in which choices are tried.

In the code, the things to choose are called **variables** (`Var` in
`Modular/Var.hs`): a package `P qpn`, a flag `F qfn` or a stanza `S qsn`. A
**goal** (`Goal` in `Modular/Dependency.hs`) is a variable together with the
**reason** it was introduced. The reason is either `UserGoal` (a target) or a
`DependencyReason`: the package, plus any flag and stanza values, whose
dependency brought it in.

## The pipeline

The whole solver is one composition of tree transformations, in
`solve` in `Modular/Solver.hs`. Read it right to left:

```haskell
solve sc cinfo idx pkgConfigDB userPrefs userConstraints userGoals =
  explorePhase      .
  detectCycles      .
  trav (
   heuristicsPhase  .
   preferencesPhase .
   validationPhase
  ) .
  validationCata    .
  trav prunePhase   $
  buildPhase
```

(The `traceTree` calls between the phases are left out here. They do nothing
unless the package is built with the `debug-tracetree` flag. See
[Debugging and testing](#debugging-and-testing).)

The idea is as follows:

1. **Build** a search tree that contains every solution, plus many non-solutions
   (`buildPhase`).
2. **Prune and validate** it with a series of independent traversals. Each one
   replaces subtrees that break one rule with a `Fail` node. After these
   passes, every `Done` leaf is a valid solution (`prunePhase`,
   `validationCata`, `validationPhase`, `detectCycles`).
3. **Reorder** it, so that preferred choices come first and the goal order
   follows the heuristics (`preferencesPhase`, `heuristicsPhase`).
4. **Explore** it depth-first, left to right, until the first `Done` leaf. Use
   conflict sets to skip subtrees that cannot help (`explorePhase`).

The tree is never built in full, because it is exponentially large. It is a
lazy Haskell value. Each phase is a tree-to-tree function that only looks at a
node when something forces it, and only exploration forces anything. So the
nodes that are actually built are the ones on the paths that exploration
visits. That is what makes the solver modular: each rule is its own pass,
written as if it worked on the whole tree. You can add, remove or reorder a
pass without touching the others.

There is a price. All the passes must stay lazy in the subtrees they do not
look at. A pass that forces too much (for example by checking the whole tree,
or by keeping references to large subtrees) makes the solver slow or causes
space leaks. The type system does not catch these mistakes. The HIW talk
already pointed this out, and it is still the main hazard when you change the
solver. One example is `addLinking` in `Modular/Builder.hs`: linking options are
added while the tree is built, not in a separate pass, to avoid a space leak
(see [#2899](https://github.com/haskell/cabal/issues/2899) and the comment on
`addLinking`).

Most passes are written with `trav` (from `Modular/Tree.hs`), which applies a
function to one node layer at a time. Because `trav f . trav g = trav (f . g)`,
the three passes inside the middle `trav (…)` run as a single traversal. The
passes in `validationCata` need state that flows down the tree (the choices
made so far), so they are written as `Reader`-based traversals instead.

## Goal scopes and qualified goals

By default, one package name gets one instance in the whole solution, because a
program cannot in general link two instances of the same package. Some
dependencies do not have this restriction, and for those the solver can solve
the same package more than once. It does this by **qualifying** goals: every
package goal is a `QPN = Qualified PackageName`, which pairs the name with a
`GoalScope` (`Distribution/Solver/Types/GoalScope.hs`):

```haskell
data GoalScope = GoalScope Stage Namespace Qualifier
```

Two goals with the same package name but different scopes are different
variables, so they can resolve to different instances.

- **`Qualifier`** says why this goal may differ from the top-level goal for the
  same package:
  - `QualToplevel` — an ordinary dependency.
  - `QualSetup p` — a dependency of `p`'s custom `Setup.hs` (`setup-depends`).
    The setup script is a separate program, so its dependencies need not match
    the library's.
  - `QualExe p e` — a dependency of the executable `e`, needed as a
    `build-tool-depends` of `p`. The tool is only run, never linked with `p`.
    The qualifier names both packages so that two packages can use different
    builds of the same tool.
  - `QualBase p` — `p`'s dependency on `base`, only when some installed `base`
    depends on another `base` (base shims, `qoBaseShim`).
- **`Namespace`** is `DefaultNamespace` unless `--independent-goals` is on. In
  that case each target `t` gets `Independent t`, so separate targets are
  solved without having to agree with each other.
- **`Stage`** is `Host` or `Build`. It says which toolchain the goal is solved
  for. In a cross build, tool dependencies (setup and `build-tool-depends`)
  are solved one stage earlier: `prevStage` turns `Host` into `Build`. In a
  native build there is only the `Host` stage, and `prevStage` keeps tools at
  `Host`.

`qualifyDeps` in `Modular/Dependency.hs` is the only place that decides these
scopes. When the solver chooses an instance, its dependencies are qualified
relative to the scope of the goal that chose it:

- a setup dependency gets `QualSetup p`, an executable dependency gets
  `QualExe p e`, and a shimmed `base` gets `QualBase p`;
- any other dependency **inherits** the parent's qualifier. A regular
  dependency of a setup dependency is therefore still in `p`'s setup scope. A
  `QualBase` parent is the exception: its dependencies go back to
  `QualToplevel`;
- the namespace is always inherited, and the stage only changes at tool
  boundaries.

Qualifiers do not nest. A setup dependency of a setup dependency gets a new
`QualSetup`, not `QualSetup` of `QualSetup`. Nesting would make the search
tree infinite (see the comment on `QualSetup`).

Qualification is **static**: it does not depend on flag choices, only on the
global `QualifyOptions`. Link validation relies on this (see `linkDeps`).

Scopes are printed in solver logs. The stage comes first, then the namespace,
then the qualifier. For example, `foo:setup.Cabal` is the `Cabal` goal of
`foo`'s setup script, `foo:happy:exe.array` is a dependency of the `happy`
executable that `foo` needs, and in a cross build `build:` appears in front of
build-stage goals.

### Linking and the single instance restriction

Qualified goals would allow the solver to pick a different `mtl` for every
setup script, but we only want that when it is necessary. Two mechanisms keep
the number of instances down.

**Linking.** When the solver makes a package choice for `foo:setup.mtl`, it can
choose a version, or it can **link** to a goal for the same package, version
and stage that has already been solved (for example, the top-level `mtl`). This
is the `Maybe GoalScope` in `POption`. Linking means using the very same
instance, so it is only valid if the two goals also agree on flags, stanzas and
all dependencies, which must be linked too. The pass that checks this is
`validateLinking` in `Modular/Linking.hs`. It keeps **link groups**: sets of
goals that must resolve to the same instance, even when some of them have not
been chosen yet. `preferLinked` puts link options before fresh choices.

**The single instance restriction (SIR).** GHC's package database can only hold
one instance of a given package version, so two unlinked goals in the same
stage must not pick the same version. `enforceSingleInstanceRestriction` in
`Modular/Preference.hs` turns such choices into a `MultipleInstances` failure.
Goals in different stages may pick the same version, because each stage has
its own store and toolchain.

User constraints use their own scope type, `ConstraintScope` in
`Distribution/Solver/Types/PackageConstraint.hs`. It decides which goal scopes a
constraint applies to: build targets only, a particular qualifier, any setup
qualifier, or all qualifiers.

## The index

`convPIs` in `Modular/IndexConversion.hs` converts `cabal-install`'s installed
and source package indices into the solver's own `Index`
(`Modular/Index.hs`):

```haskell
type Index      = Staged StageIndex
type StageIndex = Map PackageName (Map I PInfo)

data PInfo = PInfo (FlaggedDeps PackageName)
                   (Map ExposedComponent ComponentInfo)
                   FlagInfo
                   (Maybe FailReason)
```

There is one `StageIndex` per active stage, and each one is converted against
that stage's compiler, platform and installed packages. `Staged`
(`Distribution/Solver/Types/Stage.hs`) always has a host value, and has a build
value only when the build toolchain differs from the host one (`isCross`). A
native build therefore has only the `Host` index. The stages of the index are
also how the solver decides that it is cross-compiling: `defaultQualifyOptions`
copies them into `qoStages`, which `prevStage` consults.

For each instance, `PInfo` records:

- **`FlaggedDeps`**: all dependencies of all components, as a tree. `Simple`
  dependencies always apply. `Flagged` dependencies are guarded by a flag, with
  a true branch and a false branch. `Stanza` dependencies apply when a test or
  benchmark stanza is enabled. Each dependency records its component. A
  dependency can be on a library or executable of a package (with a version
  range or, for installed packages, a fixed instance), on a language
  extension, on a language, or on a pkg-config package. Conditionals on OS,
  architecture and compiler are already decided here, per stage. Only flags
  and stanzas are left for the solver.
- the **components** the instance provides, and whether each is visible and
  buildable;
- the **flags**, with defaults, manual/automatic, and whether they are weak
  (automatic flags are weak unless `--strong-flags` is given);
- an optional **failure reason** that disables the instance for reasons outside
  the solver: shadowed, broken, or a `cabal-version` that this
  `cabal-install` does not support.

Installed packages are converted with fixed dependencies on the exact unit ids
they were built against.

## The search tree

`Modular/Tree.hs`:

```haskell
data Tree d c =
    PChoice QPN RevDepMap c (WeightedPSQ [Weight] POption (Tree d c))
  | FChoice QFN RevDepMap c WeakOrTrivial FlagType Bool (WeightedPSQ [Weight] Bool (Tree d c))
  | SChoice QSN RevDepMap c WeakOrTrivial (WeightedPSQ [Weight] Bool (Tree d c))
  | GoalChoice RevDepMap (PSQ (Goal QPN) (Tree d c))
  | Done RevDepMap d
  | Fail ConflictSet FailReason
```

Nodes are choices and edges are values. The search tree is not the dependency
graph.

- **`PChoice`**, **`FChoice`** and **`SChoice`** choose a value for one
  variable: a package option, a flag value, or whether a stanza is enabled.
  The children are kept in a `WeightedPSQ`, a list that is stably sorted by
  weight. Exploration tries them in that order.
- **`GoalChoice`** chooses which open goal to solve next. Its semantics differ
  from the other nodes: all children have the same solutions, just in a
  different order. Exploration therefore only ever enters **one** child of a
  goal choice. Pruning a goal choice is a way to influence goal order, not a
  way to lose solutions.
- **`Done`** is a solution. **`Fail`** is a dead end. A `Fail` node records a
  conflict set and a reason. Passes put a `Fail` node in place of a subtree
  rather than deleting it, so the information is still there for backjumping
  and for error messages.

Every node carries a `RevDepMap`: the goals seen so far and who depends on
them. Cycle detection and the final install plan use it. The `c` parameter is
usually the `QGoalReason` of the goal, and `d` becomes the full `Assignment`
before exploration.

The module also provides the recursion schemes that every pass is built from:
`cata`, `ana`, `para` and `trav`.

## Phase 1: building the tree

`buildTree` in `Modular/Builder.hs` builds the tree as an anamorphism over a
`BuildState`. The state holds the index, the reverse dependency map and the
list of **open goals**. The builder alternates two kinds of steps:

- With open goals left, it makes a `GoalChoice` node with one child per open
  goal (`splits`). With no open goals left, it makes a `Done` node.
- For the chosen goal, it makes the node for that kind of goal:
  - a **package goal** becomes a `PChoice` with one child per instance in the
    stage's index, or a `Fail UnknownPackage` if there is none. Choosing an
    instance adds new open goals for that instance's flags, its stanzas and
    its unconditional dependencies (`scopedExtendOpen`). This is where
    `qualifyDeps` runs;
  - a **flag goal** becomes an `FChoice` with the default value first. Each
    branch adds the dependencies under that branch;
  - a **stanza goal** becomes an `SChoice`, with "disabled" first.

A package goal is only added once per qualified name. Later dependencies on it
only add reverse-dependency edges. The builder does not check versions,
extensions or anything else. That is why the tree contains non-solutions.

At the same time, `addLinking` adds a linked option next to each fresh package
option, for every related goal (same package, instance and stage) that was
chosen without linking higher up in the tree.

## Phase 2: pruning and validation

Each pass checks one rule. When a choice breaks the rule, the pass puts a
`Fail` node with a conflict set and a reason in place of the subtree. In order
of application:

| Pass | Module | Rule | Typical `FailReason` |
| --- | --- | --- | --- |
| `avoidReinstalls` (only with `--avoid-reinstalls`) | `Preference` | do not choose a source version that is already installed | `CannotReinstall` |
| `onlyConstrained` (only with `--reject-unconstrained-dependencies=all`) | `Preference` | every package must be a target or have a version constraint | `NotExplicit` |
| `validateTree` | `Validate` | dependencies, extensions, languages, pkg-config packages and required components are consistent with the choices made | `ConflictingConstraints`, `UnsupportedExtension`, `MissingPkgconfigPackage`, … |
| `validateLinking` | `Linking` | linked goals agree on flags, stanzas and dependencies | `DependenciesNotLinked` |
| `enforceSingleInstanceRestriction` | `Preference` | at most one unlinked goal per package instance and stage | `MultipleInstances` |
| `enforceManualFlags` | `Preference` | manual flags keep their default value unless a constraint sets them | `ManualFlag` |
| `enforcePackageConstraints` | `Preference` | user and project constraints on versions, installed/source, flags and stanzas | `GlobalConstraintVersion`, … |
| `detectCyclesPhase` | `Cycles` | the chosen packages do not depend on each other in a cycle | `CyclicDependencies` |

`validateTree` is the main pass. As it goes down the tree, it keeps a
**pre-assignment**: for every package it has seen, either the instance chosen
or the list of version ranges required so far, each with the reason that
required it. Each package choice "activates" the dependencies of the chosen
instance, under the flag and stanza values already known. Each flag or stanza
choice activates the dependencies under that branch. New constraints are
merged into the pre-assignment, and a conflict is reported as soon as a fixed
choice and a range disagree. Range checks are lazy: two ranges are not compared
with each other until one side is a fixed choice. A flag can appear more than
once in the tree, because more than one dependency can introduce it. Once a
value is known, later `FChoice` nodes for the same flag are collapsed to that
branch.

Cycle detection runs last, after the reordering passes, just before
exploration. It is written as a separate pass because it was added much later.
This is the one exception to "the tree is valid after validation".

## Phase 3: preferences and goal heuristics

Reordering changes which solution is found first, and how fast. It never
changes which solutions exist.

**Preferences** set the weights of package, flag and stanza options:

- `preferLinked`: prefer linking to an existing goal over making a fresh
  choice.
- `preferPackagePreferences`: first prefer versions in more preferred ranges
  (for example Hackage's preferred versions). Then, depending on the
  `InstalledPreference` (`PreferInstalled`, `PreferLatest` or
  `PreferOldest`), prefer installed instances and newer versions, or newer
  versions first, or older versions first. Stanza preferences are applied
  here too: a stanza that `cabal-install` would like enabled is tried enabled
  first, and is marked weak so that it is decided late.

Weights are lists. A pass applied later puts its weight at the front of the
list, so it takes precedence.

**Goal heuristics** change `GoalChoice` nodes, and so the order in which
goals are solved. By default, in order of application:

1. `preferBaseGoalChoice`: if `base` is an open goal, solve it first. There is
   usually only one choice, and fixing it early gives better error messages.
2. `deferWeakFlagChoices`: put weak flags and trivial flags and stanzas (ones
   that add no dependencies) last.
3. `deferSetupExeChoices`: put setup and executable goals after regular ones,
   so that by the time they are solved there is a regular goal to link to.
4. With `--reorder-goals`, `preferReallyEasyGoalChoices`: if some goal has zero
   or one remaining choice, keep only goals like that. This finds dead ends
   sooner, but each step costs more.

If a goal order is given in `SolverConfig` (`goalOrder`), the passes above are
replaced with a sort by that order, followed by `firstGoal`. The error-message
reruns described below use this.

Exploration then picks one child of each goal choice. With `--count-conflicts`
(the default), it picks the goal that has appeared in the most conflicts so
far. Otherwise it picks the first one.

## Phase 4: exploration and backjumping

`backjumpAndExplore` in `Modular/Explore.hs` first records the full assignment
on each `Done` node (`assign`). It then walks the tree with a paramorphism that
returns, for each subtree, either a solution or a **conflict set**.

A **conflict set** (`Modular/ConflictSet.hs`) is a set of variables that
together explain why a subtree has no solution. The idea is that some other
value for at least one of these variables is needed to avoid the failure.
`Fail` nodes start with the conflict set from the pass that created them, and
exploration combines them on the way up:

- **Backjumping.** At a choice node for variable `v`, if a child fails with a
  conflict set that does **not** contain `v`, then no other value of `v` can
  help. The node fails at once with that conflict set, and the remaining
  children are skipped. (`EnableBackjumping` in `SolverConfig` turns this
  off. There is no command-line option for it; the QuickCheck tests use it.)
  If every child fails with a set that contains
  `v`, the node fails with the union of those sets plus the goal's reason
  (`avoidSet`). The goal's reason is included because the goal only exists
  because of the choices that introduced it.
- **Fine-grained conflicts** (`--fine-grained-conflicts`, the default). Entries
  in a conflict set can say *how* a variable took part (`Conflict`): it
  introduced the failing goal, its constraint excluded a version, or its
  version was excluded by a constraint. Before trying the next version of a
  package, `couldResolveConflicts` checks whether that version could avoid the
  previous conflict. For example, the solver skips the version if it has the
  same dependency, or if it is excluded by the same range. Skipped versions are
  logged as such.
- **Conflict counting.** Every conflict set seen adds to a per-variable count in
  the `ConflictMap`. `--count-conflicts` uses these counts to pick goals, and
  error messages use them to rank the variables that caused the most trouble.
- **Backjump limit.** Exploration counts backjumps and stops at
  `--max-backjumps`. The default in `cabal-install` is 4000. A negative value
  means no limit, and 0 turns off backtracking completely.

The result is a `RetryLog` (`Modular/RetryLog.hs`) of solver messages
(`Modular/Message.hs`). It is a difference-list form of `Progress` that makes
it cheap to attach a retry after a failure. `Modular.hs` relies on this for
error messages.

## From a solution to an install plan

`modularResolver` in `Distribution/Solver/Modular.hs` is the interface that
`cabal-install` calls (via `resolveDependencies` in
`Distribution/Client/Dependency.hs`). After a successful search:

1. `toCPs` (`Modular/Assignment.hs`) builds the dependency graph from the
   `RevDepMap`, sorts it topologically, and produces one configured package
   (`CP`) per qualified goal, with its flags, stanzas and per-component
   dependencies.
2. `convCP` (`Modular/ConfiguredConversion.hs`) turns each one into a
   `ResolverPackage`: `PreExisting` for installed instances (looked up in the
   installed index of their stage) or `Configured` for source packages. It
   **drops the qualifiers**, but keeps the stage in the `SolverId`. So linked
   goals become identical packages, and `ordNubBy nodeKey` removes the
   duplicates.
3. Back in `cabal-install`, `validateSolverResult` checks the plan again,
   independently of the solver, before turning it into a `SolverInstallPlan`.
   The HIW talk gave this check as the reason it is easy to trust the solver:
   a solver bug shows up as a failed check, not as a broken plan.

## Error messages

When there is no solution, the first run's log is usually long and hard to
follow. `solve'` in `Distribution/Solver/Modular.hs` therefore **runs the
solver again** to build the message:

- With the final conflict set from the first run, the solver is run again with a
  goal order that solves the conflict-set variables first, and with a backjump
  limit of zero. The resulting short log, up to the first backjump, is
  printed as "Could not resolve dependencies:". It is followed by the
  variables that caused the most conflicts.
- With `--minimize-conflict-set`, the solver first tries to remove one variable
  at a time from the conflict set, rerunning once per variable, until the set
  is minimal.
- If the first run hit the backjump limit, there is no final conflict set. The
  solver is rerun with `pruneAfterFirstSuccess`, which keeps only the path to
  the first backjump, to get one.

## How the design has moved on

These are the main differences from the 2011 talk and the 2015 posts:

- **Stanzas** are a separate kind of choice (`SChoice`), alongside packages and
  flags.
- **Weights.** Options are ordered by weights in a `WeightedPSQ`, not by
  sorting the children of each node.
- **Qualified goals.** The "scoped goals" for private dependencies planned in
  the talk were first used for setup dependencies (2015). They are now also
  used for executable dependencies, base shims, independent goals and build
  stages, through `GoalScope`. The solver has no general private dependencies.
- **Stages.** The index, the compiler and the pkg-config database are kept per
  `Stage`, and goals carry their stage. The single instance restriction and
  linking apply within a stage.
- **Goal choices are no longer cut to the first goal** (`firstGoal`) by
  default. That workaround for a space leak is no longer needed, and
  exploration enters only one goal anyway. `--count-conflicts` decides which
  one.
- **Conflict sets carry `Conflict` details.** This is what makes fine-grained
  conflicts, conflict counting and the error-message reruns possible.
- **Components.** The index records components and whether they are visible
  and buildable. Validation checks that every component someone depends on
  exists, is visible and can be built.
- **Cycle detection** was added as its own pass, just before exploration.
- **The old solver is gone.** The modular solver is the only one.

Some comments in the code may still refer to things that are gone. If you find
one, fix it in the same change.

## Debugging and testing

- Below `-v2`, the main solver run throws its log away, and only the
  error-message reruns produce one (`displayLogMessages`). From `-v2` up
  (`solverVerbosity >= Verbose`), every run keeps its log, and `cabal build -v3`
  prints it.
- Building `cabal-install-solver` with the `debug-tracetree` flag makes
  `traceTree` write the tree after each phase to JSON files (`build.json`,
  `pruned.json`, `semivalidated.json`, `heuristics.json`, `cycles.json`). This
  is only practical for small examples, such as the unit tests. The
  `debug-expensive-assertions` flag turns on extra checks, such as the
  link-group invariant.
- Unit tests are in `cabal-install/tests/UnitTests/Distribution/Solver/Modular/`.
  `Solver.hs` has hand-written cases that use a small DSL (`DSL.hs`) for
  describing package databases. `QuickCheck.hs` checks properties on random
  databases, for example that target and goal order, backjumping and
  fine-grained conflicts do not change whether a solution exists.
  `MemoryUsage.hs` checks that exploring a search tree of exponential size
  does not use memory in proportion to the tree, including the linking space
  leak from #2899.
- `solver-benchmarks/` compares two `cabal` executables on many Hackage packages
  and reports differences in results and run time.

## Module map

All modules are under `cabal-install-solver/src/Distribution/Solver/`.

| Module | Role |
| --- | --- |
| `Modular` | Interface used by `cabal-install`: converts the inputs, runs the solver, builds error messages, converts the result |
| `Modular/Solver` | `solve`: the pipeline of phases, and `SolverConfig` |
| `Modular/IndexConversion` | installed and source indices to `Index` |
| `Modular/Index` | `Index`, `PInfo`, `defaultQualifyOptions` |
| `Modular/Package`, `Modular/Version`, `Modular/Flag`, `Modular/Var` | instances, versions, flag and stanza names, variables |
| `Modular/Dependency` | `FlaggedDeps`, goals and goal reasons, `qualifyDeps` |
| `Types/GoalScope` | `GoalScope`, `Qualifier`, `Namespace`, `QPN` |
| `Modular/Tree` | the search tree and its recursion schemes |
| `Modular/PSQ`, `Modular/WeightedPSQ` | ordered containers for the children of a node |
| `Modular/Builder` | phase 1: tree building and link options |
| `Modular/Validate` | phase 2: the main validation pass |
| `Modular/Linking` | phase 2: link validation and link groups |
| `Modular/Preference` | phase 2 constraint passes and phase 3 preferences and heuristics |
| `Modular/Cycles` | phase 2: cycle detection |
| `Modular/Explore` | phase 4: exploration and backjumping |
| `Modular/ConflictSet` | conflict sets and the conflict map |
| `Modular/Assignment`, `Modular/Configured`, `Modular/ConfiguredConversion` | from an assignment to `ResolverPackage`s |
| `Modular/RetryLog`, `Modular/Log`, `Modular/Message`, `Modular/MessageUtils` | logs and messages |
| `Modular/LabeledGraph` | graph utilities |
