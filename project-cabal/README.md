# Cabal Projects

We have these projects, all in the root:

```
$ tree -P '*.project' --prune -L 1
.
├── cabal.bootstrap.project
├── cabal.meta.project
├── cabal.project
├── cabal.release.project
├── cabal.validate-libonly.project
└── cabal.validate.project
```

Projects are expected to pass a `build --dry-run` standalone test,
substituting the actual project filename for `cabal.project` in
`--project-file=cabal.project`:

```
$ cabal build all --enable-tests --enable-benchmarks --dry-run \
  --project-file=cabal.project
```

The `release` project might fail to resolve dependencies with the latest GHC
compiler with its `index-state` pinned but it should build if unpinned, by
removing `index-state` from the project or setting it to `HEAD`:

```
$ cabal build all --enable-tests --enable-benchmarks \
  --project-file=cabal.project.release \
  --index-state="hackage.haskell.org HEAD"
```

## Configuration

Any project configuration that is not itself a project should use a `.config`
extension and be put into the `project-cabal` folder:

```
$ tree -P '*.config' project-cabal
project-cabal
├── constraints.config
├── ghc-latest.config
├── ghc-options.config
├── pkgs
│   ├── benchmarks.config
│   ├── buildinfo.config
│   ├── cabal.config
│   ├── install.config
│   ├── integration-tests.config
│   └── tests.config
└── pkgs.config

2 directories, 10 files
```

## Package Groups

We have one `project-cabal/pkgs.config` that includes all package groups.

```
$ cat project-cabal/pkgs.config
import: pkgs/cabal.config
import: pkgs/install.config
import: pkgs/buildinfo.config
import: pkgs/tests.config
import: pkgs/integration-tests.config
import: pkgs/benchmarks.config
```

The default and `validate` projects get their packages this way. The `libonly`,
and `validate.libonly` projects import packages from `cabal` and `tests` package
groups. The `release` project also does this but also imports the `install`
package group.

| Project          | pkgs | cabal | tests | install |
|------------------|:---: |:---:  |:---:  |:---:    |
| default          | ✓    |       |       |         |
| libonly          |      | ✓     | ✓     |         |
| release          |      | ✓     | ✓     | ✓       |
| validate         | ✓    |       |       |         |
| validate.libonly |      | ✓     | ✓     |         |

The `meta` project is a one-liner:

```
$ cat cabal.meta.project
packages: cabal-dev-scripts
```

## Extra Config

Additional configuration is imported:

| Project          | ghc-options | ghc-latest | constraints |
|------------------|:---:        |:---:       |:---:        |
| default          | ✓           | ✓          | ✓           |
| libonly          | ✓           |            |             |
| release          |             |            |             |
| validate         | ✓           | ✓          | ✓           |
| validate.libonly | ✓           |            | ✓           |
