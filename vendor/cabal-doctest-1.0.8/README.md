cabal-doctest
=============

[![Hackage](https://img.shields.io/hackage/v/cabal-doctest.svg)](https://hackage.haskell.org/package/cabal-doctest) [![Build Status](https://travis-ci.org/phadej/cabal-doctest.svg?branch=master)](https://travis-ci.org/phadej/cabal-doctest)

A `Setup.hs` helper for running `doctests`.

Simple example
--------------

For most use cases—a `.cabal` file with a single library containing
doctests—adapting the simple example located
[here](https://github.com/phadej/cabal-doctest/tree/master/simple-example)
will be sufficient. (Note that this example requires `Cabal-1.24` or later, but
you can relax this bound safely, although running doctests won't be supported
on versions of `Cabal` older than 1.24.)

To use this library in your `Setup.hs`, you should specify a `custom-setup`
section in your `.cabal` file. For example:

```
custom-setup
 setup-depends:
   base >= 4 && <5,
   Cabal,
   cabal-doctest >= 1 && <1.1
```

/Note:/ `Cabal` dependency is needed because of
[Cabal/GH-4288](https://github.com/haskell/cabal/issues/4288) bug.

You'll also need to specify `build-type: Custom` at the top of the `.cabal`
file. Now put this into your `Setup.hs` file:

```haskell
module Main where

import Distribution.Extra.Doctest (defaultMainWithDoctests)

main :: IO ()
main = defaultMainWithDoctests "doctests"
```

When you build your project, this `Setup` will generate a `Build_doctests`
module. To use it in a testsuite, simply do this:

```haskell
module Main where

import Build_doctests (flags, pkgs, module_sources)
import Data.Foldable (traverse_)
import System.Environment.Compat (unsetEnv)
import Test.DocTest (doctest)

main :: IO ()
main = do
    traverse_ putStrLn args -- optionally print arguments
    unsetEnv "GHC_ENVIRONMENT" -- see 'Notes'; you may not need this
    doctest args
  where
    args = flags ++ pkgs ++ module_sources
```

(The `System.Environment.Compat` module is from the `base-compat`
package. That's already in the transitive closure of `doctest`'s
dependencies. `System.Environment.unsetEnv` was added with GHC 7.8 so,
if you don't need to support versions of GHC older than 7.8, you can
use `System.Environment` from `base` instead.)

Example with multiple .cabal components
---------------------------------------

`cabal-doctest` also supports more exotic use cases where a `.cabal` file
contains more components with doctests than just the main library, including:

* Doctests in executables
* Doctests in Internal libraries (if using `Cabal-2.0` or later)

Unlike the simple example shown above, these examples involve _named_
components. You don't need to change the `Setup.hs` script to support
this use case. However, in this scenario `Build_doctests` will generate extra
copies of the `flags`, `pkgs`, and `module_sources` values for each additional
named component.

Simplest approach is to use `x-doctest-components` field, for example
```
x-doctest-components: lib lib:internal exe:example
```

In that case, the testdrive is general:

```haskell
module Main where

import Build_doctests (Component (..), components)
import Data.Foldable (for_)
import System.Environment.Compat (unsetEnv)
import Test.DocTest (doctest)

main :: IO ()
main = for_ components $ \(Component name flags pkgs sources) -> do
    print name
    putStrLn "----------------------------------------"
    let args = flags ++ pkgs ++ sources
    for_ args putStrLn
    unsetEnv "GHC_ENVIRONMENT"
    doctest args
```

There's also a more explicit approach: if you have an executable named `foo`,
then separate values named `flags_exe_foo`, `pkgs_exe_foo`, and `module_sources_exe_foo` will
be generated in `Build_doctests`. If the name has hyphens in it
(e.g., `my-exe`), then `cabal-doctest` will convert those hyphens to
underscores (e.g., you'd get `flags_my_exe`, `pkgs_my_exe`, and
`module_sources_my_exe`).
Internal library `bar` values will have a `_lib_bar` suffix.

An example testsuite driver for this use case might look like this:

```haskell
module Main where

import Build_doctests
       (flags,            pkgs,            module_sources,
        flags_exe_my_exe, pkgs_exe_my_exe, module_sources_exe_my_exe)
import Data.Foldable (traverse_)
import System.Environment.Compat (unsetEnv)
import Test.DocTest

main :: IO ()
main = do
    unsetEnv "GHC_ENVRIONMENT"
    -- doctests for library
    traverse_ putStrLn libArgs
    doctest libArgs

    -- doctests for executable
    traverse_ putStrLn exeArgs
    doctest exeArgs
  where
    libArgs = flags            ++ pkgs            ++ module_sources
    exeArgs = flags_exe_my_exe ++ pkgs_exe_my_exe ++ module_sources_exe_my_exe
```

See
[this example](https://github.com/phadej/cabal-doctest/tree/master/multiple-components-example)
for more details.

Additional configuration
------------------------

The `cabal-doctest` based `Setup.hs` supports few extensions fields
in `pkg.cabal` files to customise the `doctest` runner behaviour, without
customising the default `doctest.hs`.

```
test-suite doctests:
  if impl(ghc >= 8.0)
    x-doctest-options: -fdiagnostics-color=never
  x-doctest-source-dirs: test
  x-doctest-modules: Servant.Utils.LinksSpec

  ...
```

* `x-doctest-options` Additional arguments passed into `doctest` command.
* `x-doctest-modules` Additional modules to `doctest`. May be useful if you
  have `doctest` in test or executables (i.e not default library complonent).
* `x-doctest-src-dirs` Additional source directories to look for the modules.

Notes
-----

* Recent versions of `Cabal` (for instance, 2.0) can choose to build a
  package's `doctest` test suite _before_ the library. However, in order for
  `cabal-doctest` to work correctly, the library _must_ be built first, as
  `doctest` relies on the presence of generated files that are only created
  when the library is built. See
  [#19](https://github.com/phadej/cabal-doctest/issues/19).

  A hacky workaround for this problem is to depend on the library itself in a
  `doctests` test suite. See
  [the simple example's .cabal file](https://github.com/phadej/cabal-doctest/blob/master/simple-example/simple-example.cabal)
  for a demonstration. (This assumes that the test suite has the ability to
  read build artifacts from the library, a separate build component. In
  practice, this assumption holds, which is why this library works at all.)

* `custom-setup` section is supported starting from `cabal-install-1.24`.
  For older `cabal-install's` you have to install custom setup dependencies
  manually.

* `stack` respects `custom-setup` starting from version 1.3.3. Before that
  you have to use `explicit-setup-deps` setting in your `stack.yaml`.
  ([stack/GH-2094](https://github.com/commercialhaskell/stack/issues/2094))

* There is [an issue in the Cabal issue tracker](https://github.com/haskell/cabal/issues/2327)
  about adding `cabal doctest` command. After that command is implemented,
  this library will be deprecated.

* You can use `x-doctest-options` field in `test-suite doctests` to
  pass additional flags to the `doctest`.

* For `build-type: Configure` packages, you can use
  `defaultMainAutoconfWithDoctests` function to make custom `Setup.hs` script.

* If you use the default `.` in `hs-source-dirs`, then running `doctests`
  might fail with weird errors (ambiguous module errors). Workaround is
  to move sources under `src/` or some non-top-level directory.

* `extensions:` field isn't supported. Upgrade your `.cabal` file to use at least
  `cabal-version: >= 1.10` and use `default-extensions` or `other-extensions`.

* If you use QuickCheck properties (`prop>`) in your doctests,
  the `test-suite doctest` should depend on `QuickCheck` and `template-haskell`.
  This is a little HACK: These dependencies aren't needed to build the
  `doctests` test-suite executable.  However, as we let `Cabal` resolve
  dependencies, we can pass the resolved (and installed!) package identifiers to
  to the `doctest` command.  This way, `QuickCheck` and `template-haskell` are
  available to `doctest`, otherwise you'll get errors like:

```
    Variable not in scope:
      mkName
        :: [Char]
           -> template-haskell-2.11.1.0:Language.Haskell.TH.Syntax.Name
```

or

```
    Variable not in scope:
      polyQuickCheck
        :: Language.Haskell.TH.Syntax.Name -> Language.Haskell.TH.Lib.ExpQ
```

* From version 2, Stack sets the `GHC_ENVRIONMENT` variable, and GHC
  (as invoked by `doctest`) will pick that up. This is undesirable:
  `cabal-doctest` passes all the necessary information on the command
  line already, and can lead to ambiguous module errors as GHC will
  load the environment in addition to what `cabal-doctest` instructs
  it to.

  Hence, `cabal-doctest` tells GHC to ignore package environments
  altogether on the command line. However, this is only possible since
  GHC 8.2. If you are using `cabal-doctest` with Stack 2 and GHC 8.0
  or earlier and seeing ambiguous module errors or other mysterious
  failures, try manually unsetting `GHC_ENVIRONMENT` before invoking
  `doctest`.

Copyright
---------

Copyright 2017 Oleg Grenrus.

Available under the BSD 3-clause license.
