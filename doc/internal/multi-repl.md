---
author: matthew
title: Multiple Component support for cabal repl
postName: cabal-multi-unit
categories: cabal, ghc, hls, hasura, open-source
showtoc: true
---

Following on from [our work implementing support for compiling multiple units
at once in GHC](https://well-typed.com/blog/2022/01/multiple-home-units/), we have now been extending the ecosystem to take
advantage of this new support. This work has once again been made possible by
[Hasura](https://hasura.io/). This work continues our productive and
[long-running
collaboration](https://well-typed.com/blog/2022/05/hasura-supports-haskell-tooling/)
on important and difficult tooling tasks which will ultimately benefit the
entire ecosystem.

This post focuses on updates to the `cabal repl` command, allowing
multiple components to be loaded at once into an interactive session. The work is being
reviewed in [Cabal MR #8726](https://github.com/haskell/cabal/pull/8726), and should
be available in a future release of `cabal-install`.

<!-- more -->

# Multiple Component Repl

When using `cabal`, most commands take a "target" which specifies which units you want
to operate on. A command such as `cabal build <target>` will resolve all the units that
the target `<target>` resolves to, and build all of them. The behaviour of the `cabal repl`
command is different: you must specify a single unit to build.

Here are some common targets which you can specify when using `cabal`.

* `all`: Build all the locally defined components.
* `exe:haskell-language-server`: Build the executable called `haskell-language-server`
* `lib:pkg-a lib:pkg-b`: Build the local libraries pkg-a and pkg-b. pkg-a will be the active unit.
* `src/Main.hs`: Build the unit which `src/Main.hs` belongs to.

After enabling multi-repl, passing a target specification to `cabal repl` which
resolves to multiple units will load all those units into a single repl session.
The first "target" will be the active unit.
For example:

```
cabal repl --enable-multi-repl lib:pkg-a lib:pkg-b
```

When the modules are compiled, the unit which they came from is listed next
to the module name. The `interactive` herald in the build plan indicates that
the library will be loaded into GHCi rather than being built like a normal package.

```
In order, the following will be built (use -v for more details):
 - pkg-a-0 (interactive) (lib) (first run)
 - pkg-b-0 (interactive) (lib) (dependency rebuilt)
Preprocessing library for pkg-a-0..
Preprocessing library for pkg-b-0..
GHCi, version 9.4.3: https://www.haskell.org/ghc/  :? for help
[1 of 2] Compiling Foo[pkg-a-0-inplace]
[2 of 2] Compiling Bar[pkg-b-0-inplace]
Ok, two modules loaded.
```

You will need to use at least `ghc-9.4.1` in order to use multiple unit support.
It's advised to use `ghc-9.4.5` or `9.6.1`, in order to benefit from bug fixes.

## Enabling Multi-repl

There are three ways to enable the multi-repl depending on how much you like it:

* Globally: Add `multi-repl: True` to your `~/.cabal/config` file.
* Project-wide: Add `multi-repl: True` to your cabal.project file.
* Per-invocation: Pass `--enable-multi-repl` when invoking `cabal repl`.

A future cabal version is likely to enable multi-repl by default. For the time being,
and due to the experimental nature of the command and lack of support in ghci for some features,
the multi-repl feature is opt-in.

# Closure Property for Multiple Home Units

For tools or libraries using the GHC API there is one very [important closure property](https://well-typed.com/blog/2022/01/multiple-home-units/#closure-property-for-home-units)
which must be adhered to:

> Any dependency which is not a home unit must not (transitively) depend on a home unit.

For example, if you have three units `p`, `q` and `r`, and `p` depends on `q` which depends on `r`, then it
is illegal to load both `p` and `r` as home units but not `q`, because `q` is a dependency of the home unit `p` which depends
on another home unit `r`.

`cabal` will automatically enable loading of all units which are needed by the closure
property (including non-local) packages. Given the previous example, if you specify
on the command line `cabal repl lib:p lib:q` then `lib:r` will also be loaded
into the same session as it is needed for the closure property.

# Configuring and Promised Dependencies

The lowest-level interface which the `Cabal` library provides in order to build a package
is the [`Setup.hs` script](https://cabal.readthedocs.io/en/3.10/setup-commands.html).
This consists of a normal Haskell file which depends on the `Cabal` library and can be executed
in order to build the package. This is done, after compiling `Setup.hs`, via the following invocations:

```
./Setup configure
./Setup build
```

The `configure` phase checks to make sure that everything is in order so that when
the build phase is run we know that all the environmental dependencies have already
been provisioned by the user.

In the very old days, people would compile and run `Setup.hs` themselves in order to
build a package, but these days, all the interactions with `Setup.hs` are managed by a
higher-level build tool such as `cabal-install`, `stack` or `nix`. All of these tools
ultimately call `Setup.hs` scripts.

The main technical change to enable the multi-repl was to modify the `Setup.hs`
scripts to allow you to configure a package before all its dependencies are
built. Now you can **promise** to `Setup.hs`
that a certain dependency will be built by the time we attempt to build the unit. Since
all units in a project are going to be built at the same time with one GHC invocation, they
all need to be configured before anything else is built. So we just **promise** that all local
packages will be built.

```
./Setup configure --promised-dependency=pkg-a
```

In addition to the `configure` and `build` commands, `Setup.hs` also provides a `repl`
command which starts `GHCi` and loads a single component.

```
./Setup repl
```

This design is quite awkward because the `Setup.hs` scripts operate on a per-component basis. The
interface is not aware of the existence of complicated multi-component projects, that is solely the
domain of higher-level tools like `cabal-install`. Therefore, instead of starting the repl from
the `Setup.hs` script, we need to start a multi-repl from `cabal-install`. However, the `Setup.hs`
script is still responsible for computing the arguments we need to pass to GHC in order to compile
that component. The solution is to allow the `repl` command to write its arguments into a file
so that they can be collected later by `cabal-install` to correctly start a multi-component session.

```
./Setup repl --repl-multi-file=multi-args
# Arguments can be found in the `multi-args` directory.
```

This allows all the units in your project to be configured before any of them are built.
After a project is configured, the `Setup` executable can be consulted to find out what
options GHC **would** use to build the unit, and because we have **promised** to
make sure things are built in the right order, we can supply these options to GHC
in order to start a multi unit GHCi session.

# HLS support for multiple home units

Zubin has already updated HLS to use native multiple home unit support for GHC-9.4.

The missing piece has been a mechanism to set up a multi component session which
satisfies the closure property. Without such a mechanism, HLS would construct a multiple component session
incrementally by adding units to a session as they are opened by the user. For a complicated
project structure, users would have to very carefully load their files in the right order to
get a session which worked correctly.
Even worse, this doesn't even work when a non-local package is needed to satisfy the
closure property.

HLS consults cabal in order to set up a session: it invokes `cabal repl`
and intercepts the final call to `ghc` which would start the repl. That command is then
used as the options which are needed for the session in order to compile that unit.

Now that `cabal repl` supports creating a command line which specifies the options
for multiple components at once, it makes sense to augment the HLS session loading logic
to also understand these command lines in order to set up a whole multi-component session
at once.

HLS now can understand and parse the kind of command line produced by a multiple
component session. As a result:

* The correct session is initialised up-front. Loading any component in your
  local project will work seamlessly and quickly.
* The time taken to initialise a session is reduced, because no local dependencies
  are built before the session is started. All local components are configured
  before anything is built.
* Cabal ensures the closure property holds, even for non-local packages.

I have been testing this support when working on `cabal` and `ghc`, both projects
with many local dependencies and the experience is much improved. In particular for
`cabal`, the non-local `hackage-security` package is needed for the closure property but could
never be loaded before. This made using HLS on `cabal` very error-prone because if
you opened a file from the `Cabal` library and `cabal-install` library, you would
break the session without a way to recover it. For `ghc`, it is a lifeline to be able to
edit packages like `template-haskell` and see the changes ripple upwards through all
the boot libraries and compiler.

# Limitations

Now that there is a way to easily create and invoke a multi-repl session,
users are probably going to run into limitations of the multi-repl.

Many features are not yet implemented because there is not a good way to change what
the "active unit" of the repl session is. Some more careful thinking needs to be done
to modify the GHCi interface in order to work nicely with multiple components in all situations.

At this time, the multi-repl is best used for interactive development situations where
you want to use the repl to obtain fast-feedback about your project.
We have made sure that the multi-repl works with `ghcid` for example.

When evaluating code, make sure that the code is in the scope of the active unit,
which is the first target given on the command line. For example, to run the test suite
entrypoint, use:

```
ghcid --command "cabal repl --enable-multi-repl test:suite lib:pkg" --test Main.main
```

# Conclusion

Adding `cabal repl` support for multiple home units allows developers to easily
interact with multiple home unit support in GHC. There are still limitations to
the repl supported in multiple unit sessions, but as more users start using and wanting this
feature we hope to expand the repl to work properly with multiple home units as well.

Well-Typed is able to work on GHC, HLS, Cabal and other core Haskell
infrastructure thanks to funding from various sponsors. If your company might be
able to contribute to this work, sponsor maintenance efforts, or fund the
implementation of other features, please
[read about how you can help](/blog/2022/11/funding-ghc-maintenance) or
[get in touch](mailto:info@well-typed.com).
