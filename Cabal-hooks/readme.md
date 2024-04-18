# `Cabal-hooks`

This library provides an API for the `Cabal` `Hooks` build type.

## What is the `Hooks` build type?

The `Hooks` build type is a new `Cabal` build type that is scheduled to
replace the `Custom` build type, providing better integration with
the rest of the Haskell ecosystem.

The original specification for the `Hooks` build type can be found in
the associated [Haskell Foundation Tech Proposal](https://github.com/haskellfoundation/tech-proposals/pull/60).

These *setup hooks* allow package authors to customise the configuration and
building of a package by providing certain hooks that get folded into the
general package configuration and building logic within `Cabal`.

## Defining a package with custom hooks

To use the `Hooks` build type, you will need to

  * Update your `.cabal` file by:

      - using `cabal-version >= 3.14`,
      - declaring `build-type: Hooks`,
      - declaring a `custom-setup` stanza, with a `setup-depends`
        field which includes a dependency on `Cabal-hooks`.
  
  * Define a Haskell module `SetupHooks`, which must be placed
    at the root of your project and must define a value
    `setupHooks :: SetupHooks`.

That is, your `.cabal` file should contain the following

```cabal
-- my-package.cabal
cabal-version: 3.14
name: my-package
build-type: Hooks

custom-setup
  setup-depends:
    Cabal-hooks >= 0.1 && < 0.2
```

and your `SetupHooks.hs` file should look like:

```haskell
-- SetupHooks.hs
module SetupHooks ( setupHooks ) where

-- Cabal-hooks
import Distribution.Simple.SetupHooks

setupHooks :: SetupHooks
setupHooks = ...
  -- use the API provided by 'Distribution.Simple.SetupHooks'
  -- to define the hooks relevant to your package
```

## Using the API

The [Haddock documentation](https://hackage.haskell.org/package/Cabal-hooks)
should help you get started using this library's API.
