{-# LANGUAGE DeriveGeneric #-}
module Distribution.Types.ComponentEnabledSpec (
    -- $buildable_vs_enabled_components

    ComponentEnabledSpec(..),
    ComponentDisabledReason(..),

    defaultComponentEnabled,

    componentEnabled,
    componentNameEnabled,
    componentDisabledReason,
    componentNameDisabledReason,
) where

import Prelude ()
import Distribution.Compat.Prelude

import Distribution.Types.Component -- TODO: maybe remove me?
import Distribution.Types.ComponentName

-- $buildable_vs_enabled_components
-- #buildable_vs_enabled_components#
--
-- = Note: Buildable versus enabled components
-- What's the difference between a buildable component (ala
-- 'componentBuildable') versus enabled component (ala
-- 'componentEnabled')?
--
-- A component is __buildable__ if, after resolving flags and
-- conditionals, there is no @buildable: False@ property in it.
-- This is a /static/ property that arises from the
-- Cabal file and the package description flattening; once we have
-- a 'PackageDescription' buildability is known.
--
-- A component is __enabled__ if it is buildable, and the user
-- configured (@./Setup configure@) the package to build it,
-- e.g., using @--enable-tests@ or @--enable-benchmarks@.
-- Once we have a 'LocalBuildInfo', whether or not a component
-- is enabled is known.
--
-- Generally speaking, most Cabal API code cares if a component
-- is enabled, as opposed to buildable. (For example, if you
-- want to run a preprocessor on each component prior to building
-- them, you want to run this on each /enabled/ component.)

-- | Describes what components are enabled by user-interaction.
-- See also this note in
-- "Distribution.Types.ComponentEnabledSpec#buildable_vs_enabled_components".
--
-- @since 2.0.0.0
data ComponentEnabledSpec
    = ComponentEnabledSpec {
        testsEnabled :: Bool,
        benchmarksEnabled :: Bool
   }
  deriving (Generic, Read, Show)
instance Binary ComponentEnabledSpec

-- | The default set of enabled components.  Historically tests and
-- benchmarks are NOT enabled by default.
--
-- @since 2.0.0.0
defaultComponentEnabled :: ComponentEnabledSpec
defaultComponentEnabled = ComponentEnabledSpec False False

-- | Is this component enabled?  See also this note in
-- "Distribution.Types.ComponentEnabledSpec#buildable_vs_enabled_components".
--
-- @since 2.0.0.0
componentEnabled :: ComponentEnabledSpec -> Component -> Bool
componentEnabled enabled = isNothing . componentDisabledReason enabled

-- | Is this component name enabled?  See also this note in
-- "Distribution.Types.ComponentEnabledSpec#buildable_vs_enabled_components".
--
-- @since 2.0.0.0
componentNameEnabled :: ComponentEnabledSpec -> ComponentName -> Bool
componentNameEnabled enabled = isNothing . componentNameDisabledReason enabled

-- | Is this component disabled, and if so, why?
--
-- @since 2.0.0.0
componentDisabledReason :: ComponentEnabledSpec -> Component
                        -> Maybe ComponentDisabledReason
componentDisabledReason enabled comp
    = componentNameDisabledReason enabled (componentName comp)

-- | Is this component name disabled, and if so, why?
--
-- @since 2.0.0.0
componentNameDisabledReason :: ComponentEnabledSpec -> ComponentName
                            -> Maybe ComponentDisabledReason
componentNameDisabledReason enabled (CTestName _)
    | not (testsEnabled enabled) = Just DisabledAllTests
componentNameDisabledReason enabled (CBenchName _)
    | not (benchmarksEnabled enabled) = Just DisabledAllBenchmarks
componentNameDisabledReason _ _ = Nothing

-- | A reason explaining why a component is disabled.
--
-- @since 2.0.0.0
data ComponentDisabledReason = DisabledComponent
                             | DisabledAllTests
                             | DisabledAllBenchmarks
