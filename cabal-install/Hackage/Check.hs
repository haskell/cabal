-----------------------------------------------------------------------------
-- |
-- Module      :  Hackage.Check
-- Copyright   :  (c) Lennart Kolmodin 2008
-- License     :  BSD-like
--
-- Maintainer  :  kolmodin@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Check a package for common mistakes
--
-----------------------------------------------------------------------------
module Hackage.Check (
    check
  ) where

import Control.Monad ( when, unless )

import Distribution.PackageDescription.Parse ( readPackageDescription )
import Distribution.PackageDescription.Check
import Distribution.PackageDescription.Configuration ( flattenPackageDescription )
import Distribution.Verbosity ( Verbosity )
import Distribution.Simple.Utils ( defaultPackageDesc, toUTF8 )

check :: Verbosity -> IO Bool
check verbosity = do
    pdfile <- defaultPackageDesc verbosity
    ppd <- readPackageDescription verbosity pdfile
    -- flatten the generic package description into a regular package
    -- description
    -- TODO: this may give more warnings than it should give;
    --       consider two branches of a condition, one saying
    --          ghc-options: -Wall
    --       and the other
    --          ghc-options: -Werror
    --      joined into
    --          ghc-options: -Wall -Werror
    --      checkPackages will yield a warning on the last line, but it
    --      would not on each individual branch.
    --      Hovever, this is the same way hackage does it, so we will yield
    --      the exact same errors as it will.
    let pkg_desc = flattenPackageDescription ppd
    ioChecks <- checkPackageFiles pkg_desc "."
    let packageChecks = ioChecks ++ checkPackage ppd (Just pkg_desc)
        buildImpossible = [ x | x@PackageBuildImpossible {} <- packageChecks ]
        buildWarning    = [ x | x@PackageBuildWarning {}    <- packageChecks ]
        distSuspicious  = [ x | x@PackageDistSuspicious {}  <- packageChecks ]
        distInexusable  = [ x | x@PackageDistInexcusable {} <- packageChecks ]

    unless (null buildImpossible) $ do
        putStrLn "The package will not build sanely due to these errors:"
        mapM_ (putStrLn . toUTF8. explanation) buildImpossible
        putStrLn ""

    unless (null buildWarning) $ do
        putStrLn "The following warnings are likely affect your build negatively:"
        mapM_ (putStrLn . toUTF8 . explanation) buildWarning
        putStrLn ""

    unless (null distSuspicious) $ do
        putStrLn "These warnings may cause trouble when distributing the package:"
        mapM_ (putStrLn . toUTF8 . explanation) distSuspicious
        putStrLn ""

    unless (null distInexusable) $ do
        putStrLn "The following errors will cause portability problems on other environments:"
        mapM_ (putStrLn . toUTF8 . explanation) distInexusable
        putStrLn ""

    let isDistError (PackageDistSuspicious {}) = False
        isDistError _                          = True
        errors = filter isDistError packageChecks

    unless (null errors) $ do
        putStrLn "Hackage would reject this package."

    when (null packageChecks) $ do
        putStrLn "No errors or warnings could be found in the package."

    return (null packageChecks)
