{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UnitTests.Distribution.Client.Described where

import Distribution.Client.Compat.Prelude
import Test.QuickCheck.Instances.Cabal ()
import UnitTests.Distribution.Client.ArbitraryInstances ()
import UnitTests.Distribution.Client.DescribedInstances ()
import Prelude ()

import Distribution.Described (testDescribed)
import Test.Tasty (TestTree, testGroup)

import Distribution.Client.BuildReports.Types (InstallOutcome, Outcome)
import Distribution.Client.IndexUtils.ActiveRepos (ActiveRepos)
import Distribution.Client.IndexUtils.IndexState (RepoIndexState, TotalIndexState)
import Distribution.Client.IndexUtils.Timestamp (Timestamp)
import Distribution.Client.Targets (UserConstraint)
import Distribution.Client.Types (RepoName)
import Distribution.Client.Types.AllowNewer (RelaxDepSubject, RelaxDeps, RelaxedDep)

tests :: TestTree
tests =
  testGroup
    "Described"
    [ testDescribed (Proxy :: Proxy Timestamp)
    , testDescribed (Proxy :: Proxy RepoIndexState)
    , testDescribed (Proxy :: Proxy TotalIndexState)
    , testDescribed (Proxy :: Proxy RepoName)
    , testDescribed (Proxy :: Proxy ActiveRepos)
    , testDescribed (Proxy :: Proxy RelaxDepSubject)
    , testDescribed (Proxy :: Proxy RelaxedDep)
    , testDescribed (Proxy :: Proxy RelaxDeps)
    , testDescribed (Proxy :: Proxy UserConstraint)
    , testDescribed (Proxy :: Proxy InstallOutcome)
    , testDescribed (Proxy :: Proxy Outcome)
    ]
