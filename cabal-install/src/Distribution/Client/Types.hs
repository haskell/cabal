{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Client.Types
-- Copyright   :  (c) David Himmelstrup 2005
--                    Duncan Coutts 2011
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Various common data types for the entire cabal-install system
module Distribution.Client.Types
  ( module Distribution.Client.Types.AllowNewer
  , module Distribution.Client.Types.ConfiguredId
  , module Distribution.Client.Types.ConfiguredPackage
  , module Distribution.Client.Types.BuildResults
  , module Distribution.Client.Types.PackageLocation
  , module Distribution.Client.Types.PackageSpecifier
  , module Distribution.Client.Types.ReadyPackage
  , module Distribution.Client.Types.Repo
  , module Distribution.Client.Types.RepoName
  , module Distribution.Client.Types.SourcePackageDb
  , module Distribution.Client.Types.WriteGhcEnvironmentFilesPolicy
  ) where

import Distribution.Client.Types.AllowNewer
import Distribution.Client.Types.BuildResults
import Distribution.Client.Types.ConfiguredId
import Distribution.Client.Types.ConfiguredPackage
import Distribution.Client.Types.PackageLocation
import Distribution.Client.Types.PackageSpecifier
import Distribution.Client.Types.ReadyPackage
import Distribution.Client.Types.Repo
import Distribution.Client.Types.RepoName
import Distribution.Client.Types.SourcePackageDb
import Distribution.Client.Types.WriteGhcEnvironmentFilesPolicy
