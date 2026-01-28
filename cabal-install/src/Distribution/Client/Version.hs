{-# LANGUAGE CPP #-}
#ifdef GIT_REV
{-# LANGUAGE TemplateHaskell #-}
#endif

-- | Provides the version number of @cabal-install@.
module Distribution.Client.Version
  ( cabalInstallVersion
  , cabalInstallGitInfo
  ) where

import Data.List (intercalate)
import qualified Data.Version as DV
import qualified Distribution.Compat.SysInfo as SIC
import Distribution.Version
import qualified System.Info as SI

import qualified Paths_cabal_install as PackageInfo

#ifdef GIT_REV
import Data.Either (isLeft)
import GitHash
  ( giHash
  , giBranch
  , giCommitDate
  , tGitInfoCwdTry
  )
#endif

-- |
-- This value determines the output of `cabal-install --version`.
cabalInstallVersion :: Version
cabalInstallVersion = mkVersion' PackageInfo.version

-- |
-- `cabal-install` compiler information.
cabalInstallCompilerInfo :: String
cabalInstallCompilerInfo =
  concat
    [ SI.compilerName
    , " "
    , intercalate "." (map show (DV.versionBranch SIC.fullCompilerVersion))
    , " on "
    , SI.os
    , " "
    , SI.arch
    ]

-- |
-- `cabal-install` Git information. Only filled in if built in a Git tree in
-- development mode and Template Haskell is available.
cabalInstallGitInfo :: String
#ifdef GIT_REV
cabalInstallGitInfo = if giHash' == ""
                        then ""
                        else concat [ "(commit "
                                    , giHash'
                                    , branchInfo
                                    , ", "
                                    , either (const "") giCommitDate gi'
                                    , ")"
                                    ]
  where
    gi' = $$tGitInfoCwdTry
    giHash' = take 7 . either (const "") giHash $ gi'
    branchInfo | isLeft gi' = ""
               | either id giBranch gi' == "master" = ""
               | otherwise = " on " <> either id giBranch gi'
#else
cabalInstallGitInfo = ""
#endif
