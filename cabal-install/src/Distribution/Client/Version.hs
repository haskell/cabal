{-# LANGUAGE CPP #-}
#ifdef GIT_REV
{-# LANGUAGE TemplateHaskell #-}
#endif

-- | Provides the version number of @cabal-install@.
module Distribution.Client.Version
  ( cabalInstallVersion
  , cabalInstallGitInfo
  ) where

import Distribution.Version

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
