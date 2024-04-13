{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Client.VCS
  ( -- * VCS driver type
    VCS
  , vcsRepoType
  , vcsProgram

    -- ** Type re-exports
  , RepoType
  , Program
  , ConfiguredProgram

    -- * Validating 'SourceRepo's and configuring VCS drivers
  , validatePDSourceRepo
  , validateSourceRepo
  , validateSourceRepos
  , SourceRepoProblem (..)
  , configureVCS
  , configureVCSs

    -- * Running the VCS driver
  , cloneSourceRepo
  , syncSourceRepos

    -- * The individual VCS drivers
  , knownVCSs
  , vcsBzr
  , vcsDarcs
  , vcsGit
  , vcsHg
  , vcsSvn
  , vcsPijul
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.RebuildMonad
  ( MonitorFilePath
  , Rebuild
  , monitorDirectoryExistence
  , monitorFiles
  )
import Distribution.Client.Types.SourceRepo (SourceRepoMaybe, SourceRepositoryPackage (..), srpToProxy)
import qualified Distribution.PackageDescription as PD
import Distribution.Simple.Program
  ( ConfiguredProgram (programVersion)
  , Program (programFindVersion)
  , ProgramInvocation (..)
  , emptyProgramDb
  , findProgramVersion
  , getProgramInvocationOutput
  , programInvocation
  , requireProgram
  , runProgramInvocation
  , simpleProgram
  )
import Distribution.Simple.Program.Db
  ( prependProgramSearchPath
  )
import Distribution.Types.SourceRepo
  ( KnownRepoType (..)
  , RepoType (..)
  )
import Distribution.Verbosity as Verbosity
  ( normal
  )
import Distribution.Version
  ( mkVersion
  )

#if !MIN_VERSION_base(4,18,0)
import Control.Applicative
         ( liftA2 )
#endif

import Control.Exception
  ( throw
  , try
  )
import Control.Monad.Trans
  ( liftIO
  )
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Data.Map as Map
import System.Directory
  ( doesDirectoryExist
  , removeDirectoryRecursive
  )
import System.FilePath
  ( takeDirectory
  , (</>)
  )
import System.IO.Error
  ( isDoesNotExistError
  )

-- | A driver for a version control system, e.g. git, darcs etc.
data VCS program = VCS
  { vcsRepoType :: RepoType
  -- ^ The type of repository this driver is for.
  , vcsProgram :: program
  -- ^ The vcs program itself.
  -- This is used at type 'Program' and 'ConfiguredProgram'.
  , vcsCloneRepo
      :: forall f
       . Verbosity
      -> ConfiguredProgram
      -> SourceRepositoryPackage f
      -> FilePath -- Source URI
      -> FilePath -- Destination directory
      -> [ProgramInvocation]
  -- ^ The program invocation(s) to get\/clone a repository into a fresh
  -- local directory.
  , vcsSyncRepos
      :: forall f
       . Verbosity
      -> ConfiguredProgram
      -> [(SourceRepositoryPackage f, FilePath)]
      -> IO [MonitorFilePath]
  -- ^ The program invocation(s) to synchronise a whole set of /related/
  -- repositories with corresponding local directories. Also returns the
  -- files that the command depends on, for change monitoring.
  }

-- ------------------------------------------------------------

-- * Selecting repos and drivers

-- ------------------------------------------------------------

data SourceRepoProblem
  = SourceRepoRepoTypeUnspecified
  | SourceRepoRepoTypeUnsupported (SourceRepositoryPackage Proxy) RepoType
  | SourceRepoLocationUnspecified
  deriving (Show)

-- | Validates that the 'SourceRepo' specifies a location URI and a repository
-- type that is supported by a VCS driver.
--
-- | It also returns the 'VCS' driver we should use to work with it.
validateSourceRepo
  :: SourceRepositoryPackage f
  -> Either SourceRepoProblem (SourceRepositoryPackage f, String, RepoType, VCS Program)
validateSourceRepo = \repo -> do
  let rtype = srpType repo
  vcs <- Map.lookup rtype knownVCSs ?! SourceRepoRepoTypeUnsupported (srpToProxy repo) rtype
  let uri = srpLocation repo
  return (repo, uri, rtype, vcs)
  where
    a ?! e = maybe (Left e) Right a

validatePDSourceRepo
  :: PD.SourceRepo
  -> Either SourceRepoProblem (SourceRepoMaybe, String, RepoType, VCS Program)
validatePDSourceRepo repo = do
  rtype <- PD.repoType repo ?! SourceRepoRepoTypeUnspecified
  uri <- PD.repoLocation repo ?! SourceRepoLocationUnspecified
  validateSourceRepo
    SourceRepositoryPackage
      { srpType = rtype
      , srpLocation = uri
      , srpTag = PD.repoTag repo
      , srpBranch = PD.repoBranch repo
      , srpSubdir = PD.repoSubdir repo
      , srpCommand = mempty
      }
  where
    a ?! e = maybe (Left e) Right a

-- | As 'validateSourceRepo' but for a bunch of 'SourceRepo's, and return
-- things in a convenient form to pass to 'configureVCSs', or to report
-- problems.
validateSourceRepos
  :: [SourceRepositoryPackage f]
  -> Either
      [(SourceRepositoryPackage f, SourceRepoProblem)]
      [(SourceRepositoryPackage f, String, RepoType, VCS Program)]
validateSourceRepos rs =
  case partitionEithers (map validateSourceRepo' rs) of
    (problems@(_ : _), _) -> Left problems
    ([], vcss) -> Right vcss
  where
    validateSourceRepo'
      :: SourceRepositoryPackage f
      -> Either
          (SourceRepositoryPackage f, SourceRepoProblem)
          (SourceRepositoryPackage f, String, RepoType, VCS Program)
    validateSourceRepo' r =
      either
        (Left . (,) r)
        Right
        (validateSourceRepo r)

configureVCS
  :: Verbosity
  -> [FilePath]
  -- ^ Extra prog paths
  -> VCS Program
  -> IO (VCS ConfiguredProgram)
configureVCS verbosity progPaths vcs@VCS{vcsProgram = prog} = do
  progPath <- prependProgramSearchPath verbosity progPaths emptyProgramDb
  asVcsConfigured <$> requireProgram verbosity prog progPath
  where
    asVcsConfigured (prog', _) = vcs{vcsProgram = prog'}

configureVCSs
  :: Verbosity
  -> [FilePath]
  -- ^ Extra prog paths
  -> Map RepoType (VCS Program)
  -> IO (Map RepoType (VCS ConfiguredProgram))
configureVCSs verbosity progPaths = traverse (configureVCS verbosity progPaths)

-- ------------------------------------------------------------

-- * Running the driver

-- ------------------------------------------------------------

-- | Clone a single source repo into a fresh directory, using a configured VCS.
--
-- This is for making a new copy, not synchronising an existing copy. It will
-- fail if the destination directory already exists.
--
-- Make sure to validate the 'SourceRepo' using 'validateSourceRepo' first.
cloneSourceRepo
  :: Verbosity
  -> VCS ConfiguredProgram
  -> SourceRepositoryPackage f
  -> [Char]
  -> IO ()
cloneSourceRepo
  verbosity
  vcs
  repo@SourceRepositoryPackage{srpLocation = srcuri}
  destdir =
    traverse_ (runProgramInvocation verbosity) invocations
    where
      invocations =
        vcsCloneRepo
          vcs
          verbosity
          (vcsProgram vcs)
          repo
          srcuri
          destdir

-- | Synchronise a set of 'SourceRepo's referring to the same repository with
-- corresponding local directories. The local directories may or may not
-- already exist.
--
-- The 'SourceRepo' values used in a single invocation of 'syncSourceRepos',
-- or used across a series of invocations with any local directory must refer
-- to the /same/ repository. That means it must be the same location but they
-- can differ in the branch, or tag or subdir.
--
-- The reason to allow multiple related 'SourceRepo's is to allow for the
-- network or storage to be shared between different checkouts of the repo.
-- For example if a single repo contains multiple packages in different subdirs
-- and in some project it may make sense to use a different state of the repo
-- for one subdir compared to another.
syncSourceRepos
  :: Verbosity
  -> VCS ConfiguredProgram
  -> [(SourceRepositoryPackage f, FilePath)]
  -> Rebuild ()
syncSourceRepos verbosity vcs repos = do
  files <- liftIO $ vcsSyncRepos vcs verbosity (vcsProgram vcs) repos
  monitorFiles files

-- ------------------------------------------------------------

-- * The various VCS drivers

-- ------------------------------------------------------------

-- | The set of all supported VCS drivers, organised by 'RepoType'.
knownVCSs :: Map RepoType (VCS Program)
knownVCSs = Map.fromList [(vcsRepoType vcs, vcs) | vcs <- vcss]
  where
    vcss = [vcsBzr, vcsDarcs, vcsGit, vcsHg, vcsSvn]

-- | VCS driver for Bazaar.
vcsBzr :: VCS Program
vcsBzr =
  VCS
    { vcsRepoType = KnownRepoType Bazaar
    , vcsProgram = bzrProgram
    , vcsCloneRepo
    , vcsSyncRepos
    }
  where
    vcsCloneRepo
      :: Verbosity
      -> ConfiguredProgram
      -> SourceRepositoryPackage f
      -> FilePath
      -> FilePath
      -> [ProgramInvocation]
    vcsCloneRepo verbosity prog repo srcuri destdir =
      [ programInvocation
          prog
          ([branchCmd, srcuri, destdir] ++ tagArgs ++ verboseArg)
      ]
      where
        -- The @get@ command was deprecated in version 2.4 in favour of
        -- the alias @branch@
        branchCmd
          | programVersion prog >= Just (mkVersion [2, 4]) =
              "branch"
          | otherwise = "get"

        tagArgs :: [String]
        tagArgs = case srpTag repo of
          Nothing -> []
          Just tag -> ["-r", "tag:" ++ tag]
        verboseArg :: [String]
        verboseArg = ["--quiet" | verbosity < Verbosity.normal]

    vcsSyncRepos
      :: Verbosity
      -> ConfiguredProgram
      -> [(SourceRepositoryPackage f, FilePath)]
      -> IO [MonitorFilePath]
    vcsSyncRepos _v _p _rs = fail "sync repo not yet supported for bzr"

bzrProgram :: Program
bzrProgram =
  (simpleProgram "bzr")
    { programFindVersion = findProgramVersion "--version" $ \str ->
        case words str of
          -- "Bazaar (bzr) 2.6.0\n  ... lots of extra stuff"
          (_ : _ : ver : _) -> ver
          _ -> ""
    }

-- | VCS driver for Darcs.
vcsDarcs :: VCS Program
vcsDarcs =
  VCS
    { vcsRepoType = KnownRepoType Darcs
    , vcsProgram = darcsProgram
    , vcsCloneRepo
    , vcsSyncRepos
    }
  where
    vcsCloneRepo
      :: Verbosity
      -> ConfiguredProgram
      -> SourceRepositoryPackage f
      -> FilePath
      -> FilePath
      -> [ProgramInvocation]
    vcsCloneRepo verbosity prog repo srcuri destdir =
      [programInvocation prog cloneArgs]
      where
        cloneArgs :: [String]
        cloneArgs = [cloneCmd, srcuri, destdir] ++ tagArgs ++ verboseArg
        -- At some point the @clone@ command was introduced as an alias for
        -- @get@, and @clone@ seems to be the recommended one now.
        cloneCmd :: String
        cloneCmd
          | programVersion prog >= Just (mkVersion [2, 8]) =
              "clone"
          | otherwise = "get"
        tagArgs :: [String]
        tagArgs = case srpTag repo of
          Nothing -> []
          Just tag -> ["-t", tag]
        verboseArg :: [String]
        verboseArg = ["--quiet" | verbosity < Verbosity.normal]

    vcsSyncRepos
      :: Verbosity
      -> ConfiguredProgram
      -> [(SourceRepositoryPackage f, FilePath)]
      -> IO [MonitorFilePath]
    vcsSyncRepos _ _ [] = return []
    vcsSyncRepos verbosity prog ((primaryRepo, primaryLocalDir) : secondaryRepos) =
      monitors <$ do
        vcsSyncRepo verbosity prog primaryRepo primaryLocalDir Nothing
        for_ secondaryRepos $ \(repo, localDir) ->
          vcsSyncRepo verbosity prog repo localDir $ Just primaryLocalDir
      where
        dirs :: [FilePath]
        dirs = primaryLocalDir : (snd <$> secondaryRepos)
        monitors :: [MonitorFilePath]
        monitors = monitorDirectoryExistence <$> dirs

    vcsSyncRepo verbosity prog SourceRepositoryPackage{..} localDir _peer =
      try (lines <$> darcsWithOutput localDir ["log", "--last", "1"]) >>= \case
        Right (_ : _ : _ : x : _)
          | Just tag <- (List.stripPrefix "tagged " . List.dropWhile Char.isSpace) x
          , Just tag' <- srpTag
          , tag == tag' ->
              pure ()
        Left e | not (isDoesNotExistError e) -> throw e
        _ -> do
          removeDirectoryRecursive localDir `catch` liftA2 unless isDoesNotExistError throw
          darcs (takeDirectory localDir) cloneArgs
      where
        darcs :: FilePath -> [String] -> IO ()
        darcs = darcs' runProgramInvocation

        darcsWithOutput :: FilePath -> [String] -> IO String
        darcsWithOutput = darcs' getProgramInvocationOutput

        darcs' :: (Verbosity -> ProgramInvocation -> t) -> FilePath -> [String] -> t
        darcs' f cwd args =
          f
            verbosity
            (programInvocation prog args)
              { progInvokeCwd = Just cwd
              }

        cloneArgs :: [String]
        cloneArgs = ["clone"] ++ tagArgs ++ [srpLocation, localDir] ++ verboseArg
        tagArgs :: [String]
        tagArgs = case srpTag of
          Nothing -> []
          Just tag -> ["-t" ++ tag]
        verboseArg :: [String]
        verboseArg = ["--quiet" | verbosity < Verbosity.normal]

darcsProgram :: Program
darcsProgram =
  (simpleProgram "darcs")
    { programFindVersion = findProgramVersion "--version" $ \str ->
        case words str of
          -- "2.8.5 (release)"
          (ver : _) -> ver
          _ -> ""
    }

-- | VCS driver for Git.
vcsGit :: VCS Program
vcsGit =
  VCS
    { vcsRepoType = KnownRepoType Git
    , vcsProgram = gitProgram
    , vcsCloneRepo
    , vcsSyncRepos
    }
  where
    vcsCloneRepo
      :: Verbosity
      -> ConfiguredProgram
      -> SourceRepositoryPackage f
      -> FilePath
      -> FilePath
      -> [ProgramInvocation]
    vcsCloneRepo verbosity prog repo srcuri destdir =
      [programInvocation prog cloneArgs]
        -- And if there's a tag, we have to do that in a second step:
        ++ [git (resetArgs tag) | tag <- maybeToList (srpTag repo)]
        ++ [ git (["submodule", "sync", "--recursive"] ++ verboseArg)
           , git (["submodule", "update", "--init", "--force", "--recursive"] ++ verboseArg)
           ]
      where
        git args = (programInvocation prog args){progInvokeCwd = Just destdir}
        cloneArgs =
          ["clone", srcuri, destdir]
            ++ branchArgs
            ++ verboseArg
        branchArgs = case srpBranch repo of
          Just b -> ["--branch", b]
          Nothing -> []
        resetArgs tag = "reset" : verboseArg ++ ["--hard", tag, "--"]
        verboseArg = ["--quiet" | verbosity < Verbosity.normal]

    vcsSyncRepos
      :: Verbosity
      -> ConfiguredProgram
      -> [(SourceRepositoryPackage f, FilePath)]
      -> IO [MonitorFilePath]
    vcsSyncRepos _ _ [] = return []
    vcsSyncRepos
      verbosity
      gitProg
      ((primaryRepo, primaryLocalDir) : secondaryRepos) = do
        vcsSyncRepo verbosity gitProg primaryRepo primaryLocalDir Nothing
        sequence_
          [ vcsSyncRepo verbosity gitProg repo localDir (Just primaryLocalDir)
          | (repo, localDir) <- secondaryRepos
          ]
        return
          [ monitorDirectoryExistence dir
          | dir <- (primaryLocalDir : map snd secondaryRepos)
          ]

    vcsSyncRepo verbosity gitProg SourceRepositoryPackage{..} localDir peer = do
      exists <- doesDirectoryExist localDir
      if exists
        then git localDir ["fetch"]
        else git (takeDirectory localDir) cloneArgs
      -- Before trying to checkout other commits, all submodules must be
      -- de-initialised and the .git/modules directory must be deleted. This
      -- is needed because sometimes `git submodule sync` does not actually
      -- update the submodule source URL. Detailed description here:
      -- https://git.coop/-/snippets/85
      git localDir ["submodule", "deinit", "--force", "--all"]
      let gitModulesDir = localDir </> ".git" </> "modules"
      gitModulesExists <- doesDirectoryExist gitModulesDir
      when gitModulesExists $ removeDirectoryRecursive gitModulesDir
      git localDir resetArgs
      git localDir $ ["submodule", "sync", "--recursive"] ++ verboseArg
      git localDir $ ["submodule", "update", "--force", "--init", "--recursive"] ++ verboseArg
      git localDir $ ["submodule", "foreach", "--recursive"] ++ verboseArg ++ ["git clean -ffxdq"]
      git localDir $ ["clean", "-ffxdq"]
      where
        git :: FilePath -> [String] -> IO ()
        git cwd args =
          runProgramInvocation verbosity $
            (programInvocation gitProg args)
              { progInvokeCwd = Just cwd
              }

        cloneArgs =
          ["clone", "--no-checkout", loc, localDir]
            ++ case peer of
              Nothing -> []
              Just peerLocalDir -> ["--reference", peerLocalDir]
            ++ verboseArg
          where
            loc = srpLocation
        resetArgs = "reset" : verboseArg ++ ["--hard", resetTarget, "--"]
        resetTarget = fromMaybe "HEAD" (srpBranch `mplus` srpTag)
        verboseArg = ["--quiet" | verbosity < Verbosity.normal]

gitProgram :: Program
gitProgram =
  (simpleProgram "git")
    { programFindVersion = findProgramVersion "--version" $ \str ->
        case words str of
          -- "git version 2.5.5"
          (_ : _ : ver : _) | all isTypical ver -> ver
          -- or annoyingly "git version 2.17.1.windows.2" yes, really
          (_ : _ : ver : _) ->
            intercalate "."
              . takeWhile (all isNum)
              . split
              $ ver
          _ -> ""
    }
  where
    isNum c = c >= '0' && c <= '9'
    isTypical c = isNum c || c == '.'
    split cs = case break (== '.') cs of
      (chunk, []) -> chunk : []
      (chunk, _ : rest) -> chunk : split rest

-- | VCS driver for Mercurial.
vcsHg :: VCS Program
vcsHg =
  VCS
    { vcsRepoType = KnownRepoType Mercurial
    , vcsProgram = hgProgram
    , vcsCloneRepo
    , vcsSyncRepos
    }
  where
    vcsCloneRepo
      :: Verbosity
      -> ConfiguredProgram
      -> SourceRepositoryPackage f
      -> FilePath
      -> FilePath
      -> [ProgramInvocation]
    vcsCloneRepo verbosity prog repo srcuri destdir =
      [programInvocation prog cloneArgs]
      where
        cloneArgs =
          ["clone", srcuri, destdir]
            ++ branchArgs
            ++ tagArgs
            ++ verboseArg
        branchArgs = case srpBranch repo of
          Just b -> ["--branch", b]
          Nothing -> []
        tagArgs = case srpTag repo of
          Just t -> ["--rev", t]
          Nothing -> []
        verboseArg = ["--quiet" | verbosity < Verbosity.normal]

    vcsSyncRepos
      :: Verbosity
      -> ConfiguredProgram
      -> [(SourceRepositoryPackage f, FilePath)]
      -> IO [MonitorFilePath]
    vcsSyncRepos _ _ [] = return []
    vcsSyncRepos
      verbosity
      hgProg
      ((primaryRepo, primaryLocalDir) : secondaryRepos) = do
        vcsSyncRepo verbosity hgProg primaryRepo primaryLocalDir
        sequence_
          [ vcsSyncRepo verbosity hgProg repo localDir
          | (repo, localDir) <- secondaryRepos
          ]
        return
          [ monitorDirectoryExistence dir
          | dir <- (primaryLocalDir : map snd secondaryRepos)
          ]
    vcsSyncRepo verbosity hgProg repo localDir = do
      exists <- doesDirectoryExist localDir
      if exists
        then hg localDir ["pull"]
        else hg (takeDirectory localDir) cloneArgs
      hg localDir checkoutArgs
      where
        hg :: FilePath -> [String] -> IO ()
        hg cwd args =
          runProgramInvocation verbosity $
            (programInvocation hgProg args)
              { progInvokeCwd = Just cwd
              }
        cloneArgs =
          ["clone", "--noupdate", (srpLocation repo), localDir]
            ++ verboseArg
        verboseArg = ["--quiet" | verbosity < Verbosity.normal]
        checkoutArgs =
          ["checkout", "--clean"]
            ++ tagArgs
        tagArgs = case srpTag repo of
          Just t -> ["--rev", t]
          Nothing -> []

hgProgram :: Program
hgProgram =
  (simpleProgram "hg")
    { programFindVersion = findProgramVersion "--version" $ \str ->
        case words str of
          -- Mercurial Distributed SCM (version 3.5.2)\n ... long message
          (_ : _ : _ : _ : ver : _) -> takeWhile (\c -> Char.isDigit c || c == '.') ver
          _ -> ""
    }

-- | VCS driver for Subversion.
vcsSvn :: VCS Program
vcsSvn =
  VCS
    { vcsRepoType = KnownRepoType SVN
    , vcsProgram = svnProgram
    , vcsCloneRepo
    , vcsSyncRepos
    }
  where
    vcsCloneRepo
      :: Verbosity
      -> ConfiguredProgram
      -> SourceRepositoryPackage f
      -> FilePath
      -> FilePath
      -> [ProgramInvocation]
    vcsCloneRepo verbosity prog _repo srcuri destdir =
      [programInvocation prog checkoutArgs]
      where
        checkoutArgs = ["checkout", srcuri, destdir] ++ verboseArg
        verboseArg = ["--quiet" | verbosity < Verbosity.normal]
    -- TODO: branch or tag?

    vcsSyncRepos
      :: Verbosity
      -> ConfiguredProgram
      -> [(SourceRepositoryPackage f, FilePath)]
      -> IO [MonitorFilePath]
    vcsSyncRepos _v _p _rs = fail "sync repo not yet supported for svn"

svnProgram :: Program
svnProgram =
  (simpleProgram "svn")
    { programFindVersion = findProgramVersion "--version" $ \str ->
        case words str of
          -- svn, version 1.9.4 (r1740329)\n ... long message
          (_ : _ : ver : _) -> ver
          _ -> ""
    }

-- | VCS driver for Pijul.
-- Documentation for Pijul can be found at <https://pijul.org/manual/introduction.html>
--
-- 2020-04-09 Oleg:
--
--    As far as I understand pijul, there are branches and "tags" in pijul,
--    but there aren't a "commit hash" identifying an arbitrary state.
--
--    One can create `a pijul tag`, which will make a patch hash,
--    which depends on everything currently in the repository.
--    I guess if you try to apply that patch, you'll be forced to apply
--    all the dependencies too. In other words, there are no named tags.
--
--    It's not clear to me whether there is an option to
--    "apply this patch *and* all of its dependencies".
--    And relatedly, whether how to make sure that there are no other
--    patches applied.
--
--    With branches it's easier, as you can `pull` and `checkout` them,
--    and they seem to be similar enough. Yet, pijul documentations says
--
--    > Note that the purpose of branches in Pijul is quite different from Git,
--      since Git's "feature branches" can usually be implemented by just
--      patches.
--
--    I guess it means that indeed instead of creating a branch and making PR
--    in "GitHub" workflow, you'd just create a patch and offer it.
--    You can do that with `git` too. Push (a branch with) commit to remote
--    and ask other to cherry-pick that commit. Yet, in git identity of commit
--    changes when it applied to other trees, where patches in pijul have
--    will continue to have the same hash.
--
--    Unfortunately pijul doesn't talk about conflict resolution.
--    It seems that you get something like:
--
--        % pijul status
--        On branch merge
--
--        Unresolved conflicts:
--          (fix conflicts and record the resolution with "pijul record ...")
--
--                foo
--
--        % cat foo
--        first line
--        >> >>>>>>>>>>>>>>>>>>>>>>>>>>>>>
--        branch BBB
--        ================================
--        branch AAA
--        <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
--        last line
--
--    And then the `pijul dependencies` would draw you a graph like
--
--
--                    ----->  foo on branch B ----->
--    resolve conflict                                  Initial patch
--                    ----->  foo on branch A ----->
--
--    Which is seems reasonable.
--
--    So currently, pijul support is very experimental, and most likely
--    won't work, even the basics are in place. Tests are also written
--    but disabled, as the branching model differs from `git` one,
--    for which tests are written.
vcsPijul :: VCS Program
vcsPijul =
  VCS
    { vcsRepoType = KnownRepoType Pijul
    , vcsProgram = pijulProgram
    , vcsCloneRepo
    , vcsSyncRepos
    }
  where
    vcsCloneRepo
      :: Verbosity
      -- \^ it seems that pijul does not have verbose flag
      -> ConfiguredProgram
      -> SourceRepositoryPackage f
      -> FilePath
      -> FilePath
      -> [ProgramInvocation]
    vcsCloneRepo _verbosity prog repo srcuri destdir =
      [programInvocation prog cloneArgs]
        -- And if there's a tag, we have to do that in a second step:
        ++ [ (programInvocation prog (checkoutArgs tag))
            { progInvokeCwd = Just destdir
            }
           | tag <- maybeToList (srpTag repo)
           ]
      where
        cloneArgs :: [String]
        cloneArgs =
          ["clone", srcuri, destdir]
            ++ branchArgs
        branchArgs :: [String]
        branchArgs = case srpBranch repo of
          Just b -> ["--from-branch", b]
          Nothing -> []
        checkoutArgs tag = "checkout" : [tag] -- TODO: this probably doesn't work either
    vcsSyncRepos
      :: Verbosity
      -> ConfiguredProgram
      -> [(SourceRepositoryPackage f, FilePath)]
      -> IO [MonitorFilePath]
    vcsSyncRepos _ _ [] = return []
    vcsSyncRepos
      verbosity
      pijulProg
      ((primaryRepo, primaryLocalDir) : secondaryRepos) = do
        vcsSyncRepo verbosity pijulProg primaryRepo primaryLocalDir Nothing
        sequence_
          [ vcsSyncRepo verbosity pijulProg repo localDir (Just primaryLocalDir)
          | (repo, localDir) <- secondaryRepos
          ]
        return
          [ monitorDirectoryExistence dir
          | dir <- (primaryLocalDir : map snd secondaryRepos)
          ]

    vcsSyncRepo verbosity pijulProg SourceRepositoryPackage{..} localDir peer = do
      exists <- doesDirectoryExist localDir
      if exists
        then pijul localDir ["pull"] -- TODO: this probably doesn't work.
        else pijul (takeDirectory localDir) cloneArgs
      pijul localDir checkoutArgs
      where
        pijul :: FilePath -> [String] -> IO ()
        pijul cwd args =
          runProgramInvocation verbosity $
            (programInvocation pijulProg args)
              { progInvokeCwd = Just cwd
              }

        cloneArgs :: [String]
        cloneArgs =
          ["clone", loc, localDir]
            ++ case peer of
              Nothing -> []
              Just peerLocalDir -> [peerLocalDir]
          where
            loc = srpLocation
        checkoutArgs :: [String]
        checkoutArgs = "checkout" : ["--force", checkoutTarget, "--"]
        checkoutTarget = fromMaybe "HEAD" (srpBranch `mplus` srpTag) -- TODO: this is definitely wrong.

pijulProgram :: Program
pijulProgram =
  (simpleProgram "pijul")
    { programFindVersion = findProgramVersion "--version" $ \str ->
        case words str of
          -- "pijul 0.12.2
          (_ : ver : _) | all isTypical ver -> ver
          _ -> ""
    }
  where
    isNum c = c >= '0' && c <= '9'
    isTypical c = isNum c || c == '.'
