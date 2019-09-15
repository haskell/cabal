{-# LANGUAGE NamedFieldPuns, RecordWildCards, RankNTypes #-}
module Distribution.Client.VCS (
    -- * VCS driver type
    VCS,
    vcsRepoType,
    vcsProgram,
    -- ** Type re-exports
    RepoType,
    Program,
    ConfiguredProgram,

    -- * Validating 'SourceRepo's and configuring VCS drivers
    validatePDSourceRepo,
    validateSourceRepo,
    validateSourceRepos,
    SourceRepoProblem(..),
    configureVCS,
    configureVCSs,

    -- * Running the VCS driver
    cloneSourceRepo,
    syncSourceRepos,

    -- * The individual VCS drivers
    knownVCSs,
    vcsBzr,
    vcsDarcs,
    vcsGit,
    vcsHg,
    vcsSvn,
  ) where

import Prelude ()
import Distribution.Client.Compat.Prelude

import Distribution.Types.SourceRepo
         ( RepoType(..) )
import Distribution.Client.SourceRepo (SourceRepoMaybe, SourceRepositoryPackage (..), srpToProxy)
import Distribution.Client.RebuildMonad
         ( Rebuild, monitorFiles, MonitorFilePath, monitorDirectoryExistence )
import Distribution.Verbosity as Verbosity
         ( Verbosity, normal )
import Distribution.Simple.Program
         ( Program(programFindVersion)
         , ConfiguredProgram(programVersion)
         , simpleProgram, findProgramVersion
         , ProgramInvocation(..), programInvocation, runProgramInvocation
         , emptyProgramDb, requireProgram )
import Distribution.Version
         ( mkVersion )
import qualified Distribution.PackageDescription as PD

import Control.Monad
         ( mapM_ )
import Control.Monad.Trans
         ( liftIO )
import qualified Data.Char as Char
import qualified Data.Map  as Map
import Data.Either
         ( partitionEithers )
import System.FilePath
         ( takeDirectory )
import System.Directory
         ( doesDirectoryExist )


-- | A driver for a version control system, e.g. git, darcs etc.
--
data VCS program = VCS {
       -- | The type of repository this driver is for.
       vcsRepoType  :: RepoType,

       -- | The vcs program itself.
       -- This is used at type 'Program' and 'ConfiguredProgram'.
       vcsProgram   :: program,

       -- | The program invocation(s) to get\/clone a repository into a fresh
       -- local directory.
       vcsCloneRepo :: forall f. Verbosity
                    -> ConfiguredProgram
                    -> SourceRepositoryPackage f
                    -> FilePath   -- Source URI
                    -> FilePath   -- Destination directory
                    -> [ProgramInvocation],

       -- | The program invocation(s) to synchronise a whole set of /related/
       -- repositories with corresponding local directories. Also returns the
       -- files that the command depends on, for change monitoring.
       vcsSyncRepos :: forall f. Verbosity
                    -> ConfiguredProgram
                    -> [(SourceRepositoryPackage f, FilePath)]
                    -> IO [MonitorFilePath]
     }


-- ------------------------------------------------------------
-- * Selecting repos and drivers
-- ------------------------------------------------------------

data SourceRepoProblem = SourceRepoRepoTypeUnspecified
                       | SourceRepoRepoTypeUnsupported (SourceRepositoryPackage Proxy) RepoType
                       | SourceRepoLocationUnspecified
  deriving Show

-- | Validates that the 'SourceRepo' specifies a location URI and a repository
-- type that is supported by a VCS driver.
--
-- | It also returns the 'VCS' driver we should use to work with it.
--
validateSourceRepo
    :: SourceRepositoryPackage f
    -> Either SourceRepoProblem (SourceRepositoryPackage f, String, RepoType, VCS Program)
validateSourceRepo = \repo -> do
    let rtype = srpType repo
    vcs   <- Map.lookup rtype knownVCSs  ?! SourceRepoRepoTypeUnsupported (srpToProxy repo) rtype
    let uri = srpLocation repo
    return (repo, uri, rtype, vcs)
  where
    a ?! e = maybe (Left e) Right a

validatePDSourceRepo
    :: PD.SourceRepo
    -> Either SourceRepoProblem (SourceRepoMaybe, String, RepoType, VCS Program)
validatePDSourceRepo repo = do
    rtype <- PD.repoType repo      ?! SourceRepoRepoTypeUnspecified
    uri   <- PD.repoLocation repo  ?! SourceRepoLocationUnspecified
    validateSourceRepo SourceRepositoryPackage
        { srpType     = rtype
        , srpLocation = uri
        , srpTag      = PD.repoTag repo
        , srpBranch   = PD.repoBranch repo
        , srpSubdir   = PD.repoSubdir repo
        }
  where
    a ?! e = maybe (Left e) Right a



-- | As 'validateSourceRepo' but for a bunch of 'SourceRepo's, and return
-- things in a convenient form to pass to 'configureVCSs', or to report
-- problems.
--
validateSourceRepos :: [SourceRepositoryPackage f]
                    -> Either [(SourceRepositoryPackage f, SourceRepoProblem)]
                              [(SourceRepositoryPackage f, String, RepoType, VCS Program)]
validateSourceRepos rs =
    case partitionEithers (map validateSourceRepo' rs) of
      (problems@(_:_), _) -> Left problems
      ([], vcss)          -> Right vcss
  where
    validateSourceRepo' r = either (Left . (,) r) Right
                                   (validateSourceRepo r)


configureVCS :: Verbosity
             -> VCS Program
             -> IO (VCS ConfiguredProgram)
configureVCS verbosity vcs@VCS{vcsProgram = prog} =
    asVcsConfigured <$> requireProgram verbosity prog emptyProgramDb
  where
    asVcsConfigured (prog', _) = vcs { vcsProgram = prog' }

configureVCSs :: Verbosity
              -> Map RepoType (VCS Program)
              -> IO (Map RepoType (VCS ConfiguredProgram))
configureVCSs verbosity = traverse (configureVCS verbosity)


-- ------------------------------------------------------------
-- * Running the driver
-- ------------------------------------------------------------

-- | Clone a single source repo into a fresh directory, using a configured VCS.
--
-- This is for making a new copy, not synchronising an existing copy. It will
-- fail if the destination directory already exists.
--
-- Make sure to validate the 'SourceRepo' using 'validateSourceRepo' first.
--

cloneSourceRepo
    :: Verbosity
    -> VCS ConfiguredProgram
    -> SourceRepositoryPackage f
    -> [Char]
    -> IO ()
cloneSourceRepo verbosity vcs
                repo@SourceRepositoryPackage{ srpLocation = srcuri } destdir =
    mapM_ (runProgramInvocation verbosity) invocations
  where
    invocations = vcsCloneRepo vcs verbosity
                               (vcsProgram vcs) repo
                               srcuri destdir


-- | Syncronise a set of 'SourceRepo's referring to the same repository with
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
--
syncSourceRepos :: Verbosity
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
--
knownVCSs :: Map RepoType (VCS Program)
knownVCSs = Map.fromList [ (vcsRepoType vcs, vcs) | vcs <- vcss ]
  where
    vcss = [ vcsBzr, vcsDarcs, vcsGit, vcsHg, vcsSvn ]


-- | VCS driver for Bazaar.
--
vcsBzr :: VCS Program
vcsBzr =
    VCS {
      vcsRepoType = Bazaar,
      vcsProgram  = bzrProgram,
      vcsCloneRepo,
      vcsSyncRepos
    }
  where
    vcsCloneRepo :: Verbosity
                 -> ConfiguredProgram
                 -> SourceRepositoryPackage f
                 -> FilePath
                 -> FilePath
                 -> [ProgramInvocation]
    vcsCloneRepo verbosity prog repo srcuri destdir =
        [ programInvocation prog
            ([branchCmd, srcuri, destdir] ++ tagArgs ++ verboseArg) ]
      where
        -- The @get@ command was deprecated in version 2.4 in favour of
        -- the alias @branch@
        branchCmd | programVersion prog >= Just (mkVersion [2,4])
                              = "branch"
                  | otherwise = "get"

        tagArgs = case srpTag repo of
          Nothing  -> []
          Just tag -> ["-r", "tag:" ++ tag]
        verboseArg = [ "--quiet" | verbosity < Verbosity.normal ]

    vcsSyncRepos :: Verbosity -> ConfiguredProgram
                 -> [(SourceRepositoryPackage f, FilePath)] -> IO [MonitorFilePath]
    vcsSyncRepos _v _p _rs = fail "sync repo not yet supported for bzr"

bzrProgram :: Program
bzrProgram = (simpleProgram "bzr") {
    programFindVersion = findProgramVersion "--version" $ \str ->
      case words str of
        -- "Bazaar (bzr) 2.6.0\n  ... lots of extra stuff"
        (_:_:ver:_) -> ver
        _ -> ""
  }


-- | VCS driver for Darcs.
--
vcsDarcs :: VCS Program
vcsDarcs =
    VCS {
      vcsRepoType = Darcs,
      vcsProgram  = darcsProgram,
      vcsCloneRepo,
      vcsSyncRepos
    }
  where
    vcsCloneRepo :: Verbosity
                 -> ConfiguredProgram
                 -> SourceRepositoryPackage f
                 -> FilePath
                 -> FilePath
                 -> [ProgramInvocation]
    vcsCloneRepo verbosity prog repo srcuri destdir =
        [ programInvocation prog cloneArgs ]
      where
        cloneArgs  = [cloneCmd, srcuri, destdir] ++ tagArgs ++ verboseArg
        -- At some point the @clone@ command was introduced as an alias for
        -- @get@, and @clone@ seems to be the recommended one now.
        cloneCmd   | programVersion prog >= Just (mkVersion [2,8])
                               = "clone"
                   | otherwise = "get"
        tagArgs    = case srpTag repo of
          Nothing  -> []
          Just tag -> ["-t", tag]
        verboseArg = [ "--quiet" | verbosity < Verbosity.normal ]

    vcsSyncRepos :: Verbosity -> ConfiguredProgram
                 -> [(SourceRepositoryPackage f, FilePath)] -> IO [MonitorFilePath]
    vcsSyncRepos _v _p _rs = fail "sync repo not yet supported for darcs"

darcsProgram :: Program
darcsProgram = (simpleProgram "darcs") {
    programFindVersion = findProgramVersion "--version" $ \str ->
      case words str of
        -- "2.8.5 (release)"
        (ver:_) -> ver
        _ -> ""
  }


-- | VCS driver for Git.
--
vcsGit :: VCS Program
vcsGit =
    VCS {
      vcsRepoType = Git,
      vcsProgram  = gitProgram,
      vcsCloneRepo,
      vcsSyncRepos
    }
  where
    vcsCloneRepo :: Verbosity
                 -> ConfiguredProgram
                 -> SourceRepositoryPackage f
                 -> FilePath
                 -> FilePath
                 -> [ProgramInvocation]
    vcsCloneRepo verbosity prog repo srcuri destdir =
        [ programInvocation prog cloneArgs ]
        -- And if there's a tag, we have to do that in a second step:
     ++ [ (programInvocation prog (checkoutArgs tag)) {
            progInvokeCwd = Just destdir
          }
        | tag <- maybeToList (srpTag repo) ]
      where
        cloneArgs  = ["clone", srcuri, destdir]
                     ++ branchArgs ++ verboseArg
        branchArgs = case srpBranch repo of
          Just b  -> ["--branch", b]
          Nothing -> []
        checkoutArgs tag = "checkout" : verboseArg ++ [tag, "--"]
        verboseArg = [ "--quiet" | verbosity < Verbosity.normal ]

    vcsSyncRepos :: Verbosity
                 -> ConfiguredProgram
                 -> [(SourceRepositoryPackage f, FilePath)]
                 -> IO [MonitorFilePath]
    vcsSyncRepos _ _ [] = return []
    vcsSyncRepos verbosity gitProg
                 ((primaryRepo, primaryLocalDir) : secondaryRepos) = do

      vcsSyncRepo verbosity gitProg primaryRepo primaryLocalDir Nothing
      sequence_
        [ vcsSyncRepo verbosity gitProg repo localDir (Just primaryLocalDir)
        | (repo, localDir) <- secondaryRepos ]
      return [ monitorDirectoryExistence dir
             | dir <- (primaryLocalDir : map snd secondaryRepos) ]

    vcsSyncRepo verbosity gitProg SourceRepositoryPackage{..} localDir peer = do
        exists <- doesDirectoryExist localDir
        if exists
          then git localDir                 ["fetch"]
          else git (takeDirectory localDir) cloneArgs
        git localDir checkoutArgs
      where
        git :: FilePath -> [String] -> IO ()
        git cwd args = runProgramInvocation verbosity $
                         (programInvocation gitProg args) {
                           progInvokeCwd = Just cwd
                         }

        cloneArgs      = ["clone", "--no-checkout", loc, localDir]
                      ++ case peer of
                           Nothing           -> []
                           Just peerLocalDir -> ["--reference", peerLocalDir]
                      ++ verboseArg
                         where loc = srpLocation
        checkoutArgs   = "checkout" : verboseArg ++ ["--detach", "--force"
                         , checkoutTarget, "--" ]
        checkoutTarget = fromMaybe "HEAD" (srpBranch `mplus` srpTag)
        verboseArg     = [ "--quiet" | verbosity < Verbosity.normal ]

gitProgram :: Program
gitProgram = (simpleProgram "git") {
    programFindVersion = findProgramVersion "--version" $ \str ->
      case words str of
        -- "git version 2.5.5"
        (_:_:ver:_) | all isTypical ver -> ver

        -- or annoyingly "git version 2.17.1.windows.2" yes, really
        (_:_:ver:_) -> intercalate "."
                     . takeWhile (all isNum)
                     . split
                     $ ver
        _ -> ""
  }
  where
    isNum     c = c >= '0' && c <= '9'
    isTypical c = isNum c || c == '.'
    split    cs = case break (=='.') cs of
                    (chunk,[])     -> chunk : []
                    (chunk,_:rest) -> chunk : split rest

-- | VCS driver for Mercurial.
--
vcsHg :: VCS Program
vcsHg =
    VCS {
      vcsRepoType = Mercurial,
      vcsProgram  = hgProgram,
      vcsCloneRepo,
      vcsSyncRepos
    }
  where
    vcsCloneRepo :: Verbosity
                 -> ConfiguredProgram
                 -> SourceRepositoryPackage f
                 -> FilePath
                 -> FilePath
                 -> [ProgramInvocation]
    vcsCloneRepo verbosity prog repo srcuri destdir =
        [ programInvocation prog cloneArgs ]
      where
        cloneArgs  = ["clone", srcuri, destdir]
                     ++ branchArgs ++ tagArgs ++ verboseArg
        branchArgs = case srpBranch repo of
          Just b  -> ["--branch", b]
          Nothing -> []
        tagArgs = case srpTag repo of
          Just t  -> ["--rev", t]
          Nothing -> []
        verboseArg = [ "--quiet" | verbosity < Verbosity.normal ]

    vcsSyncRepos :: Verbosity
                 -> ConfiguredProgram
                 -> [(SourceRepositoryPackage f, FilePath)]
                 -> IO [MonitorFilePath]
    vcsSyncRepos _v _p _rs = fail "sync repo not yet supported for hg"

hgProgram :: Program
hgProgram = (simpleProgram "hg") {
    programFindVersion = findProgramVersion "--version" $ \str ->
      case words str of
        -- Mercurial Distributed SCM (version 3.5.2)\n ... long message
        (_:_:_:_:ver:_) -> takeWhile (\c -> Char.isDigit c || c == '.') ver
        _ -> ""
  }


-- | VCS driver for Subversion.
--
vcsSvn :: VCS Program
vcsSvn =
    VCS {
      vcsRepoType = SVN,
      vcsProgram  = svnProgram,
      vcsCloneRepo,
      vcsSyncRepos
    }
  where
    vcsCloneRepo :: Verbosity
                 -> ConfiguredProgram
                 -> SourceRepositoryPackage f
                 -> FilePath
                 -> FilePath
                 -> [ProgramInvocation]
    vcsCloneRepo verbosity prog _repo srcuri destdir =
        [ programInvocation prog checkoutArgs ]
      where
        checkoutArgs = ["checkout", srcuri, destdir] ++ verboseArg
        verboseArg   = [ "--quiet" | verbosity < Verbosity.normal ]
        --TODO: branch or tag?

    vcsSyncRepos :: Verbosity
                 -> ConfiguredProgram
                 -> [(SourceRepositoryPackage f, FilePath)]
                 -> IO [MonitorFilePath]
    vcsSyncRepos _v _p _rs = fail "sync repo not yet supported for svn"

svnProgram :: Program
svnProgram = (simpleProgram "svn") {
    programFindVersion = findProgramVersion "--version" $ \str ->
      case words str of
        -- svn, version 1.9.4 (r1740329)\n ... long message
        (_:_:ver:_) -> ver
        _ -> ""
  }

