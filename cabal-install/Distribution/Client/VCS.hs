{-# LANGUAGE NamedFieldPuns, RecordWildCards #-}
module Distribution.Client.VCS (
    VCS(vcsSyncRepos),
    vcsRepoType,
    vcsProgram,
    SourceRepo,
    RepoType,
    RepoKind,
    Program,
    ConfiguredProgram,
--    findVcsCloneInvocation,
    selectPackageSourceRepo,
    selectSourceRepoVCS,
    SourceRepoProblem(..),
    configureVCS,
    configureVCSs,
--    findUsableVCSs,
    cloneSourceRepo,
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
         ( SourceRepo(..), RepoType(..), RepoKind(..) )
import Distribution.Client.FileMonitor
         ( MonitorFilePath, monitorDirectoryExistence )
import Distribution.Client.RebuildMonad
         ( Rebuild, monitorFiles )
import Distribution.Verbosity as Verbosity
         ( Verbosity, normal )
import Distribution.Simple.Program
         ( Program(programFindVersion, programName)
         , ConfiguredProgram(programVersion)
         , simpleProgram, findProgramVersion
         , ProgramInvocation(..), programInvocation, runProgramInvocation
         , ProgramDb, emptyProgramDb, knownPrograms, configureProgram
         , lookupProgram )
import Distribution.Version
         ( mkVersion )

import Control.Monad
         ( mapM_ )
import qualified Data.Char as Char
import qualified Data.Map  as Map
import qualified Data.Set  as Set
import Data.Set (Set)
import Data.Ord
         ( comparing )
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
       vcsCloneRepo :: Verbosity
                    -> ConfiguredProgram
                    -> SourceRepo
                    -> FilePath -- ^ Source URL
                    -> FilePath -- ^ Destination directory
                    -> [ProgramInvocation],

       -- | The program invocation(s) to synchronise a whole set of /related/
       -- repositories with corresponding local directories. Also returns the
       -- files that the command depends on, for change monitoring.
       vcsSyncRepos :: Verbosity
                    -> ConfiguredProgram
                    -> [(SourceRepo, FilePath)]
                    -> IO [MonitorFilePath]
     }


-- | The set of all supported VCS drivers.
--
knownVCSs :: [VCS Program]
knownVCSs = [ vcsBzr, vcsDarcs, vcsGit, vcsHg, vcsSvn ]


-- ------------------------------------------------------------
-- * Selecting repos and drivers
-- ------------------------------------------------------------

-- | Pick the 'SourceRepo' to use to get the package sources from.
--
-- Note that this does /not/ depend on what 'VCS' drivers we are able to
-- successfully configure. It is based only on the 'SourceRepo's declared
-- in the package, and optionally on a preferred 'RepoKind'.
--
selectPackageSourceRepo :: Maybe RepoKind
                        -> [SourceRepo]
                        -> Maybe SourceRepo
selectPackageSourceRepo preferredRepoKind =
    listToMaybe
    -- Sort repositories by kind, from This to Head to Unknown. Repositories
    -- with equivalent kinds are selected based on the order they appear in
    -- the Cabal description file.
  . sortBy (comparing thisFirst)
    -- If the user has specified the repo kind, filter out the repositories
    -- they're not interested in.
  . filter (\repo -> maybe True (repoKind repo ==) preferredRepoKind)
  where
    thisFirst :: SourceRepo -> Int
    thisFirst r = case repoKind r of
        RepoThis -> 0
        RepoHead -> case repoTag r of
            -- If the type is 'head' but the author specified a tag, they
            -- probably meant to create a 'this' repository but screwed up.
            Just _  -> 0
            Nothing -> 1
        RepoKindUnknown _ -> 2

data SourceRepoProblem = SourceRepoRepoTypeUnspecified
                       | SourceRepoRepoTypeUnsupported RepoType
                       | SourceRepoLocationUnspecified

-- | Given a single 'SourceRepo', pick which VCS we should use to fetch it.
--
-- It also validates that the 'SourceRepo' specifies a repo location URL, and
-- returns that URL string.
--
selectSourceRepoVCS :: SourceRepo
                    -> Either SourceRepoProblem
                              (VCS Program, String)
selectSourceRepoVCS = \repo -> do
    rtype <- repoType repo               ?! SourceRepoRepoTypeUnspecified
    vcs   <- Map.lookup rtype knownVCSs' ?! SourceRepoRepoTypeUnsupported rtype
    url   <- repoLocation repo           ?! SourceRepoLocationUnspecified
    return (vcs, url)
  where
    a ?! e = maybe (Left e) Right a

    -- The 'knownVCSs' organised by 'RepoType'.
    knownVCSs' = Map.fromList [ (vcsRepoType vcs, vcs) | vcs <- knownVCSs ]


{-
-- | Find which usable VCS drivers (selected from 'knownVCSs') are
-- available and usable on the local machine for the given 'RepoType's.
--
findUsableVCSs :: Verbosity
               -> Set RepoType -- ^ Which repo types we are interested in.
               -> IO (Map RepoType (VCS ConfiguredProgram))
findUsableVCSs verbosity repoTypes = do
    progdb <- configurePrograms
                [ vcsProgram vcs
                | vcs <- knownVCSs
                , vcsRepoType vcs `Set.member` repoTypes ]

    let vcssByProg  = Map.fromList
                        [ (programName (vcsProgram vcs), vcs)
                        | vcs <- knownVCSs ]
        usableProgs = Map.fromList
                        [ (programName prog, cprog)
                        | (prog, Just cprog) <- knownPrograms progdb ]
        usableVCSs  = reindexByRepoType $
                        Map.intersectionWith
                          (\prog vcs -> vcs { vcsProgram = prog})
                          usableProgs
                          vcssByProg

    return usableVCSs
  where
    reindexByRepoType :: Map a (VCS p) -> Map RepoType (VCS p)
    reindexByRepoType = Map.fromList
                      . map (\vcs -> (vcsRepoType vcs, vcs))
                      . Map.elems

    --TODO: export this from Distribution.Simple.Program.Db
    configurePrograms :: [Program] -> IO ProgramDb
    configurePrograms = foldM (flip (configureProgram verbosity)) emptyProgramDb
-}

configureVCS :: Verbosity
             -> VCS Program
             -> IO (Maybe (VCS ConfiguredProgram))
configureVCS verbosity vcs@VCS{vcsProgram = prog} =
    selectConfigured <$> configureProgram verbosity prog emptyProgramDb
  where
    selectConfigured :: ProgramDb -> Maybe (VCS ConfiguredProgram)
    selectConfigured = fmap (\prog' -> vcs { vcsProgram = prog' })
                     . lookupProgram prog

--TODO: use requireProgram, we don't need optional configuration

configureVCSs :: Verbosity
              -> Map RepoType (VCS Program)
              -> IO (Map RepoType (VCS ConfiguredProgram))
configureVCSs verbosity vcss =
    keepConfigured <$> traverse (configureVCS verbosity) vcss
  where
    keepConfigured :: Map a (Maybe b) -> Map a b
    keepConfigured = Map.mapMaybe id


-- ------------------------------------------------------------
-- * Running the driver
-- ------------------------------------------------------------

-- | Clone a single source repo into a fresh directory, using a configured VCS.
--
-- This is for making a new copy, not synchronising an existing copy. It will
-- fail if the destination directory already exists.
--
cloneSourceRepo :: Verbosity
                -> VCS ConfiguredProgram
                -> SourceRepo
                -> String   -- ^ Source URL
                -> FilePath -- ^ Destination directory
                -> IO ()
cloneSourceRepo verbosity vcs repo srcurl destdir =
    mapM_ (runProgramInvocation verbosity) invocations
  where
    invocations = vcsCloneRepo vcs verbosity
                               (vcsProgram vcs) repo
                               srcurl destdir


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
                -> [(SourceRepo, FilePath)]
                -> Rebuild ()
syncSourceRepos _verbosity _vcs _repos = undefined


{-
-- | Given a set of possible VCSs, and a set of possible source
-- repositories, find a repository that is both 1) likely to be specific to
-- this source version and 2) is supported by the local machine.
findVcsCloneInvocation :: Map RepoType (VCS ConfiguredProgram)
                       -> [SourceRepo]
                       -> Maybe RepoKind
                       -> FilePath
                       -> Maybe [ProgramInvocation]
findVcsCloneInvocation vcss repos maybeKind destdir =
    listToMaybe
      [ invocations
        -- Sort repositories by kind, from This to Head to Unknown. Repositories
        -- with equivalent kinds are selected based on the order they appear in
        -- the Cabal description file.
      | repo <- sortBy (comparing thisFirst) repos
        -- If the user has specified the repo kind, filter out the repositories
        -- they're not interested in.
      , maybe True (repoKind repo ==) maybeKind
      , Just invocations <- [repoCloneCmds repo]
      ]
  where
    thisFirst :: SourceRepo -> Int
    thisFirst r = case repoKind r of
        RepoThis -> 0
        RepoHead -> case repoTag r of
            -- If the type is 'head' but the author specified a tag, they
            -- probably meant to create a 'this' repository but screwed up.
            Just _  -> 0
            Nothing -> 1
        RepoKindUnknown _ -> 2

    repoCloneCmds :: SourceRepo -> Maybe [ProgramInvocation]
    repoCloneCmds repo = do
        rtype  <- repoType repo
        srcurl <- repoLocation repo
        vcs    <- Map.lookup rtype vcss
        return (vcsCloneRepo vcs (vcsProgram vcs) repo srcurl destdir)
-}

-- ------------------------------------------------------------
-- * The various VCS drivers
-- ------------------------------------------------------------

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
                 -> SourceRepo
                 -> FilePath
                 -> FilePath
                 -> [ProgramInvocation]
    vcsCloneRepo verbosity prog repo srcurl destdir =
        [ programInvocation prog
            ([branchCmd, srcurl, destdir] ++ tagArgs ++ verboseArg) ]
      where
        -- The @get@ command was deprecated in version 2.4 in favour of
        -- the alias @branch@
        branchCmd | programVersion prog >= Just (mkVersion [2,4])
                              = "branch"
                  | otherwise = "get"

        tagArgs = case repoTag repo of
          Nothing  -> []
          Just tag -> ["-r", "tag:" ++ tag]
        verboseArg = [ "--quiet" | verbosity < Verbosity.normal ]

    vcsSyncRepos :: Verbosity -> ConfiguredProgram
                 -> [(SourceRepo, FilePath)] -> IO [MonitorFilePath]
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
                 -> SourceRepo
                 -> FilePath
                 -> FilePath
                 -> [ProgramInvocation]
    vcsCloneRepo verbosity prog repo srcurl destdir =
        [ programInvocation prog cloneArgs ]
      where
        cloneArgs  = [cloneCmd, srcurl, destdir] ++ tagArgs ++ verboseArg
        -- At some point the @clone@ command was introduced as an alias for
        -- @get@, and @clone@ seems to be the recommended one now.
        cloneCmd   | programVersion prog >= Just (mkVersion [2,8])
                               = "clone"
                   | otherwise = "get"
        tagArgs    = case repoTag repo of
          Nothing  -> []
          Just tag -> ["-t", tag]
        verboseArg = [ "--quiet" | verbosity < Verbosity.normal ]

    vcsSyncRepos :: Verbosity -> ConfiguredProgram
                 -> [(SourceRepo, FilePath)] -> IO [MonitorFilePath]
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
                 -> SourceRepo
                 -> FilePath
                 -> FilePath
                 -> [ProgramInvocation]
    vcsCloneRepo verbosity prog repo srcurl destdir =
        [ programInvocation prog cloneArgs ]
        -- And if there's a tag, we have to do that in a second step:
     ++ [ (programInvocation prog (checkoutArgs tag)) {
            progInvokeCwd = Just destdir
          }
        | tag <- maybeToList (repoTag repo) ]
      where
        cloneArgs  = ["clone", srcurl, destdir]
                     ++ branchArgs ++ verboseArg
        branchArgs = case repoBranch repo of
          Just b  -> ["--branch", b]
          Nothing -> []
        checkoutArgs tag = ["checkout", tag] ++ verboseArg
        verboseArg = [ "--quiet" | verbosity < Verbosity.normal ]

    vcsSyncRepos :: Verbosity
                 -> ConfiguredProgram
                 -> [(SourceRepo, FilePath)]
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

    vcsSyncRepo verbosity gitProg SourceRepo{..} localDir peer = do
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
                         where Just loc = repoLocation
        checkoutArgs   = [ "checkout", "--detach", "--force"
                         , checkoutTarget ] ++ verboseArg
        checkoutTarget = fromMaybe "HEAD" (repoBranch `mplus` repoTag)
        verboseArg     = [ "--quiet" | verbosity < Verbosity.normal ]

gitProgram :: Program
gitProgram = (simpleProgram "git") {
    programFindVersion = findProgramVersion "--version" $ \str ->
      case words str of
        -- "git version 2.5.5"
        (_:_:ver:_) -> ver
        _ -> ""
  }


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
                 -> SourceRepo
                 -> FilePath
                 -> FilePath
                 -> [ProgramInvocation]
    vcsCloneRepo verbosity prog repo srcurl destdir =
        [ programInvocation prog cloneArgs ]
      where
        cloneArgs  = ["clone", srcurl, destdir]
                     ++ branchArgs ++ tagArgs ++ verboseArg
        branchArgs = case repoBranch repo of
          Just b  -> ["--branch", b]
          Nothing -> []
        tagArgs = case repoTag repo of
          Just t  -> ["--rev", t]
          Nothing -> []
        verboseArg = [ "--quiet" | verbosity < Verbosity.normal ]

    vcsSyncRepos :: Verbosity
                 -> ConfiguredProgram
                 -> [(SourceRepo, FilePath)]
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
                 -> SourceRepo
                 -> FilePath
                 -> FilePath
                 -> [ProgramInvocation]
    vcsCloneRepo verbosity prog _repo srcurl destdir =
        [ programInvocation prog checkoutArgs ]
      where
        checkoutArgs = ["checkout", srcurl, destdir] ++ verboseArg
        verboseArg   = [ "--quiet" | verbosity < Verbosity.normal ]
        --TODO: branch or tag?

    vcsSyncRepos :: Verbosity
                 -> ConfiguredProgram
                 -> [(SourceRepo, FilePath)]
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

