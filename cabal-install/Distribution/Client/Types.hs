-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.Types
-- Copyright   :  (c) David Himmelstrup 2005
-- License     :  BSD-like
--
-- Maintainer  :  lemmih@gmail.com
-- Stability   :  provisional
-- Portability :  portable
--
-- All data types for the entire cabal-install system gathered here to avoid some .hs-boot files.
-----------------------------------------------------------------------------
module Distribution.Client.Types where

import Distribution.Package
         ( PackageName, PackageId, Package(..)
         , PackageFixedDeps(..), Dependency )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import Distribution.PackageDescription
         ( GenericPackageDescription, FlagAssignment, FlagName(FlagName) )
import Distribution.Client.PackageIndex
         ( PackageIndex )
import Distribution.Version
         ( VersionRange )
import Distribution.Text
          ( Text(disp,parse) )
import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
         

import Data.Char as Char
import Data.Map (Map)
import Network.URI (URI)
import Distribution.Compat.Exception
         ( SomeException )

newtype Username = Username { unUsername :: String }
newtype Password = Password { unPassword :: String }

-- | This is the information we get from a @00-index.tar.gz@ hackage index.
--
data AvailablePackageDb = AvailablePackageDb {
  packageIndex       :: PackageIndex AvailablePackage,
  packagePreferences :: Map PackageName VersionRange
}

-- | TODO: This is a hack to help us transition from Cabal-1.6 to 1.8.
-- What is new in 1.8 is that installed packages and dependencies between
-- installed packages are now identified by an opaque InstalledPackageId
-- rather than a source PackageId.
--
-- We should use simply an 'InstalledPackageInfo' here but to ease the
-- transition we are temporarily using this variant where we pretend that
-- installed packages still specify their deps in terms of PackageIds.
--
-- Crucially this means that 'InstalledPackage' can be an instance of
-- 'PackageFixedDeps' where as 'InstalledPackageInfo' is no longer an instance
-- of that class. This means we can make 'PackageIndex'es of InstalledPackage
-- where as the InstalledPackageInfo now has its own monomorphic index type.
--
data InstalledPackage = InstalledPackage
       InstalledPackageInfo
       [PackageId]

instance Package InstalledPackage where
  packageId (InstalledPackage pkg _) = packageId pkg
instance PackageFixedDeps InstalledPackage where
  depends (InstalledPackage _ deps) = deps

-- | A 'ConfiguredPackage' is a not-yet-installed package along with the
-- total configuration information. The configuration information is total in
-- the sense that it provides all the configuration information and so the
-- final configure process will be independent of the environment.
--
data ConfiguredPackage = ConfiguredPackage
       AvailablePackage    -- package info, including repo
       FlagAssignment      -- complete flag assignment for the package
       [PackageId]         -- set of exact dependencies. These must be
                           -- consistent with the 'buildDepends' in the
                           -- 'PackageDescrption' that you'd get by applying
                           -- the flag assignment.
  deriving Show

instance Package ConfiguredPackage where
  packageId (ConfiguredPackage pkg _ _) = packageId pkg

instance PackageFixedDeps ConfiguredPackage where
  depends (ConfiguredPackage _ _ deps) = deps


-- | We re-use @GenericPackageDescription@ and use the @package-url@
-- field to store the tarball URI.
data AvailablePackage = AvailablePackage {
    packageInfoId      :: PackageId,
    packageDescription :: GenericPackageDescription,
    packageSource      :: AvailablePackageSource
  }
  deriving Show

instance Package AvailablePackage where packageId = packageInfoId

data AvailablePackageSource =

    -- | The unpacked package in the current dir
    LocalUnpackedPackage

    -- | A package available as a tarball from a repository.
    --
    -- It may be from a local repository or from a remote repository, with a
    -- locally cached copy. ie a package available from hackage
  | RepoTarballPackage Repo

--  | ScmPackage
  deriving Show

--TODO:
--  * generalise local package to any local unpacked package, not just in the
--      current dir, ie add a FilePath param
--  * add support for darcs and other SCM style remote repos with a local cache

data LocalRepo = LocalRepo
  deriving (Show,Eq)

data RemoteRepo = RemoteRepo {
    remoteRepoName :: String,
    remoteRepoURI  :: URI
  }
  deriving (Show,Eq)

data Repo = Repo {
    repoKind     :: Either RemoteRepo LocalRepo,
    repoLocalDir :: FilePath
  }
  deriving (Show,Eq)

data UnresolvedDependency
    = UnresolvedDependency
    { dependency :: Dependency
    , depFlags   :: FlagAssignment
    }
  deriving (Show,Eq)


instance Text UnresolvedDependency where
  disp udep = disp (dependency udep) Disp.<+> dispFlags (depFlags udep)
    where 
      dispFlags [] = Disp.empty
      dispFlags fs = Disp.text "--flags=" 
                     Disp.<> 
                     (Disp.doubleQuotes $ flagAssToDoc fs)
      flagAssToDoc = foldr (\(FlagName fname,val) flagAssDoc -> 
                             (if not val then Disp.char '-' 
                                         else Disp.empty)
                             Disp.<> Disp.text fname 
                             Disp.<+> flagAssDoc)
                           Disp.empty 
  parse = do
    dep <- parse 
    Parse.skipSpaces
    flagAss <- Parse.option [] parseFlagAssignment
    return $ UnresolvedDependency dep flagAss 
    where
      parseFlagAssignment :: Parse.ReadP r FlagAssignment
      parseFlagAssignment = do 
        Parse.string "--flags"
        Parse.skipSpaces
        Parse.char '='
        Parse.skipSpaces
        inDoubleQuotes $ Parse.many1 flag
        where
          inDoubleQuotes :: Parse.ReadP r a -> Parse.ReadP r a
          inDoubleQuotes = Parse.between (Parse.char '"') (Parse.char '"') 

          flag = do
            Parse.skipSpaces
            val <- negative Parse.+++ positive
            name <- ident
            Parse.skipSpaces
            return (FlagName name,val)
          negative = do
            Parse.char '-'
            return False
          positive = return True

          ident :: Parse.ReadP r String
          ident = do 
            -- First character must be a letter/digit to avoid flags
            -- like "+-debug":
            c  <- Parse.satisfy Char.isAlphaNum
            cs <- Parse.munch (\ch -> Char.isAlphaNum ch || ch == '_' 
                                                         || ch == '-')
            return (c:cs)
        


type BuildResult  = Either BuildFailure BuildSuccess
data BuildFailure = DependentFailed PackageId
                  | DownloadFailed  SomeException
                  | UnpackFailed    SomeException
                  | ConfigureFailed SomeException
                  | BuildFailed     SomeException
                  | InstallFailed   SomeException
data BuildSuccess = BuildOk         DocsResult TestsResult

data DocsResult  = DocsNotTried  | DocsFailed  | DocsOk
data TestsResult = TestsNotTried | TestsFailed | TestsOk
