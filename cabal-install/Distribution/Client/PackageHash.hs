{-# LANGUAGE RecordWildCards, NamedFieldPuns, GeneralizedNewtypeDeriving #-}

-- | Functions to calculate nix-style hashes for package ids.
--
-- The basic idea is simple, hash the combination of:
--
--   * the package tarball
--   * the ids of all the direct dependencies
--   * other local configuration (flags, profiling, etc)
--
module Distribution.Client.PackageHash (
    -- * Calculating package hashes
    PackageHashInputs(..),
    PackageHashConfigInputs(..),
    PackageSourceHash,
    hashedInstalledPackageId,
    hashPackageHashInputs,
    renderPackageHashInputs,

    -- * Low level hash choice
    HashValue,
    hashValue,
    showHashValue,
    readFileHashValue,
    hashFromTUF,
  ) where

import Distribution.Package
         ( PackageId, mkUnitId )
import Distribution.System
         ( Platform )
import Distribution.PackageDescription
         ( FlagName(..), FlagAssignment )
import Distribution.Simple.Compiler
         ( CompilerId, OptimisationLevel(..), DebugInfoLevel(..)
         , ProfDetailLevel(..), showProfDetailLevel )
import Distribution.Simple.InstallDirs
         ( PathTemplate, fromPathTemplate )
import Distribution.Text
         ( display )
import Distribution.Client.Types
         ( InstalledPackageId )

import qualified Hackage.Security.Client    as Sec

import qualified Crypto.Hash.SHA256         as SHA256
import qualified Data.ByteString.Base16     as Base16
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import qualified Data.Set as Set
import           Data.Set (Set)

import Data.Maybe        (catMaybes)
import Data.List         (sortBy, intercalate)
import Data.Function     (on)
import Distribution.Compat.Binary (Binary(..))
import Control.Exception (evaluate)
import System.IO         (withBinaryFile, IOMode(..))


-------------------------------
-- Calculating package hashes
--

-- | Calculate a 'InstalledPackageId' for a package using our nix-style
-- inputs hashing method.
--
hashedInstalledPackageId :: PackageHashInputs -> InstalledPackageId
hashedInstalledPackageId pkghashinputs@PackageHashInputs{pkgHashPkgId} =
    mkUnitId $
         display pkgHashPkgId   -- to be a bit user friendly
      ++ "-"
      ++ showHashValue (hashPackageHashInputs pkghashinputs)

-- | All the information that contribues to a package's hash, and thus its
-- 'InstalledPackageId'.
--
data PackageHashInputs = PackageHashInputs {
       pkgHashPkgId         :: PackageId,
       pkgHashSourceHash    :: PackageSourceHash,
       pkgHashDirectDeps    :: Set InstalledPackageId,
       pkgHashOtherConfig   :: PackageHashConfigInputs
     }

type PackageSourceHash = HashValue

-- | Those parts of the package configuration that contribute to the
-- package hash.
--
data PackageHashConfigInputs = PackageHashConfigInputs {
       pkgHashCompilerId          :: CompilerId,
       pkgHashPlatform            :: Platform,
       pkgHashFlagAssignment      :: FlagAssignment, -- complete not partial
       pkgHashConfigureScriptArgs :: [String], -- just ./configure for build-type Configure
       pkgHashVanillaLib          :: Bool,
       pkgHashSharedLib           :: Bool,
       pkgHashDynExe              :: Bool,
       pkgHashGHCiLib             :: Bool,
       pkgHashProfLib             :: Bool,
       pkgHashProfExe             :: Bool,
       pkgHashProfLibDetail       :: ProfDetailLevel,
       pkgHashProfExeDetail       :: ProfDetailLevel,
       pkgHashCoverage            :: Bool,
       pkgHashOptimization        :: OptimisationLevel,
       pkgHashSplitObjs           :: Bool,
       pkgHashStripLibs           :: Bool,
       pkgHashStripExes           :: Bool,
       pkgHashDebugInfo           :: DebugInfoLevel,
       pkgHashExtraLibDirs        :: [FilePath],
       pkgHashExtraFrameworkDirs  :: [FilePath],
       pkgHashExtraIncludeDirs    :: [FilePath],
       pkgHashProgPrefix          :: Maybe PathTemplate,
       pkgHashProgSuffix          :: Maybe PathTemplate

--     TODO: [required eventually] extra program options
--     TODO: [required eventually] pkgHashToolsVersions     ?
--     TODO: [required eventually] pkgHashToolsExtraOptions ?
--     TODO: [research required] and what about docs?
     }
  deriving Show


-- | Calculate the overall hash to be used for an 'InstalledPackageId'.
--
hashPackageHashInputs :: PackageHashInputs -> HashValue
hashPackageHashInputs = hashValue . renderPackageHashInputs

-- | Render a textual representation of the 'PackageHashInputs'.
--
-- The 'hashValue' of this text is the overall package hash.
--
renderPackageHashInputs :: PackageHashInputs -> LBS.ByteString
renderPackageHashInputs PackageHashInputs{
                          pkgHashPkgId,
                          pkgHashSourceHash,
                          pkgHashDirectDeps,
                          pkgHashOtherConfig =
                            PackageHashConfigInputs{..}
                        } =
    -- The purpose of this somewhat laboured rendering (e.g. why not just
    -- use show?) is so that existing package hashes do not change
    -- unnecessarily when new configuration inputs are added into the hash.

    -- In particular, the assumption is that when a new configuration input
    -- is included into the hash, that existing packages will typically get
    -- the default value for that feature. So if we avoid adding entries with
    -- the default value then most of the time adding new features will not
    -- change the hashes of existing packages and so fewer packages will need
    -- to be rebuilt. 

    --TODO: [nice to have] ultimately we probably want to put this config info
    -- into the ghc-pkg db. At that point this should probably be changed to
    -- use the config file infrastructure so it can be read back in again.
    LBS.pack $ unlines $ catMaybes
      [ entry "pkgid"       display pkgHashPkgId
      , entry "src"         showHashValue pkgHashSourceHash
      , entry "deps"        (intercalate ", " . map display
                                              . Set.toList) pkgHashDirectDeps
        -- and then all the config
      , entry "compilerid"  display pkgHashCompilerId
      , entry "platform"    display pkgHashPlatform
      , opt   "flags" []    showFlagAssignment pkgHashFlagAssignment
      , opt   "configure-script" [] unwords pkgHashConfigureScriptArgs
      , opt   "vanilla-lib" True  display pkgHashVanillaLib
      , opt   "shared-lib"  False display pkgHashSharedLib
      , opt   "dynamic-exe" False display pkgHashDynExe
      , opt   "ghci-lib"    False display pkgHashGHCiLib
      , opt   "prof-lib"    False display pkgHashProfLib
      , opt   "prof-exe"    False display pkgHashProfExe
      , opt   "prof-lib-detail" ProfDetailDefault showProfDetailLevel pkgHashProfLibDetail 
      , opt   "prof-exe-detail" ProfDetailDefault showProfDetailLevel pkgHashProfExeDetail 
      , opt   "hpc"          False display pkgHashCoverage
      , opt   "optimisation" NormalOptimisation (show . fromEnum) pkgHashOptimization
      , opt   "split-objs"   False display pkgHashSplitObjs
      , opt   "stripped-lib" False display pkgHashStripLibs
      , opt   "stripped-exe" True  display pkgHashStripExes
      , opt   "debug-info"   NormalDebugInfo (show . fromEnum) pkgHashDebugInfo
      , opt   "extra-lib-dirs"     [] unwords pkgHashExtraLibDirs
      , opt   "extra-framework-dirs" [] unwords pkgHashExtraFrameworkDirs
      , opt   "extra-include-dirs" [] unwords pkgHashExtraIncludeDirs
      , opt   "prog-prefix" Nothing (maybe "" fromPathTemplate) pkgHashProgPrefix
      , opt   "prog-suffix" Nothing (maybe "" fromPathTemplate) pkgHashProgSuffix
      ]
  where
    entry key     format value = Just (key ++ ": " ++ format value)
    opt   key def format value
         | value == def = Nothing
         | otherwise    = entry key format value

    showFlagAssignment = unwords . map showEntry . sortBy (compare `on` fst)
      where
        showEntry (FlagName name, False) = '-' : name
        showEntry (FlagName name, True)  = '+' : name

-----------------------------------------------
-- The specific choice of hash implementation
--

-- Is a crypto hash necessary here? One thing to consider is who controls the
-- inputs and what's the result of a hash collision. Obviously we should not
-- install packages we don't trust because they can run all sorts of code, but
-- if I've checked there's no TH, no custom Setup etc, is there still a
-- problem? If someone provided us a tarball that hashed to the same value as
-- some other package and we installed it, we could end up re-using that
-- installed package in place of another one we wanted. So yes, in general
-- there is some value in preventing intentional hash collisions in installed
-- package ids.

newtype HashValue = HashValue BS.ByteString
  deriving (Eq, Show)

instance Binary HashValue where
  put (HashValue digest) = put digest
  get = do
    digest <- get
    -- Cannot do any sensible validation here. Although we use SHA256
    -- for stuff we hash ourselves, we can also get hashes from TUF
    -- and that can in principle use different hash functions in future.
    return (HashValue digest)

-- | Hash some data. Currently uses SHA256.
--
hashValue :: LBS.ByteString -> HashValue
hashValue = HashValue . SHA256.hashlazy

showHashValue :: HashValue -> String
showHashValue (HashValue digest) = BS.unpack (Base16.encode digest)

-- | Hash the content of a file. Uses SHA256.
--
readFileHashValue :: FilePath -> IO HashValue
readFileHashValue tarball =
    withBinaryFile tarball ReadMode $ \hnd ->
      evaluate . hashValue =<< LBS.hGetContents hnd

-- | Convert a hash from TUF metadata into a 'PackageSourceHash'.
--
-- Note that TUF hashes don't neessarily have to be SHA256, since it can
-- support new algorithms in future.
--
hashFromTUF :: Sec.Hash -> HashValue
hashFromTUF (Sec.Hash hashstr) =
    --TODO: [code cleanup] either we should get TUF to use raw bytestrings or
    -- perhaps we should also just use a base16 string as the internal rep.
    case Base16.decode (BS.pack hashstr) of
      (hash, trailing) | not (BS.null hash) && BS.null trailing
        -> HashValue hash
      _ -> error "hashFromTUF: cannot decode base16 hash"

