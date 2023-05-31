{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Distribution.Client.Types.SourceRepo
  ( SourceRepositoryPackage (..)
  , SourceRepoList
  , SourceRepoMaybe
  , SourceRepoProxy
  , srpHoist
  , srpToProxy
  , srpFanOut
  , sourceRepositoryPackageGrammar
  ) where

import Distribution.Client.Compat.Prelude
import Distribution.Compat.Lens (Lens, Lens')
import Prelude ()

import Distribution.FieldGrammar
import Distribution.Types.SourceRepo (RepoType (..))

-- | @source-repository-package@ definition
data SourceRepositoryPackage f = SourceRepositoryPackage
  { srpType :: !RepoType
  , srpLocation :: !String
  , srpTag :: !(Maybe String)
  , srpBranch :: !(Maybe String)
  , srpSubdir :: !(f FilePath)
  , srpCommand :: ![String]
  }
  deriving (Generic)

deriving instance Eq (f FilePath) => Eq (SourceRepositoryPackage f)
deriving instance Ord (f FilePath) => Ord (SourceRepositoryPackage f)
deriving instance Show (f FilePath) => Show (SourceRepositoryPackage f)
deriving instance Binary (f FilePath) => Binary (SourceRepositoryPackage f)
deriving instance (Typeable f, Structured (f FilePath)) => Structured (SourceRepositoryPackage f)

-- | Read from @cabal.project@
type SourceRepoList = SourceRepositoryPackage []

-- | Distilled from 'Distribution.Types.SourceRepo.SourceRepo'
type SourceRepoMaybe = SourceRepositoryPackage Maybe

-- | 'SourceRepositoryPackage' without subdir. Used in clone errors. Cloning doesn't care about subdirectory.
type SourceRepoProxy = SourceRepositoryPackage Proxy

srpHoist :: (forall x. f x -> g x) -> SourceRepositoryPackage f -> SourceRepositoryPackage g
srpHoist nt s = s{srpSubdir = nt (srpSubdir s)}

srpToProxy :: SourceRepositoryPackage f -> SourceRepositoryPackage Proxy
srpToProxy s = s{srpSubdir = Proxy}

-- | Split single @source-repository-package@ declaration with multiple subdirs,
-- into multiple ones with at most single subdir.
srpFanOut :: SourceRepositoryPackage [] -> NonEmpty (SourceRepositoryPackage Maybe)
srpFanOut s@SourceRepositoryPackage{srpSubdir = []} =
  s{srpSubdir = Nothing} :| []
srpFanOut s@SourceRepositoryPackage{srpSubdir = d : ds} = f d :| map f ds
  where
    f subdir = s{srpSubdir = Just subdir}

-------------------------------------------------------------------------------
-- Lens
-------------------------------------------------------------------------------

srpTypeLens :: Lens' (SourceRepositoryPackage f) RepoType
srpTypeLens f s = fmap (\x -> s{srpType = x}) (f (srpType s))
{-# INLINE srpTypeLens #-}

srpLocationLens :: Lens' (SourceRepositoryPackage f) String
srpLocationLens f s = fmap (\x -> s{srpLocation = x}) (f (srpLocation s))
{-# INLINE srpLocationLens #-}

srpTagLens :: Lens' (SourceRepositoryPackage f) (Maybe String)
srpTagLens f s = fmap (\x -> s{srpTag = x}) (f (srpTag s))
{-# INLINE srpTagLens #-}

srpBranchLens :: Lens' (SourceRepositoryPackage f) (Maybe String)
srpBranchLens f s = fmap (\x -> s{srpBranch = x}) (f (srpBranch s))
{-# INLINE srpBranchLens #-}

srpSubdirLens :: Lens (SourceRepositoryPackage f) (SourceRepositoryPackage g) (f FilePath) (g FilePath)
srpSubdirLens f s = fmap (\x -> s{srpSubdir = x}) (f (srpSubdir s))
{-# INLINE srpSubdirLens #-}

srpCommandLensNE :: Lens' (SourceRepositoryPackage f) (Maybe (NonEmpty String))
srpCommandLensNE f s = fmap (\x -> s{srpCommand = maybe [] toList x}) (f (nonEmpty (srpCommand s)))
{-# INLINE srpCommandLensNE #-}

-------------------------------------------------------------------------------
-- Parser & PPrinter
-------------------------------------------------------------------------------

sourceRepositoryPackageGrammar
  :: ( FieldGrammar c g
     , Applicative (g SourceRepoList)
     , c (Identity RepoType)
     , c (List NoCommaFSep FilePathNT String)
     , c (NonEmpty' NoCommaFSep Token String)
     )
  => g SourceRepoList SourceRepoList
sourceRepositoryPackageGrammar =
  SourceRepositoryPackage
    <$> uniqueField "type" srpTypeLens
    <*> uniqueFieldAla "location" Token srpLocationLens
    <*> optionalFieldAla "tag" Token srpTagLens
    <*> optionalFieldAla "branch" Token srpBranchLens
    <*> monoidalFieldAla "subdir" (alaList' NoCommaFSep FilePathNT) srpSubdirLens -- note: NoCommaFSep is somewhat important for roundtrip, as "." is there...
    <*> fmap (maybe [] toList) pcc
  where
    pcc = optionalFieldAla "post-checkout-command" (alaNonEmpty' NoCommaFSep Token) srpCommandLensNE
{-# SPECIALIZE sourceRepositoryPackageGrammar :: ParsecFieldGrammar' SourceRepoList #-}
{-# SPECIALIZE sourceRepositoryPackageGrammar :: PrettyFieldGrammar' SourceRepoList #-}
