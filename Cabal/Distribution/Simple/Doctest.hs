{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes    #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Haddock
-- Copyright   :  Isaac Jones 2003-2005
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module deals with the @doctest@ command.

module Distribution.Simple.Doctest (
  doctest
  ) where

import Prelude ()
import Distribution.Compat.Prelude


-- local
-- import Distribution.Backpack.DescribeUnitId (setupMessage')
import Distribution.Types.UnqualComponentName
import Distribution.Types.ComponentLocalBuildInfo
-- import Distribution.Package
import qualified Distribution.ModuleName as ModuleName
import Distribution.PackageDescription as PD hiding (Flag)
import Distribution.Simple.Program
import Distribution.Simple.PreProcess
import Distribution.Simple.Setup
import Distribution.Simple.Build
import Distribution.Simple.LocalBuildInfo hiding (substPathTemplate)
import Distribution.Simple.BuildPaths
import Distribution.Simple.Utils
import Distribution.Text
import Distribution.Version
import Distribution.Verbosity

import System.FilePath  ( (</>), normalise )

-- -----------------------------------------------------------------------------
-- Types

-- | A record that represents the arguments to the doctest executable.
data DoctestArgs = DoctestArgs {
  argTargets :: [FilePath]
  -- ^ Modules to process
} deriving (Show, Generic)

-- -----------------------------------------------------------------------------
-- Doctest support

doctest :: PackageDescription
        -> LocalBuildInfo
        -> [PPSuffixHandler]
        -> DoctestFlags
        -> IO ()
doctest pkg_descr lbi suffixes doctestFlags = do
  let verbosity = flag doctestVerbosity
      flag f    = fromFlag $ f doctestFlags
  (doctestProg, _version, _) <-
    requireProgramVersion verbosity doctestProgram
      (orLaterVersion (mkVersion [0,11])) (withPrograms lbi)

  withAllComponentsInBuildOrder pkg_descr lbi $ \component clbi -> do
     componentInitialBuildSteps (flag doctestDistPref) pkg_descr lbi clbi verbosity
     preprocessComponent pkg_descr component lbi clbi False verbosity suffixes
     -- let
     --   smsg :: IO ()
     --   smsg = setupMessage' verbosity "Running Doctest on" (packageId pkg_descr)
     --          (componentLocalName clbi) (maybeComponentInstantiatedWith clbi)

     case component of 
       CLib lib -> do
         args <- DoctestArgs . map snd <$> getLibSourceFiles verbosity lbi lib clbi
         runDoctest verbosity doctestProg args
       CExe exe -> do
         args <- DoctestArgs . map snd <$> getExeSourceFiles verbosity lbi exe clbi
         runDoctest verbosity doctestProg args
       CFLib _  -> return () -- do not doctest foreign libs
       CTest _  -> return () -- do not doctest tests
       CBench _ -> return () -- do not doctest benchmarks 
     
-- -----------------------------------------------------------------------------
-- Call doctest with the specified arguments.
runDoctest :: Verbosity
           -> ConfiguredProgram
           -> DoctestArgs
           -> IO ()
runDoctest verbosity doctestProg args = do
  renderArgs verbosity args $
    \(flags, files) -> do
      runProgram verbosity doctestProg (flags <> files)

renderArgs :: Verbosity
           -> DoctestArgs
           -> (([String],[FilePath]) -> IO a)
           -> IO a
renderArgs _verbosity args k = do
  -- inject the "--no-magic" flag, to have a rather bare
  -- doctest invocation, and disable doctests automagic discovery heuristics.
  k (["--no-magic"], argTargets args)

-- -----------------------------------------------------------------------------
-- TODO: move somewhere else (this is copied from Haddock.hs!)
getLibSourceFiles :: Verbosity
                     -> LocalBuildInfo
                     -> Library
                     -> ComponentLocalBuildInfo
                     -> IO [(ModuleName.ModuleName, FilePath)]
getLibSourceFiles verbosity lbi lib clbi = getSourceFiles verbosity searchpaths modules
  where
    bi               = libBuildInfo lib
    modules          = allLibModules lib clbi
    searchpaths      = componentBuildDir lbi clbi : hsSourceDirs bi ++
                     [ autogenComponentModulesDir lbi clbi
                     , autogenPackageModulesDir lbi ]

getExeSourceFiles :: Verbosity
                     -> LocalBuildInfo
                     -> Executable
                     -> ComponentLocalBuildInfo
                     -> IO [(ModuleName.ModuleName, FilePath)]
getExeSourceFiles verbosity lbi exe clbi = do
    moduleFiles <- getSourceFiles verbosity searchpaths modules
    srcMainPath <- findFile (hsSourceDirs bi) (modulePath exe)
    return ((ModuleName.main, srcMainPath) : moduleFiles)
  where
    bi          = buildInfo exe
    modules     = otherModules bi
    searchpaths = autogenComponentModulesDir lbi clbi
                : autogenPackageModulesDir lbi
                : exeBuildDir lbi exe : hsSourceDirs bi

getSourceFiles :: Verbosity -> [FilePath]
                  -> [ModuleName.ModuleName]
                  -> IO [(ModuleName.ModuleName, FilePath)]
getSourceFiles verbosity dirs modules = flip traverse modules $ \m -> fmap ((,) m) $
    findFileWithExtension ["hs", "lhs", "hsig", "lhsig"] dirs (ModuleName.toFilePath m)
      >>= maybe (notFound m) (return . normalise)
  where
    notFound module_ = die' verbosity $ "doctest: can't find source for module " ++ display module_

-- | The directory where we put build results for an executable
exeBuildDir :: LocalBuildInfo -> Executable -> FilePath
exeBuildDir lbi exe = buildDir lbi </> nm </> nm ++ "-tmp"
  where
    nm = unUnqualComponentName $ exeName exe

-- ------------------------------------------------------------------------------
-- Boilerplate Monoid instance.
instance Monoid DoctestArgs where
    mempty = gmempty
    mappend = (<>)

instance Semigroup DoctestArgs where
    (<>) = gmappend
