{-# OPTIONS -cpp #-}
-- OPTIONS required for ghc-6.4.x compat, and must appear first
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -cpp #-}
{-# OPTIONS_NHC98 -cpp #-}
{-# OPTIONS_JHC -fcpp  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.InstallSymlink
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  cabal-devel@haskell.org
-- Stability   :  provisional
-- Portability :  portable
--
-- Managing installing binaries with symlinks.
-----------------------------------------------------------------------------
module Distribution.Client.InstallSymlink (
    symlinkBinaries,
    symlinkBinary,
  ) where

#if mingw32_HOST_OS || mingw32_TARGET_OS

import Distribution.Client.InstallPlan (InstallPlan)
import Distribution.Client.Setup (InstallFlags)
import Distribution.Simple.Setup (ConfigFlags)

symlinkBinaries :: ConfigFlags
                -> InstallFlags
                -> InstallPlan -> IO ()
symlinkBinaries _ _ = symlinkBinary undefined undefined undefined undefined

symlinkBinary :: FilePath -> FilePath -> String -> String -> IO Bool
symlinkBinary _ _ _ _ = fail "Symlinking feature not available on Windows"

#else

import Distribution.Client.Types
         ( AvailablePackage(..), ConfiguredPackage(..) )
import Distribution.Client.Setup
         ( InstallFlags(installSymlinkBinDir) )
import qualified Distribution.Client.InstallPlan as InstallPlan
import Distribution.Client.InstallPlan (InstallPlan)

import Distribution.Package
         ( PackageIdentifier, Package(packageId) )
import Distribution.Compiler
         ( CompilerId(..) )
import qualified Distribution.PackageDescription as PackageDescription
import Distribution.PackageDescription
         ( PackageDescription )
import Distribution.PackageDescription.Configuration
         ( finalizePackageDescription )
import Distribution.Simple.Setup
         ( ConfigFlags(..), fromFlag, fromFlagOrDefault, flagToMaybe )
import qualified Distribution.Simple.InstallDirs as InstallDirs
import Distribution.Simple.PackageIndex (PackageIndex)

import System.Posix.Files
         ( getSymbolicLinkStatus, isSymbolicLink, readSymbolicLink
         , createSymbolicLink, removeLink )
import System.Directory
         ( canonicalizePath )
import System.FilePath
         ( (</>), takeDirectory, splitPath, joinPath, isAbsolute )
import System.IO.Error
         ( catch, isDoesNotExistError, ioError )
import Control.Exception
         ( assert )
import Data.Maybe
         ( catMaybes )

-- | We would like by default to install binaries into some location that is on
-- the user's PATH. For per-user installations on Unix systems that basically
-- means the @~/bin/@ directory. On the majority of platforms the @~/bin/@
-- directory will be on the user's PATH. However some people are a bit nervous
-- about letting a package manager install programs into @~/bin/@.
--
-- A comprimise solution is that instead of installing binaries directly into
-- @~/bin/@, we could install them in a private location under @~/.cabal/bin@
-- and then create symlinks in @~/bin/@. We can be careful when setting up the
-- symlinks that we do not overwrite any binary that the user installed. We can
-- check if it was a symlink we made because it would point to the private dir
-- where we install our binaries. This means we can install normally without
-- worrying and in a later phase set up symlinks, and if that fails then we
-- report it to the user, but even in this case the package is still in an ok
-- installed state.
--
-- This is an optional feature that users can choose to use or not. It is
-- controlled from the config file. Of course it only works on posix systems
-- with symlinks so is not available to Windows users.
--
symlinkBinaries :: ConfigFlags
                -> InstallFlags
                -> InstallPlan
                -> IO [(PackageIdentifier, String, FilePath)]
symlinkBinaries configFlags installFlags plan =
  case flagToMaybe (installSymlinkBinDir installFlags) of
    Nothing            -> return []
    Just symlinkBinDir -> do
      publicBinDir  <- canonicalizePath symlinkBinDir
      fmap catMaybes $ sequence
        [ let publicExeName  = PackageDescription.exeName exe
              privateExeName = prefix ++ publicExeName ++ suffix
              prefix = substTemplate pkgid prefixTemplate
              suffix = substTemplate pkgid suffixTemplate
          in do privateBinDir <- pkgBinDir pkg
                ok <- symlinkBinary
                        publicBinDir  privateBinDir
                        publicExeName privateExeName
                if ok
                  then return Nothing
                  else return (Just (pkgid, publicExeName,
                                     privateBinDir </> privateExeName))
        | InstallPlan.Installed cpkg _ <- InstallPlan.toList plan
        , let pkg   = pkgDescription cpkg
              pkgid = packageId pkg
        , exe <- PackageDescription.executables pkg
        , PackageDescription.buildable (PackageDescription.buildInfo exe) ]
  where

    pkgDescription :: ConfiguredPackage -> PackageDescription
    pkgDescription (ConfiguredPackage (AvailablePackage _ pkg _) flags _) =
      case finalizePackageDescription flags
             (Nothing :: Maybe (PackageIndex PackageDescription))
             os arch compilerId [] pkg of
        Left _ -> error "finalizePackageDescription ConfiguredPackage failed"
        Right (desc, _) -> desc

    -- This is sadly rather complicated. We're kind of re-doing part of the
    -- configuration for the package. :-(
    pkgBinDir :: PackageDescription -> IO FilePath
    pkgBinDir pkg = do
      defaultDirs <- InstallDirs.defaultInstallDirs
                       compilerFlavor
                       (fromFlag (configUserInstall configFlags))
                       (PackageDescription.hasLibs pkg)
      let templateDirs = InstallDirs.combineInstallDirs fromFlagOrDefault
                           defaultDirs (configInstallDirs configFlags)
          absoluteDirs = InstallDirs.absoluteInstallDirs
                           (packageId pkg) compilerId InstallDirs.NoCopyDest
                           templateDirs
      canonicalizePath (InstallDirs.bindir absoluteDirs)

    substTemplate pkgid = InstallDirs.fromPathTemplate
                        . InstallDirs.substPathTemplate env
      where env = InstallDirs.initialPathTemplateEnv pkgid compilerId

    fromFlagTemplate = fromFlagOrDefault (InstallDirs.toPathTemplate "")
    prefixTemplate   = fromFlagTemplate (configProgPrefix configFlags)
    suffixTemplate   = fromFlagTemplate (configProgSuffix configFlags)
    os   = InstallPlan.planOS plan
    arch = InstallPlan.planArch plan
    compilerId@(CompilerId compilerFlavor _) = InstallPlan.planCompiler plan

symlinkBinary :: FilePath -- ^ The canonical path of the public bin dir
                          --   eg @/home/user/bin@
              -> FilePath -- ^ The canonical path of the private bin dir
                          --   eg @/home/user/.cabal/bin@
              -> String   -- ^ The name of the executable to go in the public
                          --   bin dir, eg @foo@
              -> String   -- ^ The name of the executable to in the private bin
                          --   dir, eg @foo-1.0@
              -> IO Bool  -- ^ If creating the symlink was sucessful. @False@
                          --   if there was another file there already that we
                          --   did not own. Other errors like permission errors
                          --   just propagate as exceptions.
symlinkBinary publicBindir privateBindir publicName privateName = do
  ok <- targetOkToOverwrite (publicBindir </> publicName) privateBindir
  case ok of
    NotOurFile    ->                     return False
    NotExists     ->           mkLink >> return True
    OkToOverwrite -> rmLink >> mkLink >> return True
  where
    relativeBindir = makeRelative publicBindir privateBindir
    mkLink = createSymbolicLink (relativeBindir </> privateName)
                                (publicBindir   </> publicName)
    rmLink = removeLink (publicBindir </> publicName)

-- | Check a filepath of a symlink that we would like to create to see if it
-- is ok. For it to be ok to overwrite it must either not already exist yet or
-- be a symlink to our private bin dir (in which case we can assume ownership).
--
targetOkToOverwrite :: FilePath -- ^ The filepath of the symlink to the private
                                -- binary that we would like to create
                    -> FilePath -- ^ The canonical path of the private bin
                                -- directory. Use 'canonicalizePath'.
                    -> IO SymlinkStatus
targetOkToOverwrite symlink privateBinDir = handleNotExist $ do
  status <- getSymbolicLinkStatus symlink
  if not (isSymbolicLink status)
    then return NotOurFile
    else return
       . (\ok -> if ok then OkToOverwrite else NotOurFile)
       . (== privateBinDir)
       . takeDirectory
     =<< canonicalizePath
       . (symlink </>)
     =<< readSymbolicLink symlink

  where
    handleNotExist action = catch action $ \ioexception ->
      -- If the target doesn't exist then there's no problem overwriting it!
      if isDoesNotExistError ioexception
        then return NotExists
        else ioError ioexception

data SymlinkStatus
   = NotExists     -- ^ The file doesn't exist so we can make a symlink.
   | OkToOverwrite -- ^ A symlink already exists, though it is ours. We'll
                   -- have to delete it first bemore we make a new symlink.
   | NotOurFile    -- ^ A file already exists and it is not one of our existing
                   -- symlinks (either because it is not a symlink or because
                   -- it points somewhere other than our managed space).
  deriving Show

-- | Take two canonical paths and produce a relative path to get from the first
-- to the second, even if it means adding @..@ path components.
--
makeRelative :: FilePath -> FilePath -> FilePath
makeRelative a b = assert (isAbsolute a && isAbsolute b) $
  let as = splitPath a
      bs = splitPath b
      commonLen = length $ takeWhile id $ zipWith (==) as bs
   in joinPath $ [ ".." | _  <- drop commonLen as ]
              ++ [  b'  | b' <- drop commonLen bs ]

#endif
