{-# OPTIONS -cpp #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Configure
-- Copyright   :  Isaac Jones 2003-2005
-- 
-- Maintainer  :  Isaac Jones <ijones@syntaxpolice.org>
-- Stability   :  alpha
-- Portability :  portable
--
-- Explanation: Perform the \"@.\/setup configure@\" action.
-- Outputs the @dist\/setup-config@ file.

{- All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Isaac Jones nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE. -}

module Distribution.Simple.Configure (configure,
                                      writePersistBuildConfig,
                                      getPersistBuildConfig,
                                      maybeGetPersistBuildConfig,
--                                      getConfiguredPkgDescr,
                                      localBuildInfoFile,
                                      getInstalledPackages,
				      configDependency,
                                      configCompiler, configCompilerAux,
#ifdef DEBUG
                                      hunitTests
#endif
                                     )
    where

#if __GLASGOW_HASKELL__ && __GLASGOW_HASKELL__ < 604
#if __GLASGOW_HASKELL__ < 603
#include "config.h"
#else
#include "ghcconfig.h"
#endif
#endif

import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Register (removeInstalledConfig)
import Distribution.Setup(ConfigFlags(..), CopyDest(..))
import Distribution.Compiler(CompilerFlavor(..), Compiler(..),
			     compilerVersion, compilerPath, compilerPkgToolPath,
			     compilerBinaryName, extensionsToFlags)
import Distribution.Package (PackageIdentifier(..), showPackageId, 
			     parsePackageId)
import Distribution.PackageDescription(
 	PackageDescription(..), Library(..),
        GenericPackageDescription(..),
        finalizePackageDescription,
        HookedBuildInfo, sanityCheckPackage, updatePackageDescription,
	BuildInfo(..), Executable(..), setupMessage,
        satisfyDependency)
import Distribution.Simple.Utils (die, warn, rawSystemStdout)
import Distribution.Version (Version(..), Dependency(..), VersionRange(ThisVersion),
			     showVersion, showVersionRange)
--import Distribution.Configuration ( mkOSName, mkArchName )
import Distribution.Verbosity
import Distribution.ParseUtils ( showDependency )

import qualified Distribution.Simple.GHC  as GHC
import qualified Distribution.Simple.JHC  as JHC
import qualified Distribution.Simple.NHC  as NHC
import qualified Distribution.Simple.Hugs as Hugs

import Text.PrettyPrint.HughesPJ ( comma, punctuate, render, nest, sep )

import Data.List (intersperse, nub, isPrefixOf)
import Data.Char (isSpace, toLower)
import Data.Maybe(fromMaybe)
import System.Directory
import System.Environment ( getProgName )
import System.IO        ( hPutStrLn, stderr )
import System.FilePath ((</>))
import Distribution.Program(Program(..), ProgramLocation(..), programPath,
                            ProgramConfiguration(..),
			    lookupProgram, lookupPrograms,
			    updateProgram, maybeUpdateProgram,
                            findProgramAndVersion)
import System.Exit(ExitCode(..), exitWith)
--import System.Exit		( ExitCode(..) )
import qualified System.Info    ( os, arch )
import Control.Monad		( when, unless )
import Distribution.Compat.ReadP
import Distribution.Compat.Directory (createDirectoryIfMissing)
import Prelude hiding (catch)

#ifdef mingw32_HOST_OS
import Distribution.PackageDescription (hasLibs)
#endif

#ifdef DEBUG
import Test.HUnit
#endif

tryGetConfigStateFile :: (Read a) => FilePath -> IO (Either String a)
tryGetConfigStateFile filename = do
  e <- doesFileExist filename
  let dieMsg = "error reading " ++ filename ++ 
               "; run \"setup configure\" command?\n"
  if (not e) then return $ Left dieMsg else do 
    str <- readFile filename
    case reads str of
      [(bi,_)] -> return $ Right bi
      _        -> return $ Left  dieMsg

-- internal function
tryGetPersistBuildConfig :: IO (Either String LocalBuildInfo)
tryGetPersistBuildConfig = tryGetConfigStateFile localBuildInfoFile

-- |Read the 'localBuildInfoFile'.  Error if it doesn't exist.
getPersistBuildConfig :: IO LocalBuildInfo
getPersistBuildConfig = do
  lbi <- tryGetPersistBuildConfig
  either die return lbi

-- |Try to read the 'localBuildInfoFile'.
maybeGetPersistBuildConfig :: IO (Maybe LocalBuildInfo)
maybeGetPersistBuildConfig = do
  lbi <- tryGetPersistBuildConfig
  return $ either (const Nothing) Just lbi

-- |After running configure, output the 'LocalBuildInfo' to the
-- 'localBuildInfoFile'.
writePersistBuildConfig :: LocalBuildInfo -> IO ()
writePersistBuildConfig lbi = do
  createDirectoryIfMissing False distPref
  writeFile localBuildInfoFile (show lbi)

-- |@dist\/setup-config@
localBuildInfoFile :: FilePath
localBuildInfoFile = distPref </> "setup-config"


-- -----------------------------------------------------------------------------
-- * Configuration
-- -----------------------------------------------------------------------------

-- |Perform the \"@.\/setup configure@\" action.
-- Returns the @.setup-config@ file.
configure :: ( Either GenericPackageDescription PackageDescription
             , HookedBuildInfo) 
          -> ConfigFlags -> IO LocalBuildInfo
configure (pkg_descr0, pbi) cfg
  = do
	-- detect compiler
	comp <- configCompilerAux cfg
        let version = compilerVersion comp
            flavor  = compilerFlavor comp

        -- FIXME: currently only GHC has hc-pkg
        mipkgs <- case flavor of
                      GHC | version >= Version [6,3] [] ->
                        getInstalledPackagesAux comp cfg >>= return . Just
                      JHC ->
                        getInstalledPackagesJHC comp cfg >>= return . Just
                      _ -> 
                        return Nothing  
                        -- return $ map setDepByVersion (buildDepends pkg_descr)
                        
	setupMessage (configVerbose cfg) "Configuring" 
                     (either packageDescription id pkg_descr0)
        
        (pkg_descr, flags) <- case pkg_descr0 of
            Left ppd -> 
                case finalizePackageDescription 
                       (configConfigurationsFlags cfg)
                       mipkgs
                       System.Info.os
                       System.Info.arch
                       (map toLower (show flavor),version)
                       ppd
                of Right r -> return r
                   Left missing -> 
                       die $ "At least the following dependencies are missing:\n"
                         ++ (render . nest 4 . sep . punctuate comma $ 
                             map showDependency missing)
            Right pd -> return (pd,[])
              

        when (not (null flags)) $
          message $ "Flags chosen: " ++ (concat . intersperse ", " .
                      map (\(n,b) -> n ++ "=" ++ show b) $ flags)

        (warns, ers) <- sanityCheckPackage $
                          updatePackageDescription pbi pkg_descr
        
        errorOut (configVerbose cfg) warns ers

        let ipkgs = fromMaybe (map setDepByVersion (buildDepends pkg_descr)) mipkgs 

        dep_pkgs <- case flavor of
                      GHC | version >= Version [6,3] [] -> do
	                mapM (configDependency ipkgs) (buildDepends pkg_descr)
                      JHC                           -> do
	                mapM (configDependency ipkgs) (buildDepends pkg_descr)
                      _                             -> do
                        return $ map setDepByVersion (buildDepends pkg_descr)


	removeInstalledConfig

	-- installation directories
	defPrefix <- default_prefix
	defDataDir <- default_datadir pkg_descr
        let 
		pref = fromMaybe defPrefix (configPrefix cfg)
		my_bindir = fromMaybe default_bindir 
				  (configBinDir cfg)
		my_libdir = fromMaybe (default_libdir comp)
				  (configLibDir cfg)
		my_libsubdir = fromMaybe (default_libsubdir comp)
				  (configLibSubDir cfg)
		my_libexecdir = fromMaybe default_libexecdir
				  (configLibExecDir cfg)
		my_datadir = fromMaybe defDataDir
				  (configDataDir cfg)
		my_datasubdir = fromMaybe default_datasubdir
				  (configDataSubDir cfg)

        -- check extensions
        let lib = library pkg_descr
        let extlist = nub $ maybe [] (extensions . libBuildInfo) lib ++
                      concat [ extensions exeBi | Executable _ _ exeBi <- executables pkg_descr ]
        let exts = fst $ extensionsToFlags flavor extlist
        unless (null exts) $ warn (configVerbose cfg) $ -- Just warn, FIXME: Should this be an error?
            show flavor ++ " does not support the following extensions:\n " ++
            concat (intersperse ", " (map show exts))

        foundPrograms <- lookupPrograms (configPrograms cfg)

        let newConfig = foldr (\(_, p) c -> maybeUpdateProgram p c) 
                              (configPrograms cfg) foundPrograms

        -- TODO: get version checking integrated into the Program abstraction
        newConfig'  <- findHaddockVersion  (configVerbose cfg) newConfig
        newConfig'' <- findHscolourVersion (configVerbose cfg) newConfig'

	split_objs <- 
	   if not (configSplitObjs cfg)
		then return False
		else case flavor of
			    GHC | version >= Version [6,5] [] -> return True
	    		    _ -> do warn (configVerbose cfg)
                                         ("this compiler does not support " ++
					  "--enable-split-objs; ignoring")
				    return False

	let lbi = LocalBuildInfo{prefix=pref, compiler=comp,
			      buildDir=distPref </> "build",
			      scratchDir=distPref </> "scratch",
			      bindir=my_bindir,
			      libdir=my_libdir,
			      libsubdir=my_libsubdir,
			      libexecdir=my_libexecdir,
			      datadir=my_datadir,
			      datasubdir=my_datasubdir,
                              packageDeps=dep_pkgs,
                              localPkgDescr=pkg_descr,
                              withPrograms=newConfig'',
                              withVanillaLib=configVanillaLib cfg,
                              withProfLib=configProfLib cfg,
                              withProfExe=configProfExe cfg,
                              withOptimization=configOptimization cfg,
			      withGHCiLib=configGHCiLib cfg,
			      splitObjs=split_objs,
                              userConf=configUser cfg
                             }

        -- FIXME: maybe this should only be printed when verbose?
        message $ "Using install prefix: " ++ pref

        messageDir pkg_descr lbi "Binaries" mkBinDir mkBinDirRel
        messageDir pkg_descr lbi "Libraries" mkLibDir mkLibDirRel
        messageDir pkg_descr lbi "Private binaries" mkLibexecDir mkLibexecDirRel
        messageDir pkg_descr lbi "Data files" mkDataDir mkDataDirRel
        
        message $ "Using compiler: " ++ compilerPath comp
        message $ "Compiler flavor: " ++ show flavor
        message $ "Compiler version: " ++ showVersion version
        message $ "Using package tool: " ++ compilerPkgToolPath comp

        mapM_ (uncurry reportProgram) foundPrograms

	return lbi

messageDir :: PackageDescription -> LocalBuildInfo -> String
	-> (PackageDescription -> LocalBuildInfo -> CopyDest -> FilePath)
	-> (PackageDescription -> LocalBuildInfo -> CopyDest -> Maybe FilePath)
	-> IO ()
messageDir pkg_descr lbi name mkDir
#if mingw32_HOST_OS
                                    mkDirRel
#else
                                    _
#endif
 = message (name ++ " installed in: " ++ mkDir pkg_descr lbi NoCopyDest ++ rel_note)
  where
#if mingw32_HOST_OS
    rel_note
      | not (hasLibs pkg_descr) &&
        mkDirRel pkg_descr lbi NoCopyDest == Nothing
                  = "  (fixed location)"
      | otherwise = ""
#else
    rel_note      = ""
#endif

-- |Converts build dependencies to a versioned dependency.  only sets
-- version information for exact versioned dependencies.
setDepByVersion :: Dependency -> PackageIdentifier

-- if they specify the exact version, use that:
setDepByVersion (Dependency s (ThisVersion v)) = PackageIdentifier s v

-- otherwise, just set it to empty
setDepByVersion (Dependency s _) = PackageIdentifier s (Version [] [])


reportProgram :: String -> Maybe Program -> IO ()
reportProgram _ (Just Program{ programName=name
                              , programLocation=EmptyLocation})
                  = message ("No " ++ name ++ " found")
reportProgram _ (Just Program{ programName=name
                              , programLocation=FoundOnSystem p})
                  = message ("Using " ++ name ++ " found on system at: " ++ p)
reportProgram _ (Just Program{ programName=name
                              , programLocation=UserSpecified p})
                  = message ("Using " ++ name ++ " given by user at: " ++ p)
reportProgram name Nothing = message ("No " ++ name ++ " found")

hackageUrl :: String
hackageUrl = "http://hackage.haskell.org/cgi-bin/hackage-scripts/package/"

-- | Test for a package dependency and record the version we have installed.
configDependency :: [PackageIdentifier] -> Dependency -> IO PackageIdentifier
configDependency ps dep@(Dependency pkgname vrange) =
  case satisfyDependency ps dep of
        Nothing -> die ("cannot satisfy dependency " ++
                        pkgname ++ showVersionRange vrange ++ "\n" ++
                        "Perhaps you need to download and install it from\n" ++
                        hackageUrl ++ pkgname ++ "?")
        Just pkg -> do
                message ("Dependency " ++ pkgname ++
                        showVersionRange vrange ++
                        ": using " ++ showPackageId pkg)
                return pkg

getInstalledPackagesJHC :: Compiler -> ConfigFlags -> IO [PackageIdentifier]
getInstalledPackagesJHC comp cfg = do
   let verbosity = configVerbose cfg
   when (verbosity >= normal) $ message "Reading installed packages..."
   str <- rawSystemStdout verbosity (programPath $ compilerPkgTool comp) ["--list-libraries"]
   case pCheck (readP_to_S (many (skipSpaces >> parsePackageId)) str) of
     [ps] -> return ps
     _    -> die "cannot parse package list"

getInstalledPackagesAux :: Compiler -> ConfigFlags -> IO [PackageIdentifier]
getInstalledPackagesAux comp cfg = getInstalledPackages comp (configUser cfg) (configVerbose cfg)

getInstalledPackages :: Compiler -> Bool -> Verbosity -> IO [PackageIdentifier]
getInstalledPackages comp user verbosity = do
   when (verbosity >= normal) $ message "Reading installed packages..."
   let user_flag = if user then "--user" else "--global"
   str <- rawSystemStdout verbosity (programPath $ compilerPkgTool comp) [user_flag, "list"]
   let keep_line s = ':' `notElem` s && not ("Creating" `isPrefixOf` s)
       str1 = unlines (filter keep_line (lines str))
       str2 = filter (`notElem` ",(){}") str1
       --
   case pCheck (readP_to_S (many (skipSpaces >> parsePackageId)) str2) of
     [ps] -> return ps
     _    -> die "cannot parse package list"

-- -----------------------------------------------------------------------------
-- Determining the compiler details

configCompilerAux :: ConfigFlags -> IO Compiler
configCompilerAux cfg = configCompiler (configHcFlavor cfg)
                                       (configHcPath cfg)
                                       (configHcPkg cfg)
                                       (configVerbose cfg)

configCompiler :: Maybe CompilerFlavor -> Maybe FilePath -> Maybe FilePath
               -> Verbosity -> IO Compiler
configCompiler Nothing _ _ _ = die "Unknown compiler"
configCompiler (Just hcFlavor) hcPath hcPkg verbosity
  = case hcFlavor of
      GHC  -> GHC.configure  hcPath hcPkg verbosity
      JHC  -> JHC.configure  hcPath hcPkg verbosity
      Hugs -> Hugs.configure hcPath hcPkg verbosity
      NHC  -> NHC.configure  hcPath hcPkg verbosity
      _    -> die "Unknown compiler"

findHscolourVersion :: Verbosity -> ProgramConfiguration -> IO ProgramConfiguration
findHscolourVersion verbosity conf = do
   mHsColour <- lookupProgram "hscolour" conf
   case mHsColour of
     Nothing -> return conf
     Just Program { programLocation = EmptyLocation } -> return conf
     Just prog -> do
       -- Invoking "HsColour -version" gives a string like "HsColour 1.7"
       prog' <- findProgramAndVersion verbosity (programBinName prog)
                  (Just $ programPath prog) "-version" $ \str ->
                  case words str of
                    (_:ver:_) -> ver
                    _         -> ""
       return (updateProgram prog' { programName = "hscolour" } conf)

findHaddockVersion :: Verbosity -> ProgramConfiguration -> IO ProgramConfiguration
findHaddockVersion verbosity conf = do
   mHaddock <- lookupProgram "haddock" conf
   case mHaddock of
     Nothing -> return conf
     Just Program { programLocation = EmptyLocation } -> return conf
     Just prog -> do
       -- Invoking "haddock --version" gives a string like
       -- "Haddock version 0.8, (c) Simon Marlow 2006"
       prog' <- findProgramAndVersion verbosity (programBinName prog)
                  (Just $ programPath prog) "--version" $ \str ->
                  case words str of
                    (_:_:ver:_) -> takeWhile (`elem` ('.':['0'..'9'])) ver
                    _           -> ""
       return (updateProgram prog' conf)

pCheck :: [(a, [Char])] -> [a]
pCheck rs = [ r | (r,s) <- rs, all isSpace s ]

message :: String -> IO ()
message s = putStrLn $ "configure: " ++ s


-- |Output warnings and errors. Exit if any errors.
errorOut :: Verbosity -- ^Verbosity
         -> [String]  -- ^Warnings
         -> [String]  -- ^errors
         -> IO ()
errorOut verbosity warnings errors = do
  mapM_ (warn verbosity) warnings
  when (not (null errors)) $ do
    pname <- getProgName
    mapM (hPutStrLn stderr . ((pname ++ ": Error: ") ++)) errors
    exitWith (ExitFailure 1)


-- -----------------------------------------------------------------------------
-- Tests

#ifdef DEBUG

hunitTests :: [Test]
hunitTests = []
{- Too specific:
packageID = PackageIdentifier "Foo" (Version [1] [])
    = [TestCase $
       do let simonMarGHCLoc = "/usr/bin/ghc"
          simonMarGHC <- configure emptyPackageDescription {package=packageID}
                                       (Just GHC,
				       Just simonMarGHCLoc,
				       Nothing, Nothing)
	  assertEqual "finding ghc, etc on simonMar's machine failed"
             (LocalBuildInfo "/usr" (Compiler GHC 
	                    (Version [6,2,2] []) simonMarGHCLoc 
 			    (simonMarGHCLoc ++ "-pkg")) [] [])
             simonMarGHC
      ]
-}
#endif
