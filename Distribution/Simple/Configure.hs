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
                                      writeConfiguredPkgDescr,
                                      configuredPkgDescrFile,
                                      getConfiguredPkgDescr,
                                      localBuildInfoFile,
                                      findProgram,
                                      getInstalledPackages,
				      configDependency,
                                      configCompiler, configCompilerAux,
                                      hscolourVersion,
                                      haddockVersion,
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
			     compilerBinaryName, extensionsToFlags)
import Distribution.Package (PackageIdentifier(..), showPackageId, 
			     parsePackageId)
import Distribution.PackageDescription(
 	PackageDescription(..), Library(..),
        PreparedPackageDescription(..),
        finalizePackageDescription,
        HookedBuildInfo, sanityCheckPackage, updatePackageDescription,
	BuildInfo(..), Executable(..), setupMessage,
        satisfyDependency)
import Distribution.Simple.Utils (die, warn, rawSystemStdout, exeExtension)
import Distribution.Version (Version(..), Dependency(..), VersionRange(ThisVersion),
			     parseVersion, showVersion, showVersionRange)
--import Distribution.Configuration ( mkOSName, mkArchName )
import Distribution.Verbosity
import Distribution.ParseUtils ( showDependency )

import Text.PrettyPrint.HughesPJ ( comma, punctuate, render, nest, sep )

import Data.List (intersperse, nub, isPrefixOf)
import Data.Char (isSpace)
import Data.Maybe(fromMaybe)
import System.Directory
import System.Environment ( getProgName )
import System.IO        ( hPutStrLn, stderr )
import System.FilePath (takeDirectory, (</>), (<.>))
import Distribution.Program(Program(..), ProgramLocation(..),
                            lookupProgram, lookupPrograms, maybeUpdateProgram)
import System.Exit(ExitCode(..), exitWith)
--import System.Exit		( ExitCode(..) )
import qualified System.Info    ( os, arch )
import Control.Monad		( when, unless )
import Distribution.Compat.ReadP
import Distribution.Compat.Directory (findExecutable, createDirectoryIfMissing)
import Prelude hiding (catch)

#ifdef mingw32_HOST_OS
import Distribution.PackageDescription (hasLibs)
#endif

#ifdef DEBUG
import HUnit
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


configuredPkgDescrFile :: FilePath
configuredPkgDescrFile = distPref </> "configured_cabal"


writeConfiguredPkgDescr :: PackageDescription -> IO ()
writeConfiguredPkgDescr pd = do
  writeFile configuredPkgDescrFile (show pd)

tryGetConfiguredPkgDescr :: IO (Either String PackageDescription)
tryGetConfiguredPkgDescr = tryGetConfigStateFile configuredPkgDescrFile
  
getConfiguredPkgDescr :: IO PackageDescription
getConfiguredPkgDescr = tryGetConfiguredPkgDescr >>= either die return
  

-- -----------------------------------------------------------------------------
-- * Configuration
-- -----------------------------------------------------------------------------

-- |Perform the \"@.\/setup configure@\" action.
-- Returns the @.setup-config@ file.
configure :: ( Either PreparedPackageDescription PackageDescription
             , HookedBuildInfo) 
          -> ConfigFlags -> IO (LocalBuildInfo, PackageDescription)
configure (pkg_descr0, pbi) cfg
  = do
	-- detect compiler
	comp@(Compiler f' ver p' pkg) <- configCompilerAux cfg

        -- FIXME: currently only GHC has hc-pkg
        mipkgs <- case f' of
                      GHC | ver >= Version [6,3] [] ->
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

        dep_pkgs <- case f' of
                      GHC | ver >= Version [6,3] [] -> do
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
        let exts = fst $ extensionsToFlags f' extlist
        unless (null exts) $ warn (configVerbose cfg) $ -- Just warn, FIXME: Should this be an error?
            show f' ++ " does not support the following extensions:\n " ++
            concat (intersperse ", " (map show exts))

        foundPrograms <- lookupPrograms (configPrograms cfg)

        happy     <- findProgram "happy"     (configHappy cfg)
        alex      <- findProgram "alex"      (configAlex cfg)
        hsc2hs    <- findProgram "hsc2hs"    (configHsc2hs cfg)
        c2hs      <- findProgram "c2hs"      (configC2hs cfg)
        cpphs     <- findProgram "cpphs"     (configCpphs cfg)
        greencard <- findProgram "greencard" (configGreencard cfg)

        let newConfig = foldr (\(_, p) c -> maybeUpdateProgram p c) 
                              (configPrograms cfg) foundPrograms

	split_objs <- 
	   if not (configSplitObjs cfg)
		then return False
		else case f' of
			    GHC | ver >= Version [6,5] [] -> return True
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
                              withPrograms=newConfig,
                              withHappy=happy, withAlex=alex,
                              withHsc2hs=hsc2hs, withC2hs=c2hs,
                              withCpphs=cpphs,
                              withGreencard=greencard,
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
        
        message $ "Using compiler: " ++ p'
        message $ "Compiler flavor: " ++ (show f')
        message $ "Compiler version: " ++ showVersion ver
        message $ "Using package tool: " ++ pkg

        mapM (\(s,p) -> reportProgram' s p) foundPrograms

        reportProgram "happy"     happy
        reportProgram "alex"      alex
        reportProgram "hsc2hs"    hsc2hs
        reportProgram "c2hs"      c2hs
        reportProgram "cpphs"     cpphs
        reportProgram "greencard" greencard

	return (lbi, pkg_descr)

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


-- |Return the explicit path if given, otherwise look for the program
-- name in the path.
findProgram
    :: String              -- ^ program name
    -> Maybe FilePath      -- ^ optional explicit path
    -> IO (Maybe FilePath)
findProgram name Nothing = findExecutable name
findProgram _ p = return p

reportProgram :: String -> Maybe FilePath -> IO ()
reportProgram name Nothing = message ("No " ++ name ++ " found")
reportProgram name (Just p) = message ("Using " ++ name ++ ": " ++ p)

reportProgram' :: String -> Maybe Program -> IO ()
reportProgram' _ (Just Program{ programName=name
                              , programLocation=EmptyLocation})
                  = message ("No " ++ name ++ " found")
reportProgram' _ (Just Program{ programName=name
                              , programLocation=FoundOnSystem p})
                  = message ("Using " ++ name ++ " found on system at: " ++ p)
reportProgram' _ (Just Program{ programName=name
                              , programLocation=UserSpecified p})
                  = message ("Using " ++ name ++ " given by user at: " ++ p)
reportProgram' name Nothing = message ("No " ++ name ++ " found")

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
   str <- rawSystemStdout verbosity (compilerPkgTool comp) ["--list-libraries"]
   case pCheck (readP_to_S (many (skipSpaces >> parsePackageId)) str) of
     [ps] -> return ps
     _    -> die "cannot parse package list"

getInstalledPackagesAux :: Compiler -> ConfigFlags -> IO [PackageIdentifier]
getInstalledPackagesAux comp cfg = getInstalledPackages comp (configUser cfg) (configVerbose cfg)

getInstalledPackages :: Compiler -> Bool -> Verbosity -> IO [PackageIdentifier]
getInstalledPackages comp user verbosity = do
   when (verbosity >= normal) $ message "Reading installed packages..."
   let user_flag = if user then "--user" else "--global"
   str <- rawSystemStdout verbosity (compilerPkgTool comp) [user_flag, "list"]
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
configCompiler hcFlavor hcPath hcPkg verbosity
  = do let flavor = case hcFlavor of
                      Just f  -> f
                      Nothing -> error "Unknown compiler"
       comp <- 
	 case hcPath of
	   Just path -> do absolute <- doesFileExist path
                           if absolute
                             then return path
                             else findCompiler verbosity path
	   Nothing   -> findCompiler verbosity (compilerBinaryName flavor)

       ver <- configCompilerVersion flavor comp verbosity

       pkgtool <-
	 case hcPkg of
	   Just path -> return path
	   Nothing   -> guessPkgToolFromHCPath verbosity flavor comp

       return (Compiler{compilerFlavor=flavor,
			compilerVersion=ver,
			compilerPath=comp,
			compilerPkgTool=pkgtool})

findCompiler :: Verbosity -> String -> IO FilePath
findCompiler verbosity prog = do
  when (verbosity >= verbose) $ message $ "searching for " ++ prog ++ " in path."
  res <- findExecutable prog
  case res of
   Nothing   -> die ("Cannot find compiler for " ++ prog)
   Just path -> do when (verbosity >= verbose) $ message ("found " ++ prog ++ " at "++ path)
                   return path
   -- ToDo: check that compiler works?

compilerPkgToolName :: CompilerFlavor -> String
compilerPkgToolName GHC  = "ghc-pkg"
compilerPkgToolName NHC  = "hmake" -- FIX: nhc98-pkg Does not yet exist
compilerPkgToolName Hugs = "hugs"
compilerPkgToolName JHC  = "jhc"
compilerPkgToolName cmp  = error $ "Unsupported compiler: " ++ (show cmp)

configCompilerVersion :: CompilerFlavor -> FilePath -> Verbosity -> IO Version
configCompilerVersion GHC compilerP verbosity = do
  str <- rawSystemStdout verbosity compilerP ["--numeric-version"]
  case pCheck (readP_to_S parseVersion str) of
    [v] -> return v
    _   -> die ("cannot determine version of " ++ compilerP ++ ":\n  "++ str)
configCompilerVersion comp compilerP verbosity | comp `elem` [JHC,NHC] = do
  str <- rawSystemStdout verbosity compilerP ["--version"]
  case words str of
    (_:ver:_) -> case pCheck $ readP_to_S parseVersion ver of
                   [v] -> return v
                   _   -> fail ("parsing version: "++ver++" failed.")
    _        -> fail ("reading version string: "++show str++" failed.")
configCompilerVersion _ _ _ = return Version{ versionBranch=[],versionTags=[] }

hscolourVersion :: Verbosity -> LocalBuildInfo -> IO Version
hscolourVersion verbosity lbi = fmap getVer verString
 where
   -- Invoking "HsColour -version" gives a string like "HsColour 1.7"
   verString = do hscolourProg <-
                    fmap (fromMaybe noHscolour) $
                    lookupProgram "hscolour" (withPrograms lbi)
                  rawSystemStdout verbosity
                     (progLocPath (programLocation hscolourProg)) ["-version"]
   getVer    = head . pCheck . readP_to_S parseVersion . (!! 1) . words
   noHscolour = error "hscolourVersion: cannot find hscolour"
   progLocPath EmptyLocation        = noHscolour
   progLocPath (UserSpecified path) = path
   progLocPath (FoundOnSystem path) = path

haddockVersion :: Verbosity -> LocalBuildInfo -> IO Version
haddockVersion verbosity lbi = fmap getVer verString
 where
   -- Invoking "haddock --version" gives a string like
   -- "Haddock version 0.8, (c) Simon Marlow 2006" 
   verString = do haddockProg <-
                    fmap (fromMaybe noHaddock) $
                    lookupProgram "haddock" (withPrograms lbi)
                  rawSystemStdout verbosity
                    (progLocPath (programLocation haddockProg)) ["--version"]
   getVer    = head . pCheck . readP_to_S parseVersion . init . (!! 2) . words
   noHaddock = error "haddockVersion: cannot find haddock"
   progLocPath EmptyLocation        = noHaddock
   progLocPath (UserSpecified path) = path
   progLocPath (FoundOnSystem path) = path

pCheck :: [(a, [Char])] -> [a]
pCheck rs = [ r | (r,s) <- rs, all isSpace s ]

guessPkgToolFromHCPath :: Verbosity -> CompilerFlavor -> FilePath
                       -> IO FilePath
guessPkgToolFromHCPath verbosity flavor path
  = do let pkgToolName     = compilerPkgToolName flavor
           dir             = takeDirectory path
           verSuffix       = reverse $ takeWhile (`elem ` "0123456789.-") . reverse $ path
           guessNormal     = dir </> pkgToolName <.> exeExtension
           guessVersioned  = dir </> (pkgToolName ++ verSuffix) <.> exeExtension 
           guesses | null verSuffix = [guessNormal]
                   | otherwise      = [guessVersioned, guessNormal]
       when (verbosity >= verbose) $ message $ "looking for package tool: " ++ pkgToolName ++ " near compiler in " ++ dir
       file <- doesAnyFileExist guesses
       case file of
         Nothing -> die ("Cannot find package tool: " ++ pkgToolName)
         Just pkgtool -> do when (verbosity >= verbose) $ message $ "found package tool in " ++ pkgtool
                            return pkgtool

doesAnyFileExist :: [FilePath] -> IO (Maybe FilePath)
doesAnyFileExist []           = return Nothing
doesAnyFileExist (file:files) = do exists <- doesFileExist file
                                   if exists
                                     then return $ Just file
                                     else doesAnyFileExist files

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
