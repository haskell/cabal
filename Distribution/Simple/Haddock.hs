-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Haddock
-- Copyright   :  Isaac Jones 2003-2005
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module deals with the @haddock@ and @hscolour@ commands. Sadly this is
-- a rather complicated module. It deals with two versions of haddock (0.x and
-- 2.x). It has to do pre-processing for haddock 0.x which involves
-- \'unlit\'ing and using @-DHADDOCK@ for any source code that uses @cpp@. It
-- uses information about installed packages (from @ghc-pkg@) to find the
-- locations of documentation for dependent packages, so it can create links.
--
-- The @hscolour@ support allows generating html versions of the original
-- source, with coloured syntax highlighting.

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

module Distribution.Simple.Haddock (
  haddock, hscolour
  ) where

-- local
import Distribution.Package
         ( PackageIdentifier, Package(..), packageName )
import qualified Distribution.ModuleName as ModuleName
import Distribution.PackageDescription as PD
         ( PackageDescription(..), BuildInfo(..), allExtensions
         , Library(..), hasLibs, Executable(..), Component(..) )
import Distribution.Simple.Compiler
         ( Compiler(..), compilerVersion )
import Distribution.Simple.GHC ( ghcLibDir )
import Distribution.Simple.Program
         ( ConfiguredProgram(..), requireProgramVersion
         , rawSystemProgram, rawSystemProgramStdout
         , hscolourProgram, haddockProgram )
import Distribution.Simple.PreProcess (ppCpp', ppUnlit
                                      , PPSuffixHandler, runSimplePreProcessor
                                      , preprocessComponent)
import Distribution.Simple.Setup
        ( defaultHscolourFlags, Flag(..), flagToMaybe, fromFlag
        , HaddockFlags(..), HscolourFlags(..) )
import Distribution.Simple.Build (initialBuildSteps)
import Distribution.Simple.InstallDirs (InstallDirs(..), PathTemplate,
                                        PathTemplateVariable(..),
                                        toPathTemplate, fromPathTemplate,
                                        substPathTemplate,
                                        initialPathTemplateEnv)
import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(..), externalPackageDeps
         , ComponentLocalBuildInfo(..), withComponentsLBI )
import Distribution.Simple.BuildPaths ( haddockName,
                                        hscolourPref, autogenModulesDir,
                                        )
import Distribution.Simple.PackageIndex (dependencyClosure)
import qualified Distribution.Simple.PackageIndex as PackageIndex
import qualified Distribution.InstalledPackageInfo as InstalledPackageInfo
         ( InstalledPackageInfo_(..) )
import Distribution.InstalledPackageInfo
         ( InstalledPackageInfo )
import Distribution.Simple.Utils
         ( die, warn, notice, intercalate, setupMessage
         , createDirectoryIfMissingVerbose, withTempFile, copyFileVerbose
         , withTempDirectory
         , findFileWithExtension, findFile )
import Distribution.Simple.GHC (ghcOptions)
import Distribution.Text
         ( display, simpleParse )

import Distribution.Verbosity
import Language.Haskell.Extension
-- Base
import System.Directory(removeFile, doesFileExist, createDirectoryIfMissing)

import Control.Monad ( when, guard )
import Control.Exception (assert)
import Data.Monoid
import Data.Maybe    ( fromMaybe, listToMaybe )

import System.FilePath((</>), (<.>), splitFileName, splitExtension,
                       normalise, splitPath, joinPath)
import System.IO (hClose, hPutStrLn)
import Distribution.Version

-- Types

-- | record that represents the arguments to the haddock executable, a product monoid.
data HaddockArgs = HaddockArgs {
 argInterfaceFile :: Flag FilePath,               -- ^ path of the interface file, relative to argOutputDir, required.
 argPackageName :: Flag PackageIdentifier,        -- ^ package name,                                         required.
 argHideModules :: (All,[ModuleName.ModuleName]), -- ^ (hide modules ?, modules to hide)
 argIgnoreExports :: Any,                         -- ^ ingore export lists in modules?
 argLinkSource :: Flag (Template,Template),       -- ^ (template for modules, template for symbols)
 argCssFile :: Flag FilePath,                     -- ^ optinal custom css file.
 argVerbose :: Any,
 argOutput :: Flag [Output],                      -- ^ Html or Hoogle doc or both?                                   required.
 argInterfaces :: [(FilePath, Maybe FilePath)],   -- ^ [(interface file, path to the html docs for links)]
 argOutputDir :: Directory,                       -- ^ where to generate the documentation.
 argTitle :: Flag String,                         -- ^ page's title,                                         required.
 argPrologue :: Flag String,                      -- ^ prologue text,                                        required.
 argGhcFlags :: [String],                         -- ^ additional flags to pass to ghc for haddock-2
 argGhcLibDir :: Flag FilePath,                   -- ^ to find the correct ghc,                              required by haddock-2.
 argTargets :: [FilePath]                         -- ^ modules to process.
}

-- | the FilePath of a directory, it's a monoid under (</>)
newtype Directory = Dir { unDir' :: FilePath } deriving (Read,Show,Eq,Ord)

unDir :: Directory -> FilePath
unDir = joinPath . filter (\p -> p /="./" && p /= ".") . splitPath . unDir'

type Template = String

data Output = Html | Hoogle

-- --------------------------------------------------------------------------
-- Haddock support

haddock :: PackageDescription -> LocalBuildInfo -> [PPSuffixHandler] -> HaddockFlags -> IO ()
haddock pkg_descr _ _ haddockFlags
  |    not (hasLibs pkg_descr)
    && not (fromFlag $ haddockExecutables haddockFlags) =
      warn (fromFlag $ haddockVerbosity haddockFlags) $
           "No documentation was generated as this package does not contain "
        ++ "a library. Perhaps you want to use the --executables flag."

haddock pkg_descr lbi suffixes flags = do

    setupMessage verbosity "Running Haddock for" (packageId pkg_descr)
    (confHaddock, version, _) <-
      requireProgramVersion verbosity haddockProgram
        (orLaterVersion (Version [0,6] [])) (withPrograms lbi)

    -- various sanity checks
    let isVersion2   = version >= Version [2,0] []

    when ( flag haddockHoogle
           && version > Version [2] []
           && version < Version [2,2] []) $
         die "haddock 2.0 and 2.1 do not support the --hoogle flag."

    when (flag haddockHscolour && version < Version [0,8] []) $
         die "haddock --hyperlink-source requires Haddock version 0.8 or later"

    when isVersion2 $ do
      haddockGhcVersionStr <- rawSystemProgramStdout verbosity confHaddock
                                ["--ghc-version"]
      case simpleParse haddockGhcVersionStr of
        Nothing -> die "Could not get GHC version from Haddock"
        Just haddockGhcVersion
          | haddockGhcVersion == ghcVersion -> return ()
          | otherwise -> die $
                 "Haddock's internal GHC version must match the configured "
              ++ "GHC version.\n"
              ++ "The GHC version is " ++ display ghcVersion ++ " but "
              ++ "haddock is using GHC version " ++ display haddockGhcVersion
          where ghcVersion = compilerVersion (compiler lbi)

    -- the tools match the requests, we can proceed

    initialBuildSteps (flag haddockDistPref) pkg_descr lbi verbosity

    when (flag haddockHscolour) $ hscolour' pkg_descr lbi suffixes $
         defaultHscolourFlags `mappend` haddockToHscolour flags

    args <- fmap mconcat . sequence $
            [ getInterfaces verbosity lbi (flagToMaybe (haddockHtmlLocation flags))
            , getGhcLibDir  verbosity lbi isVersion2 ]
           ++ map return
            [ fromFlags flags
            , fromPackageDescription pkg_descr ]

    let pre c = preprocessComponent pkg_descr c lbi False verbosity suffixes
    withComponentsLBI lbi $ \comp clbi -> do
      pre comp
      case comp of
        CLib lib -> do
          withTempDirectory verbosity (buildDir lbi) "tmp" $ \tmp -> do
            let bi = libBuildInfo lib
            libArgs  <- fromLibrary tmp lbi lib clbi
            libArgs' <- prepareSources verbosity tmp
                          lbi isVersion2 bi (args `mappend` libArgs)
            runHaddock verbosity confHaddock libArgs'
        CExe exe -> when (flag haddockExecutables) $ do
          withTempDirectory verbosity (buildDir lbi) "tmp" $ \tmp -> do
            let bi = buildInfo exe
            exeArgs  <- fromExecutable tmp lbi exe clbi
            exeArgs' <- prepareSources verbosity tmp
                          lbi isVersion2 bi (args `mappend` exeArgs)
            runHaddock verbosity confHaddock exeArgs'
        _ -> return ()
  where
    verbosity = flag haddockVerbosity
    flag f    = fromFlag $ f flags

-- | performs cpp and unlit preprocessing where needed on the files in
-- | argTargets, which must have an .hs or .lhs extension.
prepareSources :: Verbosity
                  -> FilePath
                  -> LocalBuildInfo
                  -> Bool            -- haddock == 2.*
                  -> BuildInfo
                  -> HaddockArgs
                  -> IO HaddockArgs
prepareSources verbosity tmp lbi isVersion2 bi args@HaddockArgs{argTargets=files} =
              mapM (mockPP tmp) files >>= \targets -> return args {argTargets=targets}
          where
            mockPP pref file = do
                 let (filePref, fileName) = splitFileName file
                     targetDir  = pref </> filePref
                     targetFile = targetDir </> fileName
                     (targetFileNoext, targetFileExt) = splitExtension $ targetFile
                     hsFile = targetFileNoext <.> "hs"

                 assert (targetFileExt `elem` [".lhs",".hs"]) $ return ()

                 createDirectoryIfMissing True targetDir

                 if needsCpp
                    then do
                      runSimplePreProcessor (ppCpp' defines bi lbi)
                                            file targetFile verbosity
                    else
                      copyFileVerbose verbosity file targetFile

                 when (targetFileExt == ".lhs") $ do
                     runSimplePreProcessor ppUnlit targetFile hsFile verbosity
                     removeFile targetFile

                 return hsFile
            needsCpp = EnableExtension CPP `elem` allExtensions bi
            defines | isVersion2 = []
                    | otherwise  = ["-D__HADDOCK__"]

--------------------------------------------------------------------------------------------------
-- constributions to HaddockArgs

fromFlags :: HaddockFlags -> HaddockArgs
fromFlags flags =
    mempty {
      argHideModules = (maybe mempty (All . not) $ flagToMaybe (haddockInternal flags), mempty),
      argLinkSource = if fromFlag (haddockHscolour flags)
                               then Flag ("src/%{MODULE/./-}.html"
                                         ,"src/%{MODULE/./-}.html#%{NAME}")
                               else NoFlag,
      argCssFile = haddockCss flags,
      argVerbose = maybe mempty (Any . (>= deafening)) . flagToMaybe $ haddockVerbosity flags,
      argOutput = 
          Flag $ case [ Html | Flag True <- [haddockHtml flags] ] ++
                      [ Hoogle | Flag True <- [haddockHoogle flags] ]
                 of [] -> [ Html ]
                    os -> os,
      argOutputDir = maybe mempty Dir . flagToMaybe $ haddockDistPref flags
    }

fromPackageDescription :: PackageDescription -> HaddockArgs
fromPackageDescription pkg_descr =
      mempty {
                argInterfaceFile = Flag $ haddockName pkg_descr,
                argPackageName = Flag $ packageId $ pkg_descr,
                argOutputDir = Dir $ "doc" </> "html" </> display (packageName pkg_descr),
                argPrologue = Flag $ if null desc then synopsis pkg_descr else desc,
                argTitle = Flag $ showPkg ++ subtitle
             }
      where
        desc = PD.description pkg_descr
        showPkg = display (packageId pkg_descr)
        subtitle | null (synopsis pkg_descr) = ""
                 | otherwise                 = ": " ++ synopsis pkg_descr

fromLibrary :: FilePath
            -> LocalBuildInfo -> Library -> ComponentLocalBuildInfo
            -> IO HaddockArgs
fromLibrary tmp lbi lib clbi =
            do inFiles <- map snd `fmap` getLibSourceFiles lbi lib
               return $ mempty {
                            argHideModules = (mempty,otherModules $ bi),
                            argGhcFlags = ghcOptions lbi bi clbi (buildDir lbi)
                                       -- Noooooooooo!!!!!111
                                       -- haddock stomps on our precious .hi
                                       -- and .o files. Workaround by telling
                                       -- haddock to write them elsewhere.
                                       ++ [ "-odir", tmp, "-hidir", tmp
                                          , "-stubdir", tmp ],
                            argTargets = inFiles
                          }
    where
      bi = libBuildInfo lib

fromExecutable :: FilePath
               -> LocalBuildInfo -> Executable -> ComponentLocalBuildInfo
               -> IO HaddockArgs
fromExecutable tmp lbi exe clbi =
            do inFiles <- map snd `fmap` getExeSourceFiles lbi exe
               return $ mempty {
                            argGhcFlags = ghcOptions lbi bi clbi (buildDir lbi)
                                       -- Noooooooooo!!!!!111
                                       -- haddock stomps on our precious .hi
                                       -- and .o files. Workaround by telling
                                       -- haddock to write them elsewhere.
                                       ++ [ "-odir", tmp, "-hidir", tmp
                                          , "-stubdir", tmp ],
                            argOutputDir = Dir (exeName exe),
                            argTitle = Flag (exeName exe),
                            argTargets = inFiles
                          }
    where
      bi = buildInfo exe

getInterfaces :: Verbosity
                 -> LocalBuildInfo
                 -> Maybe String -- ^ template for html location
                 -> IO HaddockArgs
getInterfaces verbosity lbi location = do
    let htmlTemplate = fmap toPathTemplate $ location
    (packageFlags, warnings) <- haddockPackageFlags lbi htmlTemplate
    maybe (return ()) (warn verbosity) warnings
    return $ mempty {
                 argInterfaces = packageFlags
               }

getGhcLibDir :: Verbosity -> LocalBuildInfo
             -> Bool -- ^ are we using haddock-2.x ?
             -> IO HaddockArgs
getGhcLibDir verbosity lbi isVersion2
    | isVersion2 =
        do l <- ghcLibDir verbosity lbi
           return $ mempty { argGhcLibDir = Flag l }
    | otherwise  =
        return mempty

----------------------------------------------------------------------------------------------

-- | Call haddock with the specified arguments.
runHaddock :: Verbosity -> ConfiguredProgram -> HaddockArgs -> IO ()
runHaddock verbosity confHaddock args = do
  let haddockVersion = fromMaybe (error "unable to determine haddock version")
                       (programVersion confHaddock)
  renderArgs verbosity haddockVersion args $ \(flags,result)-> do

      rawSystemProgram verbosity confHaddock flags

      notice verbosity $ "Documentation created: " ++ result


renderArgs :: Verbosity
              -> Version
              -> HaddockArgs
              -> (([[Char]], FilePath) -> IO a)
              -> IO a
renderArgs verbosity version args k = do
  createDirectoryIfMissingVerbose verbosity True outputDir
  withTempFile outputDir "haddock-prolog.txt" $ \prologFileName h -> do
          do
             hPutStrLn h $ fromFlag $ argPrologue args
             hClose h
             let pflag = (:[]).("--prologue="++) $ prologFileName
             k $ (pflag ++ renderPureArgs version args, result)
    where
      isVersion2 = version >= Version [2,0] []
      outputDir = (unDir $ argOutputDir args)
      result = intercalate ", "
             . map (\o -> outputDir </>
                            case o of
                              Html -> "index.html"
                              Hoogle -> pkgstr <.> "txt")
             $ arg argOutput
            where
              pkgstr | isVersion2 = display $ packageName pkgid
                     | otherwise = display pkgid
              pkgid = arg argPackageName
      arg f = fromFlag $ f args

renderPureArgs :: Version -> HaddockArgs -> [[Char]]
renderPureArgs version args = concat
    [
     (:[]) . (\f -> "--dump-interface="++ unDir (argOutputDir args) </> f)
     . fromFlag . argInterfaceFile $ args,
     (\pkgName -> if isVersion2
                  then ["--optghc=-package-name", "--optghc=" ++ pkgName]
                  else ["--package=" ++ pkgName]) . display . fromFlag . argPackageName $ args,
     (\(All b,xs) -> bool (map (("--hide=" ++). display) xs) [] b) . argHideModules $ args,
     bool ["--ignore-all-exports"] [] . getAny . argIgnoreExports $ args,
     maybe [] (\(m,e) -> ["--source-module=" ++ m
                         ,"--source-entity=" ++ e]) . flagToMaybe . argLinkSource $ args,
     maybe [] ((:[]).("--css="++)) . flagToMaybe . argCssFile $ args,
     bool [] [verbosityFlag] . getAny . argVerbose $ args,
     map (\o -> case o of Hoogle -> "--hoogle"; Html -> "--html") . fromFlag . argOutput $ args,
     renderInterfaces . argInterfaces $ args,
     (:[]).("--odir="++) . unDir . argOutputDir $ args,
     (:[]).("--title="++) . (bool (++" (internal documentation)") id (getAny $ argIgnoreExports args))
              . fromFlag . argTitle $ args,
     bool id (const []) isVersion2 . map ("--optghc=" ++) . argGhcFlags $ args,
     maybe [] (\l -> ["-B"++l]) $ guard isVersion2 >> flagToMaybe (argGhcLibDir args), -- error if isVersion2 and Nothing?
     argTargets $ args
    ]
    where
      renderInterfaces = map (\(i,mh) -> "--read-interface=" ++ maybe "" (++",") mh ++ i)
      bool a b c = if c then a else b
      isVersion2 = version >= Version [2,0] []
      isVersion2_5 = version >= Version [2,5] []
      verbosityFlag
       | isVersion2_5 = "--verbosity=1"
       | otherwise = "--verbose"

-----------------------------------------------------------------------------------------------------------

haddockPackageFlags :: LocalBuildInfo
                    -> Maybe PathTemplate
                    -> IO ([(FilePath,Maybe FilePath)], Maybe String)
haddockPackageFlags lbi htmlTemplate = do
  let allPkgs = installedPkgs lbi
      directDeps = map fst (externalPackageDeps lbi)
  transitiveDeps <- case dependencyClosure allPkgs directDeps of
                    Left x -> return x
                    Right _ -> die "Can't find transitive deps for haddock"
  interfaces <- sequence
    [ case interfaceAndHtmlPath ipkg of
        Nothing -> return (Left (packageId ipkg))
        Just (interface, html) -> do
          exists <- doesFileExist interface
          if exists
            then return (Right (interface, html))
            else return (Left (packageId ipkg))
    | ipkg <- PackageIndex.allPackages transitiveDeps ]

  let missing = [ pkgid | Left pkgid <- interfaces ]
      warning = "The documentation for the following packages are not "
             ++ "installed. No links will be generated to these packages: "
             ++ intercalate ", " (map display missing)
      flags = [ (interface, if null html then Nothing else Just html)
              | Right (interface, html) <- interfaces ]

  return (flags, if null missing then Nothing else Just warning)

  where
    interfaceAndHtmlPath :: InstalledPackageInfo -> Maybe (FilePath, FilePath)
    interfaceAndHtmlPath pkg = do
      interface <- listToMaybe (InstalledPackageInfo.haddockInterfaces pkg)
      html <- case htmlTemplate of
        Nothing -> listToMaybe (InstalledPackageInfo.haddockHTMLs pkg)
        Just htmlPathTemplate -> Just (expandTemplateVars htmlPathTemplate)
      return (interface, html)

      where expandTemplateVars = fromPathTemplate . substPathTemplate env
            env = (PrefixVar, prefix (installDirTemplates lbi))
                : initialPathTemplateEnv (packageId pkg) (compilerId (compiler lbi))

-- --------------------------------------------------------------------------
-- hscolour support

hscolour :: PackageDescription -> LocalBuildInfo -> [PPSuffixHandler] -> HscolourFlags -> IO ()
hscolour pkg_descr lbi suffixes flags = do
  -- we preprocess even if hscolour won't be found on the machine
  -- will this upset someone?
  initialBuildSteps distPref pkg_descr lbi verbosity
  hscolour' pkg_descr lbi suffixes flags
 where
   verbosity  = fromFlag (hscolourVerbosity flags)
   distPref = fromFlag $ hscolourDistPref flags

hscolour' :: PackageDescription
          -> LocalBuildInfo
          -> [PPSuffixHandler]
          -> HscolourFlags
          -> IO ()
hscolour' pkg_descr lbi suffixes flags = do
    let distPref = fromFlag $ hscolourDistPref flags
    (hscolourProg, _, _) <-
      requireProgramVersion
        verbosity hscolourProgram
        (orLaterVersion (Version [1,8] [])) (withPrograms lbi)

    setupMessage verbosity "Running hscolour for" (packageId pkg_descr)
    createDirectoryIfMissingVerbose verbosity True $ hscolourPref distPref pkg_descr

    let pre c = preprocessComponent pkg_descr c lbi False verbosity suffixes
    withComponentsLBI lbi $ \comp _ -> do
      pre comp
      case comp of
        CLib lib -> do
          let outputDir = hscolourPref distPref pkg_descr </> "src"
          runHsColour hscolourProg outputDir =<< getLibSourceFiles lbi lib
        CExe exe | fromFlag (hscolourExecutables flags) -> do
          let outputDir = hscolourPref distPref pkg_descr </> exeName exe </> "src"
          runHsColour hscolourProg outputDir =<< getExeSourceFiles lbi exe
        _ -> return ()
  where
    stylesheet = flagToMaybe (hscolourCSS flags)

    verbosity  = fromFlag (hscolourVerbosity flags)

    runHsColour prog outputDir moduleFiles = do
         createDirectoryIfMissingVerbose verbosity True outputDir

         case stylesheet of -- copy the CSS file
           Nothing | programVersion prog >= Just (Version [1,9] []) ->
                       rawSystemProgram verbosity prog
                          ["-print-css", "-o" ++ outputDir </> "hscolour.css"]
                   | otherwise -> return ()
           Just s -> copyFileVerbose verbosity s (outputDir </> "hscolour.css")

         flip mapM_ moduleFiles $ \(m, inFile) ->
             rawSystemProgram verbosity prog
                    ["-css", "-anchor", "-o" ++ outFile m, inFile]
        where
          outFile m = outputDir </> intercalate "-" (ModuleName.components m) <.> "html"

haddockToHscolour :: HaddockFlags -> HscolourFlags
haddockToHscolour flags =
    HscolourFlags {
      hscolourCSS         = haddockHscolourCss flags,
      hscolourExecutables = haddockExecutables flags,
      hscolourVerbosity   = haddockVerbosity   flags,
      hscolourDistPref    = haddockDistPref    flags
    }
----------------------------------------------------------------------------------------------
-- TODO these should be moved elsewhere.

getLibSourceFiles :: LocalBuildInfo
                     -> Library
                     -> IO [(ModuleName.ModuleName, FilePath)]
getLibSourceFiles lbi lib = getSourceFiles searchpaths modules
  where
    bi               = libBuildInfo lib
    modules          = PD.exposedModules lib ++ otherModules bi
    searchpaths      = autogenModulesDir lbi : buildDir lbi : hsSourceDirs bi

getExeSourceFiles :: LocalBuildInfo
                     -> Executable
                     -> IO [(ModuleName.ModuleName, FilePath)]
getExeSourceFiles lbi exe = do
    moduleFiles <- getSourceFiles searchpaths modules
    srcMainPath <- findFile (hsSourceDirs bi) (modulePath exe)
    return ((ModuleName.main, srcMainPath) : moduleFiles)
  where
    bi          = buildInfo exe
    modules     = otherModules bi
    searchpaths = autogenModulesDir lbi : exeBuildDir lbi exe : hsSourceDirs bi

getSourceFiles :: [FilePath]
                  -> [ModuleName.ModuleName]
                  -> IO [(ModuleName.ModuleName, FilePath)]
getSourceFiles dirs modules = flip mapM modules $ \m -> fmap ((,) m) $
    findFileWithExtension ["hs", "lhs"] dirs (ModuleName.toFilePath m)
      >>= maybe (notFound m) (return . normalise)
  where
    notFound module_ = die $ "can't find source for module " ++ display module_

-- | The directory where we put build results for an executable
exeBuildDir :: LocalBuildInfo -> Executable -> FilePath
exeBuildDir lbi exe = buildDir lbi </> exeName exe </> exeName exe ++ "-tmp"

---------------------------------------------------------------------------------------------




-- boilerplate monoid instance.
instance Monoid HaddockArgs where
    mempty = HaddockArgs {
                argInterfaceFile = mempty,
                argPackageName = mempty,
                argHideModules = mempty,
                argIgnoreExports = mempty,
                argLinkSource = mempty,
                argCssFile = mempty,
                argVerbose = mempty,
                argOutput = mempty,
                argInterfaces = mempty,
                argOutputDir = mempty,
                argTitle = mempty,
                argPrologue = mempty,
                argGhcFlags = mempty,
                argGhcLibDir = mempty,
                argTargets = mempty
             }
    mappend a b = HaddockArgs {
                argInterfaceFile = mult argInterfaceFile,
                argPackageName = mult argPackageName,
                argHideModules = mult argHideModules,
                argIgnoreExports = mult argIgnoreExports,
                argLinkSource = mult argLinkSource,
                argCssFile = mult argCssFile,
                argVerbose = mult argVerbose,
                argOutput = mult argOutput,
                argInterfaces = mult argInterfaces,
                argOutputDir = mult argOutputDir,
                argTitle = mult argTitle,
                argPrologue = mult argPrologue,
                argGhcFlags = mult argGhcFlags,
                argGhcLibDir = mult argGhcLibDir,
                argTargets = mult argTargets
             }
      where mult f = f a `mappend` f b

instance Monoid Directory where
    mempty = Dir "."
    mappend (Dir m) (Dir n) = Dir $ m </> n
