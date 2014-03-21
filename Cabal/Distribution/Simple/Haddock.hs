-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Simple.Haddock
-- Copyright   :  Isaac Jones 2003-2005
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This module deals with the @haddock@ and @hscolour@ commands. Sadly this is a
-- rather complicated module. It deals with two versions of haddock (0.x and
-- 2.x). It has to do pre-processing which involves \'unlit\'ing and using
-- @-D__HADDOCK__@ for any source code that uses @cpp@. It uses information
-- about installed packages (from @ghc-pkg@) to find the locations of
-- documentation for dependent packages, so it can create links.
--
-- The @hscolour@ support allows generating html versions of the original
-- source, with coloured syntax highlighting.

module Distribution.Simple.Haddock (
  haddock, hscolour,

  haddockPackagePaths
  ) where

-- local
import Distribution.Package
         ( PackageIdentifier(..)
         , Package(..)
         , PackageName(..), packageName )
import qualified Distribution.ModuleName as ModuleName
import Distribution.PackageDescription as PD
         ( PackageDescription(..), BuildInfo(..), allExtensions
         , Library(..), hasLibs, Executable(..)
         , TestSuite(..), TestSuiteInterface(..)
         , Benchmark(..), BenchmarkInterface(..) )
import Distribution.Simple.Compiler
         ( Compiler(..), compilerVersion )
import Distribution.Simple.GHC ( componentGhcOptions, ghcLibDir )
import Distribution.Simple.Program.GHC
         ( GhcOptions(..), GhcDynLinkMode(..), renderGhcOptions )
import Distribution.Simple.Program
         ( ConfiguredProgram(..), requireProgramVersion
         , rawSystemProgram, rawSystemProgramStdout
         , hscolourProgram, haddockProgram )
import Distribution.Simple.PreProcess (ppCpp', ppUnlit
                                      , PPSuffixHandler, runSimplePreProcessor
                                      , preprocessComponent)
import Distribution.Simple.Setup
        ( defaultHscolourFlags, Flag(..), toFlag, flagToMaybe, flagToList, fromFlag
        , HaddockFlags(..), HscolourFlags(..) )
import Distribution.Simple.Build (initialBuildSteps)
import Distribution.Simple.InstallDirs (InstallDirs(..), PathTemplateEnv, PathTemplate,
                                        PathTemplateVariable(..),
                                        toPathTemplate, fromPathTemplate,
                                        substPathTemplate, initialPathTemplateEnv)
import Distribution.Simple.LocalBuildInfo
         ( LocalBuildInfo(..), Component(..), ComponentLocalBuildInfo(..)
         , withAllComponentsInBuildOrder )
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
         ( die, copyFileTo, warn, notice, intercalate, setupMessage
         , createDirectoryIfMissingVerbose
         , TempFileOptions(..), defaultTempFileOptions
         , withTempFileEx, copyFileVerbose
         , withTempDirectoryEx, matchFileGlob
         , findFileWithExtension, findFile )
import Distribution.Text
         ( display, simpleParse )

import Distribution.Verbosity
import Language.Haskell.Extension
-- Base
import System.Directory(removeFile, doesFileExist, createDirectoryIfMissing)

import Control.Monad ( when, guard, forM_ )
import Control.Exception (assert)
import Data.Either   ( rights )
import Data.Monoid
import Data.Maybe    ( fromMaybe, listToMaybe )

import System.FilePath((</>), (<.>), splitFileName, splitExtension,
                       normalise, splitPath, joinPath, isAbsolute )
import System.IO (hClose, hPutStrLn, hSetEncoding, utf8)
import Distribution.Version

-- ------------------------------------------------------------------------------
-- Types

-- | record that represents the arguments to the haddock executable, a product monoid.
data HaddockArgs = HaddockArgs {
 argInterfaceFile :: Flag FilePath,               -- ^ path of the interface file, relative to argOutputDir, required.
 argPackageName :: Flag PackageIdentifier,        -- ^ package name,                                         required.
 argHideModules :: (All,[ModuleName.ModuleName]), -- ^ (hide modules ?, modules to hide)
 argIgnoreExports :: Any,                         -- ^ ingore export lists in modules?
 argLinkSource :: Flag (Template,Template,Template), -- ^ (template for modules, template for symbols, template for lines)
 argCssFile :: Flag FilePath,                     -- ^ optinal custom css file.
 argContents :: Flag String,                      -- ^ optional url to contents page
 argVerbose :: Any,
 argOutput :: Flag [Output],                      -- ^ Html or Hoogle doc or both?                                   required.
 argInterfaces :: [(FilePath, Maybe String)],     -- ^ [(interface file, URL to the html docs for links)]
 argOutputDir :: Directory,                       -- ^ where to generate the documentation.
 argTitle :: Flag String,                         -- ^ page's title,                                         required.
 argPrologue :: Flag String,                      -- ^ prologue text,                                        required.
 argGhcOptions :: Flag (GhcOptions, Version),     -- ^ additional flags to pass to ghc for haddock-2
 argGhcLibDir :: Flag FilePath,                   -- ^ to find the correct ghc,                              required by haddock-2.
 argTargets :: [FilePath]                         -- ^ modules to process.
}

-- | the FilePath of a directory, it's a monoid under (</>)
newtype Directory = Dir { unDir' :: FilePath } deriving (Read,Show,Eq,Ord)

unDir :: Directory -> FilePath
unDir = joinPath . filter (\p -> p /="./" && p /= ".") . splitPath . unDir'

type Template = String

data Output = Html | Hoogle

-- ------------------------------------------------------------------------------
-- Haddock support

haddock :: PackageDescription -> LocalBuildInfo -> [PPSuffixHandler] -> HaddockFlags -> IO ()
haddock pkg_descr _ _ haddockFlags
  |    not (hasLibs pkg_descr)
    && not (fromFlag $ haddockExecutables haddockFlags)
    && not (fromFlag $ haddockTestSuites  haddockFlags)
    && not (fromFlag $ haddockBenchmarks  haddockFlags) =
      warn (fromFlag $ haddockVerbosity haddockFlags) $
           "No documentation was generated as this package does not contain "
        ++ "a library. Perhaps you want to use the --executables, --tests or"
        ++ " --benchmarks flags."

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
          where ghcVersion = compilerVersion comp

    -- the tools match the requests, we can proceed

    initialBuildSteps (flag haddockDistPref) pkg_descr lbi verbosity

    when (flag haddockHscolour) $ hscolour' pkg_descr lbi suffixes $
         defaultHscolourFlags `mappend` haddockToHscolour flags

    libdirArgs <- getGhcLibDir  verbosity lbi isVersion2
    let commonArgs = mconcat
            [ libdirArgs
            , fromFlags (haddockTemplateEnv lbi (packageId pkg_descr)) flags
            , fromPackageDescription pkg_descr ]

    let pre c = preprocessComponent pkg_descr c lbi False verbosity suffixes
    withAllComponentsInBuildOrder pkg_descr lbi $ \component clbi -> do
      pre component
      let
        doExe com = case (compToExe com) of
          Just exe -> do
            withTempDirectoryEx verbosity tmpFileOpts (buildDir lbi) "tmp" $ \tmp -> do
              let bi = buildInfo exe
              exeArgs  <- fromExecutable verbosity tmp lbi exe clbi htmlTemplate
              exeArgs' <- prepareSources verbosity tmp
                            lbi version bi (commonArgs `mappend` exeArgs)
              runHaddock verbosity tmpFileOpts comp confHaddock exeArgs'
          Nothing -> do
           warn (fromFlag $ haddockVerbosity flags)
             "Unsupported component, skipping..."
           return ()
      case component of
        CLib lib -> do
          withTempDirectoryEx verbosity tmpFileOpts (buildDir lbi) "tmp" $ \tmp -> do
            let bi = libBuildInfo lib
            libArgs  <- fromLibrary verbosity tmp lbi lib clbi htmlTemplate
            libArgs' <- prepareSources verbosity tmp
                          lbi version bi (commonArgs `mappend` libArgs)
            runHaddock verbosity tmpFileOpts comp confHaddock libArgs'
        CExe   _ -> when (flag haddockExecutables) $ doExe component
        CTest  _ -> when (flag haddockTestSuites)  $ doExe component
        CBench _ -> when (flag haddockBenchmarks)  $ doExe component

    forM_ (extraDocFiles pkg_descr) $ \ fpath -> do
      files <- matchFileGlob fpath
      forM_ files $ copyFileTo verbosity (unDir $ argOutputDir commonArgs)
  where
    verbosity     = flag haddockVerbosity
    keepTempFiles = flag haddockKeepTempFiles
    comp          = compiler lbi
    tmpFileOpts   = defaultTempFileOptions { optKeepTempFiles = keepTempFiles }
    flag f        = fromFlag $ f flags
    htmlTemplate  = fmap toPathTemplate . flagToMaybe . haddockHtmlLocation $ flags

-- | performs cpp and unlit preprocessing where needed on the files in
-- | argTargets, which must have an .hs or .lhs extension.
prepareSources :: Verbosity
                  -> FilePath
                  -> LocalBuildInfo
                  -> Version
                  -> BuildInfo
                  -> HaddockArgs
                  -> IO HaddockArgs
prepareSources verbosity tmp lbi haddockVersion bi args@HaddockArgs{argTargets=files} =
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
            needsCpp             = EnableExtension CPP `elem` allExtensions bi
            isVersion2           = haddockVersion >= Version [2,0] []
            defines | isVersion2 = [haddockVersionMacro]
                    | otherwise  = ["-D__HADDOCK__", haddockVersionMacro]
            haddockVersionMacro  = "-D__HADDOCK_VERSION__="
                                   ++ show (v1 * 1000 + v2 * 10 + v3)
              where
                [v1, v2, v3] = take 3 $ versionBranch haddockVersion ++ [0,0]

-- ------------------------------------------------------------------------------
-- Contributions to HaddockArgs.

fromFlags :: PathTemplateEnv -> HaddockFlags -> HaddockArgs
fromFlags env flags =
    mempty {
      argHideModules = (maybe mempty (All . not) $ flagToMaybe (haddockInternal flags), mempty),
      argLinkSource = if fromFlag (haddockHscolour flags)
                               then Flag ("src/%{MODULE/./-}.html"
                                         ,"src/%{MODULE/./-}.html#%{NAME}"
                                         ,"src/%{MODULE/./-}.html#line-%{LINE}")
                               else NoFlag,
      argCssFile = haddockCss flags,
      argContents = fmap (fromPathTemplate . substPathTemplate env) (haddockContents flags),
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

fromLibrary :: Verbosity
            -> FilePath
            -> LocalBuildInfo -> Library -> ComponentLocalBuildInfo
            -> Maybe PathTemplate -- ^ template for html location
            -> IO HaddockArgs
fromLibrary verbosity tmp lbi lib clbi htmlTemplate = do
    inFiles <- map snd `fmap` getLibSourceFiles lbi lib
    ifaceArgs <- getInterfaces verbosity lbi clbi htmlTemplate
    let vanillaOpts = (componentGhcOptions normal lbi bi clbi (buildDir lbi)) {
                          -- Noooooooooo!!!!!111
                          -- haddock stomps on our precious .hi
                          -- and .o files. Workaround by telling
                          -- haddock to write them elsewhere.
                          ghcOptObjDir  = toFlag tmp,
                          ghcOptHiDir   = toFlag tmp,
                          ghcOptStubDir = toFlag tmp
                      }
        sharedOpts = vanillaOpts {
                         ghcOptDynLinkMode = toFlag GhcDynamicOnly,
                         ghcOptFPic        = toFlag True,
                         ghcOptHiSuffix    = toFlag "dyn_hi",
                         ghcOptObjSuffix   = toFlag "dyn_o",
                         ghcOptExtra       = ghcSharedOptions bi
                     }
    opts <- if withVanillaLib lbi
            then return vanillaOpts
            else if withSharedLib lbi
            then return sharedOpts
            else die "Must have vanilla or shared libraries enabled in order to run haddock"
    return ifaceArgs {
      argHideModules = (mempty,otherModules $ bi),
      argGhcOptions  = toFlag (opts, ghcVersion),
      argTargets     = inFiles
    }
  where
    bi = libBuildInfo lib
    ghcVersion = compilerVersion (compiler lbi)

fromExecutable :: Verbosity
               -> FilePath
               -> LocalBuildInfo -> Executable -> ComponentLocalBuildInfo
               -> Maybe PathTemplate -- ^ template for html location
               -> IO HaddockArgs
fromExecutable verbosity tmp lbi exe clbi htmlTemplate = do
    inFiles <- map snd `fmap` getExeSourceFiles lbi exe
    ifaceArgs <- getInterfaces verbosity lbi clbi htmlTemplate
    let vanillaOpts = (componentGhcOptions normal lbi bi clbi (buildDir lbi)) {
                          -- Noooooooooo!!!!!111
                          -- haddock stomps on our precious .hi
                          -- and .o files. Workaround by telling
                          -- haddock to write them elsewhere.
                          ghcOptObjDir  = toFlag tmp,
                          ghcOptHiDir   = toFlag tmp,
                          ghcOptStubDir = toFlag tmp
                      }
        sharedOpts = vanillaOpts {
                         ghcOptDynLinkMode = toFlag GhcDynamicOnly,
                         ghcOptFPic        = toFlag True,
                         ghcOptHiSuffix    = toFlag "dyn_hi",
                         ghcOptObjSuffix   = toFlag "dyn_o",
                         ghcOptExtra       = ghcSharedOptions bi
                     }
    opts <- if withVanillaLib lbi
            then return vanillaOpts
            else if withSharedLib lbi
            then return sharedOpts
            else die "Must have vanilla or shared libraries enabled in order to run haddock"
    return ifaceArgs {
      argGhcOptions = toFlag (opts, ghcVersion),
      argOutputDir  = Dir (exeName exe),
      argTitle      = Flag (exeName exe),
      argTargets    = inFiles
    }
  where
    bi = buildInfo exe
    ghcVersion = compilerVersion (compiler lbi)

compToExe :: Component -> Maybe Executable
compToExe comp =
  case comp of
    CTest test@TestSuite { testInterface = TestSuiteExeV10 _ f } ->
      Just Executable {
        exeName    = testName test,
        modulePath = f,
        buildInfo  = testBuildInfo test
      }
    CBench bench@Benchmark { benchmarkInterface = BenchmarkExeV10 _ f } ->
      Just Executable {
        exeName    = benchmarkName bench,
        modulePath = f,
        buildInfo  = benchmarkBuildInfo bench
      }
    CExe exe -> Just exe
    _ -> Nothing

getInterfaces :: Verbosity
              -> LocalBuildInfo
              -> ComponentLocalBuildInfo
              -> Maybe PathTemplate -- ^ template for html location
              -> IO HaddockArgs
getInterfaces verbosity lbi clbi htmlTemplate = do
    (packageFlags, warnings) <- haddockPackageFlags lbi clbi htmlTemplate
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

-- ------------------------------------------------------------------------------
-- | Call haddock with the specified arguments.
runHaddock :: Verbosity
              -> TempFileOptions
              -> Compiler
              -> ConfiguredProgram
              -> HaddockArgs
              -> IO ()
runHaddock verbosity tmpFileOpts comp confHaddock args = do
  let haddockVersion = fromMaybe (error "unable to determine haddock version")
                       (programVersion confHaddock)
  renderArgs verbosity tmpFileOpts haddockVersion comp args $
    \(flags,result)-> do

      rawSystemProgram verbosity confHaddock flags

      notice verbosity $ "Documentation created: " ++ result


renderArgs :: Verbosity
              -> TempFileOptions
              -> Version
              -> Compiler
              -> HaddockArgs
              -> (([String], FilePath) -> IO a)
              -> IO a
renderArgs verbosity tmpFileOpts version comp args k = do
  createDirectoryIfMissingVerbose verbosity True outputDir
  withTempFileEx tmpFileOpts outputDir "haddock-prolog.txt" $ \prologFileName h -> do
          do
             when (version >= Version [2,15] []) (hSetEncoding h utf8)
             hPutStrLn h $ fromFlag $ argPrologue args
             hClose h
             let pflag = "--prologue=" ++ prologFileName
             k (pflag : renderPureArgs version comp args, result)
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

renderPureArgs :: Version -> Compiler -> HaddockArgs -> [String]
renderPureArgs version comp args = concat
    [
     (:[]) . (\f -> "--dump-interface="++ unDir (argOutputDir args) </> f)
     . fromFlag . argInterfaceFile $ args,
     (\pname ->   if isVersion2
                  then ["--optghc=-package-name", "--optghc=" ++ pname]
                  else ["--package=" ++ pname]) . display . fromFlag . argPackageName $ args,
     (\(All b,xs) -> bool (map (("--hide=" ++). display) xs) [] b) . argHideModules $ args,
     bool ["--ignore-all-exports"] [] . getAny . argIgnoreExports $ args,
     maybe [] (\(m,e,l) -> ["--source-module=" ++ m
                           ,"--source-entity=" ++ e]
                           ++ if isVersion2_14 then ["--source-entity-line=" ++ l]
                                               else []
              ) . flagToMaybe . argLinkSource $ args,
     maybe [] ((:[]).("--css="++)) . flagToMaybe . argCssFile $ args,
     maybe [] ((:[]).("--use-contents="++)) . flagToMaybe . argContents $ args,
     bool [] [verbosityFlag] . getAny . argVerbose $ args,
     map (\o -> case o of Hoogle -> "--hoogle"; Html -> "--html") . fromFlag . argOutput $ args,
     renderInterfaces . argInterfaces $ args,
     (:[]).("--odir="++) . unDir . argOutputDir $ args,
     (:[]).("--title="++) . (bool (++" (internal documentation)") id (getAny $ argIgnoreExports args))
              . fromFlag . argTitle $ args,
     [ "--optghc=" ++ opt | isVersion2
                          , (opts, _ghcVer) <- flagToList (argGhcOptions args)
                          , opt <- renderGhcOptions comp opts ],
     maybe [] (\l -> ["-B"++l]) $ guard isVersion2 >> flagToMaybe (argGhcLibDir args), -- error if isVersion2 and Nothing?
     argTargets $ args
    ]
    where
      renderInterfaces =
        map (\(i,mh) -> "--read-interface=" ++
          maybe "" (++",") mh ++ i)
      bool a b c = if c then a else b
      isVersion2    = version >= Version [2,0]  []
      isVersion2_5  = version >= Version [2,5]  []
      isVersion2_14 = version >= Version [2,14] []
      verbosityFlag
       | isVersion2_5 = "--verbosity=1"
       | otherwise = "--verbose"

---------------------------------------------------------------------------------

-- | Given a list of 'InstalledPackageInfo's, return a list of interfaces and
-- HTML paths, and an optional warning for packages with missing documentation.
haddockPackagePaths :: [InstalledPackageInfo]
                    -> Maybe (InstalledPackageInfo -> FilePath)
                    -> IO ([(FilePath, Maybe FilePath)], Maybe String)
haddockPackagePaths ipkgs mkHtmlPath = do
  interfaces <- sequence
    [ case interfaceAndHtmlPath ipkg of
        Nothing -> return (Left (packageId ipkg))
        Just (interface, html) -> do
          exists <- doesFileExist interface
          if exists
            then return (Right (interface, html))
            else return (Left pkgid)
    | ipkg <- ipkgs, let pkgid = packageId ipkg
    , pkgName pkgid `notElem` noHaddockWhitelist
    ]

  let missing = [ pkgid | Left pkgid <- interfaces ]
      warning = "The documentation for the following packages are not "
             ++ "installed. No links will be generated to these packages: "
             ++ intercalate ", " (map display missing)
      flags = rights interfaces

  return (flags, if null missing then Nothing else Just warning)

  where
    -- Don't warn about missing documentation for these packages. See #1231.
    noHaddockWhitelist = map PackageName [ "rts" ]

    -- Actually extract interface and HTML paths from an 'InstalledPackageInfo'.
    interfaceAndHtmlPath :: InstalledPackageInfo
                         -> Maybe (FilePath, Maybe FilePath)
    interfaceAndHtmlPath pkg = do
      interface <- listToMaybe (InstalledPackageInfo.haddockInterfaces pkg)
      html <- case mkHtmlPath of
        Nothing -> fmap fixFileUrl
                        (listToMaybe (InstalledPackageInfo.haddockHTMLs pkg))
        Just mkPath -> Just (mkPath pkg)
      return (interface, if null html then Nothing else Just html)
      where
        -- The 'haddock-html' field in the hc-pkg output is often set as a
        -- native path, but we need it as a URL. See #1064.
        fixFileUrl f | isAbsolute f = "file://" ++ f
                     | otherwise    = f

haddockPackageFlags :: LocalBuildInfo
                    -> ComponentLocalBuildInfo
                    -> Maybe PathTemplate
                    -> IO ([(FilePath, Maybe FilePath)], Maybe String)
haddockPackageFlags lbi clbi htmlTemplate = do
  let allPkgs = installedPkgs lbi
      directDeps = map fst (componentPackageDeps clbi)
  transitiveDeps <- case dependencyClosure allPkgs directDeps of
    Left x    -> return x
    Right inf -> die $ "internal error when calculating transitive "
                    ++ "package dependencies.\nDebug info: " ++ show inf
  haddockPackagePaths (PackageIndex.allPackages transitiveDeps) mkHtmlPath
    where
      mkHtmlPath                  = fmap expandTemplateVars htmlTemplate
      expandTemplateVars tmpl pkg =
        fromPathTemplate . substPathTemplate (env pkg) $ tmpl
      env pkg                     = haddockTemplateEnv lbi (packageId pkg)


haddockTemplateEnv :: LocalBuildInfo -> PackageIdentifier -> PathTemplateEnv
haddockTemplateEnv lbi pkg_id =
  (PrefixVar, prefix (installDirTemplates lbi))
  : initialPathTemplateEnv pkg_id (compilerId (compiler lbi))
  (hostPlatform lbi)

-- ------------------------------------------------------------------------------
-- hscolour support.

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
    withAllComponentsInBuildOrder pkg_descr lbi $ \comp _ -> do
      pre comp
      let
        doExe com = case (compToExe com) of
          Just exe -> do
            let outputDir = hscolourPref distPref pkg_descr </> exeName exe </> "src"
            runHsColour hscolourProg outputDir =<< getExeSourceFiles lbi exe
          Nothing -> do
           warn (fromFlag $ hscolourVerbosity flags)
             "Unsupported component, skipping..."
           return ()
      case comp of
        CLib lib -> do
          let outputDir = hscolourPref distPref pkg_descr </> "src"
          runHsColour hscolourProg outputDir =<< getLibSourceFiles lbi lib
        CExe   _ -> when (fromFlag (hscolourExecutables flags)) $ doExe comp
        CTest  _ -> when (fromFlag (hscolourTestSuites  flags)) $ doExe comp
        CBench _ -> when (fromFlag (hscolourBenchmarks  flags)) $ doExe comp
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

         forM_ moduleFiles $ \(m, inFile) ->
             rawSystemProgram verbosity prog
                    ["-css", "-anchor", "-o" ++ outFile m, inFile]
        where
          outFile m = outputDir </> intercalate "-" (ModuleName.components m) <.> "html"

haddockToHscolour :: HaddockFlags -> HscolourFlags
haddockToHscolour flags =
    HscolourFlags {
      hscolourCSS         = haddockHscolourCss flags,
      hscolourExecutables = haddockExecutables flags,
      hscolourTestSuites  = haddockTestSuites  flags,
      hscolourBenchmarks  = haddockBenchmarks  flags,
      hscolourVerbosity   = haddockVerbosity   flags,
      hscolourDistPref    = haddockDistPref    flags
    }
---------------------------------------------------------------------------------
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

-- ------------------------------------------------------------------------------
-- Boilerplate Monoid instance.
instance Monoid HaddockArgs where
    mempty = HaddockArgs {
                argInterfaceFile = mempty,
                argPackageName = mempty,
                argHideModules = mempty,
                argIgnoreExports = mempty,
                argLinkSource = mempty,
                argCssFile = mempty,
                argContents = mempty,
                argVerbose = mempty,
                argOutput = mempty,
                argInterfaces = mempty,
                argOutputDir = mempty,
                argTitle = mempty,
                argPrologue = mempty,
                argGhcOptions = mempty,
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
                argContents = mult argContents,
                argVerbose = mult argVerbose,
                argOutput = mult argOutput,
                argInterfaces = mult argInterfaces,
                argOutputDir = mult argOutputDir,
                argTitle = mult argTitle,
                argPrologue = mult argPrologue,
                argGhcOptions = mult argGhcOptions,
                argGhcLibDir = mult argGhcLibDir,
                argTargets = mult argTargets
             }
      where mult f = f a `mappend` f b

instance Monoid Directory where
    mempty = Dir "."
    mappend (Dir m) (Dir n) = Dir $ m </> n
