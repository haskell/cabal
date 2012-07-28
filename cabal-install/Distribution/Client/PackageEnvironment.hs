-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.PackageEnvironment
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Utilities for working with the package environment file. Patterned after
-- Distribution.Client.Config.
-----------------------------------------------------------------------------

module Distribution.Client.PackageEnvironment (
    PackageEnvironment(..),
    loadPackageEnvironment,
    dumpPackageEnvironment
  ) where

import Distribution.Client.Config      ( SavedConfig(..), baseSavedConfig,
                                         commentSavedConfig, initialSavedConfig,
                                         configFieldDescriptions,
                                         installDirsFields )
import Distribution.Client.ParseUtils  ( parseFields, ppFields, ppSection )
import Distribution.Client.Setup       ( SandboxFlags(..) )
import Distribution.Simple.InstallDirs ( InstallDirs(..), PathTemplate )
import Distribution.Simple.Setup       ( Flag(..), fromFlagOrDefault, toFlag )
import Distribution.Simple.Utils       ( notice, warn, lowercase )
import Distribution.ParseUtils         ( FieldDescr(..), ParseResult(..),
                                         liftField, lineNo, locatedErrorMsg,
                                         parseFilePathQ, readFields,
                                         showPWarning, simpleField, warning )
import Distribution.Verbosity          ( Verbosity )
import Control.Monad                   ( foldM, when )
import Data.List                       ( partition )
import Data.Monoid                     ( Monoid(..) )
import Distribution.Compat.Exception   ( catchIO )
import System.Directory                ( canonicalizePath,
                                         createDirectoryIfMissing, renameFile )
import System.FilePath                 ( (<.>), takeDirectory )
import System.IO.Error                 ( isDoesNotExistError )
import Text.PrettyPrint                ( ($+$) )

import qualified Text.PrettyPrint          as Disp
import qualified Distribution.Compat.ReadP as Parse
import qualified Distribution.ParseUtils   as ParseUtils ( Field(..) )


--
-- * Configuration saved in the package environment file
--

-- TODO: better defaults, constraints field (?), remove duplication between
-- D.C.PackageEnvironment and D.C.Config
data PackageEnvironment = PackageEnvironment {
  pkgEnvInherit       :: Flag FilePath,
  pkgEnvSavedConfig   :: SavedConfig
}

instance Monoid PackageEnvironment where
  mempty = PackageEnvironment {
    pkgEnvInherit       = mempty,
    pkgEnvSavedConfig   = mempty
    }

  mappend a b = PackageEnvironment {
    pkgEnvInherit       = combine pkgEnvInherit,
    pkgEnvSavedConfig   = combine pkgEnvSavedConfig
    }
    where
      combine f = f a `mappend` f b

-- | Values that *must* be initialised.
basePackageEnvironment :: IO PackageEnvironment
basePackageEnvironment = do
  baseConf <- baseSavedConfig
  return $ mempty { pkgEnvSavedConfig = baseConf }

-- | Initial configuration that we write out to the package environment file if
-- it does not exist. When the package environment gets loaded it gets layered
-- on top of 'basePackageEnvironment'.
initialPackageEnvironment :: FilePath -> IO PackageEnvironment
initialPackageEnvironment pkgEnvDir = do
  initialConf <- initialSavedConfig
  return $ mempty { pkgEnvSavedConfig = initialConf }

-- | Default values that get used if no value is given. Used here to include in
-- comments when we write out the initial package environment.
commentPackageEnvironment :: FilePath -> IO PackageEnvironment
commentPackageEnvironment pkgEnvDir = do
  commentConf <- commentSavedConfig
  return $ mempty { pkgEnvSavedConfig = commentConf }

-- | Entry point for the 'cabal dump-pkgenv' command.
dumpPackageEnvironment :: Verbosity -> SandboxFlags -> FilePath -> IO ()
dumpPackageEnvironment verbosity sandboxFlags path = do
  pkgEnv <- loadPackageEnvironment verbosity path
  putStrLn . showPackageEnvironment $ pkgEnv

-- | Load the package environment file, creating it if doesn't exist.
loadPackageEnvironment :: Verbosity -> FilePath -> IO PackageEnvironment
loadPackageEnvironment verbosity path = addBasePkgEnv $ do
  pkgEnvDir <- canonicalizePath . takeDirectory $ path
  minp <- readPackageEnvironmentFile mempty path
  case minp of
    Nothing -> do
      notice verbosity $ "Writing default package environment to " ++ path
      commentPkgEnv <- commentPackageEnvironment pkgEnvDir
      initialPkgEnv <- initialPackageEnvironment pkgEnvDir
      writePackageEnvironmentFile path commentPkgEnv initialPkgEnv
      return initialPkgEnv
    Just (ParseOk warns pkgEnv) -> do
      when (not $ null warns) $ warn verbosity $
        unlines (map (showPWarning path) warns)
      return pkgEnv
    Just (ParseFailed err) -> do
      let (line, msg) = locatedErrorMsg err
      warn verbosity $
        "Error parsing package environment file " ++ path
        ++ maybe "" (\n -> ":" ++ show n) line ++ ":\n" ++ msg
      warn verbosity $ "Using default package environment."
      initialPackageEnvironment pkgEnvDir
  where
    addBasePkgEnv body = do
      base  <- basePackageEnvironment
      extra <- body
      return $ base `mappend` extra

-- | Descriptions of all fields in the package environment file.
pkgEnvFieldDescrs :: [FieldDescr PackageEnvironment]
pkgEnvFieldDescrs = [
  simpleField "inherit"
    (fromFlagOrDefault Disp.empty . fmap Disp.text) (optional parseFilePathQ)
    pkgEnvInherit (\v pkgEnv -> pkgEnv { pkgEnvInherit = v })
  ]
  ++ map toPkgEnv configFieldDescriptions
  where
    optional = Parse.option mempty . fmap toFlag

    toPkgEnv :: FieldDescr SavedConfig -> FieldDescr PackageEnvironment
    toPkgEnv fieldDescr =
      liftField pkgEnvSavedConfig
      (\savedConfig pkgEnv -> pkgEnv { pkgEnvSavedConfig = savedConfig})
      fieldDescr

-- | Read the package environment file.
readPackageEnvironmentFile :: PackageEnvironment -> FilePath
                              -> IO (Maybe (ParseResult PackageEnvironment))
readPackageEnvironmentFile initial file =
  handleNotExists $
  fmap (Just . parsePackageEnvironment initial) (readFile file)
  where
    handleNotExists action = catchIO action $ \ioe ->
      if isDoesNotExistError ioe
        then return Nothing
        else ioError ioe

-- | Parse the package environment file.
parsePackageEnvironment :: PackageEnvironment -> String
                           -> ParseResult PackageEnvironment
parsePackageEnvironment initial str = do
  fields <- readFields str
  let (knownSections, others) = partition isKnownSection fields

  pkgEnv <- parse others
  let config       = pkgEnvSavedConfig pkgEnv
      installDirs0 = savedUserInstallDirs config
  -- 'install-dirs' is the only section that we care about.
  installDirs <- foldM parseSection installDirs0 knownSections
  return pkgEnv {
    pkgEnvSavedConfig = config {
       savedUserInstallDirs   = installDirs,
       savedGlobalInstallDirs = installDirs
       }
    }

  where
    isKnownSection :: ParseUtils.Field -> Bool
    isKnownSection (ParseUtils.Section _ "install-dirs" _ _) = True
    isKnownSection _                                         = False

    parse :: [ParseUtils.Field] -> ParseResult PackageEnvironment
    parse = parseFields pkgEnvFieldDescrs initial

    parseSection :: InstallDirs (Flag PathTemplate)
                    -> ParseUtils.Field
                    -> ParseResult (InstallDirs (Flag PathTemplate))
    parseSection accum (ParseUtils.Section _ "install-dirs" name fs)
      | name' == "" = do accum' <- parseFields installDirsFields accum fs
                         return accum'
      | otherwise   = do warning "The install-dirs section should be unnamed"
                         return accum
      where name' = lowercase name
    parseSection accum f = do
      warning $ "Unrecognized stanza on line " ++ show (lineNo f)
      return accum

-- | Write out the package environment file.
writePackageEnvironmentFile :: FilePath -> PackageEnvironment
                               -> PackageEnvironment -> IO ()
writePackageEnvironmentFile path comments pkgEnv = do
  let tmpPath = (path <.> "tmp")
  createDirectoryIfMissing True (takeDirectory path)
  writeFile tmpPath $ explanation
    ++ showPackageEnvironmentWithComments comments pkgEnv ++ "\n"
  renameFile tmpPath path
  where
    -- TODO: Better explanation
    explanation = unlines
      ["-- This is a Cabal package environment file."
      ,""
      ,"-- The available configuration options are listed below."
      ,"-- Some of them have default values listed."
      ,""
      ,"-- Lines (like this one) beginning with '--' are comments."
      ,"-- Be careful with spaces and indentation because they are"
      ,"-- used to indicate layout for nested sections."
      ,"",""
      ]

-- | Pretty-print the package environment data.
showPackageEnvironment :: PackageEnvironment -> String
showPackageEnvironment = showPackageEnvironmentWithComments mempty

showPackageEnvironmentWithComments :: PackageEnvironment -> PackageEnvironment
                                      -> String
showPackageEnvironmentWithComments defPkgEnv pkgEnv = Disp.render $
      ppFields pkgEnvFieldDescrs defPkgEnv pkgEnv
  $+$ Disp.text ""
  $+$ ppSection "install-dirs" "" installDirsFields
                (field defPkgEnv) (field pkgEnv)
  where
    field = savedUserInstallDirs . pkgEnvSavedConfig
