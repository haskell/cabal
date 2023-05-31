{-# LANGUAGE DeriveGeneric #-}

-----------------------------------------------------------------------------

-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Client.Sandbox.PackageEnvironment
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Utilities for working with the package environment file. Patterned after
-- Distribution.Client.Config.
module Distribution.Client.Sandbox.PackageEnvironment
  ( PackageEnvironment (..)
  , PackageEnvironmentType (..)
  , classifyPackageEnvironment
  , readPackageEnvironmentFile
  , showPackageEnvironment
  , showPackageEnvironmentWithComments
  , loadUserConfig
  , userPackageEnvironmentFile
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.Config
  ( SavedConfig (..)
  , configFieldDescriptions
  , haddockFlagsFields
  , installDirsFields
  , withProgramOptionsFields
  , withProgramsFields
  )
import Distribution.Client.ParseUtils (parseFields, ppFields, ppSection)
import Distribution.Client.Setup
  ( ConfigExFlags (..)
  )
import Distribution.Client.Targets (userConstraintPackageName)
import Distribution.Deprecated.ParseUtils
  ( FieldDescr (..)
  , ParseResult (..)
  , commaListFieldParsec
  , commaNewLineListFieldParsec
  , liftField
  , lineNo
  , locatedErrorMsg
  , readFields
  , showPWarning
  , syntaxError
  , warning
  )
import Distribution.Simple.InstallDirs (InstallDirs (..), PathTemplate)
import Distribution.Simple.Setup
  ( ConfigFlags (..)
  , Flag (..)
  , HaddockFlags (..)
  )
import Distribution.Simple.Utils (debug, warn)
import Distribution.Solver.Types.ConstraintSource
import System.Directory (doesFileExist)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import Text.PrettyPrint (($+$))

import qualified Data.ByteString as BS
import qualified Distribution.Deprecated.ParseUtils as ParseUtils (Field (..))
import qualified Text.PrettyPrint as Disp

--

-- * Configuration saved in the package environment file

--

-- TODO: would be nice to remove duplication between
-- D.C.Sandbox.PackageEnvironment and D.C.Config.
data PackageEnvironment = PackageEnvironment
  { pkgEnvSavedConfig :: SavedConfig
  }
  deriving (Generic)

instance Monoid PackageEnvironment where
  mempty = gmempty
  mappend = (<>)

instance Semigroup PackageEnvironment where
  (<>) = gmappend

-- | Optional package environment file that can be used to customize the default
-- settings. Created by the user.
userPackageEnvironmentFile :: FilePath
userPackageEnvironmentFile = "cabal.config"

-- | Type of the current package environment.
data PackageEnvironmentType
  = -- | './cabal.config'
    UserPackageEnvironment
  | -- | '~/.config/cabal/config'
    AmbientPackageEnvironment

-- | Is there a 'cabal.config' in this directory?
classifyPackageEnvironment :: FilePath -> IO PackageEnvironmentType
classifyPackageEnvironment pkgEnvDir = do
  isUser <- configExists userPackageEnvironmentFile
  return (classify isUser)
  where
    configExists fname = doesFileExist (pkgEnvDir </> fname)

    classify :: Bool -> PackageEnvironmentType
    classify True = UserPackageEnvironment
    classify False = AmbientPackageEnvironment

-- | Load the user package environment if it exists (the optional "cabal.config"
-- file). If it does not exist locally, attempt to load an optional global one.
userPackageEnvironment
  :: Verbosity
  -> FilePath
  -> Maybe FilePath
  -> IO PackageEnvironment
userPackageEnvironment verbosity pkgEnvDir globalConfigLocation = do
  let path = pkgEnvDir </> userPackageEnvironmentFile
  minp <-
    readPackageEnvironmentFile
      (ConstraintSourceUserConfig path)
      mempty
      path
  case (minp, globalConfigLocation) of
    (Just parseRes, _) -> processConfigParse path parseRes
    (_, Just globalLoc) -> do
      minp' <-
        readPackageEnvironmentFile
          (ConstraintSourceUserConfig globalLoc)
          mempty
          globalLoc
      maybe
        ( warn verbosity ("no constraints file found at " ++ globalLoc)
            >> return mempty
        )
        (processConfigParse globalLoc)
        minp'
    _ -> do
      debug verbosity ("no user package environment file found at " ++ pkgEnvDir)
      return mempty
  where
    processConfigParse path (ParseOk warns parseResult) = do
      unless (null warns) $
        warn verbosity $
          unlines (map (showPWarning path) warns)
      return parseResult
    processConfigParse path (ParseFailed err) = do
      let (line, msg) = locatedErrorMsg err
      warn verbosity $
        "Error parsing package environment file "
          ++ path
          ++ maybe "" (\n -> ":" ++ show n) line
          ++ ":\n"
          ++ msg
      return mempty

-- | Same as @userPackageEnvironmentFile@, but returns a SavedConfig.
loadUserConfig :: Verbosity -> FilePath -> Maybe FilePath -> IO SavedConfig
loadUserConfig verbosity pkgEnvDir globalConfigLocation =
  fmap pkgEnvSavedConfig $
    userPackageEnvironment verbosity pkgEnvDir globalConfigLocation

-- | Descriptions of all fields in the package environment file.
pkgEnvFieldDescrs :: ConstraintSource -> [FieldDescr PackageEnvironment]
pkgEnvFieldDescrs src =
  [ commaNewLineListFieldParsec
      "constraints"
      (pretty . fst)
      ((\pc -> (pc, src)) `fmap` parsec)
      ( sortConstraints
          . configExConstraints
          . savedConfigureExFlags
          . pkgEnvSavedConfig
      )
      ( \v pkgEnv ->
          updateConfigureExFlags
            pkgEnv
            (\flags -> flags{configExConstraints = v})
      )
  , commaListFieldParsec
      "preferences"
      pretty
      parsec
      (configPreferences . savedConfigureExFlags . pkgEnvSavedConfig)
      ( \v pkgEnv ->
          updateConfigureExFlags
            pkgEnv
            (\flags -> flags{configPreferences = v})
      )
  ]
    ++ map toPkgEnv configFieldDescriptions'
  where
    configFieldDescriptions' :: [FieldDescr SavedConfig]
    configFieldDescriptions' =
      filter
        (\(FieldDescr name _ _) -> name /= "preference" && name /= "constraint")
        (configFieldDescriptions src)

    toPkgEnv :: FieldDescr SavedConfig -> FieldDescr PackageEnvironment
    toPkgEnv fieldDescr =
      liftField
        pkgEnvSavedConfig
        (\savedConfig pkgEnv -> pkgEnv{pkgEnvSavedConfig = savedConfig})
        fieldDescr

    updateConfigureExFlags
      :: PackageEnvironment
      -> (ConfigExFlags -> ConfigExFlags)
      -> PackageEnvironment
    updateConfigureExFlags pkgEnv f =
      pkgEnv
        { pkgEnvSavedConfig =
            (pkgEnvSavedConfig pkgEnv)
              { savedConfigureExFlags =
                  f . savedConfigureExFlags . pkgEnvSavedConfig $
                    pkgEnv
              }
        }

    sortConstraints = sortBy (comparing $ userConstraintPackageName . fst)

-- | Read the package environment file.
readPackageEnvironmentFile
  :: ConstraintSource
  -> PackageEnvironment
  -> FilePath
  -> IO (Maybe (ParseResult PackageEnvironment))
readPackageEnvironmentFile src initial file =
  handleNotExists $
    fmap (Just . parsePackageEnvironment src initial) (BS.readFile file)
  where
    handleNotExists action = catchIO action $ \ioe ->
      if isDoesNotExistError ioe
        then return Nothing
        else ioError ioe

-- | Parse the package environment file.
parsePackageEnvironment
  :: ConstraintSource
  -> PackageEnvironment
  -> BS.ByteString
  -> ParseResult PackageEnvironment
parsePackageEnvironment src initial str = do
  fields <- readFields str
  let (knownSections, others) = partition isKnownSection fields
  pkgEnv <- parse others
  let config = pkgEnvSavedConfig pkgEnv
      installDirs0 = savedUserInstallDirs config
  (haddockFlags, installDirs, paths, args) <-
    foldM
      parseSections
      (savedHaddockFlags config, installDirs0, [], [])
      knownSections
  return
    pkgEnv
      { pkgEnvSavedConfig =
          config
            { savedConfigureFlags =
                (savedConfigureFlags config)
                  { configProgramPaths = paths
                  , configProgramArgs = args
                  }
            , savedHaddockFlags = haddockFlags
            , savedUserInstallDirs = installDirs
            , savedGlobalInstallDirs = installDirs
            }
      }
  where
    isKnownSection :: ParseUtils.Field -> Bool
    isKnownSection (ParseUtils.Section _ "haddock" _ _) = True
    isKnownSection (ParseUtils.Section _ "install-dirs" _ _) = True
    isKnownSection (ParseUtils.Section _ "program-locations" _ _) = True
    isKnownSection (ParseUtils.Section _ "program-default-options" _ _) = True
    isKnownSection _ = False

    parse :: [ParseUtils.Field] -> ParseResult PackageEnvironment
    parse = parseFields (pkgEnvFieldDescrs src) initial

    parseSections
      :: SectionsAccum
      -> ParseUtils.Field
      -> ParseResult SectionsAccum
    parseSections
      accum@(h, d, p, a)
      (ParseUtils.Section _ "haddock" name fs)
        | name == "" = do
            h' <- parseFields haddockFlagsFields h fs
            return (h', d, p, a)
        | otherwise = do
            warning "The 'haddock' section should be unnamed"
            return accum
    parseSections
      (h, d, p, a)
      (ParseUtils.Section line "install-dirs" name fs)
        | name == "" = do
            d' <- parseFields installDirsFields d fs
            return (h, d', p, a)
        | otherwise =
            syntaxError line $
              "Named 'install-dirs' section: '"
                ++ name
                ++ "'. Note that named 'install-dirs' sections are not allowed in the '"
                ++ userPackageEnvironmentFile
                ++ "' file."
    parseSections
      accum@(h, d, p, a)
      (ParseUtils.Section _ "program-locations" name fs)
        | name == "" = do
            p' <- parseFields withProgramsFields p fs
            return (h, d, p', a)
        | otherwise = do
            warning "The 'program-locations' section should be unnamed"
            return accum
    parseSections
      accum@(h, d, p, a)
      (ParseUtils.Section _ "program-default-options" name fs)
        | name == "" = do
            a' <- parseFields withProgramOptionsFields a fs
            return (h, d, p, a')
        | otherwise = do
            warning "The 'program-default-options' section should be unnamed"
            return accum
    parseSections accum f = do
      warning $ "Unrecognized stanza on line " ++ show (lineNo f)
      return accum

-- | Accumulator type for 'parseSections'.
type SectionsAccum =
  ( HaddockFlags
  , InstallDirs (Flag PathTemplate)
  , [(String, FilePath)]
  , [(String, [String])]
  )

-- | Pretty-print the package environment.
showPackageEnvironment :: PackageEnvironment -> String
showPackageEnvironment pkgEnv = showPackageEnvironmentWithComments Nothing pkgEnv

-- | Pretty-print the package environment with default values for empty fields
-- commented out (just like the default Cabal config file).
showPackageEnvironmentWithComments
  :: (Maybe PackageEnvironment)
  -> PackageEnvironment
  -> String
showPackageEnvironmentWithComments mdefPkgEnv pkgEnv =
  Disp.render $
    ppFields
      (pkgEnvFieldDescrs ConstraintSourceUnknown)
      mdefPkgEnv
      pkgEnv
      $+$ Disp.text ""
      $+$ ppSection
        "install-dirs"
        ""
        installDirsFields
        (fmap installDirsSection mdefPkgEnv)
        (installDirsSection pkgEnv)
  where
    installDirsSection = savedUserInstallDirs . pkgEnvSavedConfig
