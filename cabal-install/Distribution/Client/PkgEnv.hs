-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.PkgEnv
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Utilities for working with the package environment file. Patterned after
-- Distribution.Client.Config.
-----------------------------------------------------------------------------

module Distribution.Client.PkgEnv (dumpPkgEnv)
       where

import Distribution.Client.Config      ( SavedConfig(..), baseSavedConfig,
                                         configFieldDescriptions,
                                         installDirsFields )
import Distribution.Client.ParseUtils  ( parseFields, ppFields, ppSection )
import Distribution.Client.Setup       ( SandboxFlags(..) )
import Distribution.Simple.InstallDirs ( InstallDirs(..), PathTemplate )
import Distribution.Simple.Setup       ( Flag(..), fromFlagOrDefault, toFlag )
import Distribution.Simple.Utils       ( notice, warn, lowercase )
import Distribution.ParseUtils         ( FieldDescr(..), ParseResult(..),
                                         liftField, lineNo,
                                         parseFilePathQ, readFields,
                                         showPWarning, simpleField, warning )
import Distribution.Verbosity          ( Verbosity )
import Control.Monad                   ( foldM, when )
import Data.List                       ( partition )
import Data.Monoid                     ( Monoid(..) )
import Distribution.Compat.Exception   ( catchIO )
import System.Directory                ( createDirectoryIfMissing, renameFile )
import System.FilePath                 ( (<.>), takeDirectory )
import System.IO.Error                 ( isDoesNotExistError )
import Text.PrettyPrint                ( ($+$) )

import qualified Text.PrettyPrint          as Disp
import qualified Distribution.Compat.ReadP as Parse
import qualified Distribution.ParseUtils   as ParseUtils ( Field(..) )

--
-- * Configuration saved in the package environment file
--

-- TODO: constraints field, sensible defaults, loadPkgEnv function,
-- remove duplication between D.C.PkgEnv and D.C.Config
data PkgEnv = PkgEnv {
  pkgEnvInherit       :: Flag FilePath,
  pkgEnvSavedConfig   :: SavedConfig
}

defaultPkgEnv :: PkgEnv
defaultPkgEnv = mempty

instance Monoid PkgEnv where
  mempty = PkgEnv {
    pkgEnvInherit       = mempty,
    pkgEnvSavedConfig   = mempty
    }

  mappend a b = PkgEnv {
    pkgEnvInherit       = combine pkgEnvInherit,
    pkgEnvSavedConfig   = combine pkgEnvSavedConfig
    }
    where
      combine f = f a `mappend` f b

basePkgEnv :: IO PkgEnv
basePkgEnv = do baseConf <- baseSavedConfig
                return $ mempty { pkgEnvSavedConfig = baseConf }

-- | Entry point for the 'cabal dump-pkgenv' command.
dumpPkgEnv :: Verbosity -> SandboxFlags -> FilePath -> IO ()
dumpPkgEnv verbosity sandboxFlags path = do
  minp <- readPkgEnvFile defaultPkgEnv path
  case minp of
    Nothing
      -> notice verbosity $ "File '" ++ path ++ "' not found"
    Just (ParseFailed err)
      -> warn verbosity $ "Failed to parse file '" ++ path ++ "'."
    Just (ParseOk warns pkgEnv)
      -> do when (not $ null warns) $ warn verbosity $
              unlines (map (showPWarning path) warns)
            base <- basePkgEnv
            let pkgEnv' = base `mappend` pkgEnv
            putStrLn . showPkgEnvWithComments base $ pkgEnv'

-- | Descriptions of all fields in the package environment file.
pkgEnvFieldDescrs :: [FieldDescr PkgEnv]
pkgEnvFieldDescrs = [
  simpleField "inherit"
    (fromFlagOrDefault Disp.empty . fmap Disp.text) (optional parseFilePathQ)
    pkgEnvInherit (\v pkgEnv -> pkgEnv { pkgEnvInherit = v })
  ]
  ++ map toPkgEnv configFieldDescriptions
  where
    optional = Parse.option mempty . fmap toFlag

    toPkgEnv :: FieldDescr SavedConfig -> FieldDescr PkgEnv
    toPkgEnv fieldDescr =
      liftField pkgEnvSavedConfig
      (\savedConfig pkgEnv -> pkgEnv { pkgEnvSavedConfig = savedConfig})
      fieldDescr

-- | Read the package environment file.
readPkgEnvFile :: PkgEnv -> FilePath -> IO (Maybe (ParseResult PkgEnv))
readPkgEnvFile initial file = handleNotExists $
                              fmap (Just . parsePkgEnv initial) (readFile file)
  where
    handleNotExists action = catchIO action $ \ioe ->
      if isDoesNotExistError ioe
        then return Nothing
        else ioError ioe

-- | Parse the package environment file.
parsePkgEnv :: PkgEnv -> String -> ParseResult PkgEnv
parsePkgEnv initial str = do
  fields <- readFields str
  let (knownSections, others) = partition isKnownSection fields

  pkgEnv <- parse others
  let config       = pkgEnvSavedConfig pkgEnv
      installDirs0 = savedUserInstallDirs config
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

    parse :: [ParseUtils.Field] -> ParseResult PkgEnv
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
writePkgEnvFile :: FilePath -> PkgEnv -> IO ()
writePkgEnvFile path pkgEnv = do
  let tmpPath = (path <.> "tmp")
  createDirectoryIfMissing True (takeDirectory path)
  writeFile tmpPath (showPkgEnv pkgEnv)
  renameFile tmpPath path

-- | Pretty-print the package environment data.
showPkgEnv :: PkgEnv -> String
showPkgEnv = showPkgEnvWithComments mempty

showPkgEnvWithComments :: PkgEnv -> PkgEnv -> String
showPkgEnvWithComments defPkgEnv pkgEnv = Disp.render $
      ppFields pkgEnvFieldDescrs defPkgEnv pkgEnv
  $+$ Disp.text ""
  $+$ ppSection "install-dirs" "" installDirsFields
                (field defPkgEnv) (field pkgEnv)
  where
    field = savedUserInstallDirs . pkgEnvSavedConfig
