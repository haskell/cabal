-----------------------------------------------------------------------------
-- |
-- Module      :  Distribution.Client.PkgEnv
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- Utilities for working with the package environment file. Patterned after
-- Distribution.Client.Config.
--
--
-----------------------------------------------------------------------------

module Distribution.Client.PkgEnv (dumpPkgEnv)
       where

import Distribution.Client.Config    ( SavedConfig(..), baseSavedConfig,
                                       configFieldDescriptions )
import Distribution.Client.Setup     ( SandboxFlags(..) )
import Distribution.Simple.Setup     ( Flag(..), fromFlagOrDefault, toFlag )
import Distribution.Simple.Utils     ( notice, warn )
import Distribution.ParseUtils       ( FieldDescr(..), ParseResult(..),
                                       parseFilePathQ, parseFields,
                                       liftField, ppFields, simpleField )
import Distribution.Verbosity        ( Verbosity )
import Data.Monoid                   ( Monoid(..) )
import Distribution.Compat.Exception ( catchIO )
import System.Directory              ( createDirectoryIfMissing, renameFile )
import System.FilePath               ( (<.>), takeDirectory )
import System.IO.Error               ( isDoesNotExistError )

import qualified Text.PrettyPrint          as Disp
import qualified Distribution.Compat.ReadP as Parse

--
-- * Configuration saved in the package environment file
--

-- TODO: Print w/comments & defaults, install-dirs section, constraints field
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
      -> do base <- baseSavedConfig
            let pkgEnv' = pkgEnv { pkgEnvSavedConfig =
                                      base `mappend` pkgEnvSavedConfig pkgEnv }
            putStrLn . showPkgEnv $ pkgEnv'

-- | Descriptions of all fields in the package environment file.
pkgEnvFieldDescrs :: [FieldDescr PkgEnv]
pkgEnvFieldDescrs = [
  simpleField "inherit"
    (Disp.text . fromFlagOrDefault "") (optional parseFilePathQ)
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
parsePkgEnv initial str = parseFields pkgEnvFieldDescrs initial str

-- | Write out the package environment file.
writePkgEnvFile :: FilePath -> PkgEnv -> IO ()
writePkgEnvFile path pkgEnv = do
  let tmpPath = (path <.> "tmp")
  createDirectoryIfMissing True (takeDirectory path)
  writeFile tmpPath (showPkgEnv pkgEnv)
  renameFile tmpPath path

-- | Pretty-print the package environment data.
showPkgEnv :: PkgEnv -> String
showPkgEnv pkgEnv = Disp.render $
                    ppFields pkgEnvFieldDescrs pkgEnv
