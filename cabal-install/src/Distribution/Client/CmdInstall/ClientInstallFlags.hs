{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Distribution.Client.CmdInstall.ClientInstallFlags
  ( InstallMethod (..)
  , ClientInstallFlags (..)
  , defaultClientInstallFlags
  , clientInstallOptions
  , clientInstallFlagsGrammar
  ) where

import Distribution.Client.Compat.Prelude
import Distribution.Compat.Lens (Lens')
import Distribution.FieldGrammar
import Prelude ()

import Distribution.ReadE
  ( parsecToReadE
  , succeedReadE
  )
import Distribution.Simple.Command
  ( OptionField (..)
  , ShowOrParseArgs (..)
  , option
  , reqArg
  )
import Distribution.Simple.Setup
  ( Flag (..)
  , flagToList
  , toFlag
  , trueArg
  )

import Distribution.Client.Types.InstallMethod
  ( InstallMethod (..)
  )
import Distribution.Client.Types.OverwritePolicy
  ( OverwritePolicy (..)
  )
import Distribution.Client.Utils.Parsec

import qualified Distribution.Compat.CharParsing as P

data ClientInstallFlags = ClientInstallFlags
  { cinstInstallLibs :: Flag Bool
  , cinstEnvironmentPath :: Flag FilePath
  , cinstOverwritePolicy :: Flag OverwritePolicy
  , cinstInstallMethod :: Flag InstallMethod
  , cinstInstalldir :: Flag FilePath
  }
  deriving (Eq, Show, Generic)

instance Monoid ClientInstallFlags where
  mempty = gmempty
  mappend = (<>)

instance Semigroup ClientInstallFlags where
  (<>) = gmappend

instance Binary ClientInstallFlags
instance Structured ClientInstallFlags

defaultClientInstallFlags :: ClientInstallFlags
defaultClientInstallFlags =
  ClientInstallFlags
    { cinstInstallLibs = toFlag False
    , cinstEnvironmentPath = mempty
    , cinstOverwritePolicy = mempty
    , cinstInstallMethod = mempty
    , cinstInstalldir = mempty
    }

clientInstallOptions :: ShowOrParseArgs -> [OptionField ClientInstallFlags]
clientInstallOptions _ =
  [ option
      []
      ["lib"]
      ( "Install libraries rather than executables from the target package "
          <> "(provisional, see https://github.com/haskell/cabal/issues/6481 for more information)."
      )
      cinstInstallLibs
      (\v flags -> flags{cinstInstallLibs = v})
      trueArg
  , option
      []
      ["package-env", "env"]
      "Set the environment file that may be modified."
      cinstEnvironmentPath
      (\pf flags -> flags{cinstEnvironmentPath = pf})
      (reqArg "ENV" (succeedReadE Flag) flagToList)
  , option
      []
      ["overwrite-policy"]
      "How to handle already existing symlinks."
      cinstOverwritePolicy
      (\v flags -> flags{cinstOverwritePolicy = v})
      $ reqArg
        "always|never|prompt"
        (parsecToReadE (\err -> "Error parsing overwrite-policy: " ++ err) (toFlag `fmap` parsec))
        (map prettyShow . flagToList)
  , option
      []
      ["install-method"]
      "How to install the executables."
      cinstInstallMethod
      (\v flags -> flags{cinstInstallMethod = v})
      $ reqArg
        "default|copy|symlink"
        (parsecToReadE (\err -> "Error parsing install-method: " ++ err) (toFlag `fmap` parsecInstallMethod))
        (map prettyShow . flagToList)
  , option
      []
      ["installdir"]
      "Where to install (by symlinking or copying) the executables in."
      cinstInstalldir
      (\v flags -> flags{cinstInstalldir = v})
      $ reqArg "DIR" (succeedReadE Flag) flagToList
  ]

clientInstallFlagsGrammar
  :: ( FieldGrammar c g
     , Applicative (g ClientInstallFlags)
     , c (Identity (Flag Bool))
     , c ((Flag' FilePathNT FilePath))
     , c (Identity (Flag OverwritePolicy))
     , c (Identity (Flag InstallMethod))
     )
  => g ClientInstallFlags ClientInstallFlags
clientInstallFlagsGrammar =
  ClientInstallFlags
    <$> optionalFieldDef "lib" cinstInstallLibsLens mempty
    <*> ( optionalFieldDefAla "package-env" (alaFlag FilePathNT) cinstEnvironmentPathLens mempty
            <* optionalFieldDefAla "env" (alaFlag FilePathNT) cinstEnvironmentPathLens mempty
        )
    <*> optionalFieldDef "overwrite-policy" cinstOverwritePolicyLens mempty
    <*> optionalFieldDef "install-method" cinstInstallMethodLens mempty
    <*> optionalFieldDefAla "installdir" (alaFlag FilePathNT) cinstInstalldirLens mempty
{-# SPECIALIZE clientInstallFlagsGrammar :: ParsecFieldGrammar' ClientInstallFlags #-}

parsecInstallMethod :: CabalParsing m => m InstallMethod
parsecInstallMethod = do
  name <- P.munch1 isAlpha
  case name of
    "copy" -> pure InstallMethodCopy
    "symlink" -> pure InstallMethodSymlink
    _ -> P.unexpected $ "InstallMethod: " ++ name

cinstInstallLibsLens :: Lens' ClientInstallFlags (Flag Bool)
cinstInstallLibsLens f c = fmap (\x -> c{cinstInstallLibs = x}) (f (cinstInstallLibs c))
{-# INLINEABLE cinstInstallLibsLens #-}

cinstEnvironmentPathLens :: Lens' ClientInstallFlags (Flag FilePath)
cinstEnvironmentPathLens f c = fmap (\x -> c{cinstEnvironmentPath = x}) (f (cinstEnvironmentPath c))
{-# INLINEABLE cinstEnvironmentPathLens #-}

cinstOverwritePolicyLens :: Lens' ClientInstallFlags (Flag OverwritePolicy)
cinstOverwritePolicyLens f c = fmap (\x -> c{cinstOverwritePolicy = x}) (f (cinstOverwritePolicy c))
{-# INLINEABLE cinstOverwritePolicyLens #-}

cinstInstallMethodLens :: Lens' ClientInstallFlags (Flag InstallMethod)
cinstInstallMethodLens f c = fmap (\x -> c{cinstInstallMethod = x}) (f (cinstInstallMethod c))
{-# INLINEABLE cinstInstallMethodLens #-}

cinstInstalldirLens :: Lens' ClientInstallFlags (Flag FilePath)
cinstInstalldirLens f c = fmap (\x -> c{cinstInstalldir = x}) (f (cinstInstalldir c))
{-# INLINEABLE cinstInstalldirLens #-}
