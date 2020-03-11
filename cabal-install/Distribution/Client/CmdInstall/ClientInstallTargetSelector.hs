module Distribution.Client.CmdInstall.ClientInstallTargetSelector (
    WithoutProjectTargetSelector (..),
    parseWithoutProjectTargetSelector,
    woPackageNames,
    woPackageTargets,
    woPackageSpecifiers,
    ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Client.TargetSelector
import Distribution.Client.Types
import Distribution.Compat.CharParsing             (char, optional)
import Distribution.Package
import Distribution.Parsec
import Distribution.Simple.LocalBuildInfo          (ComponentName (CExeName))
import Distribution.Simple.Utils                   (die')
import Distribution.Solver.Types.PackageConstraint (PackageProperty (..))
import Distribution.Verbosity                      (Verbosity)
import Distribution.Version

data WithoutProjectTargetSelector
    = WoPackageId PackageId
    | WoPackageComponent PackageId ComponentName
    -- | WoURI URI
  deriving (Show)

parseWithoutProjectTargetSelector :: Verbosity -> String -> IO WithoutProjectTargetSelector
parseWithoutProjectTargetSelector verbosity input =
    case explicitEitherParsec parser input of
        Right ts -> return ts
        Left err -> die' verbosity $ "Invalid package ID: " ++ input ++ "\n" ++ err
  where
    parser :: ParsecParser WithoutProjectTargetSelector
    parser = do
        pid <- parsec
        cn  <- optional (char ':' *> parsec)
        return $ case cn of
            Nothing -> WoPackageId pid
            Just cn' -> WoPackageComponent pid (CExeName cn')

woPackageNames  :: WithoutProjectTargetSelector -> [PackageName]
woPackageNames (WoPackageId pid)          = [pkgName pid]
woPackageNames (WoPackageComponent pid _) = [pkgName pid]

woPackageTargets  :: WithoutProjectTargetSelector -> TargetSelector
woPackageTargets (WoPackageId pid) =
    TargetPackageNamed (pkgName pid) Nothing
woPackageTargets (WoPackageComponent pid cn) =
    TargetComponentUnknown (pkgName pid) (Right cn) WholeComponent

woPackageSpecifiers  :: WithoutProjectTargetSelector -> PackageSpecifier pkg
woPackageSpecifiers (WoPackageId pid)          = pidPackageSpecifiers pid
woPackageSpecifiers (WoPackageComponent pid _) = pidPackageSpecifiers pid

pidPackageSpecifiers :: PackageId -> PackageSpecifier pkg
pidPackageSpecifiers pid
    | pkgVersion pid == nullVersion = NamedPackage (pkgName pid) []
    | otherwise                     = NamedPackage (pkgName pid)
        [ PackagePropertyVersion (thisVersion (pkgVersion pid))
        ]
