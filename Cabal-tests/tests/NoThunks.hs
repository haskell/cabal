{-# LANGUAGE CPP                 #-}
#if !(__GLASGOW_HASKELL__ >= 806 && defined(MIN_VERSION_nothunks))
module Main (main) where
main :: IO ()
main = putStrLn "Old GHC, no nothunks"
#else

{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Main (main) where

import Control.Applicative                    ((<|>))
import Data.Foldable                          (toList)
import Data.Proxy                             (Proxy (..))
import Data.Typeable                          (Typeable, typeRep)
import Distribution.CabalSpecVersion          (CabalSpecVersion)
import Distribution.Compat.NonEmptySet        (NonEmptySet)
import Distribution.Compiler                  (CompilerFlavor, PerCompilerFlavor)
import Distribution.Fields                    (runParseResult)
import Distribution.ModuleName                (ModuleName)
import Distribution.PackageDescription.Parsec (parseGenericPackageDescription)
import Distribution.SPDX                      (License, LicenseExceptionId, LicenseExpression, LicenseId, LicenseRef, SimpleLicenseExpression)
import Distribution.System                    (Arch, OS)
import Distribution.Utils.Path                (SymbolicPathX)
import Distribution.Utils.ShortText           (ShortText)
import Distribution.Version                   (Version, VersionRange)
import Language.Haskell.Extension             (Extension, KnownExtension, Language)
import NoThunks.Class                         (NoThunks (..), OnlyCheckWhnf (..), noThunksInValues)
import Test.Tasty                             (defaultMain, testGroup)
import Test.Tasty.HUnit                       (assertFailure, testCase)

import Distribution.PackageDescription

import qualified Data.ByteString      as BS
import qualified Distribution.License as License

main :: IO ()
main = defaultMain $ testGroup "nothunks"
    [ testCase "parsing Cabal.cabal" noThunksParse
    ]

noThunksParse :: IO ()
noThunksParse = do
    bs <- BS.readFile "Cabal/Cabal.cabal" <|> BS.readFile "../Cabal/Cabal.cabal"
    let res = parseGenericPackageDescription bs
    gpd <- either (assertFailure . show) return $ snd $
        runParseResult res

    noThunks [] gpd >>= \case
        -- ok
        Nothing -> return ()

        -- found thunk
        Just info -> assertFailure $ "Thunk in " ++ show info

-------------------------------------------------------------------------------
-- NoThunks instances
-------------------------------------------------------------------------------

instance NoThunks Arch
instance NoThunks Benchmark
instance NoThunks BenchmarkInterface
instance NoThunks BenchmarkType
instance NoThunks BuildInfo
instance NoThunks BuildType
instance NoThunks CabalSpecVersion
instance NoThunks CompilerFlavor
instance NoThunks ConfVar
instance NoThunks Dependency
instance NoThunks Executable
instance NoThunks ExecutableScope
instance NoThunks FlagName
instance NoThunks ForeignLib
instance NoThunks ForeignLibOption
instance NoThunks ModuleReexport
instance NoThunks LibraryVisibility
instance NoThunks ForeignLibType
instance NoThunks GenericPackageDescription
instance NoThunks KnownRepoType
instance NoThunks Library
instance NoThunks LibraryName
instance NoThunks Mixin
instance NoThunks License
instance NoThunks License.License
instance NoThunks LicenseExpression
instance NoThunks LicenseRef
instance NoThunks ModuleName
instance NoThunks OS
instance NoThunks PackageDescription
instance NoThunks PackageFlag
instance NoThunks PackageIdentifier
instance NoThunks PackageName
instance NoThunks LegacyExeDependency
instance NoThunks ExeDependency
instance NoThunks PkgconfigName
instance NoThunks PkgconfigDependency
instance NoThunks PkgconfigVersion
instance NoThunks PkgconfigVersionRange
instance NoThunks LibVersionInfo
instance NoThunks RepoKind
instance NoThunks RepoType
instance NoThunks Extension
instance NoThunks Language
instance NoThunks SetupBuildInfo
instance NoThunks SimpleLicenseExpression
instance NoThunks KnownExtension
instance NoThunks SourceRepo
instance NoThunks IncludeRenaming
instance NoThunks ModuleRenaming
instance NoThunks TestSuite
instance NoThunks TestSuiteInterface
instance NoThunks TestType
instance NoThunks UnqualComponentName
instance NoThunks Version
instance NoThunks VersionRange

instance NoThunks ShortText where

instance NoThunks a => NoThunks (PerCompilerFlavor a)

instance (Typeable allowAbs, Typeable from, Typeable to)
       => NoThunks (SymbolicPathX allowAbs from to)

deriving via (OnlyCheckWhnf LicenseId) instance NoThunks LicenseId
deriving via (OnlyCheckWhnf LicenseExceptionId) instance NoThunks LicenseExceptionId
deriving via (CheckFoldableNamed NonEmptySet a) instance NoThunks a => NoThunks (NonEmptySet a)

instance (NoThunks v, NoThunks c, NoThunks a) => NoThunks (CondTree v c a)
instance (NoThunks v, NoThunks c, NoThunks a) => NoThunks (CondBranch v c a)
instance (NoThunks c) => NoThunks (Condition c)

-------------------------------------------------------------------------------
-- NoThunks helpers
-------------------------------------------------------------------------------

newtype CheckFoldableNamed f a = CheckFoldableNamed (f a)

instance (NoThunks a, Foldable f, Typeable f) => NoThunks (CheckFoldableNamed f a) where
    showTypeOf _ = show (typeRep (Proxy :: Proxy f))
    wNoThunks ctxt (CheckFoldableNamed xs) = noThunksInValues ctxt (toList xs)

#endif
