{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Simple.InstallDirs.Internal
  ( PathComponent (..)
  , PathTemplateVariable (..)
  ) where

import Distribution.Compat.Prelude
import Prelude ()

data PathComponent
  = Ordinary FilePath
  | Variable PathTemplateVariable
  deriving (Eq, Ord, Generic, Typeable)

instance Binary PathComponent
instance Structured PathComponent

data PathTemplateVariable
  = -- | The @$prefix@ path variable
    PrefixVar
  | -- | The @$bindir@ path variable
    BindirVar
  | -- | The @$libdir@ path variable
    LibdirVar
  | -- | The @$libsubdir@ path variable
    LibsubdirVar
  | -- | The @$dynlibdir@ path variable
    DynlibdirVar
  | -- | The @$datadir@ path variable
    DatadirVar
  | -- | The @$datasubdir@ path variable
    DatasubdirVar
  | -- | The @$docdir@ path variable
    DocdirVar
  | -- | The @$htmldir@ path variable
    HtmldirVar
  | -- | The @$pkg@ package name path variable
    PkgNameVar
  | -- | The @$version@ package version path variable
    PkgVerVar
  | -- | The @$pkgid@ package Id path variable, eg @foo-1.0@
    PkgIdVar
  | -- | The @$libname@ path variable
    LibNameVar
  | -- | The compiler name and version, eg @ghc-6.6.1@
    CompilerVar
  | -- | The operating system name, eg @windows@ or @linux@
    OSVar
  | -- | The CPU architecture name, eg @i386@ or @x86_64@
    ArchVar
  | -- | The compiler's ABI identifier,
    AbiVar
  | ---  $arch-$os-$compiler-$abitag

    -- | The optional ABI tag for the compiler
    AbiTagVar
  | -- | The executable name; used in shell wrappers
    ExecutableNameVar
  | -- | The name of the test suite being run
    TestSuiteNameVar
  | -- | The result of the test suite being run, eg
    -- @pass@, @fail@, or @error@.
    TestSuiteResultVar
  | -- | The name of the benchmark being run
    BenchmarkNameVar
  deriving (Eq, Ord, Generic, Typeable)

instance Binary PathTemplateVariable
instance Structured PathTemplateVariable

instance Show PathTemplateVariable where
  show PrefixVar = "prefix"
  show LibNameVar = "libname"
  show BindirVar = "bindir"
  show LibdirVar = "libdir"
  show LibsubdirVar = "libsubdir"
  show DynlibdirVar = "dynlibdir"
  show DatadirVar = "datadir"
  show DatasubdirVar = "datasubdir"
  show DocdirVar = "docdir"
  show HtmldirVar = "htmldir"
  show PkgNameVar = "pkg"
  show PkgVerVar = "version"
  show PkgIdVar = "pkgid"
  show CompilerVar = "compiler"
  show OSVar = "os"
  show ArchVar = "arch"
  show AbiTagVar = "abitag"
  show AbiVar = "abi"
  show ExecutableNameVar = "executablename"
  show TestSuiteNameVar = "test-suite"
  show TestSuiteResultVar = "result"
  show BenchmarkNameVar = "benchmark"

instance Read PathTemplateVariable where
  readsPrec _ s =
    take
      1
      [ (var, drop (length varStr) s)
      | (varStr, var) <- vars
      , varStr `isPrefixOf` s
      ]
    where
      -- NB: order matters! Longer strings first
      vars =
        [ ("prefix", PrefixVar)
        , ("bindir", BindirVar)
        , ("libdir", LibdirVar)
        , ("libsubdir", LibsubdirVar)
        , ("dynlibdir", DynlibdirVar)
        , ("datadir", DatadirVar)
        , ("datasubdir", DatasubdirVar)
        , ("docdir", DocdirVar)
        , ("htmldir", HtmldirVar)
        , ("pkgid", PkgIdVar)
        , ("libname", LibNameVar)
        , ("pkgkey", LibNameVar) -- backwards compatibility
        , ("pkg", PkgNameVar)
        , ("version", PkgVerVar)
        , ("compiler", CompilerVar)
        , ("os", OSVar)
        , ("arch", ArchVar)
        , ("abitag", AbiTagVar)
        , ("abi", AbiVar)
        , ("executablename", ExecutableNameVar)
        , ("test-suite", TestSuiteNameVar)
        , ("result", TestSuiteResultVar)
        , ("benchmark", BenchmarkNameVar)
        ]

instance Show PathComponent where
  show (Ordinary path) = path
  show (Variable var) = '$' : show var
  showList = foldr (\x -> (shows x .)) id

instance Read PathComponent where
  -- for some reason we collapse multiple $ symbols here
  readsPrec _ = lex0
    where
      lex0 [] = []
      lex0 ('$' : '$' : s') = lex0 ('$' : s')
      lex0 ('$' : s') = case [ (Variable var, s'')
                             | (var, s'') <- reads s'
                             ] of
        [] -> lex1 "$" s'
        ok -> ok
      lex0 s' = lex1 [] s'
      lex1 "" "" = []
      lex1 acc "" = [(Ordinary (reverse acc), "")]
      lex1 acc ('$' : '$' : s) = lex1 acc ('$' : s)
      lex1 acc ('$' : s) = [(Ordinary (reverse acc), '$' : s)]
      lex1 acc (c : s) = lex1 (c : acc) s
  readList [] = [([], "")]
  readList s =
    [ (component : components, s'')
    | (component, s') <- reads s
    , (components, s'') <- readList s'
    ]
