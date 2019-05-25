{-# LANGUAGE DeriveGeneric #-}
module Distribution.Simple.InstallDirs.Internal
  ( PathComponent(..)
  , PathTemplateVariable(..)
  ) where

import Prelude ()
import Distribution.Compat.Prelude

data PathComponent =
       Ordinary FilePath
     | Variable PathTemplateVariable
     deriving (Eq, Ord, Generic)

instance Binary PathComponent

data PathTemplateVariable =
       PrefixVar     -- ^ The @$prefix@ path variable
     | BindirVar     -- ^ The @$bindir@ path variable
     | LibdirVar     -- ^ The @$libdir@ path variable
     | LibsubdirVar  -- ^ The @$libsubdir@ path variable
     | DynlibdirVar  -- ^ The @$dynlibdir@ path variable
     | DatadirVar    -- ^ The @$datadir@ path variable
     | DatasubdirVar -- ^ The @$datasubdir@ path variable
     | DocdirVar     -- ^ The @$docdir@ path variable
     | HtmldirVar    -- ^ The @$htmldir@ path variable
     | PkgNameVar    -- ^ The @$pkg@ package name path variable
     | PkgVerVar     -- ^ The @$version@ package version path variable
     | PkgIdVar      -- ^ The @$pkgid@ package Id path variable, eg @foo-1.0@
     | LibNameVar    -- ^ The @$libname@ path variable
     | CompilerVar   -- ^ The compiler name and version, eg @ghc-6.6.1@
     | OSVar         -- ^ The operating system name, eg @windows@ or @linux@
     | ArchVar       -- ^ The CPU architecture name, eg @i386@ or @x86_64@
     | AbiVar        -- ^ The compiler's ABI identifier,
                     ---  $arch-$os-$compiler-$abitag
     | AbiTagVar     -- ^ The optional ABI tag for the compiler
     | ExecutableNameVar -- ^ The executable name; used in shell wrappers
     | TestSuiteNameVar   -- ^ The name of the test suite being run
     | TestSuiteResultVar -- ^ The result of the test suite being run, eg
                          -- @pass@, @fail@, or @error@.
     | BenchmarkNameVar   -- ^ The name of the benchmark being run
  deriving (Eq, Ord, Generic)

instance Binary PathTemplateVariable

instance Show PathTemplateVariable where
  show PrefixVar     = "prefix"
  show LibNameVar    = "libname"
  show BindirVar     = "bindir"
  show LibdirVar     = "libdir"
  show LibsubdirVar  = "libsubdir"
  show DynlibdirVar  = "dynlibdir"
  show DatadirVar    = "datadir"
  show DatasubdirVar = "datasubdir"
  show DocdirVar     = "docdir"
  show HtmldirVar    = "htmldir"
  show PkgNameVar    = "pkg"
  show PkgVerVar     = "version"
  show PkgIdVar      = "pkgid"
  show CompilerVar   = "compiler"
  show OSVar         = "os"
  show ArchVar       = "arch"
  show AbiTagVar     = "abitag"
  show AbiVar        = "abi"
  show ExecutableNameVar = "executablename"
  show TestSuiteNameVar   = "test-suite"
  show TestSuiteResultVar = "result"
  show BenchmarkNameVar   = "benchmark"

instance Read PathTemplateVariable where
  readsPrec _ s =
    take 1
    [ (var, drop (length varStr) s)
    | (varStr, var) <- vars
    , varStr `isPrefixOf` s ]
    -- NB: order matters! Longer strings first
    where vars = [("prefix",     PrefixVar)
                 ,("bindir",     BindirVar)
                 ,("libdir",     LibdirVar)
                 ,("libsubdir",  LibsubdirVar)
                 ,("dynlibdir",  DynlibdirVar)
                 ,("datadir",    DatadirVar)
                 ,("datasubdir", DatasubdirVar)
                 ,("docdir",     DocdirVar)
                 ,("htmldir",    HtmldirVar)
                 ,("pkgid",      PkgIdVar)
                 ,("libname",    LibNameVar)
                 ,("pkgkey",     LibNameVar) -- backwards compatibility
                 ,("pkg",        PkgNameVar)
                 ,("version",    PkgVerVar)
                 ,("compiler",   CompilerVar)
                 ,("os",         OSVar)
                 ,("arch",       ArchVar)
                 ,("abitag",     AbiTagVar)
                 ,("abi",        AbiVar)
                 ,("executablename", ExecutableNameVar)
                 ,("test-suite", TestSuiteNameVar)
                 ,("result", TestSuiteResultVar)
                 ,("benchmark", BenchmarkNameVar)]

instance Show PathComponent where
  show (Ordinary path) = path
  show (Variable var)  = '$':show var
  showList = foldr (\x -> (shows x .)) id

instance Read PathComponent where
  -- for some reason we collapse multiple $ symbols here
  readsPrec _ = lex0
    where lex0 [] = []
          lex0 ('$':'$':s') = lex0 ('$':s')
          lex0 ('$':s') = case [ (Variable var, s'')
                               | (var, s'') <- reads s' ] of
                            [] -> lex1 "$" s'
                            ok -> ok
          lex0 s' = lex1 [] s'
          lex1 ""  ""      = []
          lex1 acc ""      = [(Ordinary (reverse acc), "")]
          lex1 acc ('$':'$':s) = lex1 acc ('$':s)
          lex1 acc ('$':s) = [(Ordinary (reverse acc), '$':s)]
          lex1 acc (c:s)   = lex1 (c:acc) s
  readList [] = [([],"")]
  readList s  = [ (component:components, s'')
                | (component, s') <- reads s
                , (components, s'') <- readList s' ]
