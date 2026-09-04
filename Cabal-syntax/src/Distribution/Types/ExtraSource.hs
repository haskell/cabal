{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Distribution.Types.ExtraSource
  ( ExtraSource (..)
  , extraSourceFromPath
  , extraSourceOptsSpecVersion
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.CabalSpecVersion (CabalSpecVersion (..), showCabalSpecVersion)
import Distribution.Parsec
import Distribution.Pretty
import Distribution.Utils.Path (FileOrDir (..), Pkg, SymbolicPath)

import qualified Distribution.Compat.CharParsing as P
import Distribution.FieldGrammar.Newtypes (SymbolicPathNT (..))
import qualified Text.PrettyPrint as PP

-- | An entry in one of the extra-source fields (@c-sources@, @cxx-sources@,
-- @asm-sources@, @cmm-sources@, @js-sources@): a source file together with
-- any per-file options to pass to the compiler for that file.
data ExtraSource = ExtraSource
  { extraSourceFile :: SymbolicPath Pkg File
  , extraSourceOpts :: [String]
  }
  deriving (Generic, Show, Read, Eq, Ord, Data)

instance Binary ExtraSource
instance Structured ExtraSource
instance NFData ExtraSource

-- | Per-file options, spelled @file.c (-opt1 -opt2)@, are only recognised from
-- @cabal-version: 3.20@ onwards; below that they are a parse error, so that a
-- @.cabal@ file cannot express something an older 'Cabal' would misread (this
-- is the class of bug described in
-- <https://github.com/haskell/cabal/issues/9331>). An older 'Cabal' does not
-- reject the syntax: a path is any non-space token, so @Cabal-syntax-3.14@
-- reads @foo.c (-DFOO -O2) bar.c@ as four source files and only fails when it
-- tries to compile them. 3.20 is the specification version under development;
-- 3.18 is already published, so a released 'Cabal' 3.18 would misread the
-- syntax and cannot serve as the gate.
extraSourceOptsSpecVersion :: CabalSpecVersion
extraSourceOptsSpecVersion = CabalSpecV3_20

instance Parsec ExtraSource where
  parsec = do
    SymbolicPathNT path <- parsec <* P.spaces
    -- Always consume any parenthesised options so that they are not mistaken
    -- for a second file name; whether they are allowed at all depends on the
    -- spec version.
    mopts <- P.optional (parensLax (P.sepBy parsecExtraSourceOpt P.spaces))
    opts <- case mopts of
      Nothing -> pure []
      Just os -> do
        versionGuardExtraSourceOpts
        pure os
    return (ExtraSource path opts)

versionGuardExtraSourceOpts :: CabalParsing m => m ()
versionGuardExtraSourceOpts = do
  csv <- askCabalSpecVersion
  when (csv < extraSourceOptsSpecVersion) $
    fail $
      unwords
        [ "Per-file options on extra source files used."
        , "To use this syntax the package needs to specify at least 'cabal-version: "
            ++ showCabalSpecVersion extraSourceOptsSpecVersion
            ++ "'."
        ]

-- | Parse a single per-file option.
--
-- Options are separated by whitespace and terminated by the closing paren, so
-- an option that itself contains whitespace, a @)@ or a @\"@ has to be written
-- as a Haskell string literal. 'showExtraSourceOpt' renders it back that way.
parsecExtraSourceOpt :: CabalParsing m => m String
parsecExtraSourceOpt =
  parsecHaskellString
    <|> (P.munch1 (\c -> not (isSpace c) && c /= ')' && c /= '"') P.<?> "extra source option")

-- | Render a single per-file option, quoting it when 'parsecExtraSourceOpt'
-- would not read it back verbatim.
showExtraSourceOpt :: String -> PP.Doc
showExtraSourceOpt opt
  | null opt || any dodgy opt = PP.text (show opt)
  | otherwise = PP.text opt
  where
    dodgy c = isSpace c || c == ')' || c == '"'

parensLax :: P.CharParsing m => m a -> m a
parensLax p = P.between (P.char '(' *> P.spaces) (P.char ')' *> P.spaces) p

instance Pretty ExtraSource where
  pretty (ExtraSource path []) = pretty (SymbolicPathNT path)
  pretty (ExtraSource path opts) =
    pretty (SymbolicPathNT path) <+> PP.parens (PP.hsep (map showExtraSourceOpt opts))

extraSourceFromPath :: SymbolicPath Pkg File -> ExtraSource
extraSourceFromPath fp = ExtraSource fp mempty
