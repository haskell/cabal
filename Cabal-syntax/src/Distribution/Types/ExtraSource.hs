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
  , extraSourceOpts :: String
  -- ^ The text between the parentheses, verbatim. It is split into individual
  -- options by @splitArgs@ where it is used, so that these options obey the
  -- same quoting rules as @--PROG-options@ on the command line. Empty when the
  -- entry carries no options at all.
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
    mopts <- P.optional parsecExtraSourceOpts
    opts <- maybe (pure "") (<$ versionGuardExtraSourceOpts) mopts
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

-- | Parse the parenthesised per-file options, taking the text between the
-- parentheses verbatim.
--
-- @(@ and @)@ are structural: they may nest, as long as they balance, so
-- @-DX=f(1)@ needs no escaping. A backslash escapes @(@, @)@ and @\\@; any
-- other backslash stands for itself, so a Windows path such as @C:\\foo\\bar@
-- needs no doubling. In particular @\\"@ is passed through untouched, for
-- @splitArgs@ to interpret later.
parsecExtraSourceOpts :: (P.CharParsing m, Monad m) => m String
parsecExtraSourceOpts = P.char '(' *> go (0 :: Int) <* P.spaces
  where
    go depth = do
      chunk <- P.munch (\c -> c /= '(' && c /= ')' && c /= '\\')
      let continue d s = ((chunk ++ s) ++) <$> go d
      c <- P.anyChar
      case c of
        '\\' -> do
          -- Never a parenthesis in the non-escape branch, so the nesting
          -- depth below stays accurate.
          e <- P.anyChar
          continue depth (if e `elem` "()\\" then [e] else ['\\', e])
        '(' -> continue (depth + 1) "("
        ')'
          | depth == 0 -> pure chunk
          | otherwise -> continue (depth - 1) ")"
        _ -> continue depth [c]

-- | Render the per-file options, escaping only when a verbatim copy would not
-- be read back as itself by 'parsecExtraSourceOpts'.
showExtraSourceOpts :: String -> PP.Doc
showExtraSourceOpts opts
  | verbatimSafe opts = PP.text opts
  | otherwise = PP.text (concat (zipWith esc opts (followedBy opts)))
  where
    -- Once we are escaping at all every parenthesis has to be escaped, but a
    -- backslash only where it would otherwise be read as escaping whatever
    -- comes after it.
    esc '\\' c
      | c `elem` "()\\" = "\\\\"
      | otherwise = "\\"
    esc c _
      | c `elem` "()" = ['\\', c]
      | otherwise = [c]

-- | Whether 'showExtraSourceOpts' can print this text as it stands: the
-- parentheses have to balance, and no backslash may sit in front of a
-- character the parser would treat as escaped.
verbatimSafe :: String -> Bool
verbatimSafe opts =
  balanced (0 :: Int) opts && not (or (zipWith risky opts (followedBy opts)))
  where
    risky '\\' c = c `elem` "()\\"
    risky _ _ = False
    balanced d [] = d == 0
    balanced d ('(' : cs) = balanced (d + 1) cs
    balanced 0 (')' : _) = False
    balanced d (')' : cs) = balanced (d - 1) cs
    balanced d (_ : cs) = balanced d cs

-- | The character each character is followed by once rendered. The last one is
-- followed by the closing parenthesis 'pretty' appends, which is why a
-- backslash at the very end still has to be escaped.
followedBy :: String -> String
followedBy opts = drop 1 opts ++ ")"

instance Pretty ExtraSource where
  pretty (ExtraSource path opts)
    | null opts = pretty (SymbolicPathNT path)
    | otherwise = pretty (SymbolicPathNT path) <+> PP.parens (showExtraSourceOpts opts)

extraSourceFromPath :: SymbolicPath Pkg File -> ExtraSource
extraSourceFromPath fp = ExtraSource fp mempty
