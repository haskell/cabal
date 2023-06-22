{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Distribution.Utils.GrammarRegex
  ( -- * Regular expressions
    GrammarRegex (..)
  , reEps
  , reChar
  , reChars
  , reMunchCS
  , reMunch1CS

    -- * Variables
  , reVar0
  , reVar1

    -- * Pretty-printing
  , regexDoc
  ) where

import Data.Char (isAlphaNum, isControl, ord)
import Data.Foldable (Foldable)
import Data.Maybe (fromMaybe)
import Data.Monoid (Monoid (..))
import Data.String (IsString (..))
import Data.Traversable (Traversable)
import Data.Void (Void, vacuous)
import Distribution.Compat.Semigroup (Semigroup (..))
import Prelude (Bool (..), Char, Eq (..), Functor, Int, Maybe (..), Ord (..), Show, String, fmap, length, map, otherwise, ($), (++), (.))

import qualified Distribution.Utils.CharSet as CS
import qualified Text.PrettyPrint as PP

(<<>>) :: PP.Doc -> PP.Doc -> PP.Doc
(<<>>) = (PP.<>)

-------------------------------------------------------------------------------
-- GrammarRegex
-------------------------------------------------------------------------------

-- | Recursive regular expressions tuned for 'Described' use-case.
data GrammarRegex a
  = -- | append @ab@
    REAppend [GrammarRegex a]
  | -- | union @a|b@
    REUnion [GrammarRegex a]
  | -- repetition

    -- | star @a*@, with a separator
    REMunch (GrammarRegex a) (GrammarRegex a)
  | -- | plus @a+@, with a separator
    REMunch1 (GrammarRegex a) (GrammarRegex a)
  | -- | 1-n, with a separator
    REMunchR Int (GrammarRegex a) (GrammarRegex a)
  | -- | optional @r?@
    REOpt (GrammarRegex a)
  | -- | literal string @abcd@
    REString String
  | -- | charset @[:alnum:]@
    RECharSet CS.CharSet
  | -- | variable
    REVar a
  | -- | named expression
    RENamed String (GrammarRegex a)
  | -- | recursive expressions
    RERec String (GrammarRegex (Maybe a))
  | -- cabal syntax specifics

    -- | zero-or-more spaces
    RESpaces
  | -- | one-or-more spaces
    RESpaces1
  | -- | comma list (note, leading or trailing commas)
    RECommaList (GrammarRegex a)
  | -- | comma non-empty list (note, leading or trailing commas)
    RECommaNonEmpty (GrammarRegex a)
  | -- | opt comma list
    REOptCommaList (GrammarRegex a)
  | -- | unspecified
    RETodo
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance IsString (GrammarRegex a) where
  fromString = REString

instance Semigroup (GrammarRegex a) where
  x <> y = REAppend (unAppend x ++ unAppend y)
    where
      unAppend (REAppend rs) = rs
      unAppend r = [r]

instance Monoid (GrammarRegex a) where
  mempty = REAppend []
  mappend = (<>)

-------------------------------------------------------------------------------
-- Smart constructors
-------------------------------------------------------------------------------

reEps :: GrammarRegex a
reEps = REAppend []

reChar :: Char -> GrammarRegex a
reChar = RECharSet . CS.singleton

reChars :: [Char] -> GrammarRegex a
reChars = RECharSet . CS.fromList

reMunch1CS :: CS.CharSet -> GrammarRegex a
reMunch1CS = REMunch1 reEps . RECharSet

reMunchCS :: CS.CharSet -> GrammarRegex a
reMunchCS = REMunch reEps . RECharSet

-------------------------------------------------------------------------------
-- Variables
-------------------------------------------------------------------------------

reVar0 :: GrammarRegex (Maybe a)
reVar0 = REVar Nothing

reVar1 :: GrammarRegex (Maybe (Maybe a))
reVar1 = REVar (Just Nothing)

-------------------------------------------------------------------------------
-- Pretty-printing
-------------------------------------------------------------------------------

-- |
--
-- >>> regexDoc $ REString "True"
-- \mathop{\mathord{``}\mathtt{True}\mathord{"}}
--
-- Note: we don't simplify regexps yet:
--
-- >>> regexDoc $ REString "foo" <> REString "bar"
-- \mathop{\mathord{``}\mathtt{foo}\mathord{"}}\mathop{\mathord{``}\mathtt{bar}\mathord{"}}
regexDoc :: GrammarRegex Void -> PP.Doc
regexDoc = go 0 . vacuous
  where
    go :: Int -> GrammarRegex PP.Doc -> PP.Doc
    go _ (REAppend []) = ""
    go d (REAppend rs) = parensIf (d > 2) $ PP.hcat (map (go 2) rs)
    go d (REUnion [r]) = go d r
    go _ (REUnion rs) =
      PP.hsep
        [ "\\left\\{"
        , if length rs < 4
            then PP.hcat (PP.punctuate (PP.text "\\mid") (map (go 0) rs))
            else
              "\\begin{gathered}"
                <<>> PP.hcat (PP.punctuate "\\\\" (map (go 0) rs))
                <<>> "\\end{gathered}"
        , "\\right\\}"
        ]
    go d (REMunch sep r) =
      parensIf (d > 3) $
        PP.text "{" <<>> go 4 r <<>> PP.text "}^\\ast_{" <<>> go 4 sep <<>> PP.text "}"
    go d (REMunch1 sep r) =
      parensIf (d > 3) $
        PP.text "{" <<>> go 4 r <<>> PP.text "}^+_{" <<>> go 4 sep <<>> PP.text "}"
    go d (REMunchR n sep r) =
      parensIf (d > 3) $
        PP.text "{" <<>> go 4 r <<>> PP.text "}^{\\in [0\\ldots" <<>> PP.int n <<>> "]}_{" <<>> go 4 sep <<>> PP.text "}"
    go d (REOpt r) =
      parensIf (d > 3) $
        PP.text "{" <<>> go 4 r <<>> PP.text "}^?"
    go _ (REString s) = PP.text "\\mathop{\\mathord{``}\\mathtt{" <<>> PP.hcat (map charDoc s) <<>> PP.text "}\\mathord{\"}}"
    go _ (RECharSet cs) = charsetDoc cs
    go _ RESpaces = "\\circ"
    go _ RESpaces1 = "\\bullet"
    go _ (RECommaList r) =
      "\\mathrm{commalist}" <<>> go 4 r
    go _ (RECommaNonEmpty r) =
      "\\mathrm{commanonempty}" <<>> go 4 r
    go _ (REOptCommaList r) =
      "\\mathrm{optcommalist}" <<>> go 4 r
    go _ (REVar a) = a
    go _ (RENamed n _) = terminalDoc n
    go d (RERec n r) =
      parensIf (d > 0) $
        "\\mathbf{fix}\\;"
          <<>> n'
          <<>> "\\;\\mathbf{in}\\;"
          <<>> go 0 (fmap (fromMaybe n') r)
      where
        n' = terminalDoc n
    go _ RETodo = PP.text "\\mathsf{\\color{red}{TODO}}"

    parensIf :: Bool -> PP.Doc -> PP.Doc
    parensIf True d = PP.text "\\left(" <<>> d <<>> PP.text "\\right)"
    parensIf False d = d

terminalDoc :: String -> PP.Doc
terminalDoc s = PP.text "\\mathop{\\mathit{" <<>> PP.hcat (map charDoc s) <<>> PP.text "}}"

charDoc :: Char -> PP.Doc
charDoc ' ' = PP.text "\\ "
charDoc '{' = PP.text "\\{"
charDoc '}' = PP.text "\\}"
charDoc '\\' = PP.text "\\text{\\\\}"
charDoc c
  | isAlphaNum c = PP.char c
  | isControl c = PP.int (ord c) -- TODO: some syntax
  | otherwise = PP.text ("\\text{" ++ c : "}")

inquotes :: PP.Doc -> PP.Doc
inquotes d = "\\mathop{\\mathord{``}" <<>> d <<>> "\\mathord{\"}}"

mathtt :: PP.Doc -> PP.Doc
mathtt d = "\\mathtt{" <<>> d <<>> "}"

charsetDoc :: CS.CharSet -> PP.Doc
charsetDoc acs
  | acs == CS.alpha = terminalDoc "alpha"
  | acs == CS.alphanum = terminalDoc "alpha-num"
  | acs == CS.upper = terminalDoc "upper"
charsetDoc acs = case CS.toIntervalList acs of
  [] -> "\\emptyset"
  [(x, y)] | x == y -> inquotes $ mathtt $ charDoc x
  rs
    | CS.size acs <= CS.size notAcs ->
        PP.brackets $ PP.hcat $ map rangeDoc rs
    | otherwise ->
        PP.braces $ PP.brackets (PP.hcat $ map rangeDoc (CS.toIntervalList notAcs)) <<>> PP.text "^c"
  where
    notAcs = CS.complement acs

    rangeDoc :: (Char, Char) -> PP.Doc
    rangeDoc (x, y)
      | x == y = inquotes (mathtt $ charDoc x)
      | otherwise = inquotes (mathtt $ charDoc x) <<>> PP.text "\\cdots" <<>> inquotes (mathtt $ charDoc y)
