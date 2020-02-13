{-# LANGUAGE DeriveFoldable      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE DeriveTraversable   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Distribution.Utils.Regex (
    -- * Regular expressions
    Regex (..),
    reEps,
    reChar,
    reChars,
    reMunchCS,
    reMunch1CS,
    -- * Variables
    reVar0,
    reVar1,
    -- * Pretty-printing
    regexDoc,
    ) where

import Data.Char                   (isControl)
import Distribution.Compat.Prelude
import Prelude ()

import qualified Distribution.Utils.CharSet as CS
import qualified Text.PrettyPrint           as PP

-------------------------------------------------------------------------------
-- Regex
-------------------------------------------------------------------------------

-- | Recursive regular expressions tuned for 'Described' use-case.
data Regex a
    = REAppend  [Regex a]                 -- ^ append @ab@
    | REUnion   [Regex a]                 -- ^ union @a|b@

    -- repetition
    | REMunch   (Regex a) (Regex a)       -- ^ star @a*@, with a separator
    | REMunch1  (Regex a) (Regex a)       -- ^ plus @a+@, with a separator
    | REMunchR Int (Regex a) (Regex a)    -- ^ 1-n, with a separator
    | REOpt     (Regex a)                 -- ^ optional @r?@

    | REString  String                    -- ^ literal string @abcd@
    | RECharSet CS.CharSet           -- ^ charset @[:alnum:]@
    | REVar     a                         -- ^ variable
    | RENamed   String (Regex a)          -- ^ named expression
    | RERec     String (Regex (Maybe a))  -- ^ recursive expressions

    -- cabal syntax specifics
    | RESpaces                            -- ^ zero-or-more spaces
    | RESpaces1                           -- ^ one-or-more spaces
    | RECommaList (Regex a)               -- ^ comma list (note, leading or trailing commas)
    | REOptCommaList (Regex a)            -- ^ opt comma list

    | RETodo                              -- ^ unspecified
  deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

-------------------------------------------------------------------------------
-- Instances
-------------------------------------------------------------------------------

instance IsString (Regex a)  where
    fromString = REString

instance Semigroup (Regex a) where
    x <> y = REAppend (unAppend x ++ unAppend y) where
        unAppend (REAppend rs) = rs
        unAppend r             = [r]

instance Monoid (Regex a) where
    mempty = REAppend []
    mappend = (<>)

-------------------------------------------------------------------------------
-- Smart constructors
-------------------------------------------------------------------------------

reEps :: Regex a
reEps = REAppend []

reChar :: Char -> Regex a
reChar = RECharSet . CS.singleton

reChars :: [Char] -> Regex a
reChars = RECharSet . CS.fromList

reMunch1CS :: CS.CharSet -> Regex a
reMunch1CS = REMunch1 reEps . RECharSet

reMunchCS :: CS.CharSet -> Regex a
reMunchCS = REMunch reEps . RECharSet

-------------------------------------------------------------------------------
-- Variables
-------------------------------------------------------------------------------

reVar0 :: Regex (Maybe a)
reVar0 = REVar Nothing

reVar1 :: Regex (Maybe (Maybe a))
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
--
regexDoc :: Regex Void -> PP.Doc
regexDoc = go 0 . vacuous where
    go :: Int -> Regex PP.Doc -> PP.Doc
    go _ (REAppend [])    = ""
    go d (REAppend rs)    = parensIf (d > 2) $ PP.hcat (map (go 2) rs)
    go d (REUnion [r])    = go d r
    go _ (REUnion rs)     = PP.hsep
        [ "\\left\\{"
        , if length rs < 4
          then PP.hcat (PP.punctuate (PP.text "\\mid") (map (go 0) rs))
          else "\\begin{gathered}" <<>>
               PP.hcat (PP.punctuate "\\\\" (map (go 0) rs)) <<>>
              "\\end{gathered}"
        , "\\right\\}" ]

    go d (REMunch sep r)  = parensIf (d > 3) $
        PP.text "{" <<>> go 4 r <<>> PP.text "}^\\ast_{" <<>> go 4 sep <<>> PP.text "}"
    go d (REMunch1 sep r) = parensIf (d > 3) $
        PP.text "{" <<>> go 4 r <<>> PP.text "}^+_{" <<>> go 4 sep <<>> PP.text "}"
    go d (REMunchR n sep r) = parensIf (d > 3) $
        PP.text "{" <<>> go 4 r <<>> PP.text "}^{\\in [0\\ldots" <<>> PP.int n <<>> "]}_{" <<>> go 4 sep <<>> PP.text "}"
    go d (REOpt r)        = parensIf (d > 3) $
        PP.text "{" <<>> go 4 r <<>> PP.text "}^?"

    go _ (REString s)     = PP.text "\\mathop{\\mathord{``}\\mathtt{" <<>> PP.hcat (map charDoc s) <<>> PP.text "}\\mathord{\"}}"
    go _ (RECharSet cs)   = charsetDoc cs

    go _ RESpaces         = "\\circ"
    go _ RESpaces1        = "\\bullet"

    go _ (RECommaList r)  =
        "\\mathrm{commalist}" <<>> go 4 r
    go _ (REOptCommaList r) =
        "\\mathrm{optcommalist}" <<>> go 4 r

    go _ (REVar a)         = a
    go _ (RENamed n _)     = terminalDoc n
    go d (RERec n r)       = parensIf (d > 0) $
        "\\mathbf{fix}\\;" <<>> n' <<>> "\\;\\mathbf{in}\\;" <<>>
        go 0 (fmap (fromMaybe n') r)
      where
        n' = terminalDoc n

    go _ RETodo            = PP.text "\\mathsf{\\color{red}{TODO}}"

    parensIf :: Bool -> PP.Doc -> PP.Doc
    parensIf True  d = PP.text "\\left(" <<>> d <<>> PP.text "\\right)"
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
    | isControl  c = PP.int (ord c) -- TODO: some syntax
    | otherwise    = PP.text ("\\text{" ++ c : "}")

inquotes :: PP.Doc -> PP.Doc
inquotes d = "\\mathop{\\mathord{``}" <<>> d <<>> "\\mathord{\"}}"

mathtt :: PP.Doc -> PP.Doc
mathtt d = "\\mathtt{" <<>> d <<>> "}"

charsetDoc :: CS.CharSet -> PP.Doc
charsetDoc acs
    | acs == CS.alpha    = terminalDoc "alpha"
    | acs == CS.alphanum = terminalDoc "alpha-num"
charsetDoc acs = case CS.toIntervalList acs of
    []               -> "\\emptyset"
    [(x,y)] | x == y -> inquotes $ mathtt $ charDoc x
    rs
        | CS.size acs <= CS.size notAcs
        -> PP.brackets $ PP.hcat $ map rangeDoc rs
        | otherwise
        -> PP.braces $ PP.brackets (PP.hcat $ map rangeDoc (CS.toIntervalList notAcs)) <<>> PP.text "^c"
  where
    notAcs = CS.complement acs

    rangeDoc :: (Char, Char) -> PP.Doc
    rangeDoc (x, y) | x == y    = inquotes (mathtt $ charDoc x)
                    | otherwise = inquotes (mathtt $ charDoc x) <<>> PP.text "\\cdots" <<>> inquotes (mathtt $ charDoc y)
