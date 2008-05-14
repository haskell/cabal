{-# LANGUAGE CPP, ForeignFunctionInterface #-}
{-# OPTIONS_GHC -cpp -fffi #-}
{-# OPTIONS_NHC98 -cpp #-}
{-# OPTIONS_JHC -fcpp #-}
-- #hide
module Distribution.Compat.Char (isSymbol) where

import Data.Char
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ < 606)
import Foreign.C

isSymbol :: Char -> Bool
isSymbol c = case generalCategory c of
        MathSymbol              -> True
        CurrencySymbol          -> True
        ModifierSymbol          -> True
        OtherSymbol             -> True
        _                       -> False

-- Euch euch euch: This isn't at all nice. isSymbol is actually defined in
-- GHC.Unicode, but not exported! And nor is generalCategory.

generalCategory :: Char -> GeneralCategory
generalCategory c = toEnum (wgencat (fromIntegral (ord c)))

data GeneralCategory
        = UppercaseLetter       -- Lu  Letter, Uppercase
        | LowercaseLetter       -- Ll  Letter, Lowercase
        | TitlecaseLetter       -- Lt  Letter, Titlecase
        | ModifierLetter        -- Lm  Letter, Modifier
        | OtherLetter           -- Lo  Letter, Other
        | NonSpacingMark        -- Mn  Mark, Non-Spacing
        | SpacingCombiningMark  -- Mc  Mark, Spacing Combining
        | EnclosingMark         -- Me  Mark, Enclosing
        | DecimalNumber         -- Nd  Number, Decimal
        | LetterNumber          -- Nl  Number, Letter
        | OtherNumber           -- No  Number, Other
        | ConnectorPunctuation  -- Pc  Punctuation, Connector
        | DashPunctuation       -- Pd  Punctuation, Dash
        | OpenPunctuation       -- Ps  Punctuation, Open
        | ClosePunctuation      -- Pe  Punctuation, Close
        | InitialQuote          -- Pi  Punctuation, Initial quote
        | FinalQuote            -- Pf  Punctuation, Final quote
        | OtherPunctuation      -- Po  Punctuation, Other
        | MathSymbol            -- Sm  Symbol, Math
        | CurrencySymbol        -- Sc  Symbol, Currency
        | ModifierSymbol        -- Sk  Symbol, Modifier
        | OtherSymbol           -- So  Symbol, Other
        | Space                 -- Zs  Separator, Space
        | LineSeparator         -- Zl  Separator, Line
        | ParagraphSeparator    -- Zp  Separator, Paragraph
        | Control               -- Cc  Other, Control
        | Format                -- Cf  Other, Format
        | Surrogate             -- Cs  Other, Surrogate
        | PrivateUse            -- Co  Other, Private Use
        | NotAssigned           -- Cn  Other, Not Assigned
        deriving (Eq, Ord, Enum, Read, Show, Bounded)

foreign import ccall unsafe "u_gencat"
  wgencat :: CInt -> Int
#endif

