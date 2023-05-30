module Distribution.Pretty
  ( Pretty (..)
  , prettyShow
  , defaultStyle
  , flatStyle

    -- * Utilities
  , showFilePath
  , showToken
  , showTokenStr
  , showFreeText
  , showFreeTextV3

    -- * Deprecated
  , Separator
  ) where

import Distribution.CabalSpecVersion
import Distribution.Compat.Prelude
import Prelude ()

import qualified Text.PrettyPrint as PP

class Pretty a where
  pretty :: a -> PP.Doc

  prettyVersioned :: CabalSpecVersion -> a -> PP.Doc
  prettyVersioned _ = pretty

-- | @since 3.4.0.0
instance Pretty PP.Doc where
  pretty = id

instance Pretty Bool where
  pretty = PP.text . show

instance Pretty Int where
  pretty = PP.text . show

instance Pretty a => Pretty (Identity a) where
  pretty = pretty . runIdentity

prettyShow :: Pretty a => a -> String
prettyShow = PP.renderStyle defaultStyle . pretty

-- | The default rendering style used in Cabal for console
-- output. It has a fixed page width and adds line breaks
-- automatically.
defaultStyle :: PP.Style
defaultStyle =
  PP.Style
    { PP.mode = PP.PageMode
    , PP.lineLength = 79
    , PP.ribbonsPerLine = 1.0
    }

-- | A style for rendering all on one line.
flatStyle :: PP.Style
flatStyle =
  PP.Style
    { PP.mode = PP.LeftMode
    , PP.lineLength = err "lineLength"
    , PP.ribbonsPerLine = err "ribbonsPerLine"
    }
  where
    err x =
      error
        ( "flatStyle: tried to access "
            ++ x
            ++ " in LeftMode. "
            ++ "This should never happen and indicates a bug in Cabal."
        )

-------------------------------------------------------------------------------
-- Utilities
-------------------------------------------------------------------------------

-- TODO: remove when ReadP parser is gone.
type Separator = [PP.Doc] -> PP.Doc

showFilePath :: FilePath -> PP.Doc
showFilePath = showToken

showToken :: String -> PP.Doc
showToken = PP.text . showTokenStr

showTokenStr :: String -> String
showTokenStr str
  -- if token looks like a comment (starts with --), print it in quotes
  | "--" `isPrefixOf` str = show str
  -- also if token ends with a colon (e.g. executable name), print it in quotes
  | ":" `isSuffixOf` str = show str
  | not (any dodgy str) && not (null str) = str
  | otherwise = show str
  where
    dodgy c = isSpace c || c == ','

-- | Pretty-print free-format text, ensuring that it is vertically aligned,
-- and with blank lines replaced by dots for correct re-parsing.
showFreeText :: String -> PP.Doc
showFreeText "" = mempty
showFreeText s = PP.vcat [PP.text (if null l then "." else l) | l <- lines_ s]

-- | Pretty-print free-format text.
-- Since @cabal-version: 3.0@ we don't replace blank lines with dots.
--
-- @since 3.0.0.0
showFreeTextV3 :: String -> PP.Doc
showFreeTextV3 "" = mempty
showFreeTextV3 s = PP.vcat [PP.text l | l <- lines_ s]

-- | 'lines_' breaks a string up into a list of strings at newline
-- characters.  The resulting strings do not contain newlines.
lines_ :: String -> [String]
lines_ [] = [""]
lines_ s =
  let (l, s') = break (== '\n') s
   in l : case s' of
        [] -> []
        (_ : s'') -> lines_ s''
