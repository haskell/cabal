-- Since @3.0@ this is a compat module.
module Distribution.Text (display, simpleParse) where
{- {-# DEPRECATED "Use Distribution.Parsec or Distribution.Pretty" #-} -}

import Distribution.Pretty
import Distribution.Parsec

display :: Pretty a => a -> String
display = prettyShow

simpleParse :: Parsec a => String -> Maybe a
simpleParse = simpleParsec
