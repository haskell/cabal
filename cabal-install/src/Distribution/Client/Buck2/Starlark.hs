-- | A minimal Starlark (BUCK file) pretty-printer.
--
-- This deliberately doesn't attempt to represent the whole Starlark
-- language: it covers exactly the subset (string/bool scalars, lists,
-- dicts, @load@ statements and top-level rule calls) that generated
-- @BUCK@\/@.bzl@ files need, so every generator in "Distribution.Client.Buck2"
-- builds output through the same renderer instead of hand-formatting
-- strings.
module Distribution.Client.Buck2.Starlark
  ( Value (..)
  , str
  , strList
  , Call (..)
  , call
  , renderCall
  , renderLoad
  , renderFile
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

-- | A Starlark expression, restricted to what BUCK files actually need:
-- string\/bool scalars and (recursively) lists, dicts and tuples of them.
-- 'VTuple' exists only for @cabal_component = (pkg, component)@ (see
-- buck2\/haskell.bzl's own @cabal_component@ kwarg) - nothing else
-- generated here needs Starlark's tuple\/list distinction.
data Value
  = VStr String
  | VBool Bool
  | VList [Value]
  | VDict [(String, Value)]
  | VTuple [Value]

str :: String -> Value
str = VStr

strList :: [String] -> Value
strList = VList . map VStr

-- | A single top-level rule invocation, e.g. @haskell_library(name = ..., ...)@.
data Call = Call
  { callFn :: String
  , callArgs :: [(String, Value)]
  }

call :: String -> [(String, Value)] -> Call
call = Call

indent :: Int -> String
indent n = replicate (4 * n) ' '

-- | Render like Python's @repr@ for a single-quoted string: this is what
-- both buck2/gen-haskell-prebuilt.py's generated output and every
-- hand-written @.bzl@ file in buck2/ already use.
renderStr :: String -> String
renderStr s = '\'' : concatMap escape s ++ "'"
  where
    escape '\\' = "\\\\"
    escape '\'' = "\\'"
    escape '\n' = "\\n"
    escape '\t' = "\\t"
    escape c = [c]

renderValue :: Int -> Value -> String
renderValue _ (VStr s) = renderStr s
renderValue _ (VBool b) = if b then "True" else "False"
renderValue _ (VList []) = "[]"
renderValue ind (VList xs) =
  "[\n"
    ++ concat [indent (ind + 1) ++ renderValue (ind + 1) x ++ ",\n" | x <- xs]
    ++ indent ind
    ++ "]"
-- | Always rendered compactly on one line (unlike 'VList'\/'VDict') - its
-- only use is @cabal_component = (pkg, component)@, and matching the
-- pair literal shape it's meant to look like matters more than matching
-- every other value's one-element-per-line style.
renderValue _ (VTuple xs) = "(" ++ intercalate ", " (map (renderValue 0) xs) ++ ")"
renderValue _ (VDict []) = "{}"
renderValue ind (VDict kvs) =
  "{\n"
    ++ concat
      [ indent (ind + 1) ++ renderStr k ++ ": " ++ renderValue (ind + 1) v ++ ",\n"
      | (k, v) <- kvs
      ]
    ++ indent ind
    ++ "}"

-- | Render a rule call as a standalone top-level statement, one argument
-- per line (matching the style buck2/gen-haskell-prebuilt.py's own
-- generated BUCK file uses), terminated by a blank line.
renderCall :: Call -> String
renderCall (Call fn args) =
  fn
    ++ "(\n"
    ++ concat
      [ indent 1 ++ k ++ " = " ++ renderValue 1 v ++ ",\n"
      | (k, v) <- args
      ]
    ++ ")\n"

-- | Render a @load("target", "name1", "name2")@ statement.
renderLoad :: String -> [String] -> String
renderLoad target names =
  "load(" ++ intercalate ", " (renderStr target : map renderStr names) ++ ")\n"

-- | Render a full generated file: a header comment, the accumulated
-- @load@ statements, then each rule call in turn, separated by blank
-- lines.
renderFile :: String -> [(String, [String])] -> [Call] -> String
renderFile header loads calls =
  unlines (map ("# " ++) headerLines)
    ++ "\n"
    ++ concatMap (uncurry renderLoad) loads
    ++ "\n"
    ++ intercalate "\n" (map renderCall calls)
  where
    headerLines = lines header
