module Distribution.Glob.Type where

-- | A part of a glob. The aim is to be reasonably close to bash; see
-- http://wiki.bash-hackers.org/syntax/expansion/globs
--
-- We do not implement the whole bash globbing syntax here; it doesn't seem
-- worth implementing some of the more unusual cases. Notably (although this
-- list is not exhaustive):
--
-- * the POSIX character class patterns, like [[:alpha:]],
-- * the whole `extglob` extended language.
data GlobPart
  = Literal String
  -- ^ Match a part of a file with this exact string only. For example:
  -- "dictionary.txt" would be parsed as [Literal "dictionary.txt"], and
  -- would match "dictionary.txt" and nothing else.

  | PathSeparator
  -- ^ A path separator, '/'. Multiple '/'s are condensed down to one.

  | CharList [CharListPart]
  -- ^ Match exactly one character from any of the literal characters listed.
  -- For example:
  --
  -- "[abc]" matches "a", "b", "c", and nothing else.
  -- "[a-z]" matches any lower case English letter.
  -- "[a-zA-Z]" matches any English letter, either lower or upper case.
  --
  -- Special characters inside a CharList are:
  -- * exclamation mark !
  -- * hyphen-minus -
  -- * caret ^
  --
  -- To match these characters, they must be escaped with a backslash, eg:
  -- "[\^\!]"
  --
  -- Path separators may not appear in a CharList.

  | CharListComplement [CharListPart]
  -- ^ Match exactly one character, as long as it is not in any of the listed
  -- literal characters; the complement of a CharList. Written as "[!..]" or
  -- "[^..]". Escaping rules are the same as for a CharList. Examples:
  --
  -- "[!a]" matches anything except "a".
  -- "[^abc]" matches anything except "a", "b", or "c".

  | WildOne
  -- ^ Match exactly one character, excluding path separators, and also
  -- excluding dots at the beginning of file names. Written "?". Example:
  --
  -- "Cab?l" matches "Cabal", "Cabbl", "Cabcl"...

  | WildMany
  -- ^ Match zero or more characters of any part of a file name, excluding
  -- path separators, and also excluding dots at the beginning of filenames.
  -- Written "*".  Examples:
  --
  -- "jquery.*.js" matches "jquery.1.js", "jquery.2.js", "jquery.3-pre.js"...
  -- "*" matches "jquery.js" but not "jquery/index.js" or ".vimrc".

  | WildManyRecursive
  -- ^ Recursively matches all files and directories, excluding dots at the
  -- beginning of filenames. Written "**". Examples:
  --
  -- "**/*Test.hs" matches "GlobTest.hs", "test/HttpTest.hs",
  -- "test/examples/ExampleTest.hs"...

  | Choice [[GlobPart]]
  -- ^ Match exactly one of the given glob patterns. Written with curly
  -- braces, separated by commas. For example:
  --
  -- "{a,b,c*}" should be parsed as:
  --      [ Choice [ [Literal "a"]
  --               , [Literal "b"]
  --               , [Literal "c", MatchAny]
  --               ]
  --      ]
  --
  -- that is, "a", "b", or anything starting with "c".

  deriving (Show, Eq)

isLiteral :: GlobPart -> Bool
isLiteral (Literal _) = True
isLiteral _ = False

isPathSeparator :: GlobPart -> Bool
isPathSeparator PathSeparator = True
isPathSeparator _ = False

-- | A part of a bracket pattern, like [abc].
data CharListPart
  = Range Char Char
  -- ^ A character range, like "a-z".

  | CharLiteral Char
  -- ^ A single character, like "a".

  deriving (Show, Eq)

-- | A glob pattern that can match any number of files.
-- We purposefully omit an Eq instance because the derived instance would
-- return False in cases where the globs are actually the same, and also
-- because we don't really need one.
--
-- For example, using the derived Eq instance, we would have:
--
--     parseGlob "[abc]" /= parseGlob "{a,b,c}"
--
-- even though these are really the same glob.
newtype RealGlob = RealGlob { runRealGlob :: [GlobPart] }
  deriving (Show)

-- | A Glob which might just be a literal FilePath.
data Glob
  = Glob RealGlob
  | NoGlob FilePath
  deriving (Show)

isRealGlob :: Glob -> Bool
isRealGlob (Glob _) = True
isRealGlob (NoGlob _) = False
