module Distribution.Utils.Glob.Match where

import Control.Monad
    ( (>=>) )
import Data.Maybe
    ( listToMaybe )
import Data.List
    ( stripPrefix, tails )
import Distribution.Utils.Glob.Type

isMatch :: Glob -> FilePath -> Bool
isMatch (Glob realGlob) fp = realIsMatch realGlob fp
isMatch (NoGlob fp') fp = fp' == fp

realIsMatch :: RealGlob -> FilePath -> Bool
realIsMatch (RealGlob parts) fp = isMatch' True parts (toSegments fp)

toSegments :: FilePath -> [String]
toSegments = filter (not . null) . endBy '/'

-- Not quite the same as the function from Data.List.Split (whose first
-- argument is a sublist, not a single list element). However, we only need to
-- split on individual elements here, and this allows for a simpler
-- implementation.
endBy :: Eq a => a -> [a] -> [[a]]
endBy _ [] = []
endBy splitter list =
  let (next, rest) = span (/= splitter) list
  in  next : endBy splitter (drop 1 rest)

-- | Given:
-- * A Bool which records whether we are at the beginning of the current
-- segment
-- * A list of GlobParts
-- * A list of path segments in a file path
-- Return whether the glob parts list matches the file path.
isMatch' :: Bool -> [GlobPart] -> [String] -> Bool
isMatch' _ (Literal l : parts) (seg : segs) =
  case stripPrefix l seg of
    Just seg' -> isMatch' False parts (seg' : segs)
    Nothing -> False
isMatch' _ (PathSeparator : parts) (seg : segs)
  | seg == "" = isMatch' True parts segs
  | otherwise = False
isMatch' _ (CharList cs : parts) ((h:tl) : segs) =
  if charListIsMatch cs h
    then isMatch' False parts (tl : segs)
    else False
isMatch' _ (CharListComplement cs : parts) ((h:tl) : segs) =
  if charListIsMatch cs h
    then False
    else isMatch' False parts (tl : segs)
isMatch' startSegment (WildOne : parts) ((h:tl) : segs)
  | startSegment && h == '.' = False
  | otherwise = isMatch' False parts (tl : segs)
isMatch' startSegment (WildMany : parts) segs
  | startSegment && (listToMaybe >=> listToMaybe) segs == Just '.' = False
  | otherwise =
    case segs of
      first : rest ->
        let candidates = map (:rest) (tails first)
        in  any (isMatch' False parts) candidates
      [] ->
        isMatch' startSegment parts segs
isMatch' startSegment (WildManyRecursive : parts) segs
   | startSegment && (listToMaybe >=> listToMaybe) segs == Just '.' = False
   | otherwise =
     anyCandidates || handlePathSep
     where
     anyCandidates =
       any (\(start, segs') -> isMatch' start parts segs') candidates
     candidates = iterateWhile (drop1' . snd) (False, segs)
     handlePathSep =
       case parts of
         PathSeparator : parts' -> isMatch' startSegment parts' segs
         _ -> False

isMatch' startSegment (Choice gs : parts) segs =
  any (\g -> isMatch' startSegment (g ++ parts) segs) gs
isMatch' _ [] [""] = True
isMatch' _ _ _ = False

charListIsMatch :: [CharListPart] -> Char -> Bool
charListIsMatch parts c = any (matches c) parts
  where
  matches x (CharLiteral y) = x == y
  matches x (Range start end) = start <= x && x <= end

-- | Drop one character from a list of path segments, or if the first segment
-- is empty, move on to the next segment.
drop1' :: [String] -> Maybe (Bool, [String])
drop1' [] = Nothing
drop1' ("" : segs) = Just (True, segs)
drop1' (seg : segs) = Just (False, drop 1 seg : segs)

-- | Generate a list of values obtained by repeatedly applying a function
-- to an initial value, until it stops returning Just.
iterateWhile :: (a -> Maybe a) -> a -> [a]
iterateWhile f x = x : rest
  where
  rest = case f x of
    Just y -> iterateWhile f y
    Nothing -> []
