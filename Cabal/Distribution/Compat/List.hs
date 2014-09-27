-- | List utility functions not available on all supported versions of
-- GHC. Since the names in this module potentially clash with names in
-- @Data.List@, it is expected to be imported @qualified@.
module Distribution.Compat.List (dropWhileEnd, takeWhileEnd)
       where

-- | @dropWhileEnd p@ is exactly the same as @reverse . dropWhile p . reverse@
-- but quite a bit faster. The difference between "Data.List.dropWhileEnd" (as
-- of base-4.7) and this version is that the one in "Data.List" is
-- element-strict.
--
-- Example:
--
-- @
-- > tail $ Data.List.dropWhileEnd (<3) [undefined, 5, 4, 3, 2, 1]
-- *** Exception: Prelude.undefined
-- > tail $ dropWhileEnd (<3) [undefined, 5, 4, 3, 2, 1]
-- [5,4,3]
-- @
dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x r -> if null r && p x then [] else x:r) []

-- @takeWhileEnd p@ is exactly the same as @reverse . takeWhile p . reverse@ but
-- is usually faster (as well as being easier to read).
takeWhileEnd :: (a -> Bool) -> [a] -> [a]
takeWhileEnd p = fst . foldr go ([], False)
  where
    go x (rest, done)
      | not done && p x = (x:rest, False)
      | otherwise = (rest, True)
