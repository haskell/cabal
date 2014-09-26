module Distribution.Compat.List (dropWhileEndLE, takeWhileEndLE) where

-- dropWhileEndLE p is exactly the same as reverse . dropWhile p . reverse
-- but is quite a bit faster. Whereas Data.List.dropWhileEnd is as lazy
-- as possible in the spine of the list, dropWhileEndLE is as lazy as
-- possible in the elements of the list.
dropWhileEndLE :: (a -> Bool) -> [a] -> [a]
dropWhileEndLE p = foldr (\x r -> if null r && p x then [] else x:r) []

-- takeWhileEndLE p is exactly the same as reverse . takeWhile p . reverse
-- but is often somewhat faster.
takeWhileEndLE :: (a -> Bool) -> [a] -> [a]
takeWhileEndLE p = fst . foldr go ([], False)
  where
    go x (rest, done)
      | not done && p x = (x:rest, False)
      | otherwise = (rest, True)
