module Main where
import System.FilePath
import System.Environment

main :: IO ()
main = do
  (tgt:rest) <- getArgs
  let (srcDirs, ghcArgs) = splitArgs rest
  let isGood = srcDirs == ["."] && "-outputdir" `elem` ghcArgs
  if isGood 
   then writeFile (tgt </> "Main.hs") $ "module Main where main = pure ()"
   else writeFile (tgt </> "Main.hs") $ "module Main where main = error \"failure\""

splitArgs = go []
  where
    go r ("--":xs) = (reverse r, xs)
    go r (x:xs) = go (x:r) xs
    go r [] = (reverse r, [])
