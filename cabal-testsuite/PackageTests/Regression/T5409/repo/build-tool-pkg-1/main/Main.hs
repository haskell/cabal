module Main where

import BuildToolLibrary (buildToolLibraryVersion)
import System.Environment

main = do
  (_:source:target:_) <- getArgs
  writeFile target . unlines . map replaceVersion . lines =<< readFile source

replaceVersion "    BUILD_TOOL_VERSION" = "    " ++ show buildToolLibraryVersion
replaceVersion line                     = line
