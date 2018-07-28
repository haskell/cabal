{-# OPTIONS_GHC -F -pgmF build-tool-exe #-}

import BuildToolLibrary (buildToolLibraryVersion)

main = do
  putStrLn $ "build-tool library version: " ++ show buildToolLibraryVersion ++ ","
  putStrLn $ "build-tool exe version: " ++ show buildToolExeVersion

buildToolExeVersion :: Int
buildToolExeVersion =
    BUILD_TOOL_VERSION
