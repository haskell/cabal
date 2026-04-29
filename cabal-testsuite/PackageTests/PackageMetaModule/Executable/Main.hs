module Main where

import PackageMeta_PackageMetaModule (compiler, compilerVersion, os, arch, gitRevision, gitDirty)

main :: IO ()
main = do
  putStrLn compiler
  print compilerVersion
  putStrLn os
  putStrLn arch
  putStrLn gitRevision
  print gitDirty
