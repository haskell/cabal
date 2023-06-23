-----------------------------------------------------------------------------

-- Module      :  Distribution.Simple.Errors
-- Copyright   :  
-- License     :  
--
-- Maintainer  :  
-- Portability :  
--
-- A collection of Exception Types used throughout 
--the rest of the Cabal library package

module Distribution.Simple.Errors
(
 CabalException (..)
 ,exceptionCode
 ,exceptionMessage
) where

import Distribution.Compat.Prelude
import Distribution.Compiler
import Distribution.Pretty
  ( prettyShow
  )


-- Types representing exceptions thrown by functions in all the modules of Cabal Package
data CabalException =  NoBenchMarkProgram FilePath
                    | EnableBenchMark
                    | BenchMarkNameDisable String
                    | NoBenchMark String            
                    | NoLibraryFound
                    | CompilerNotInstalled CompilerFlavor
                    | CantFindIncludeFile String
                    | SourceDistException String
 deriving (Show,Typeable)

exceptionCode :: CabalException -> Int
exceptionCode e = case e of 
  (NoBenchMarkProgram _)   -> 1678
  EnableBenchMark          -> 1453
  (BenchMarkNameDisable _) -> 2781
  (NoBenchMark _)          -> 1654
  NoLibraryFound           -> 2546
  (CompilerNotInstalled _) -> 7465
  (CantFindIncludeFile _)  -> 3876
  (SourceDistException _ ) -> 3245

exceptionMessage :: CabalException -> String
exceptionMessage e = case e of
    NoBenchMarkProgram cmd          -> "Could not find benchmark program \"" ++ cmd ++ "\". Did you build the package first?"
    EnableBenchMark                 -> "No benchmarks enabled. Did you remember to configure with " ++ "\'--enable-benchmarks\'?"
    BenchMarkNameDisable bmName     -> "Package configured with benchmark " ++ bmName ++ " disabled."
    NoBenchMark bmName              -> "no such benchmark: " ++ bmName
    NoLibraryFound                  -> "No executables and no library found. Nothing to do."
    CompilerNotInstalled compilerFlavor -> "installing with " ++ prettyShow compilerFlavor ++ "is not implemented"
    CantFindIncludeFile file        -> "can't find include file " ++ file
    (SourceDistException _ )        -> "3245"

     

