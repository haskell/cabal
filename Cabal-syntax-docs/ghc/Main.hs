{-# LANGUAGE DeriveGeneric #-}
module Main (main) where

import GHC.Generics                    (Generic)
import System.Environment              (getArgs)
import System.Exit                     (exitFailure)

import Distribution.PackageDescription.FieldGrammar (buildInfoFieldGrammar)

import qualified Zinza as Z

import Distribution.Described
import Distribution.Described.Extension
import Distribution.Utils.GrammarRegex
import Cabal.Syntax.Docs.ZFields

main :: IO ()
main = do
    args <- getArgs
    case args of
        [tmpl] -> do
            let biGhc = filter isGhcBuildInfo $ fromReference buildInfoFieldGrammar
            run <- Z.parseAndCompileTemplateIO tmpl
            contents <- run $ Z
                { zGhcBuildInfoFields = biGhc
                , zProductions =
                    [ zproduction "disable-extension" reDisableExtension
                        "Disable a language extension by prepending the extension with \"No\"."
                    , zproduction "enable-extension" reKnownExtension
                        "All GHC language extensions known to cabal. There may be more and some of these may be on by default."
                    , zproduction "interactive-extension" (reXs xGroupInteractive)
                        "Language Extensions related to GHC interactive."
                    , zproduction "phase-extension" (reXs xGroupPhase)
                        "Language Extensions related to a particular GHC phase."
                    , zproduction "syntax-extension" (reXs xGroupSyntax)
                        "Syntax Language Extensions."
                    , zproduction "import-export-extension" (reXs xGroupImportExport)
                        "Import and Export Language Extensions."
                    , zproduction "type-extension" (reXs xGroupTypes)
                        "Language Extensions for Types."
                    , zproduction "record-extension" (reXs xGroupRecords)
                        "Record Language Extensions."
                    , zproduction "deriving-extension" (reXs xGroupDeriving)
                        "Language Extensions for deriving mechanisms."
                    , zproduction "pattern-extension" (reXs xGroupPatterns)
                        "Patterns Language Extensions."
                    , zproduction "classes-instances-extension" (reXs xGroupClassesInstances)
                        "Language Extensions for class and instance declarations."
                    , zproduction "literal-extension" (reXs xGroupLiterals)
                        "Literals Language Extensions."
                    , zproduction "constraint-extension" (reXs xGroupConstraints)
                        "Constraint Language Extensions."
                    , zproduction "type-signature-extension" (reXs xGroupTypeSignatures)
                        "Type Signature Language Extensions."
                    , zproduction "binding-generalisation-extension" (reXs xGroupBindingsGeneralisation)
                        "Language Extensions for bindings and generalisation "
                    , zproduction "template-haskell-extension" (reXs xGroupTemplates)
                        "Template Haskell Language Extensions."
                    , zproduction "bang-strict-extension" (reXs xGroupBangStrict)
                        "Bang pattern and Strict Haskell Language Extensions."
                    , zproduction "parallel-concurrent-extension" (reXs xGroupParallelConcurrent)
                        "Parallel and Concurrent Language Extensions."
                    , zproduction "unboxed-primitive-extension" (reXs xGroupUnboxedPrimitive)
                        "Unboxed types and Primitive operations Language Extensions."
                    , zproduction "foreign-extension" (reXs xGroupForeign)
                        "Foreign function interface (FFI) Language Extensions."
                    , zproduction "safe-extension" (reXs xGroupSafe)
                        "Safe Haskell Language Extensions."
                    , zproduction "miscellaneous-extension" (reXs xGroupMiscellaneous)
                        "Miscellaneous Language Extensions."
                    , zproduction "bugs-extension" (reXs xGroupBugs)
                        "Language Extensions related to GHC bugs and infelicities."
                    , zproduction "ungrouped-extension" (reXs xUngrouped)
                        "Language Extensions not belonging to other extension groups."
                    ]
                , zSpaceList                = show $ regexDoc $
                    REMunch RESpaces1 (RENamed "element" RETodo)
                , zCommaList                = show $ regexDoc $
                    expandedCommaList (RENamed "element" RETodo)
                , zOptCommaList             = show $ regexDoc $
                    expandedOptCommaList (RENamed "element" RETodo)

                , zNull                     = null
                , zNotNull                  = not . null
                }

            putStrLn contents
        _ -> do
          putStrLn "Usage: generator <tmpl>"
          exitFailure

data Z = Z
    { zGhcBuildInfoFields       :: [ZField]
    , zProductions              :: [ZProduction]
    , zSpaceList                :: String
    , zCommaList                :: String
    , zOptCommaList             :: String
    , zNull                     :: String -> Bool
    , zNotNull                  :: String -> Bool
    }
  deriving (Generic)

instance Z.Zinza Z where
    toType    = Z.genericToTypeSFP
    toValue   = Z.genericToValueSFP
    fromValue = Z.genericFromValueSFP