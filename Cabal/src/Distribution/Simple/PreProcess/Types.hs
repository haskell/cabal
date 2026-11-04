{-# LANGUAGE DataKinds #-}

-- |
-- Module      :  Distribution.Simple.PreProcess.Types
-- Copyright   :  (c) 2003-2005, Isaac Jones, Malcolm Wallace
-- License     :  BSD3
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- This defines a 'PreProcessor' abstraction which represents a pre-processor
-- that can transform one kind of file into another.
module Distribution.Simple.PreProcess.Types
  ( Suffix (..)
  , PreProcessor (..)
  , PreProcessorInvocation (..)
  , builtinHaskellSuffixes
  , builtinHaskellBootSuffixes
  )
where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.ModuleName (ModuleName)
import Distribution.Pretty
import Distribution.Simple.Program.Types (ConfiguredProgram)
import Distribution.Utils.Path
import Distribution.Verbosity
import qualified Text.PrettyPrint as Disp

-- | The interface to a preprocessor, which may be implemented using an
--  external program, but need not be. The arguments are the name of
--  the input file, the name of the output file and a verbosity level.
--  Here is a simple example that merely prepends a comment to the given
--  source file:
--
--  > ppTestHandler :: PreProcessor
--  > ppTestHandler =
--  >   PreProcessor {
--  >     platformIndependent = True,
--  >     ppOrdering = \_ _ -> return,
--  >     getPreProcessor = \_ -> do
--  >       let runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity ->
--  >               do info verbosity (inFile++" has been preprocessed to "++outFile)
--  >                  stuff <- readFile inFile
--  >                  writeFile outFile ("-- preprocessed as a test\n\n" ++ stuff)
--  >                  return ()
--  >           ppKey = PreProcessorBuiltin
--  >       return (runPreProcessor, ppKey)
--
--  We split the input and output file names into a base directory and the
--  rest of the file name. The input base dir is the path in the list of search
--  dirs that this file was found in. The output base dir is the build dir where
--  all the generated source files are put.
--
--  The reason for splitting it up this way is that some pre-processors don't
--  simply generate one output .hs file from one input file but have
--  dependencies on other generated files (notably c2hs, where building one
--  .hs file may require reading other .chi files, and then compiling the .hs
--  file may require reading a generated .h file). In these cases the generated
--  files need to embed relative path names to each other (eg the generated .hs
--  file mentions the .h file in the FFI imports). This path must be relative to
--  the base directory where the generated files are located, it cannot be
--  relative to the top level of the build tree because the compilers do not
--  look for .h files relative to there, ie we do not use \"-I .\", instead we
--  use \"-I dist\/build\" (or whatever dist dir has been set by the user)
--
--  Most pre-processors do not care of course, so mkSimplePreProcessor and
--  runSimplePreProcessor functions handle the simple case.
data PreProcessor = PreProcessor
  { -- Is the output of the pre-processor platform independent? eg happy output
    -- is portable haskell but c2hs's output is platform dependent.
    -- This matters since only platform independent generated code can be
    -- included into a source tarball.
    platformIndependent :: Bool
  , -- TODO: deal with pre-processors that have implementation dependent output
    --       eg alex and happy have --ghc flags. However we can't really include
    --       ghc-specific code into supposedly portable source tarballs.

    ppOrdering
      :: Verbosity
      -> [SymbolicPath Pkg (Dir Source)] -- Source directories
      -> [ModuleName] -- Module names
      -> IO [ModuleName] -- Sorted modules

  -- ^ This function can reorder /all/ modules, not just those that the
  -- require the preprocessor in question. As such, this function should be
  -- well-behaved and not reorder modules it doesn't have dominion over!
  --
  -- @since 3.8.1.0
  , getPreProcessor
      :: Verbosity
      -> (FilePath, FilePath) -- ^ input
      -> (FilePath, FilePath) -- ^ output
      -> IO PreProcessorInvocation
  }

-- | Uniquely identifies a specific preprocessor, including the preprocessor
-- itself and any arguments that need to be tracked for monitoring purposes.
-- This value should change iff the preprocessor itself changes in a way that
-- requires re-running it for a given input file.
-- See Note [Preprocessor monitoring] in Distribution.Simple.PreProcess
data PreProcessorInvocation
  = -- | The Unlit pre-processor. This one is built in, and only changes when
    -- Cabal itself does, so there is no need to track any changes for it, other
    -- than the input and output files.
    PreProcessorUnlit
      FilePath -- ^ input file
      FilePath -- ^ output file
  | -- | A preprocessor that runs some program, identified uniquely by the
    -- configured program and its arguments.
    PreProcessorProgram
      ConfiguredProgram -- ^ the main program to run
      [String] -- ^ arguments to pass to the main program
      [ConfiguredProgram] -- ^ additional programs that the preprocessor depends on
  | -- | A preprocessor that depends on a response file (a temporary file
    -- available for the duration of the invocation).
    PreProcessorViaResponseFile
      FilePath -- ^ filename pattern for the response file
      ConfiguredProgram -- ^ the main program to run
      [String] -- ^ arguments to pass directly
      [String] -- ^ arguments to write into the response file
      [ConfiguredProgram] -- ^ additional programs that the preprocessor depends on
  deriving (Generic, Eq)

instance Binary PreProcessorInvocation
instance Structured PreProcessorInvocation

-- | A suffix (or file extension).
--
-- Mostly used to decide which preprocessor to use, e.g. files with suffix @"y"@
-- are usually processed by the @"happy"@ build tool.
newtype Suffix = Suffix String
  deriving (Eq, Ord, Show, Generic, IsString)

instance Pretty Suffix where
  pretty (Suffix s) = Disp.text s

instance Binary Suffix
instance Structured Suffix

builtinHaskellSuffixes :: [Suffix]
builtinHaskellSuffixes = map Suffix ["hs", "lhs", "hsig", "lhsig"]

builtinHaskellBootSuffixes :: [Suffix]
builtinHaskellBootSuffixes = map Suffix ["hs-boot", "lhs-boot"]
