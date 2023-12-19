{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}

-----------------------------------------------------------------------------

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
  , builtinHaskellSuffixes
  , builtinHaskellBootSuffixes
  )
where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.ModuleName (ModuleName)
import Distribution.Pretty
import Distribution.Verbosity
import qualified Text.PrettyPrint as Disp

-- | The interface to a preprocessor, which may be implemented using an
--  external program, but need not be.  The arguments are the name of
--  the input file, the name of the output file and a verbosity level.
--  Here is a simple example that merely prepends a comment to the given
--  source file:
--
--  > ppTestHandler :: PreProcessor
--  > ppTestHandler =
--  >   PreProcessor {
--  >     platformIndependent = True,
--  >     runPreProcessor = mkSimplePreProcessor $ \inFile outFile verbosity ->
--  >       do info verbosity (inFile++" has been preprocessed to "++outFile)
--  >          stuff <- readFile inFile
--  >          writeFile outFile ("-- preprocessed as a test\n\n" ++ stuff)
--  >          return ExitSuccess
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
      -> [FilePath] -- Source directories
      -> [ModuleName] -- Module names
      -> IO [ModuleName] -- Sorted modules

  -- ^ This function can reorder /all/ modules, not just those that the
  -- require the preprocessor in question. As such, this function should be
  -- well-behaved and not reorder modules it doesn't have dominion over!
  --
  -- @since 3.8.1.0
  , runPreProcessor
      :: (FilePath, FilePath) -- Location of the source file relative to a base dir
      -> (FilePath, FilePath) -- Output file name, relative to an output base dir
      -> Verbosity -- verbosity
      -> IO () -- Should exit if the preprocessor fails
  }

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
