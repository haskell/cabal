-----------------------------------------------------------------------------

-- |
-- Module      :  Distribution.Simple.Program.Builtin
-- Copyright   :  Isaac Jones 2006, Duncan Coutts 2007-2009
--
-- Maintainer  :  cabal-devel@haskell.org
-- Portability :  portable
--
-- The module defines all the known built-in 'Program's.
--
-- Where possible we try to find their version numbers.
module Distribution.Simple.Program.Builtin
  ( -- * The collection of unconfigured and configured programs
    builtinPrograms

    -- * Programs that Cabal knows about
  , ghcProgram
  , ghcPkgProgram
  , runghcProgram
  , ghcjsProgram
  , ghcjsPkgProgram
  , hmakeProgram
  , jhcProgram
  , haskellSuiteProgram
  , haskellSuitePkgProgram
  , uhcProgram
  , gccProgram
  , arProgram
  , stripProgram
  , happyProgram
  , alexProgram
  , hsc2hsProgram
  , c2hsProgram
  , cpphsProgram
  , hscolourProgram
  , doctestProgram
  , haddockProgram
  , greencardProgram
  , ldProgram
  , tarProgram
  , cppProgram
  , pkgConfigProgram
  , hpcProgram
  ) where

import Distribution.Compat.Prelude
import Prelude ()

import Distribution.Simple.Program.Find
import Distribution.Simple.Program.GHC
import Distribution.Simple.Program.Internal
import Distribution.Simple.Program.Run
import Distribution.Simple.Program.Types
import Distribution.Simple.Utils
import Distribution.Verbosity
import Distribution.Version

import qualified Data.Map as Map

-- ------------------------------------------------------------

-- * Known programs

-- ------------------------------------------------------------

-- | The default list of programs.
-- These programs are typically used internally to Cabal.
builtinPrograms :: [Program]
builtinPrograms =
  [ -- compilers and related progs
    ghcProgram
  , runghcProgram
  , ghcPkgProgram
  , ghcjsProgram
  , ghcjsPkgProgram
  , haskellSuiteProgram
  , haskellSuitePkgProgram
  , hmakeProgram
  , jhcProgram
  , uhcProgram
  , hpcProgram
  , -- preprocessors
    hscolourProgram
  , doctestProgram
  , haddockProgram
  , happyProgram
  , alexProgram
  , hsc2hsProgram
  , c2hsProgram
  , cpphsProgram
  , greencardProgram
  , -- platform toolchain
    gccProgram
  , arProgram
  , stripProgram
  , ldProgram
  , tarProgram
  , -- configuration tools
    pkgConfigProgram
  ]

ghcProgram :: Program
ghcProgram =
  (simpleProgram "ghc")
    { programFindVersion = findProgramVersion "--numeric-version" id
    , -- Workaround for https://gitlab.haskell.org/ghc/ghc/-/issues/8825
      -- (spurious warning on non-english locales)
      programPostConf = \_verbosity ghcProg ->
        do
          let ghcProg' =
                ghcProg
                  { programOverrideEnv =
                      ("LANGUAGE", Just "en")
                        : programOverrideEnv ghcProg
                  }
              -- Only the 7.8 branch seems to be affected. Fixed in 7.8.4.
              affectedVersionRange =
                intersectVersionRanges
                  (laterVersion $ mkVersion [7, 8, 0])
                  (earlierVersion $ mkVersion [7, 8, 4])
          return $
            maybe
              ghcProg
              ( \v ->
                  if withinRange v affectedVersionRange
                    then ghcProg'
                    else ghcProg
              )
              (programVersion ghcProg)
    , programNormaliseArgs = normaliseGhcArgs
    }

runghcProgram :: Program
runghcProgram =
  (simpleProgram "runghc")
    { programFindVersion = findProgramVersion "--version" $ \str ->
        case words str of
          -- "runghc 7.10.3"
          (_ : ver : _) -> ver
          _ -> ""
    }

ghcPkgProgram :: Program
ghcPkgProgram =
  (simpleProgram "ghc-pkg")
    { programFindVersion = findProgramVersion "--version" $ \str ->
        -- Invoking "ghc-pkg --version" gives a string like
        -- "GHC package manager version 6.4.1"
        case words str of
          (_ : _ : _ : _ : ver : _) -> ver
          _ -> ""
    }

ghcjsProgram :: Program
ghcjsProgram =
  (simpleProgram "ghcjs")
    { programFindVersion = findProgramVersion "--numeric-ghcjs-version" id
    }

-- note: version is the version number of the GHC version that ghcjs-pkg was built with
ghcjsPkgProgram :: Program
ghcjsPkgProgram =
  (simpleProgram "ghcjs-pkg")
    { programFindVersion = findProgramVersion "--version" $ \str ->
        -- Invoking "ghcjs-pkg --version" gives a string like
        -- "GHCJS package manager version 6.4.1"
        case words str of
          (_ : _ : _ : _ : ver : _) -> ver
          _ -> ""
    }

hmakeProgram :: Program
hmakeProgram =
  (simpleProgram "hmake")
    { programFindVersion = findProgramVersion "--version" $ \str ->
        -- Invoking "hmake --version" gives a string line
        -- "/usr/local/bin/hmake: 3.13 (2006-11-01)"
        case words str of
          (_ : ver : _) -> ver
          _ -> ""
    }

jhcProgram :: Program
jhcProgram =
  (simpleProgram "jhc")
    { programFindVersion = findProgramVersion "--version" $ \str ->
        -- invoking "jhc --version" gives a string like
        -- "jhc 0.3.20080208 (wubgipkamcep-2)
        -- compiled by ghc-6.8 on a x86_64 running linux"
        case words str of
          (_ : ver : _) -> ver
          _ -> ""
    }

uhcProgram :: Program
uhcProgram =
  (simpleProgram "uhc")
    { programFindVersion = findProgramVersion "--version-dotted" id
    }

hpcProgram :: Program
hpcProgram =
  (simpleProgram "hpc")
    { programFindVersion = findProgramVersion "version" $ \str ->
        case words str of
          (_ : _ : _ : ver : _) -> ver
          _ -> ""
    }

-- This represents a haskell-suite compiler. Of course, the compiler
-- itself probably is not called "haskell-suite", so this is not a real
-- program. (But we don't know statically the name of the actual compiler,
-- so this is the best we can do.)
--
-- Having this Program value serves two purposes:
--
-- 1. We can accept options for the compiler in the form of
--
--   --haskell-suite-option(s)=...
--
-- 2. We can find a program later using this static id (with
-- requireProgram).
--
-- The path to the real compiler is found and recorded in the ProgramDb
-- during the configure phase.
haskellSuiteProgram :: Program
haskellSuiteProgram =
  simpleProgram "haskell-suite"

-- This represent a haskell-suite package manager. See the comments for
-- haskellSuiteProgram.
haskellSuitePkgProgram :: Program
haskellSuitePkgProgram =
  simpleProgram "haskell-suite-pkg"

happyProgram :: Program
happyProgram =
  (simpleProgram "happy")
    { programFindVersion = findProgramVersion "--version" $ \str ->
        -- Invoking "happy --version" gives a string like
        -- "Happy Version 1.16 Copyright (c) ...."
        case words str of
          (_ : _ : ver : _) -> ver
          _ -> ""
    }

alexProgram :: Program
alexProgram =
  (simpleProgram "alex")
    { programFindVersion = findProgramVersion "--version" $ \str ->
        -- Invoking "alex --version" gives a string like
        -- "Alex version 2.1.0, (c) 2003 Chris Dornan and Simon Marlow"
        case words str of
          (_ : _ : ver : _) -> takeWhile (\x -> isDigit x || x == '.') ver
          _ -> ""
    }

gccProgram :: Program
gccProgram =
  (simpleProgram "gcc")
    { programFindVersion = findProgramVersion "-dumpversion" id
    }

arProgram :: Program
arProgram = simpleProgram "ar"

stripProgram :: Program
stripProgram =
  (simpleProgram "strip")
    { programFindVersion = \verbosity ->
        findProgramVersion "--version" stripExtractVersion (lessVerbose verbosity)
    }

hsc2hsProgram :: Program
hsc2hsProgram =
  (simpleProgram "hsc2hs")
    { programFindVersion =
        findProgramVersion "--version" $ \str ->
          -- Invoking "hsc2hs --version" gives a string like "hsc2hs version 0.66"
          case words str of
            (_ : _ : ver : _) -> ver
            _ -> ""
    }

c2hsProgram :: Program
c2hsProgram =
  (simpleProgram "c2hs")
    { programFindVersion = findProgramVersion "--numeric-version" id
    }

cpphsProgram :: Program
cpphsProgram =
  (simpleProgram "cpphs")
    { programFindVersion = findProgramVersion "--version" $ \str ->
        -- Invoking "cpphs --version" gives a string like "cpphs 1.3"
        case words str of
          (_ : ver : _) -> ver
          _ -> ""
    }

hscolourProgram :: Program
hscolourProgram =
  (simpleProgram "hscolour")
    { programFindLocation = \v p -> findProgramOnSearchPath v p "HsColour"
    , programFindVersion = findProgramVersion "-version" $ \str ->
        -- Invoking "HsColour -version" gives a string like "HsColour 1.7"
        case words str of
          (_ : ver : _) -> ver
          _ -> ""
    }

-- TODO: Ensure that doctest is built against the same GHC as the one
--       that's being used.  Same for haddock.  @phadej pointed this out.
doctestProgram :: Program
doctestProgram =
  (simpleProgram "doctest")
    { programFindLocation = \v p -> findProgramOnSearchPath v p "doctest"
    , programFindVersion = findProgramVersion "--version" $ \str ->
        -- "doctest version 0.11.2"
        case words str of
          (_ : _ : ver : _) -> ver
          _ -> ""
    }

haddockProgram :: Program
haddockProgram =
  (simpleProgram "haddock")
    { programFindVersion = findProgramVersion "--version" $ \str ->
        -- Invoking "haddock --version" gives a string like
        -- "Haddock version 0.8, (c) Simon Marlow 2006"
        case words str of
          (_ : _ : ver : _) -> takeWhile (`elem` ('.' : ['0' .. '9'])) ver
          _ -> ""
    , programNormaliseArgs = \_ _ args -> args
    }

greencardProgram :: Program
greencardProgram = simpleProgram "greencard"

ldProgram :: Program
ldProgram =
  (simpleProgram "ld")
    { programPostConf = \verbosity ldProg -> do
        -- The `lld` linker cannot create merge (relocatable) objects so we
        -- want to detect this.
        -- If the linker does support relocatable objects, we want to use that
        -- to create partially pre-linked objects for GHCi, so we get much
        -- faster loading as we do not have to do the separate loading and
        -- in-memory linking the static linker in GHC does, but can offload
        -- parts of this process to a pre-linking step.
        -- However this requires the linker to support this features. Not all
        -- linkers do, and notably as of this writing `lld` which is a popular
        -- choice for windows linking does not support this feature. However
        -- if using binutils ld or another linker that supports --relocatable,
        -- we should still be good to generate pre-linked objects.
        ldHelpOutput <-
          getProgramInvocationOutput
            verbosity
            (programInvocation ldProg ["--help"])
            -- In case the linker does not support '--help'. Eg the LLVM linker,
            -- `lld` only accepts `-help`.
            `catchIO` (\_ -> return "")
        let k = "Supports relocatable output"
            -- Standard GNU `ld` uses `--relocatable` while `ld.gold` uses
            -- `-relocatable` (single `-`).
            v
              | "-relocatable" `isInfixOf` ldHelpOutput = "YES"
              -- ld64 on macOS has this lovely response for "--help"
              --
              --   ld64: For information on command line options please use 'man ld'.
              --
              -- it does however support -r, if you read the manpage
              -- (e.g. https://www.manpagez.com/man/1/ld64/)
              | "ld64:" `isPrefixOf` ldHelpOutput = "YES"
              | otherwise = "NO"

            m = Map.insert k v (programProperties ldProg)
        return $ ldProg{programProperties = m}
    }

tarProgram :: Program
tarProgram =
  (simpleProgram "tar")
    { -- See #1901. Some versions of 'tar' (OpenBSD, NetBSD, ...) don't support the
      -- '--format' option.
      programPostConf = \verbosity tarProg -> do
        tarHelpOutput <-
          getProgramInvocationOutput
            verbosity
            (programInvocation tarProg ["--help"])
            -- Some versions of tar don't support '--help'.
            `catchIO` (\_ -> return "")
        let k = "Supports --format"
            v = if ("--format" `isInfixOf` tarHelpOutput) then "YES" else "NO"
            m = Map.insert k v (programProperties tarProg)
        return $ tarProg{programProperties = m}
    }

cppProgram :: Program
cppProgram = simpleProgram "cpp"

pkgConfigProgram :: Program
pkgConfigProgram =
  (simpleProgram "pkg-config")
    { programFindVersion = findProgramVersion "--version" id
    , programPostConf = \_ pkgConfProg ->
        let programOverrideEnv' =
              programOverrideEnv pkgConfProg
                ++ [ ("PKG_CONFIG_ALLOW_SYSTEM_CFLAGS", Just "1")
                   , ("PKG_CONFIG_ALLOW_SYSTEM_LIBS", Just "1")
                   ]
         in pure $ pkgConfProg{programOverrideEnv = programOverrideEnv'}
    }
