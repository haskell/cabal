{-# LANGUAGE StandaloneDeriving #-}

-- | Command line options for nix-style / v2 commands.
--
-- The commands take a lot of the same options, which affect how install plan
-- is constructed.
module Distribution.Client.NixStyleOptions
  ( NixStyleFlags (..)
  , nixStyleOptions
  , defaultNixStyleFlags
  ) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Simple.Command (OptionField (..), ShowOrParseArgs)
import Distribution.Simple.Setup (BenchmarkFlags, HaddockFlags, TestFlags)
import Distribution.Solver.Types.ConstraintSource (ConstraintSource (..))

import Distribution.Client.ProjectFlags
  ( ProjectFlags (..)
  , defaultProjectFlags
  , projectFlagsOptions
  )
import Distribution.Client.Setup
  ( ConfigExFlags
  , ConfigFlags (..)
  , InstallFlags (..)
  , benchmarkOptions
  , configureExOptions
  , configureOptions
  , haddockOptions
  , installOptions
  , liftOptions
  , testOptions
  )

data NixStyleFlags a = NixStyleFlags
  { configFlags :: ConfigFlags
  , configExFlags :: ConfigExFlags
  , installFlags :: InstallFlags
  , haddockFlags :: HaddockFlags
  , testFlags :: TestFlags
  , benchmarkFlags :: BenchmarkFlags
  , projectFlags :: ProjectFlags
  , extraFlags :: a
  }

nixStyleOptions
  :: (ShowOrParseArgs -> [OptionField a])
  -> ShowOrParseArgs
  -> [OptionField (NixStyleFlags a)]
nixStyleOptions commandOptions showOrParseArgs =
  liftOptions
    configFlags
    set1
    -- Note: [Hidden Flags]
    -- We reuse the configure options from v1 commands which on their turn
    -- reuse the ones from Cabal) but we hide some of them in v2 commands.
    ( filter
        ( ( `notElem`
              [ "cabal-file"
              , "constraint"
              , "dependency"
              , "promised-dependency"
              , "exact-configuration"
              ]
          )
            . optionName
        )
        $ configureOptions showOrParseArgs
    )
    ++ liftOptions
      configExFlags
      set2
      ( configureExOptions
          showOrParseArgs
          ConstraintSourceCommandlineFlag
      )
    ++ liftOptions
      installFlags
      set3
      -- hide "target-package-db" and "symlink-bindir" flags from the
      -- install options.
      -- "symlink-bindir" is obsoleted by "installdir" in ClientInstallFlags
      ( filter
          ( (`notElem` ["target-package-db", "symlink-bindir"])
              . optionName
          )
          $ installOptions showOrParseArgs
      )
    ++ liftOptions
      haddockFlags
      set4
      -- hide "verbose" and "builddir" flags from the
      -- haddock options.
      ( filter
          ( (`notElem` ["v", "verbose", "builddir"])
              . optionName
          )
          $ haddockOptions showOrParseArgs
      )
    ++ liftOptions testFlags set5 (testOptions showOrParseArgs)
    ++ liftOptions benchmarkFlags set6 (benchmarkOptions showOrParseArgs)
    ++ liftOptions projectFlags set7 (projectFlagsOptions showOrParseArgs)
    ++ liftOptions extraFlags set8 (commandOptions showOrParseArgs)
  where
    set1 x flags = flags{configFlags = x}
    set2 x flags = flags{configExFlags = x}
    set3 x flags = flags{installFlags = x}
    set4 x flags = flags{haddockFlags = x}
    set5 x flags = flags{testFlags = x}
    set6 x flags = flags{benchmarkFlags = x}
    set7 x flags = flags{projectFlags = x}
    set8 x flags = flags{extraFlags = x}

defaultNixStyleFlags :: a -> NixStyleFlags a
defaultNixStyleFlags x =
  NixStyleFlags
    { configFlags = mempty
    , configExFlags = mempty
    , installFlags = mempty
    , haddockFlags = mempty
    , testFlags = mempty
    , benchmarkFlags = mempty
    , projectFlags = defaultProjectFlags
    , extraFlags = x
    }
