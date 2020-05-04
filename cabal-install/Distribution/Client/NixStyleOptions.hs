-- | Command line options for nix-style / v2 commands.
--
-- The commands take a lot of the same options, which affect how install plan
-- is constructed.
module Distribution.Client.NixStyleOptions (
    NixStyleFlags, nixStyleOptions, defaultNixStyleFlags,
) where

import Distribution.Client.Compat.Prelude
import Prelude ()

import Distribution.Simple.Command                       (OptionField (..), ShowOrParseArgs)
import Distribution.Simple.Setup                         (BenchmarkFlags, HaddockFlags, TestFlags)
import Distribution.Solver.Types.ConstraintSource        (ConstraintSource (..))

import Distribution.Client.Setup
       (ConfigExFlags, ConfigFlags (..), InstallFlags (..), benchmarkOptions, configureExOptions,
       configureOptions, haddockOptions, installOptions, liftOptions, testOptions)

-- TODO: turn into data record
-- Then we could use RecordWildCards in command implementation.
type NixStyleFlags a = (ConfigFlags, ConfigExFlags, InstallFlags, HaddockFlags, TestFlags, BenchmarkFlags, a)

nixStyleOptions
    :: (ShowOrParseArgs -> [OptionField a])
    -> ShowOrParseArgs -> [OptionField (NixStyleFlags a)]
nixStyleOptions commandOptions showOrParseArgs =
        liftOptions get1 set1
        -- Note: [Hidden Flags]
        -- hide "constraint", "dependency", and
        -- "exact-configuration" from the configure options.
        (filter ((`notElem` ["constraint", "dependency"
                            , "exact-configuration"])
                 . optionName) $ configureOptions showOrParseArgs)
     ++ liftOptions get2 set2 (configureExOptions showOrParseArgs
                               ConstraintSourceCommandlineFlag)
     ++ liftOptions get3 set3
        -- hide "target-package-db" and "symlink-bindir" flags from the
        -- install options.
        -- "symlink-bindir" is obsoleted by "installdir" in ClientInstallFlags
        (filter ((`notElem` ["target-package-db", "symlink-bindir"])
                 . optionName) $
                               installOptions showOrParseArgs)
       ++ liftOptions get4 set4
          -- hide "verbose" and "builddir" flags from the
          -- haddock options.
          (filter ((`notElem` ["v", "verbose", "builddir"])
                  . optionName) $
                                haddockOptions showOrParseArgs)
     ++ liftOptions get5 set5 (testOptions showOrParseArgs)
     ++ liftOptions get6 set6 (benchmarkOptions showOrParseArgs)
     ++ liftOptions get7 set7 (commandOptions showOrParseArgs)
  where
    get1 (a,_,_,_,_,_,_) = a; set1 a (_,b,c,d,e,f,g) = (a,b,c,d,e,f,g)
    get2 (_,b,_,_,_,_,_) = b; set2 b (a,_,c,d,e,f,g) = (a,b,c,d,e,f,g)
    get3 (_,_,c,_,_,_,_) = c; set3 c (a,b,_,d,e,f,g) = (a,b,c,d,e,f,g)
    get4 (_,_,_,d,_,_,_) = d; set4 d (a,b,c,_,e,f,g) = (a,b,c,d,e,f,g)
    get5 (_,_,_,_,e,_,_) = e; set5 e (a,b,c,d,_,f,g) = (a,b,c,d,e,f,g)
    get6 (_,_,_,_,_,f,_) = f; set6 f (a,b,c,d,e,_,g) = (a,b,c,d,e,f,g)
    get7 (_,_,_,_,_,_,g) = g; set7 g (a,b,c,d,e,f,_) = (a,b,c,d,e,f,g)

defaultNixStyleFlags :: a ->  NixStyleFlags a
defaultNixStyleFlags x = ( mempty, mempty, mempty, mempty, mempty, mempty, x )
