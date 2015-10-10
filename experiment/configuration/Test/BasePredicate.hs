module Test.BasePredicate where

import BasePredicate
import PartialValuation

import Test.QuickCheck

import Control.Monad (liftM, liftM2, liftM3)
import Data.Monoid
         ( Monoid(..) )

instance Arbitrary SystemParams where
  arbitrary = liftM3 SystemParams arbitrary arbitrary arbitrary
  shrink (SystemParams os arch compid) =
       [ SystemParams os' arch  compid  | os' <- shrink os ]
    ++ [ SystemParams os  arch' compid  | arch' <- shrink arch ]
    ++ [ SystemParams os  arch  compid' | compid' <- shrink compid ]

instance Arbitrary BasePredicate where
  arbitrary = frequency
    [ (1, liftM  OSPredicate arbitrary)
    , (1, liftM  ArchPredicate arbitrary)
    , (1, liftM2 CompilerPredicate arbitrary arbitrary)
    , (2, liftM  FlagPredicate arbitrary)
    ]
  shrink (OSPredicate   os  ) = [ OSPredicate   os'   | os'   <- shrink os   ]
  shrink (ArchPredicate arch) = [ ArchPredicate arch' | arch' <- shrink arch ]
  shrink (CompilerPredicate comp range) =
                   [ CompilerPredicate comp' range  | comp'  <- shrink comp  ]
                ++ [ CompilerPredicate comp  range' | range' <- shrink range ]
  shrink (FlagPredicate flag) = [ FlagPredicate flag' | flag' <- shrink flag ]


v === v' = forAll arbitrary $ \e -> applyPartialValuation v  e
                                 == applyPartialValuation v' e

prop_eval sysParams flags = eval  sysParams flags
                        === eval' sysParams flags
  where
    eval' (SystemParams os arch comp) flagAssignment =
                applyOS os
      `mappend` applyArch arch
      `mappend` applyCompiler comp
      `mappend` applyFlagAssignment flagAssignment
