{-# LANGUAGE CPP #-}

-- | This module only compiles if the @builder(...)@ conditionals in the
-- .cabal file selected the expected branches. The expectations are that, when
-- built with cabal:
--
--   * @builder(cabal >= 1.0)@ is true   -> CABAL_AT_LEAST_1 is defined
--   * @builder(cabal >= 999999)@ is false -> CABAL_FROM_THE_FUTURE is undefined
--   * @builder(mcabal)@ is false        -> BUILT_WITH_MCABAL is undefined
module Lib (tag) where

#if !defined(CABAL_AT_LEAST_1)
#error "builder(cabal >= 1.0) should have been selected but was not"
#endif

#if defined(CABAL_FROM_THE_FUTURE)
#error "builder(cabal >= 999999) should not have been selected"
#endif

#if defined(BUILT_WITH_MCABAL)
#error "builder(mcabal) should not have been selected when building with cabal"
#endif

tag :: String
tag = "built with cabal"
