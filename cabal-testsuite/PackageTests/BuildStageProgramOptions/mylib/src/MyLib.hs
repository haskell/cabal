{-# LANGUAGE CPP #-}
module MyLib (libStage) where

-- | Which stage's options this copy of the library was compiled with: the
-- test passes @-DHOST_STAGE@ as a host-stage option and @-DBUILD_STAGE@ as a
-- build-stage option.
libStage :: String
#if defined(HOST_STAGE) && defined(BUILD_STAGE)
libStage = "both"
#elif defined(HOST_STAGE)
libStage = "host"
#elif defined(BUILD_STAGE)
libStage = "build"
#else
libStage = "none"
#endif
