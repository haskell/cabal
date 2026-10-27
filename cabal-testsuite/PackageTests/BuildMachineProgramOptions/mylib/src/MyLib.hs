{-# LANGUAGE CPP #-}
module MyLib (libMachine) where

-- | Which machine's options this copy of the library was compiled with: the
-- test passes @-DHOST_MACHINE@ as a host-machine option and @-DBUILD_MACHINE@ as a
-- build-machine option.
libMachine :: String
#if defined(HOST_MACHINE) && defined(BUILD_MACHINE)
libMachine = "both"
#elif defined(HOST_MACHINE)
libMachine = "host"
#elif defined(BUILD_MACHINE)
libMachine = "build"
#else
libMachine = "none"
#endif
