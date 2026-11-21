{-# LANGUAGE CPP #-}

-- | The versions that the macros of cabal_macros.h give, which Cabal passes to
-- mhs as -D options.
module Versions (versions) where

versions :: [String]
versions = [base, bytestring, "package " ++ CURRENT_PACKAGE_VERSION, tool]

base :: String
#if MIN_VERSION_base(4,19,0) && !MIN_VERSION_base(5,0,0)
base = "base " ++ VERSION_base
#else
base = "MIN_VERSION_base is missing or wrong"
#endif

bytestring :: String
#if MIN_VERSION_bytestring(0,11,0)
bytestring = "bytestring " ++ VERSION_bytestring
#else
bytestring = "MIN_VERSION_bytestring is missing or wrong"
#endif

tool :: String
#if defined(MIN_TOOL_VERSION_mhs)
tool = "tool mhs"
#else
tool = "MIN_TOOL_VERSION_mhs is missing"
#endif
