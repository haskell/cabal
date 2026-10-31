{-# LANGUAGE CPP #-}

-- | Documentation requires the same component and configured compiler options.
module PluginClient (value) where

-- | The value supplied through the configured compiler arguments.
-- The fallback deliberately fails typechecking when the arguments are missing.
value :: Int
#if defined(COMPONENT_OPTION_PRESENT) && defined(PROGRAM_OPTION_VALUE) && PROGRAM_OPTION_VALUE != 0
value = PROGRAM_OPTION_VALUE
#else
value = compilerArgumentsWereNotForwarded
#endif
