{-# LANGUAGE CPP #-}

-- | Win32 API 'GetShortPathName' function, which returns MS DOS short path name
-- (up to 8 characters for file name + 3 for file extension).
--
-- What's going on here? Why do we care about MS DOS?
-- In practice the short name serves as an alternative name,
-- which does not contain spaces even if the original name does.
-- Some applications (including certain versions of Autoconf) do not like
-- spaces in filenames, so getting a short path name gives us
-- a chance to work around it. That's not bullet proof though:
-- some objects might not have a short name.
--
-- Writing this comment in 2026, I don't know whether the aforementioned
-- issue with Autoconf and spaces remains relevant. It's possible
-- that things have improved during the last 10 years
-- since https://github.com/haskell/cabal/issues/3185 was merged.
--
-- Compare to the similar functionality in Stack:
-- https://hackage.haskell.org/package/stack-3.3.1/docs/src/Stack.Config.html#local-6989586621679973356
module Distribution.Compat.GetShortPathName (getShortPathName)
where

#ifdef mingw32_HOST_OS

import System.Win32.Info (getShortPathName)

#else

getShortPathName :: FilePath -> IO FilePath
getShortPathName path = return path

#endif
