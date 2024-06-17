{- FOURMOLU_DISABLE -}
{-# LANGUAGE DeriveGeneric #-}
module Distribution.Simple.Build.PathsModule.Z (render, Z(..)) where
import Distribution.ZinzaPrelude
data Z
    = Z {zPackageName :: PackageName,
         zVersionDigits :: String,
         zSupportsCpp :: Bool,
         zSupportsNoRebindableSyntax :: Bool,
         zAbsolute :: Bool,
         zRelocatable :: Bool,
         zIsWindows :: Bool,
         zIsI386 :: Bool,
         zIsX8664 :: Bool,
         zPrefix :: FilePath,
         zBindir :: FilePath,
         zLibdir :: FilePath,
         zDynlibdir :: FilePath,
         zDatadir :: FilePath,
         zLibexecdir :: FilePath,
         zSysconfdir :: FilePath,
         zNot :: (Bool -> Bool),
         zManglePkgName :: (PackageName -> String)}
    deriving Generic
render :: Z -> String
render z_root = execWriter $ do
  if (zSupportsCpp z_root)
  then do
    tell "{-# LANGUAGE CPP #-}\n"
    return ()
  else do
    return ()
  if (zSupportsNoRebindableSyntax z_root)
  then do
    tell "{-# LANGUAGE NoRebindableSyntax #-}\n"
    return ()
  else do
    return ()
  if (zNot z_root (zAbsolute z_root))
  then do
    tell "{-# LANGUAGE ForeignFunctionInterface #-}\n"
    return ()
  else do
    return ()
  if (zSupportsCpp z_root)
  then do
    tell "#if __GLASGOW_HASKELL__ >= 810\n"
    tell "{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}\n"
    tell "#endif\n"
    return ()
  else do
    return ()
  tell "{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}\n"
  tell "{-# OPTIONS_GHC -w #-}\n"
  tell "module Paths_"
  tell (zManglePkgName z_root (zPackageName z_root))
  tell " (\n"
  tell "    version,\n"
  tell "    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,\n"
  tell "    getDataFileName, getSysconfDir\n"
  tell "  ) where\n"
  tell "\n"
  if (zNot z_root (zAbsolute z_root))
  then do
    tell "import Foreign\n"
    tell "import Foreign.C\n"
    return ()
  else do
    return ()
  tell "\n"
  tell "import qualified Control.Exception as Exception\n"
  tell "import qualified Data.List as List\n"
  tell "import Data.Version (Version(..))\n"
  tell "import System.Environment (getEnv)\n"
  tell "import Prelude\n"
  tell "\n"
  if (zRelocatable z_root)
  then do
    tell "import System.Environment (getExecutablePath)\n"
    return ()
  else do
    return ()
  tell "\n"
  if (zSupportsCpp z_root)
  then do
    tell "#if defined(VERSION_base)\n"
    tell "\n"
    tell "#if MIN_VERSION_base(4,0,0)\n"
    tell "catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a\n"
    tell "#else\n"
    tell "catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a\n"
    tell "#endif\n"
    tell "\n"
    tell "#else\n"
    tell "catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a\n"
    tell "#endif\n"
    tell "catchIO = Exception.catch\n"
    return ()
  else do
    tell "catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a\n"
    tell "catchIO = Exception.catch\n"
    return ()
  tell "\n"
  tell "version :: Version\n"
  tell "version = Version "
  tell (zVersionDigits z_root)
  tell " []\n"
  tell "\n"
  tell "getDataFileName :: FilePath -> IO FilePath\n"
  tell "getDataFileName name = do\n"
  tell "  dir <- getDataDir\n"
  tell "  return (dir `joinFileName` name)\n"
  tell "\n"
  tell "getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath\n"
  tell "\n"
  let
    z_var0_function_defs = do
      tell "minusFileName :: FilePath -> String -> FilePath\n"
      tell "minusFileName dir \"\"     = dir\n"
      tell "minusFileName dir \".\"    = dir\n"
      tell "minusFileName dir suffix =\n"
      tell "  minusFileName (fst (splitFileName dir)) (fst (splitFileName suffix))\n"
      tell "\n"
      tell "splitFileName :: FilePath -> (String, String)\n"
      tell "splitFileName p = (reverse (path2++drive), reverse fname)\n"
      tell "  where\n"
      tell "    (path,drive) = case p of\n"
      tell "      (c:':':p') -> (reverse p',[':',c])\n"
      tell "      _          -> (reverse p ,\"\")\n"
      tell "    (fname,path1) = break isPathSeparator path\n"
      tell "    path2 = case path1 of\n"
      tell "      []                           -> \".\"\n"
      tell "      [_]                          -> path1   -- don't remove the trailing slash if\n"
      tell "                                              -- there is only one character\n"
      tell "      (c:path') | isPathSeparator c -> path'\n"
      tell "      _                             -> path1\n"
      return ()
  tell "\n"
  tell "\n"
  if (zRelocatable z_root)
  then do
    tell "\n"
    tell "getPrefixDirReloc :: FilePath -> IO FilePath\n"
    tell "getPrefixDirReloc dirRel = do\n"
    tell "  exePath <- getExecutablePath\n"
    tell "  let (dir,_) = splitFileName exePath\n"
    tell "  return ((dir `minusFileName` "
    tell (zBindir z_root)
    tell ") `joinFileName` dirRel)\n"
    tell "\n"
    tell "getBinDir     = catchIO (getEnv \""
    tell (zManglePkgName z_root (zPackageName z_root))
    tell "_bindir\")     (\\_ -> getPrefixDirReloc $ "
    tell (zBindir z_root)
    tell ")\n"
    tell "getLibDir     = catchIO (getEnv \""
    tell (zManglePkgName z_root (zPackageName z_root))
    tell "_libdir\")     (\\_ -> getPrefixDirReloc $ "
    tell (zLibdir z_root)
    tell ")\n"
    tell "getDynLibDir  = catchIO (getEnv \""
    tell (zManglePkgName z_root (zPackageName z_root))
    tell "_dynlibdir\")  (\\_ -> getPrefixDirReloc $ "
    tell (zDynlibdir z_root)
    tell ")\n"
    tell "getDataDir    = catchIO (getEnv \""
    tell (zManglePkgName z_root (zPackageName z_root))
    tell "_datadir\")    (\\_ -> getPrefixDirReloc $ "
    tell (zDatadir z_root)
    tell ")\n"
    tell "getLibexecDir = catchIO (getEnv \""
    tell (zManglePkgName z_root (zPackageName z_root))
    tell "_libexecdir\") (\\_ -> getPrefixDirReloc $ "
    tell (zLibexecdir z_root)
    tell ")\n"
    tell "getSysconfDir = catchIO (getEnv \""
    tell (zManglePkgName z_root (zPackageName z_root))
    tell "_sysconfdir\") (\\_ -> getPrefixDirReloc $ "
    tell (zSysconfdir z_root)
    tell ")\n"
    tell "\n"
    z_var0_function_defs
    tell "\n"
    return ()
  else do
    if (zAbsolute z_root)
    then do
      tell "\n"
      tell "bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath\n"
      tell "bindir     = "
      tell (zBindir z_root)
      tell "\n"
      tell "libdir     = "
      tell (zLibdir z_root)
      tell "\n"
      tell "dynlibdir  = "
      tell (zDynlibdir z_root)
      tell "\n"
      tell "datadir    = "
      tell (zDatadir z_root)
      tell "\n"
      tell "libexecdir = "
      tell (zLibexecdir z_root)
      tell "\n"
      tell "sysconfdir = "
      tell (zSysconfdir z_root)
      tell "\n"
      tell "\n"
      tell "getBinDir     = catchIO (getEnv \""
      tell (zManglePkgName z_root (zPackageName z_root))
      tell "_bindir\")     (\\_ -> return bindir)\n"
      tell "getLibDir     = catchIO (getEnv \""
      tell (zManglePkgName z_root (zPackageName z_root))
      tell "_libdir\")     (\\_ -> return libdir)\n"
      tell "getDynLibDir  = catchIO (getEnv \""
      tell (zManglePkgName z_root (zPackageName z_root))
      tell "_dynlibdir\")  (\\_ -> return dynlibdir)\n"
      tell "getDataDir    = catchIO (getEnv \""
      tell (zManglePkgName z_root (zPackageName z_root))
      tell "_datadir\")    (\\_ -> return datadir)\n"
      tell "getLibexecDir = catchIO (getEnv \""
      tell (zManglePkgName z_root (zPackageName z_root))
      tell "_libexecdir\") (\\_ -> return libexecdir)\n"
      tell "getSysconfDir = catchIO (getEnv \""
      tell (zManglePkgName z_root (zPackageName z_root))
      tell "_sysconfdir\") (\\_ -> return sysconfdir)\n"
      tell "\n"
      return ()
    else do
      if (zIsWindows z_root)
      then do
        tell "\n"
        tell "prefix :: FilePath\n"
        tell "prefix = "
        tell (zPrefix z_root)
        tell "\n"
        tell "\n"
        tell "getBinDir     = getPrefixDirRel $ "
        tell (zBindir z_root)
        tell "\n"
        tell "getLibDir     = "
        tell (zLibdir z_root)
        tell "\n"
        tell "getDynLibDir  = "
        tell (zDynlibdir z_root)
        tell "\n"
        tell "getDataDir    = catchIO (getEnv \""
        tell (zManglePkgName z_root (zPackageName z_root))
        tell "_datadir\")    (\\_ -> "
        tell (zDatadir z_root)
        tell ")\n"
        tell "getLibexecDir = "
        tell (zLibexecdir z_root)
        tell "\n"
        tell "getSysconfDir = "
        tell (zSysconfdir z_root)
        tell "\n"
        tell "\n"
        tell "getPrefixDirRel :: FilePath -> IO FilePath\n"
        tell "getPrefixDirRel dirRel = try_size 2048 -- plenty, PATH_MAX is 512 under Win32.\n"
        tell "  where\n"
        tell "    try_size size = allocaArray (fromIntegral size) $ \\buf -> do\n"
        tell "        ret <- c_GetModuleFileName nullPtr buf size\n"
        tell "        case ret of\n"
        tell "          0 -> return (prefix `joinFileName` dirRel)\n"
        tell "          _ | ret < size -> do\n"
        tell "              exePath <- peekCWString buf\n"
        tell "              let (bindir,_) = splitFileName exePath\n"
        tell "              return ((bindir `minusFileName` "
        tell (zBindir z_root)
        tell ") `joinFileName` dirRel)\n"
        tell "            | otherwise  -> try_size (size * 2)\n"
        tell "\n"
        z_var0_function_defs
        tell "\n"
        if (zIsI386 z_root)
        then do
          tell "foreign import stdcall unsafe \"windows.h GetModuleFileNameW\"\n"
          tell "  c_GetModuleFileName :: Ptr () -> CWString -> Int32 -> IO Int32\n"
          return ()
        else do
          if (zIsX8664 z_root)
          then do
            tell "foreign import ccall unsafe \"windows.h GetModuleFileNameW\"\n"
            tell "  c_GetModuleFileName :: Ptr () -> CWString -> Int32 -> IO Int32\n"
            return ()
          else do
            tell "-- win32 supported only with I386, X86_64\n"
            tell "c_GetModuleFileName :: Ptr () -> CWString -> Int32 -> IO Int32\n"
            tell "c_GetModuleFileName  = _\n"
            return ()
          return ()
        tell "\n"
        return ()
      else do
        tell "\n"
        tell "notRelocAbsoluteOrWindows :: ()\n"
        tell "notRelocAbsoluteOrWindows = _\n"
        tell "\n"
        return ()
      return ()
    return ()
  tell "\n"
  tell "\n"
  tell "joinFileName :: String -> String -> FilePath\n"
  tell "joinFileName \"\"  fname = fname\n"
  tell "joinFileName \".\" fname = fname\n"
  tell "joinFileName dir \"\"    = dir\n"
  tell "joinFileName dir fname\n"
  tell "  | isPathSeparator (List.last dir) = dir ++ fname\n"
  tell "  | otherwise                       = dir ++ pathSeparator : fname\n"
  tell "\n"
  tell "pathSeparator :: Char\n"
  if (zIsWindows z_root)
  then do
    tell "pathSeparator = '\\\\'\n"
    return ()
  else do
    tell "pathSeparator = '/'\n"
    return ()
  tell "\n"
  tell "isPathSeparator :: Char -> Bool\n"
  if (zIsWindows z_root)
  then do
    tell "isPathSeparator c = c == '/' || c == '\\\\'\n"
    return ()
  else do
    tell "isPathSeparator c = c == '/'\n"
    return ()
