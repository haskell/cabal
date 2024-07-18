{-# LANGUAGE CPP #-}
#if mingw32_HOST_OS
{-# OPTIONS_GHC -F -pgmF my-foreign-preprocessor.bat #-}
#else
{-# OPTIONS_GHC -F -pgmF my-foreign-preprocessor #-}
#endif
module A where

a :: String
a = "0000"
