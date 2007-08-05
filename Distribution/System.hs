
module Distribution.System where

data OS = Linux | Windows Windows | Other String
data Windows = MingW

os :: OS
os =
#if defined(linux_HOST_OS)
    Linux
#elif defined(mingw32_HOST_OS)
    Windows MingW
#else
    Other System.Info.os
#endif

