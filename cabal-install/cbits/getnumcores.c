#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 612) && !defined(CABAL_NO_THREADED)
/* Since version 6.12, GHC's threaded RTS includes a getNumberOfProcessors
   function, so we try to use that if available. cabal-install is always built
   with -threaded nowadays.  */
#define HAS_GET_NUMBER_OF_PROCESSORS
#endif


#ifndef HAS_GET_NUMBER_OF_PROCESSORS

#ifdef _WIN32
#include <windows.h>
#elif MACOS
#include <sys/param.h>
#include <sys/sysctl.h>
#elif __linux__
#include <unistd.h>
#endif

int getNumberOfProcessors() {
#ifdef WIN32
    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);
    return sysinfo.dwNumberOfProcessors;
#elif MACOS
    int nm[2];
    size_t len = 4;
    uint32_t count;

    nm[0] = CTL_HW; nm[1] = HW_AVAILCPU;
    sysctl(nm, 2, &count, &len, NULL, 0);

    if(count < 1) {
        nm[1] = HW_NCPU;
        sysctl(nm, 2, &count, &len, NULL, 0);
        if(count < 1) { count = 1; }
    }
    return count;
#elif __linux__
    return sysconf(_SC_NPROCESSORS_ONLN);
#else
    return 1;
#endif
}

#endif /* HAS_GET_NUMBER_OF_PROCESSORS */
