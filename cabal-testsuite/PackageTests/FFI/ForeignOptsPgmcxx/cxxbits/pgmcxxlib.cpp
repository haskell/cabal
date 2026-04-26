#include "pgmcxxlib.h"

#ifndef __TESTOPT_PGMCXX__
#error "Did not get required __TESTOPT_PGMCXX__ from the -pgmcxx wrapper"
#endif

int meaning_of_life_pgmcxx() {
    return __TESTOPT_PGMCXX__;
}
