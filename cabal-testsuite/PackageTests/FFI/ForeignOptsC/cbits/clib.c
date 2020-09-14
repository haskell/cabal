#include "clib.h"

#ifndef __TESTOPT_C__
#error "Did not get required __TESTOPT_C__ from cc-options"
#endif

#ifdef __TESTOPT_CXX__
#error "Got unexpected __TESTOPT_CXX__ from cxx-options"
#endif

int meaning_of_life_c() {
    return __TESTOPT_C__;
}
