#include "cxxlib.h"

#ifdef __TESTOPT_C__
#error "Got unexpected __TESTOPT_C__ from cc-options"
#endif

#ifndef __TESTOPT_CXX__
#error "Did not get required __TESTOPT_CXX__ from cxx-options"
#endif

int meaning_of_life_cxx() {
    return __TESTOPT_CXX__;
}
