#include "pgmclib.h"

#ifndef __TESTOPT_PGMC__
#error "Did not get required __TESTOPT_PGMC__ from the -pgmc wrapper"
#endif

int meaning_of_life_pgmc() {
    return __TESTOPT_PGMC__;
}
