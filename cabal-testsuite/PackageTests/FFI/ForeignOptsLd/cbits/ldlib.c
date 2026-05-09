#include "ldlib.h"

/* The "real" implementation - returns 0, the wrong value.
 * With ld-options: -Wl,--wrap=meaning_of_life_ld_real, the linker redirects
 * all calls to this function to __wrap_meaning_of_life_ld_real below. */
int meaning_of_life_ld_real() {
    return 0;
}

/* The wrapper that the linker substitutes in place of the real function.
 * Returns 55 - see ld-options in the cabal file. */
int __wrap_meaning_of_life_ld_real() {
    return 55;
}
