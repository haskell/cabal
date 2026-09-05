#include "pgmllib.h"

/* The "real" implementation - returns 0, the wrong value.
 * When the linker is driven by scripts/cc-wrapper.sh (passed to GHC by
 * Cabal as -pgml, because the wrapper is the --with-gcc compiler), the
 * linker option -Wl,--wrap=meaning_of_life_pgml redirects all calls to
 * this function to __wrap_meaning_of_life_pgml below. */
int meaning_of_life_pgml(void) {
    return 0;
}

/* The wrapper the linker substitutes in place of the real function.
 * Returns 66 - see scripts/cc-wrapper.sh. */
int __wrap_meaning_of_life_pgml(void) {
    return 66;
}
