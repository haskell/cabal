#ifndef LDLIB_H
#define LDLIB_H

/* The "real" function; with --wrap, calls to this are redirected by the linker. */
int meaning_of_life_ld_real();

/* The wrapper that the linker calls instead of the real function. */
int __wrap_meaning_of_life_ld_real();

#endif
