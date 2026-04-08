#ifndef MYDEF
#error "Did not get required MYDEF from cc-options"
#endif

static inline int myplus(int a, int b) {
    return a + b;
}
