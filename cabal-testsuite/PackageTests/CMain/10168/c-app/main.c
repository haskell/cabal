#include <stdio.h>
#include <HsFFI.h>
#ifdef __GLASGOW_HASKELL__
#include "Lib_stub.h"
#endif

int main(int argc, char *argv[]) {
  hs_init(&argc, &argv);
  printf("%lld\n", myMax(10,100));
  hs_exit();
  return 0;
}
