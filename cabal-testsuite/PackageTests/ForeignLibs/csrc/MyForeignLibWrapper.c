#include <stdlib.h>
#include <stdbool.h>
#include "HsFFI.h"

bool myForeignLibInit(void){
  int argc = 2;
  char *argv[] = { "+RTS", "-A32m", NULL };
  char **pargv = argv;

  // Initialize Haskell runtime
  hs_init(&argc, &pargv);

  // do any other initialization here and
  // return false if there was a problem
  return true;
}

void myForeignLibExit(void){
  hs_exit();
}

int cFoo2() {
   return 1234;
}
