#include <stdio.h>
#include <stdbool.h>

/* Forward declarations */
bool myForeignLibInit();
void myForeignLibExit();
void sayHi();

int main()
{
  myForeignLibInit();
  sayHi();
  myForeignLibExit();
  return 0;
}
