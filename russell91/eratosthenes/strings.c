#include "stdio.h"
#include "stdlib.h"

typedef struct AbChar {
  unsigned char x;
  unsigned char y;
  unsigned char z;
} AbChar;

void printAbChar(AbChar c) {
  printf("%u.%u.%u\n", c.x, c.y, c.z);
}

int main() {
  AbChar c;
  c.x = 'a';
  c.y = 'b';
  c.z = 'c';

  AbChar* const pc = &c;
  pc->x = 'b';

  printf("hello, world\n");
  printAbChar(c);
}
