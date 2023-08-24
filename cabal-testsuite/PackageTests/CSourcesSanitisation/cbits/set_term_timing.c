#include <termios.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>

void vty_set_term_timing(int fd, int vmin, int vtime)
{
    struct termios trm;
    tcgetattr(fd, &trm);
    trm.c_cc[VMIN] = vmin;
    trm.c_cc[VTIME] = vtime;
    tcsetattr(fd, TCSANOW, &trm);
}
