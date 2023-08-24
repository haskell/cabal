#include <sys/ioctl.h>

unsigned long vty_c_get_window_size(int fd) {
    struct winsize w;
    if (ioctl (fd, TIOCGWINSZ, &w) >= 0)
        return (w.ws_row << 16) + w.ws_col;
    else
        return 0x190050;
}

void vty_c_set_window_size(int fd, unsigned long val) {
    struct winsize w;
    if (ioctl(fd, TIOCGWINSZ, &w) >= 0) {
        w.ws_row = val >> 16;
        w.ws_col = val & 0xFFFF;
        ioctl(fd, TIOCSWINSZ, &w);
    }
}
