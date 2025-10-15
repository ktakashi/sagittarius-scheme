#include "pty.h"
#include <utmp.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <sys/wait.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#ifdef HAVE_UTIL_H
# include <util.h>
#endif
#ifdef HAVE_LIBUTIL_H
# icnlude <libutil.h>
#endif

int init_pty(pty_t *pty)
{
  if (openpty((int *)&pty->in_fd, (int *)&pty->slave_fd, NULL, NULL, NULL) == -1) {
    return errno;
  }
  pty->out_fd = pty->in_fd;	/* the same */
  tcgetattr(pty->slave_fd, &pty->tmio);
  return 0;
}

void pty_close(pty_t *pty)
{
  close(pty->in_fd);
}

int pty_resize(pty_t *pty, int cols, int rows)
{
  struct winsize ws;
  ws.ws_col = cols;
  ws.ws_row = rows;
  ws.ws_xpixel = 0;
  ws.ws_ypixel = 0;
  if (ioctl(pty->in_fd, TIOCSWINSZ, &ws) == -1) {
    return errno;
  }
  return 0;
}


int pty_tcsetattr(pty_t *pty, struct termios *termios)
{
  if (tcsetattr(pty->slave_fd, TCSANOW, termios) < 0) {
    return errno;
  }
  return 0;
}
