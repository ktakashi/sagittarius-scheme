#include "pty.h"
#define WIN32_LEAN_AND_MEAN
#include <stdio.h>
#include <stdlib.h>

/* unfortunately, second argument will be ignored :( */
int init_pty(pty_t *pty)
{
  HANDLE hInRead, hOutWrite;
  if (!CreatePipe(&hInRead, &pty->out_fd, NULL, 0)) return LastError();
  if (!CreatePipe(&pty->in_fd, &hOutWrite, NULL, 0)) return LastError();

  COORD size = {80, 25};
  return CreatePseudoConsole(size, hInRead, hOutWrite, 0, &pty->hPC);
}

void pty_close(pty_t *pty) {
  CloseHandle(pty->out_fd);
  CloseHandle(pty->in_fd);
  ClosePseudoConsole(pty->hPC);
}

int pty_resize(pty_t *pty, int cols, int rows)
{
  COORD size;
  size.X = (SHORT)cols;
  size.Y = (SHORT)rows;
  return ResizePseudoConsole(pty->hPC, size);
}

int pty_tcsetattr(pty_t *pty, struct termios *termios)
{
  HANDLE hDup = NULL;
  int fd;
  if (DuplicateHandle(GetCurrentProcess(), pty->hPC,,
		      GetCurrentProcess(), &hDup,
		      0, FALSE, DUPLICATE_SAME_ACCESS)) {
    return GetLastError();
  }
  fd = _open_osfhandle((intptr_t)hDup, _O_APPEND | _O_RDONLY);
  if (tcsetattr(fd, TCSANOW, termios) < 0) {
    return GetLastError();
  }
  return 0;
}
