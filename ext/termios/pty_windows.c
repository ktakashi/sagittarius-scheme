#include "pty.h"
#define WIN32_LEAN_AND_MEAN
#include <stdio.h>
#include <stdlib.h>

/* unfortunately, second argument will be ignored :( */
int init_pty(pty_t *pty)
{
  HANDLE hInRead, hOutWrite;
  if (!CreatePipe(&hInRead, (PHANDLE)&pty->out_fd, NULL, 0))
    return GetLastError();
  if (!CreatePipe((PHANDLE)&pty->in_fd, &hOutWrite, NULL, 0))
    return GetLastError();

  COORD size = {80, 25};
  return CreatePseudoConsole(size, hInRead, hOutWrite, 0, &pty->hPC);
}

void pty_close(pty_t *pty) {
  CloseHandle((HANDLE)pty->out_fd);
  CloseHandle((HANDLE)pty->in_fd);
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
  if (tcsetattr((HANDLE)pty->hPC, TCSANOW, termios) < 0) {
    return GetLastError();
  }
  return 0;
}
