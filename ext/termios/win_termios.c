/* win_termios.c                                 -*- mode: c; coding: utf-8; -*-
 *
 *   Copyright (c) 2015  Takashi Kato <ktakashi@ymail.com>
 *
 *   Redistribution and use in source and binary forms, with or without
 *   modification, are permitted provided that the following conditions
 *   are met:
 *
 *   1. Redistributions of source code must retain the above copyright
 *      notice, this list of conditions and the following disclaimer.
 *
 *   2. Redistributions in binary form must reproduce the above copyright
 *      notice, this list of conditions and the following disclaimer in the
 *      documentation and/or other materials provided with the distribution.
 *
 *   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 *   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 *   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
 *   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
 *   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 *   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
 *   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 *   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 *   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 *   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 *   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#ifndef WIN32_LEAN_AND_MEAN
# define WIN32_LEAN_AND_MEAN
#endif
#include <windows.h>
#include <errno.h>
#include <io.h>
#if defined(WIN_TERMIOS_DLL)
#define WIN_TERMIOS_DLL_BODY
#endif
#include "win_termios.h"

#define set_errno(e) errno = (e)
/* #ifdef WIN_TERMIOS_DEBUG */
#if 0
# define report_error(name) 	/* dummy */
#else
# include <stdio.h>
# define report_error(name)					\
  do {								\
    fprintf(stderr, name": last_error = %d\n", GetLastError());	\
  } while (0)
#endif

/* accessors. these are simple */
speed_t cfgetispeed(const struct termios *termios_p)
{
  return termios_p->c_ispeed;
}
speed_t cfgetospeed(const struct termios * termios_p)
{
  return termios_p->c_ospeed;
}

static const speed_t speed_table[] = {
  B0, B50, B75, B110, B134, B150, B200, B300, B600, B1200, B1800, B2400,
  B4800, B9600, B19200, B38400
};

static int is_supported_speed(speed_t s)
{
  for (size_t i = 0; i < sizeof(speed_table)/sizeof(speed_table[0]); i++) {
    if (speed_table[i] == s) return TRUE;
  }
  return FALSE;
}
int cfsetispeed(struct termios *termios_p, speed_t ispeed)
{
  if (!is_supported_speed(ispeed)) return -1;
  termios_p->c_ispeed = ispeed;
  return 0;
}
int cfsetospeed(struct termios *termios_p, speed_t ospeed)
{
  if (!is_supported_speed(ospeed)) return -1;
  termios_p->c_ospeed = ospeed;
  return 0;
}

static int finish_up(HANDLE hComm, DWORD event)
{
  DWORD flag;
  /* get original flag */
  if (!GetCommMask(hComm, &flag)) {
    report_error("finish_up");
    /* TODO */
    set_errno(ENOTTY);
    return -1;
  }
  /* Send event */
  SetCommMask(hComm, flag | event);
  return 0;
}

/*
  FlushFileBuffers.
 */
int tcdrain(HANDLE hComm)
{
  if (hComm == INVALID_HANDLE_VALUE) {
    /* EBADF is set by _get_osfhandle */
    return -1;
  }
  if (!FlushFileBuffers(hComm)) {
    /* why then failed? */
    if (GetLastError() == 0) {
      set_errno(0);
      return 0;
    }
    report_error("tcdrain");
    set_errno(ENOTTY);
    return -1;
  }
  return finish_up(hComm, EV_TXEMPTY);
}

int tcflow(HANDLE hComm, int action)
{
  switch (action) {
  /* If action is TCOOFF, output is suspended. */
  case TCOOFF:
  /* If action is TCOON, suspended output is restarted. */
  case TCOON:
  /* If action is TCIOFF, the system transmits a STOP character,
     which is intended to cause the terminal device to stop
     transmitting data to the system. */
  case TCIOFF:
  /* If action is TCION, the system transmits a START character,
     which is intended to cause the terminal device to start
     transmitting data to the system.  */
  case TCION:
    /* FIXME sorry do nothing */
    break;
  default:
    set_errno(EINVAL);
    return -1;
  }
  return 0;
}

/*
   PurgeComm
 */
int tcflush(int fd, int queue_selector)
{
  HANDLE hComm = (HANDLE)_get_osfhandle(fd);
  DWORD flag;

  if (hComm == INVALID_HANDLE_VALUE) {
    /* EBADF is set by _get_osfhandle */
    return -1;
  }
  switch (queue_selector) {
    /* If queue_selector is TCIFLUSH it flushes data received but
       not read. */
  case TCIFLUSH: flag = PURGE_RXABORT; break;
    /* If queue_selector is TCOFLUSH it flushes data written but
       not transmitted. */
  case TCOFLUSH: flag = PURGE_TXABORT; break;
    /* If queue_selector is TCIOFLUSH it flushes both data received
       but not read and data written but not transmitted.  */
  case TCIOFLUSH: flag = PURGE_RXABORT | PURGE_TXABORT; break;
  default:
    set_errno(EINVAL);
    return -1;
  }
  if (!PurgeComm(hComm, flag)) {
    report_error("tcflush");
    set_errno(ENOTTY);
    return -1;
  }
  return 0;
}

/* GetCommState, GetCommTimeouts
   GetConsoleMode
 */
int tcgetattr(HANDLE hComm, struct termios * termios_p)
{
#define toggle_flag(expr, termois_p, slot, flag)	\
  do {							\
    if (expr) {						\
      (termois_p)-> slot |= (flag);			\
    } else {						\
      (termois_p)-> slot &= ~(flag);			\
    }							\
  } while (0)

  DCB dcb;
  COMMTIMEOUTS timeouts;

  if (hComm == INVALID_HANDLE_VALUE) {
    /* EBADF is set by _get_osfhandle */
    return -1;
  }
  /* initialise it */
  memset(termios_p, 0, sizeof(struct termios));

  dcb.DCBlength = sizeof(DCB);
  /* most likely not a serial port so ignore */
  if (GetCommState(hComm, &dcb)) {
    /* should we separate this process? */
    /* c_iflag */
    toggle_flag(dcb.fParity, termios_p, c_iflag, INPCK);
    toggle_flag(!dcb.fParity, termios_p, c_iflag, IGNPAR);
    /* TOOD IGNBRK */
    /* TODO BRKINT */
    toggle_flag(dcb.fOutX, termios_p, c_iflag, IXON);
    toggle_flag(dcb.fInX, termios_p, c_iflag, IXOFF);
    toggle_flag(dcb.fTXContinueOnXoff, termios_p, c_iflag, IXANY);

    /* c_cflag */
    switch(dcb.StopBits) {
    case TWOSTOPBITS: termios_p->c_cflag |= CSTOPB; break;
    case ONESTOPBIT: termios_p->c_cflag &= ~CSTOPB; break;
      /* TODO ONE5STOPBITS 1.5 stop bits*/
    default: break;
    }

    if (dcb.fParity) {
      termios_p->c_cflag |= PARENB;
      switch (dcb.Parity) {
	/* TODO space parity */
      case MARKPARITY: case ODDPARITY: termios_p->c_cflag |= PARODD; break;
      case NOPARITY: termios_p->c_cflag &= ~(PARODD | PARENB); break;
      case EVENPARITY:		/* not even */
	/* TODO  */
      case SPACEPARITY:
      default:
	break;
      }
    }
    switch (dcb.ByteSize) {
    case 5: termios_p->c_cflag |= CS5; break;
    case 6: termios_p->c_cflag |= CS6; break;
    case 7: termios_p->c_cflag |= CS7; break;
    case 8:
      /* fall through  */
    default:
      termios_p->c_cflag |= CS8; break;
    }
    /* TODO should we add hardware flow control? but it's not posix */
    if (!GetCommTimeouts(hComm, &timeouts)) {
      report_error("tcgetattr(timeouts)");
      set_errno(ENOTTY);
      return -1;
    }
    termios_p->c_cc[VTIME] = (cc_t)(timeouts.ReadTotalTimeoutConstant/100);
    termios_p->c_cc[VSTART] = dcb.XonChar;
    termios_p->c_cc[VSTOP] = dcb.XoffChar;
    termios_p->c_cc[VEOF] = dcb.EofChar;
    if (dcb.BaudRate > B38400) {
      set_errno(EINVAL);
      return -1;
    }
    termios_p->c_ispeed = dcb.BaudRate;
    termios_p->c_ospeed = dcb.BaudRate;
    SetLastError(0);
    /* TODO lflags on serial port... but how?*/
  } else {
    /* OK this must be a keybord input thing */
    DWORD mode;
    if (!GetConsoleMode(hComm, &mode)) {
      /* you are not interesting at all, dump! */
      set_errno(GetLastError());
      return -1;
    }
    /* ok on Windows, the keyboard input flags are very limited */
    toggle_flag(mode&ENABLE_ECHO_INPUT, termios_p, c_lflag, ECHO);
    toggle_flag(mode&ENABLE_LINE_INPUT, termios_p, c_lflag, ICANON);
    toggle_flag(mode&ENABLE_PROCESSED_INPUT, termios_p, c_lflag,
		(ISIG|IEXTEN));
    /* ok some things */
    if (!(mode&ENABLE_PROCESSED_INPUT) && mode&ENABLE_ECHO_INPUT) {
      /* this means there is nothing is processed but showing */
      termios_p->c_lflag |= ECHOK;
      termios_p->c_lflag |= ECHONL;
      termios_p->c_lflag |= ECHOE;
    }
    termios_p->c_oflag |= OPOST;
    termios_p->c_cflag |= CREAD | CS8;
    termios_p->c_cc[VMIN] = 1;
    termios_p->c_cc[VTIME] = 0;
    SetLastError(0);
    
  }
  return 0;
#undef toggle_flag
}

pid_t tcgetsid(HANDLE hComm)
{
  /* FIXME */
  set_errno(EBADF);
  return -1;
}

/* SetCommBreak and ClearCommBreak */
int tcsendbreak(HANDLE hComm, int duration)
{
  COMSTAT stat;
  DWORD errorCode;
  if (hComm == INVALID_HANDLE_VALUE) {
    /* EBADF is set by _get_osfhandle */
    return -1;
  }
  if (duration <= 0) duration = 1;

  if (!SetCommBreak(hComm)) {
    ClearCommError(hComm, &errorCode, &stat);
  }
  /* this sleeps multiple of 250ms  */
  Sleep(duration * 250);
  if (!ClearCommBreak(hComm)) {
    ClearCommError(hComm, &errorCode, &stat);
  }
  return 0;
}

int tcsetattr(HANDLE hComm, int actions, struct termios *termios_p)
{
  DCB dcb;

  if (hComm == INVALID_HANDLE_VALUE) {
    /* EBADF is set by _get_osfhandle */
    return -1;
  }

  dcb.DCBlength = sizeof(DCB);

  if (GetCommState(hComm, &dcb)) {
    int vtime;
    COMMTIMEOUTS timeouts;
    /* ok serial port */
    if (!GetCommTimeouts(hComm, &timeouts)) {
      set_errno(GetLastError());
      return -1;
    }
    dcb.fBinary = (termios_p->c_iflag & ISTRIP) ? TRUE: FALSE;
    /* TODO IGNBRK */
    /* TODO BRKINT */
    dcb.fOutX = (termios_p->c_iflag & IXON)  ? TRUE: FALSE;
    dcb.fInX  = (termios_p->c_iflag & IXOFF) ? TRUE: FALSE;;
    dcb.fTXContinueOnXoff  = (termios_p->c_iflag & IXANY) ? TRUE: FALSE;;
    dcb.XonChar  = termios_p->c_cc[VSTART];
    dcb.XoffChar = termios_p->c_cc[VSTOP];
    dcb.EofChar  = termios_p->c_cc[VEOF];

#if 0
    /* should we add this? */
    dcb.BaudRate = termios_p->c_ospeed;
    switch (termios_p->c_cflag & CSIZE) {
    case CS5: dcb.ByteSize = 5; break;
    case CS6: dcb.ByteSize = 6; break;
    case CS7: dcb.ByteSize = 7; break;
    case CS8: dcb.ByteSize = 8; break;
    }
    if (termios_p->c_cflag & PARENB) {
      dcb.fParity = TRUE;
      dcb.Parity = (tio->c_cflag & PARODD) ? ODDPARITY : EVENPARITY;
    } else {
      dcb.fParity = FALSE;
      dcb.Parity = NOPARITY;
    }
    dcb.StopBits = (termios_p->c_cflag & CSTOPB) : TWOSTOPBITS : ONE5STOPBITS;
    dcb.fBinary = TRUE;
#endif
    /* overwrites ISTRIP */
    if (dcb.EofChar != '\0') {
      dcb.fBinary = FALSE;
    }
    if (!SetCommState(hComm, &dcb)) {
      set_errno(EINVAL);
      return -1;
    }
    /* keep default interval */
    vtime = termios_p->c_cc[VTIME] * 100;
    timeouts.ReadTotalTimeoutConstant = vtime;
    timeouts.WriteTotalTimeoutConstant = vtime;
    /* set it */
    if (!SetCommTimeouts(hComm, &timeouts)) {
      set_errno(EINVAL);
      return -1;
    }
    SetLastError(0);
  } else {
    /* keyboard input */
    DWORD mode;
    if (!GetConsoleMode(hComm, &mode)) {
      set_errno(GetLastError());
      return -1;
    }
#define toggle_flag(mode, termios_p, slot, flag, set)	\
    do {						\
      if ((termios_p)-> slot & (flag)) {		\
	(mode) |= (set);				\
      } else {						\
	(mode) &= ~(set);				\
      }							\
    } while (0)

    /* again we ignores a lot */
    /* ICANON should treat control chars on system
       this must be first so that ECHO can set ENABLE_LINE_INPUT
       when ICANON is off.
     */
    toggle_flag(mode, termios_p, c_lflag, ICANON,
		(ENABLE_PROCESSED_INPUT|ENABLE_LINE_INPUT));

    /* ENABLE_ECHO_INPUT requires ENABLE_LINE_INPUT */
    toggle_flag(mode, termios_p, c_lflag, ECHO,
		(ENABLE_ECHO_INPUT|ENABLE_LINE_INPUT));
    if (!SetConsoleMode(hComm, mode)) {
      set_errno(GetLastError());
      return -1;
    }
    if (termios_p->c_iflag & IUTF8) {
      SetConsoleCP(CP_UTF8);
      SetConsoleOutputCP(CP_UTF8);
    }
    
    SetLastError(0);
    /* TODO how can we simulate others? */
#undef toggle_flag
  }

  return 0;
}
