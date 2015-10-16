/* stty.posix.c                                  -*- mode: c; coding: utf-8; -*-
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
#include <termios.h>
#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "sagittarius-stty.h"

void Sg_SetConsoleMode(SgObject port, int mode)
{
  SgObject file = Sg_PortFile(SG_PORT(port));
  int fd = (int)Sg_FileFD(file);
  struct termios ti;

  tcgetattr(fd, &ti);		/* call this first to retrieve others. */
  /* set the c_lflag (local) */
  ti.c_lflag = mode;
  if (tcsetattr(fd, TCSANOW, &ti) < 0) {
    int e = errno;
    Sg_SystemError(e, UC("Failed to set console mode: %A"),
		   Sg_GetLastErrorMessageWithErrorCode(e));
  }
}

SgObject Sg_GetConsoleMode(SgObject port)
{
  SgObject file = Sg_PortFile(SG_PORT(port));
  int fd = (int)Sg_FileFD(file);
  struct termios ti;

  if (tcgetattr(fd, &ti) < 0) {
    return SG_FALSE;
  }
  return SG_MAKE_INT(ti.c_lflag);
}

int Sg_GetConsoleModeFlag(int mode)
{
  switch (mode) {
    /* Windows ENABLE_PROCESSED_OUTPUT / ENABLE_PROCESSED_INPUT
       does the same as ICANON | ISIG. (Ctrl+C won't be send to
       the system without it). So CANON shold do like this.
     */
  case SG_CONSOLE_CANON: return ICANON | ISIG;
  case SG_CONSOLE_ECHO: return ECHO;
  }
  return 0;
}
