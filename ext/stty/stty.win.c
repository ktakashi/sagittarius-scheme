/* stty.win.c                                    -*- mode: c; coding: utf-8; -*-
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
#include <windows.h>
#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "sagittarius-stty.h"

void Sg_SetConsoleMode(SgObject port, int mode)
{
  SgObject file = Sg_PortFile(SG_PORT(port));
  HANDLE h = (HANDLE)Sg_FileFD(file);
  if (!SetConsoleMode(h, mode)) {
    int e = GetLastError();
    Sg_SystemError(e, UC("Failed to set console mode: %A"),
		   Sg_GetLastErrorMessageWithErrorCode(e));
  }
}

SgObject Sg_GetConsoleMode(SgObject port)
{
  SgObject file = Sg_PortFile(SG_PORT(port));
  HANDLE h = (HANDLE)Sg_FileFD(file);
  DWORD mode;
  if (!GetConsoleMode(h, &mode)) {
    return SG_FALSE;
  }
  return SG_MAKE_INT(mode);
}

int Sg_GetConsoleModeFlag(int mode)
{
  switch (mode) {
  case SG_CONSOLE_CANON: return ENABLE_PROCESSED_INPUT | ENABLE_LINE_INPUT;
  case SG_CONSOLE_ECHO: return ENABLE_ECHO_INPUT;
  }
  return 0;
}
