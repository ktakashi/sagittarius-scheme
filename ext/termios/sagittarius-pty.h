/* sagittarius-pty.h                              -*- mode: c; coding: utf-8; -*-
 *
 *   Copyright (c) 2025  Takashi Kato <ktakashi@ymail.com>
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

#ifndef SAGITTARIUS_PTY_H_
#define SAGITTARIUS_PTY_H_

#include <sagittarius.h>
#include "ppty.h"

SG_CLASS_DECL(Sg_PtyClass);
#define SG_CLASS_PTY (&Sg_PtyClass)

typedef struct SgPtyRec {
  SG_HEADER;
  pty_t pty;
  uintptr_t pid;
  SgObject inp;
  SgObject outp;
} SgPty;

#define SG_PTY(o)  ((SgPty *)o)
#define SG_PTYP(o) SG_XTYPEP(o, SG_CLASS_PTY)

#define SG_PTY_PTY(o) (&(SG_PTY(o)->pty))

SG_CDECL_BEGIN

SgObject Sg_MakePty();
SgObject Sg_PtySpawn(SgObject pty, SgObject name, SgObject args,
		     SgObject dir, SgObject token);
void     Sg_PtyClose(SgObject pty);
int      Sg_PtyClosedP(SgObject pty);
void     Sg_PtyResize(SgObject pty, int cols, int rows);
void     Sg_PtyTcSetAttr(SgObject pty, SgObject termios);
SgObject Sg_PtyInputPort(SgObject pty);
SgObject Sg_PtyOutputPort(SgObject pty);

SG_CDECL_END
#endif
