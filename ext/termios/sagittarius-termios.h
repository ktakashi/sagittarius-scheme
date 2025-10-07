/* sagittarius-termios.h                         -*- mode: c; coding: utf-8; -*-
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

#ifndef SAGITTARIUS_TERMIOS_H_
#define SAGITTARIUS_TERMIOS_H_

#include <sagittarius.h>

#ifdef _WIN32
# include "win_termios.h"
#else
# include <termios.h>
#endif

SG_CLASS_DECL(Sg_TermiosClass);
#define SG_CLASS_TERMIOS (&Sg_TermiosClass)

typedef struct {
  SG_HEADER;
  struct termios term;
} SgTermios;
#define SG_TERMIOS(o) ((SgTermios *)o)
#define SG_TERMIOSP(o) SG_XTYPEP(o, SG_CLASS_TERMIOS)

#define SG_TERMIOS_TERMIOS(o) (&(SG_TERMIOS(o)->term))

SG_CDECL_BEGIN

SgObject Sg_MakeTermios();
SgObject Sg_FromTermios(struct termios * termios);

SG_CDECL_END
#endif
