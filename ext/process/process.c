/* -*- mode: c; coding: utf-8; -*- */
/*
 * process.h
 *
 *   Copyright (c) 2010  Takashi Kato <ktakashi@ymail.com>
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
 *
 *  $Id: $
 */
#include <sagittarius/extend.h>
#include "process.h"

static void process_printer(SgPort *port, SgObject self, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<process %S>"), SG_PROCESS(self)->name);
}

SG_INIT_META_OBJ(Sg_ProcessMeta, &process_printer, NULL);

static SgProcess* make_process(SgString *name, SgString *args)
{
  SgProcess *p = SG_NEW(SgProcess);
  SG_SET_META_OBJ(p, SG_META_PROCESS);
  p->name = name;
  p->args = args;
  p->handle = 0;
  p->in = SG_UNDEF;
  p->out = SG_UNDEF;
  p->err = SG_UNDEF;
  return p;
}

#if defined(_MSC_VER)
# include "win.c"
#else
# include "posix.c"
#endif


extern void Sg__Init_sagittarius_process_impl();

SG_EXTENSION_ENTRY void Sg_Init_sagittarius__process()
{
  SG_INIT_EXTENSION(sagittarius__process);
  init_process();
  Sg__Init_sagittarius_process_impl();
}
