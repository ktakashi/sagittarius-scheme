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
#include <sagittarius.h>
#define LIBSAGITTARIUS_BODY
#include <sagittarius/extend.h>
#include "process.h"

static void process_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<process %S>"), SG_PROCESS(self)->name);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_ProcessClass, process_printer);

static SgProcess* make_process(SgString *name, SgObject args)
{
  SgProcess *p = SG_NEW(SgProcess);
  SG_SET_CLASS(p, SG_CLASS_PROCESS);
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
  SgLibrary *lib;
  SG_INIT_EXTENSION(sagittarius__process);
  init_process();
  Sg__Init_sagittarius_process_impl();
  lib = 
    SG_LIBRARY(Sg_FindLibrary(SG_INTERN("(sagittarius process impl)"), FALSE));
  Sg_InitStaticClassWithMeta(SG_CLASS_PROCESS, UC("<process>"), lib, NULL,
			     SG_FALSE, NULL, 0);
}
