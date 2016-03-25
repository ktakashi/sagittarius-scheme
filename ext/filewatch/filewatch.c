/* -*- mode: c; coding: utf-8; -*- 
 *
 * filewatch.h
 *
 *   Copyright (c) 2016  Takashi Kato <ktakashi@ymail.com>
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
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "filewatch.h"

static void filewatch_ctx_print(SgObject o, SgPort *p, SgWriteContext *ctx)
{
  Sg_Printf(p, UC("#<filewatch-context>"));
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_FileWatchContextClass, filewatch_ctx_print);

void Sg_StopRequest(SgFileWatchContext *ctx)
{
  ctx->stopRequest = TRUE;
}

extern void Sg__Init_filewatch_stub(SgLibrary *lib);

SgObject Sg_FlagSymbols[] = {
  SG_UNDEF,			/* access */
  SG_UNDEF,			/* modify */
  SG_UNDEF,			/* delete */
  SG_UNDEF,			/* move */
  SG_UNDEF,			/* attribute */
  SG_UNDEF,			/* accessed */
  SG_UNDEF,			/* modified */
  SG_UNDEF,			/* removed */
  SG_UNDEF			/* renamed */
};

SG_EXTENSION_ENTRY void Sg_Init_sagittarius__filewatch()
{
  SgLibrary *lib;
  SG_INIT_EXTENSION(sagittarius__filewatch);
  lib = SG_LIBRARY(Sg_FindLibrary(SG_INTERN("(sagittarius filewatch)"),
				  FALSE));
  
  SG_ACCESS    = SG_INTERN("access");
  SG_MODIFY    = SG_INTERN("modify");
  SG_DELETE    = SG_INTERN("delete");
  SG_MOVE      = SG_INTERN("move");
  SG_ATTRIBUTE = SG_INTERN("attribute");
  SG_ACCESSED  = SG_INTERN("accessed");
  SG_MODIFIED  = SG_INTERN("modified");
  SG_REMOVED   = SG_INTERN("removed");
  SG_RENAMED   = SG_INTERN("renamed");

  Sg__Init_filewatch_stub(lib);
  Sg_InitStaticClass(SG_CLASS_FILE_WATCH_CONTEXT, UC("<filewatch-context>"),
		     lib, NULL, 0);

}
