/* -*- C -*- */
/*
 * file.c
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
#define LIBSAGITTARIUS_BODY
#include "sagittarius/file.h"
#include "sagittarius/error.h"
#include "sagittarius/pair.h"
#include "sagittarius/port.h"
#include "sagittarius/string.h"
#include "sagittarius/library.h"
#include "sagittarius/system.h"

static void file_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  /* never reach */
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_FileClass, file_print);


SgObject Sg_FindFile(SgString *path, SgObject loadPaths,
		     SgString *suffix, int quiet)
{
  SgObject dir;
  SgObject realPath;
  const SgObject sep = Sg_MakeString(Sg_NativeFileSeparator(), SG_LITERAL_STRING);
  SG_FOR_EACH(dir, loadPaths) {
    if (suffix) {
      realPath = Sg_StringAppend(SG_LIST4(SG_CAR(dir),
					  sep,
					  path,
					  suffix));
    } else {
      realPath = Sg_StringAppend(SG_LIST3(SG_CAR(dir),
					  sep,
					  path));
    }
    if (Sg_FileExistP(SG_STRING(realPath))) {
      return realPath;
    }
  }
  if (!quiet) {
    Sg_Error(UC("given file was not found %S"), path);
  }
  return SG_FALSE;
}

void Sg__InitFile()
{
}
