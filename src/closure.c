/* -*- C -*- */
/*
 * closure.c
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
#include "sagittarius/closure.h"
#include "sagittarius/code.h"

SgObject Sg_MakeClosure(SgObject code,
			SgObject *frees)
{
  SgClosure *cl;
  int req, opt, i, freec;
  SgObject info;
  ASSERT(SG_CODE_BUILDERP(code));

  cl = SG_NEW2(SgClosure *, sizeof(SgClosure) +
	       (sizeof(SgObject) * SG_CODE_BUILDER_FREEC(code)));
  info = Sg_CodeBuilderFullName(SG_CODE_BUILDER(code));
  req = SG_CODE_BUILDER_ARGC(code);
  opt = SG_CODE_BUILDER_OPTIONAL(code);

  SG_SET_CLASS(cl, SG_CLASS_PROCEDURE);
  SG_PROCEDURE_INIT(cl, req, opt, SG_PROC_CLOSURE, info);

  freec = SG_CODE_BUILDER_FREEC(code);
  cl->code = code;
  cl->mark = -1;
  cl->size = freec;		/* dummy */
  for (i = 0; i < freec; i++) {
    cl->frees[i] = frees[freec - i - 1];
  }
  return SG_OBJ(cl);
}
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
