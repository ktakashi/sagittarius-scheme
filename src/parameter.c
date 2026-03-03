/* parameter.c                                      -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2026  Takashi Kato <ktakashi@ymail.com>
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
#define LIBSAGITTARIUS_BODY
#include "sagittarius/private/parameter.h"
#include "sagittarius/private/library.h"
#include "sagittarius/private/pair.h"
#include "sagittarius/private/symbol.h"
#include "sagittarius/private/vector.h"
#include "sagittarius/private/writer.h"
#include "sagittarius/private/vm.h"

static void parameterization_print(SgObject p, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<parameterization %d>"),
	    Sg_Length(SG_PARAMETERIZATION_CELLS(p)));
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_ParameterizationClass, parameterization_print);

static SgObject continuation_mark = SG_FALSE;

SgObject Sg_MakeParameterization(SgObject cells)
{
  SgParameterization *p = SG_NEW(SgParameterization);
  SG_SET_CLASS(p, SG_CLASS_PARAMETERIZATION);
  p->cells = cells;
  return SG_OBJ(p);
}

SgObject Sg_ParameterizationContinuationMarkKey()
{
  return continuation_mark;
}

SgObject Sg_CurrentParameterization()
{
  /* maybe should we make specific C API for this to reduce allocation? */
  SgObject marks = Sg_CurrentContinuationMarks(SG_FALSE);
  SgObject frames = SG_VECTOR_ELEMENT(marks, 1), cp;

  SG_FOR_EACH(cp, frames) {
    SgObject r = Sg_Assq(continuation_mark, SG_CAR(cp));
    if (!SG_FALSEP(r)) return SG_CDR(r);
  }
  /* no parameterization, so make one */
  return Sg_MakeParameterization(SG_NIL);
}

SgObject Sg_ParameterizationRef(SgObject p, SgObject key)
{
  return Sg_Assq(key, SG_PARAMETERIZATION_CELLS(p));
}


void Sg__InitParameter()
{
  SgObject sym = Sg_MakeSymbol(SG_MAKE_STRING("parameterization-mark"), FALSE);
  SgLibrary *lib = Sg_FindLibrary(SG_INTERN("(sagittarius clos)"), FALSE);

  continuation_mark = SG_LIST1(sym);

  Sg_InitStaticClass(SG_CLASS_PARAMETERIZATION, UC("<parameterization>"), lib,
		     NULL, 0);
}
