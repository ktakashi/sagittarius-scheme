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
#include "sagittarius/private/port.h"
#include "sagittarius/private/subr.h"
#include "sagittarius/private/symbol.h"
#include "sagittarius/private/vector.h"
#include "sagittarius/private/writer.h"
#include "sagittarius/private/vm.h"

static void parameterization_print(SgObject p, SgPort *port, SgWriteContext *ctx)
{
  if (SG_WRITE_MODE(ctx) == SG_WRITE_DISPLAY) {
    Sg_Printf(port, UC("#<parameterization %d>"),
	      Sg_Length(SG_PARAMETERIZATION_CELLS(p)));
  } else {
    Sg_Printf(port, UC("#<parameterization %S>"),
	      SG_PARAMETERIZATION_CELLS(p));
  }
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
  return SG_FALSE;
}

SgObject Sg_ParameterizationRef(SgObject p, SgObject key)
{
  return Sg_Assq(key, SG_PARAMETERIZATION_CELLS(p));
}

static SgObject core_parameter_apply(SgObject *args, int argc, void *d)
{
  void **data = (void **)d;
  SgObject key = data[0];
  SgObject p = Sg_CurrentParameterization();
  SgObject cell = SG_FALSEP(p) ? p : Sg_ParameterizationRef(p, key);
  if (SG_NULLP(args[0])) {
    if (SG_FALSEP(cell)) {
      SgCoreParameterRef *ref = (SgCoreParameterRef *)data[1];
      SgObject r = ref(key);
      if (SG_UNBOUNDP(r)) return data[3];
      return r;
    }
    return SG_CDR(cell);
  } else {
    if (SG_FALSEP(cell)) {
      SgCoreParameterSet *set = (SgCoreParameterSet *)data[2];
      set(key, SG_CAR(args[0]));
    } else {
      SG_SET_CDR(cell, SG_CAR(args[0]));
    }
    return SG_UNDEF;
  }
}

static SgObject parameter_mark = SG_FALSE;

SgObject Sg_MakeCoreParameter(SgObject name,
			      SgObject initValue,
			      SgCoreParameterRef ref,
			      SgCoreParameterSet set)
{
  void **data = SG_NEW_ARRAY(void *, 4);
  SgObject mark = Sg_Cons(name, parameter_mark);
  data[0] = Sg_MakeSubr(core_parameter_apply, data, 0, 1, mark);
  data[1] = ref;
  data[2] = set;
  data[3] = initValue;
  return SG_OBJ(data[0]);
}

static int check_name(SgObject subr)
{
  SgObject name = SG_PROCEDURE_NAME(subr);
  return SG_PAIRP(name) && SG_EQ(SG_CDR(name), parameter_mark);
}

int Sg_CoreParameterP(SgObject o)
{
  return SG_SUBRP(o) && check_name(o);
}

void Sg__InitParameter()
{
  SgObject sym = Sg_MakeSymbol(SG_MAKE_STRING("parameterization-mark"), FALSE);
  SgLibrary *lib = Sg_FindLibrary(SG_INTERN("(sagittarius clos)"), FALSE);

  parameter_mark = Sg_MakeSymbol(SG_MAKE_STRING("core-parameter"), FALSE);
  continuation_mark = SG_LIST1(sym);

  Sg_InitStaticClass(SG_CLASS_PARAMETERIZATION, UC("<parameterization>"), lib,
		     NULL, 0);
}
