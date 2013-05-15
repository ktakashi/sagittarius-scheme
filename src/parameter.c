/* parameter.c                                                 -*- coding: utf-8; -*-
 *
 *   Copyright (c) 2010-2013  Takashi Kato <ktakashi@ymail.com>
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
#include "sagittarius/parameter.h"
#include "sagittarius/clos.h"
#include "sagittarius/generic.h"
#include "sagittarius/keyword.h"
#include "sagittarius/library.h"
#include "sagittarius/port.h"
#include "sagittarius/subr.h"
#include "sagittarius/symbol.h"
#include "sagittarius/vm.h"
#include "sagittarius/weak.h"

static void parameter_print(SgObject o, SgPort *port, SgWriteContext *ctx)
{
  /* TODO */
  Sg_PutuzUnsafe(port, UC("#<parameter>"));
}
SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_ParameterClass, parameter_print);

static SgObject register_parameter(SgObject p, SgObject init)
{
  SgVM *vm = Sg_VM();
  Sg_WeakHashTableSet(vm->parameters, p, init, 0);
  return p;
}

static SgObject convert_cc(SgObject result, void **data)
{
  return register_parameter(data[0], result);
}

static SgObject return_or_covert(SgParameter *p, SgObject v)
{
  if (!SG_FALSEP(p->converter)) {
    /* must be procedure */
    void *d[1];
    d[0] = p;
    Sg_VMPushCC(convert_cc, d, 1);
    return Sg_VMApply1(p->converter, v);
  } else {
    return register_parameter(p, v);
  }
}

SgObject Sg_VMMakeParameter(SgObject init, SgObject conv)
{
  SgParameter *p = SG_NEW(SgParameter);
  SG_SET_CLASS(p, SG_CLASS_PARAMETER);
  p->converter = conv;
  return return_or_covert(p, init);
}

/* object-apply */
static SgObject parameter_0_impl(SgObject *args, int argc, void *data)
{
  SgObject p = args[0];
  SgVM *vm = Sg_VM();
  return Sg_WeakHashTableRef(vm->parameters, p, SG_FALSE);
}

static SgClass *parameter_0_SPEC[] = {
  SG_CLASS_PARAMETER
};

SG_DEFINE_SUBR(parameter_0_subr, 1, 0, parameter_0_impl, SG_FALSE, NULL);

static SG_DEFINE_METHOD(parameter_0, &Sg_GenericObjectApply,
			1, 0, parameter_0_SPEC, &parameter_0_subr);

/* setter */
static SgObject parameter_1_impl(SgObject *args, int argc, void *data)
{
  SgParameter *p = SG_PARAMETER(args[0]);
  SgObject v = args[1];
  return return_or_covert(p, v);
}

static SgClass *parameter_1_SPEC[] = {
  SG_CLASS_PARAMETER, SG_CLASS_TOP
};

SG_DEFINE_SUBR(parameter_1_subr, 2, 0, parameter_1_impl, SG_FALSE, NULL);

static SG_DEFINE_METHOD(parameter_1, &Sg_GenericObjectApply,
			2, 0, parameter_1_SPEC, &parameter_1_subr);

void Sg__InitParameter()
{
  SgLibrary *lib = Sg_FindLibrary(SG_INTERN("(sagittarius clos)"), TRUE);

  Sg_InitStaticClassWithMeta(SG_CLASS_PARAMETER, UC("<parameter>"), 
			     lib, NULL, SG_FALSE, NULL, 0);
  Sg_InitBuiltinMethod(&parameter_0);
  Sg_InitBuiltinMethod(&parameter_1);
}
