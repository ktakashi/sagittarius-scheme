/* clos.c                                                 -*- coding: utf-8; -*-
 *
 *   Copyright (c) 2010-2011  Takashi Kato <ktakashi@ymail.com>
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
#include "sagittarius/clos.h"
#include "sagittarius/number.h"
#include "sagittarius/pair.h"
#include "sagittarius/writer.h"

SgClass *Sg_DefaultCPL[] = {
  SG_CLASS_TOP,
  NULL,
};
SgClass *Sg_ObjectCPL[] = {
  SG_CLASS_OBJECT,
  SG_CLASS_TOP,
  NULL,
};

SG_DEFINE_ABSTRACT_CLASS(Sg_TopClass, NULL);

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_BoolClass, NULL);
SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_CharClass, NULL);
SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_UnknownClass, NULL);
SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_UndefinedClass, NULL);
SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_EOFObjectClass, NULL);

static void class_print(SgObject, SgPort *, SgWriteContext *);

/* allocate */
static SgObject class_allocate(SgClass *klass, SgObject iniargs);

SG_DEFINE_BASE_CLASS(Sg_ObjectClass, SgInstance,
		     NULL, NULL, NULL, /* Sg_ObjectAllocate */ NULL,
		     SG_CLASS_DEFAULT_CPL);
SG_DEFINE_BASE_CLASS(Sg_ClassClass, SgClass,
		     class_print, NULL, NULL, class_allocate,
		     SG_CLASS_OBJECT_CPL);


SgClass* Sg_ClassOf(SgObject obj)
{
  if (!SG_PTRP(obj)) {
    if (SG_TRUEP(obj) || SG_FALSEP(obj)) return SG_CLASS_BOOL;
    if (SG_NULLP(obj)) return SG_CLASS_NULL;
    if (SG_CHARP(obj)) return SG_CLASS_CHAR;
    if (SG_INTP(obj)) return SG_CLASS_INTEGER;
    if (SG_EOFP(obj)) return SG_CLASS_EOF_OBJECT;
    if (SG_UNDEFP(obj)) return SG_CLASS_UNDEFINED_OBJECT;
    else return SG_CLASS_UNKNOWN;
  }
  if (SG_FLONUMP(obj)) return SG_CLASS_REAL;
  if (SG_PAIRP(obj)) return SG_CLASS_PAIR;
  return SG_CLASS_OF(obj);
}


/* <class> */
static void class_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<class %A%s>"),
	    SG_CLASS(obj)->name,
	    /* for now */
	    UC(""));
}

static SgObject class_allocate(SgClass *klass, SgObject iniargs)
{
  /* dummy for now */
  return NULL;
}
