/* -*- C -*- */
/*
 * values.c
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
#include "sagittarius/values.h"
#include "sagittarius/port.h"
#include "sagittarius/writer.h"

static void values_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  int i;
  SgValues *v = SG_VALUES(obj);
  Sg_Putuz(port, UC("#<values"));
  for (i = 0; i < v->size; i++) {
    Sg_Putc(port, ' ');
    Sg_Write(v->elements[i], port, ctx->mode);
  }
  Sg_Putc(port, '>');
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_ValuesClass, values_print);

static SgValues* make_values(int size)
{
  SgValues *z = SG_NEW2(SgValues*, sizeof(SgValues)+sizeof(SgObject)*(size-1));
  SG_SET_CLASS(z, SG_CLASS_VALUES);
  z->size = size;
  return z;
}

SgObject Sg_MakeValues(int argc)
{
  SgValues *z = make_values(argc);
  return SG_OBJ(z);
}

SgObject Sg_Values2(SgObject v1, SgObject v2)
{
  SgValues *v = make_values(2);
  SG_VALUES_ELEMENT(v, 0) = v1;
  SG_VALUES_ELEMENT(v, 1) = v2;
  return SG_OBJ(v);
}
