/* -*- C -*- */
/*
 * weak.c
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
#include "sagittarius/weak.h"
#include "sagittarius/core.h"
#include "sagittarius/error.h"

static void weakvector_finalize(SgObject obj, void *data)
{
  int i;
  SgWeakVector *v = SG_WEAK_VECTOR(obj);
  SgObject *p = (SgObject*)v->pointers;
  for (i = 0; i < v->size; i++) {
    if (p[i] == NULL || SG_PTRP(p[i])) {
      Sg_UnregisterDisappearingLink((void **)&p[i]);
    }
    p[i] = SG_FALSE;
  }
}

SgObject Sg_MakeWeakVector(int size)
{
  int i;
  SgObject *p;
  SgWeakVector *v = SG_NEW(SgWeakVector);

  SG_SET_HEADER(v, TC_WEAK_VECTOR);
  v->size = size;
  /* Allocate pointer array by ATOMIC, so that GC won't trace the
     pointers in it.
   */
  p = SG_NEW_ATOMIC2(SgObject *, size * sizeof(SgObject));
  for (i = 0; i < size; i++) p[i] = SG_FALSE;
  v->pointers = (void*)p;
  Sg_RegisterFinalizer(SG_OBJ(v), weakvector_finalize, NULL);
  return SG_OBJ(v);
}

SgObject Sg_WeakVectorRef(SgWeakVector *v, int index, SgObject fallback)
{
  SgObject *p;
  if (index < 0 || index >= v->size) {
    if (SG_UNBOUNDP(fallback)) {
      Sg_Error(UC("argument out of range: %d"), index);
    }
    return fallback;
  }
  p = (SgObject*)v->pointers;
  if (p[index] == NULL) {
    if (SG_UNBOUNDP(fallback)) return SG_FALSE;
    else return fallback;
  } else {
    return p[index];
  }
}

SgObject Sg_WeakVectorSet(SgWeakVector *v, int index, SgObject value)
{
  SgObject *p;
  if (index < 0 || index >= v->size) {
    Sg_Error(UC("argument out of range: %d"), index);
  }
  p = (SgObject*)v->pointers;
  /* unregister the location if it was registered before */
  if (p[index] == NULL || SG_PTRP(p[index])) {
    Sg_UnregisterDisappearingLink((void **)&p[index]);
  }
  p[index] = value;
  /* register the location if the value is a heap object */
  if (SG_PTRP(value)) {
    Sg_RegisterDisappearingLink((void **)&p[index], (void *)value);
  }
  return SG_UNDEF;
}
