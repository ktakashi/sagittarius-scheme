/* vector.c                                        -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2021  Takashi Kato <ktakashi@ymail.com>
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
#include "sagittarius/private/vector.h"
#include "sagittarius/private/collection.h"
#include "sagittarius/private/error.h"
#include "sagittarius/private/symbol.h"
#include "sagittarius/private/compare.h"
#include "sagittarius/private/pair.h"

static void vector_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  /* do nothing, vector will be treated in writer.c */
}

SG_DEFINE_BUILTIN_CLASS(Sg_VectorClass, vector_print, NULL, NULL, NULL,
			SG_CLASS_SEQUENCE_CPL);


static SgVector* make_vector(long size)
{
  SgVector *v = SG_NEW2(SgVector*, sizeof(SgVector)+sizeof(SgObject)*(size-1));
  SG_SET_CLASS(v, SG_CLASS_VECTOR);
  v->size = size;
  return v;
}

SgObject Sg_MakeVector(long size, SgObject fill)
{
  long i;
  SgVector *v;
  if (size < 0) {
    Sg_Error(UC("vector size must be a positive integer, but got %d"), size);
  }
  v = make_vector(size);
  if (SG_EQ(fill, SG_UNBOUND)) fill = SG_UNDEF;
  for (i = 0; i < size; i++) v->elements[i] = fill;
  return SG_OBJ(v);
}

int Sg_IsVector(SgObject obj)
{
  return SG_VECTORP(obj);
}

long Sg_VectorLength(SgObject vec)
{
  /* TODO should we check the type here? */
  return SG_VECTOR_SIZE(vec);
}

SgObject Sg_VectorRef(SgObject vec, long i, SgObject fallback)
{
  if (i < 0 || i >= SG_VECTOR_SIZE(vec)) return fallback;
  return SG_VECTOR_ELEMENT(vec, i);
}

SgObject Sg_VectorSet(SgObject vec, long i, SgObject obj)
{
  if (i >= 0 && i < SG_VECTOR_SIZE(vec)) SG_VECTOR_ELEMENT(vec, i) = obj;
  return obj;
}

SgObject Sg_VectorFill(SgObject vec, SgObject fill, long start, long end)
{
  long i, len = SG_VECTOR_SIZE(vec);
  SG_CHECK_START_END(start, end, len);
  for (i = start; i < end; i++) {
    SG_VECTOR_ELEMENT(vec, i) = fill;
  }
  return SG_OBJ(vec);
}

SgObject Sg_ListToVector(SgObject l, long start, long end)
{
  SgObject v;
  SgObject e;
  long i;

  if (end < 0) {
    long size = Sg_Length(l);
    if (size < 0) Sg_Error(UC("bad list: %S"), l);
    SG_CHECK_START_END(start, end, size);
    v = make_vector(size - start);
  } else {
    SG_CHECK_START_END(start, end, end);
    v = make_vector(end - start);
  }
  e = Sg_ListTail(l, start, SG_UNBOUND);
  for (i = 0; i < end - start; i++, e = SG_CDR(e)) {
    if (!SG_PAIRP(e)) {
      Sg_Error(UC("list too short: %S"), l);
    }
    SG_VECTOR_ELEMENT(v, i) = SG_CAR(e);
  }
  return SG_OBJ(v);
}

SgObject Sg_VectorToList(SgObject v, long start, long end)
{
  long len = SG_VECTOR_SIZE(v);
  SgObject h = SG_NIL, t = SG_NIL;
  SgObject *elts = SG_VECTOR_ELEMENTS(v);
  SG_CHECK_START_END(start, end, len);
  if (elts) {
    long i;
    for (i = start; i < end; i++) SG_APPEND1(h, t, *(elts + i));
  }
  return h;
}

SgObject Sg_VectorCopy(SgObject vec, long start, long end, SgObject fill)
{
  long i, len = SG_VECTOR_SIZE(vec);
  SgObject v = NULL;
  if (end < 0) end = len;
  if (end < start) {
    Sg_Error(UC("vector-copy: start (%d) is greater then end (%d)"),
	     start, end);
  } else if (end == start) {
    v = make_vector(0);
  } else {
    if (SG_EQ(fill, SG_UNBOUND)) fill = SG_UNDEF;
    v = make_vector(end - start);
    for (i = 0; i < end - start; i++) {
      if (i + start < 0 || i + start >= len) {
	SG_VECTOR_ELEMENT(v, i) = fill;
      } else {
	SG_VECTOR_ELEMENT(v, i) = SG_VECTOR_ELEMENT(vec, i + start);
      }
    }
  }
  return SG_OBJ(v);
}

SgObject Sg_VectorConcatenate(SgObject vecList)
{
  SgObject r, cp;
  long size = 0, i;
  SG_FOR_EACH(cp, vecList) {
    if (!SG_VECTORP(SG_CAR(cp))) {
      Sg_WrongTypeOfArgumentViolation(SG_INTERN("vector-concatenate"),
				      SG_INTERN("vector"), 
				      SG_CAR(cp), vecList);
    }
    size += SG_VECTOR_SIZE(SG_CAR(cp));
  }
  r = make_vector(size);
  if (size == 0) return r;
  i = 0;
  SG_FOR_EACH(cp, vecList) {
    int j;
    for (j = 0; j < SG_VECTOR_SIZE(SG_CAR(cp)); j++, i++) {
      SG_VECTOR_ELEMENT(r, i) = SG_VECTOR_ELEMENT(SG_CAR(cp), j);
    }
  }
  return r;
}

SgObject Sg_VectorReverseX(SgObject vec, long start, long end)
{
  SgObject t;
  long i, n = SG_VECTOR_SIZE(vec), e, c;
  SG_CHECK_START_END(start, end, n);

  n = (end-start)/2;

  for (i = start, e = end-1, c = 0; c < n; i++, c++, e--) {
    t = SG_VECTOR_ELEMENT(vec, i);
    SG_VECTOR_ELEMENT(vec, i) = SG_VECTOR_ELEMENT(vec, e);
    SG_VECTOR_ELEMENT(vec, e) = t;
  }
  return vec;
}

SgObject Sg_VectorReverse(SgObject vec, long start, long end)
{
  SgObject cp = Sg_VectorCopy(vec, 0, SG_VECTOR_SIZE(vec), SG_UNBOUND);
  return Sg_VectorReverseX(cp, start, end);
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
