/* -*- C -*- */
/*
 * vector.c
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
#include "sagittarius/vector.h"
#include "sagittarius/collection.h"
#include "sagittarius/error.h"
#include "sagittarius/symbol.h"
#include "sagittarius/compare.h"
#include "sagittarius/pair.h"

static void vector_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  /* do nothing, vector will be treated in writer.c */
}

SG_DEFINE_BUILTIN_CLASS(Sg_VectorClass, vector_print, NULL, NULL, NULL,
			SG_CLASS_SEQUENCE_CPL);


static SgVector* make_vector(int size)
{
  SgVector *v = SG_NEW2(SgVector*, sizeof(SgVector)+sizeof(SgObject)*(size-1));
  SG_SET_CLASS(v, SG_CLASS_VECTOR);
  v->size = size;
  return v;
}

SgObject Sg_MakeVector(int size, SgObject fill)
{
  int i;
  SgVector *v;
  if (size < 0) {
    Sg_Error(UC("vector size must be a positive integer, but got %d"), size);
  }
  v = make_vector(size);
  if (SG_EQ(fill, SG_UNBOUND)) fill = SG_UNDEF;
  for (i = 0; i < size; i++) v->elements[i] = fill;
  return SG_OBJ(v);
}

SgObject Sg_VectorRef(SgVector *vec, int i, SgObject fallback)
{
  if (i < 0 || i >= vec->size) return fallback;
  return vec->elements[i];
}

SgObject Sg_VectorSet(SgVector *vec, int i, SgObject obj)
{
  if (i >= 0 && i < vec->size) vec->elements[i] = obj;
  return obj;
}

SgObject Sg_VectorFill(SgVector *vec, SgObject fill, int start, int end)
{
  int i, len = SG_VECTOR_SIZE(vec);
  SG_CHECK_START_END(start, end, len);
  for (i = start; i < end; i++) {
    SG_VECTOR_ELEMENT(vec, i) = fill;
  }
  return SG_OBJ(vec);
}


SgObject Sg_ListToVector(SgObject l, int start, int end)
{
  SgVector *v;
  SgObject e;
  int i;

  if (end < 0) {
    int size = Sg_Length(l);
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

SgObject Sg_VectorToList(SgVector *v, int start, int end)
{
  int len = SG_VECTOR_SIZE(v);
  SgObject h = SG_NIL, t = SG_NIL;
  SgObject *elts = SG_VECTOR_ELEMENTS(v);
  SG_CHECK_START_END(start, end, len);
  if (elts) {
    int i;
    for (i = start; i < end; i++) SG_APPEND1(h, t, *(elts + i));
  }
  return h;
}

SgObject Sg_VectorCopy(SgVector *vec, int start, int end, SgObject fill)
{
  int i, len = SG_VECTOR_SIZE(vec);
  SgVector *v = NULL;
  if (end < 0) end = len;
  if (end < start) {
    Sg_Error(UC("vector-copy: start (%d) is greater then end (%d)"), start, end);
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
  int size = 0, i;
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

SgObject Sg_VectorReverseX(SgObject vec, int start, int end)
{
  SgObject t;
  int i, n = SG_VECTOR_SIZE(vec), e;
  SG_CHECK_START_END(start, end, n);

  for (i = start, e = end-1; i < end/2; i++, e--) {
    t = SG_VECTOR_ELEMENT(vec, i);
    SG_VECTOR_ELEMENT(vec, i) = SG_VECTOR_ELEMENT(vec, e);
    SG_VECTOR_ELEMENT(vec, e) = t;
  }
  return vec;
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
