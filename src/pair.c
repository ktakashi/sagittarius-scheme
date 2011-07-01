/* -*- C -*- */
/*
 * pair.c
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
#include "sagittarius/pair.h"
#include "sagittarius/compare.h"
#include "sagittarius/error.h"

static inline SgPair* make_pair()
{
  SgPair *z = SG_NEW(SgPair);
  return z;
}

SgObject Sg_Cons(SgObject car, SgObject cdr)
{
  SgPair *z = make_pair();
  SG_SET_CAR(z, car);
  SG_SET_CDR(z, cdr);
  return SG_OBJ(z);
}

SgObject Sg_Acons(SgObject caar, SgObject cdar, SgObject cdr)
{
  SgPair *y = make_pair();
  SgPair *z = make_pair();
  SG_SET_CAR(y, caar);
  SG_SET_CDR(y, cdar);
  SG_SET_CAR(z, SG_OBJ(y));
  SG_SET_CDR(z, cdr);
  return SG_OBJ(z);
}

SgObject Sg_List(SgObject elt, ...)
{
  va_list pvar;
  SgObject cdr;
  
  if (elt == NULL) return SG_NIL;

  va_start(pvar, elt);
  cdr = Sg_VaList(pvar);
  va_end(pvar);
  return Sg_Cons(elt, cdr);
}

SgObject Sg_VaList(va_list elts)
{
  SgObject start = SG_NIL, cp = SG_NIL, obj;

  for (obj = va_arg(elts, SgObject);
       obj != NULL;
       obj = va_arg(elts, SgObject));
  {
    if (SG_NULLP(start)) {
      start = SG_OBJ(make_pair());
      SG_SET_CAR(start, obj);
      SG_SET_CDR(start, SG_NIL);
      cp = start;
    } else {
      SgObject item;
      item = SG_OBJ(make_pair());
      SG_SET_CDR(cp, item);
      SG_SET_CAR(item, obj);
      SG_SET_CDR(item, SG_NIL);
      cp = item;
    }
  }
  return start;
}

SgObject Sg_ArrayToList(SgObject *array, int nelts)
{
  return Sg_ArrayToListWithTail(array, nelts, SG_NIL);
}

SgObject Sg_ArrayToListWithTail(SgObject *array, int nelts, SgObject tail)
{
  SgObject h = SG_NIL, t = SG_NIL;
  if (array) {
    int i;
    for (i = 0; i < nelts; i++) SG_APPEND1(h, t, *array++);
  }
  if (!SG_NULLP(tail)) SG_APPEND(h, t, tail);
  return h;
}

#define CXR(cname, sname, body)			\
SgObject cname (SgObject obj)			\
{						\
  SgObject obj2 = obj;				\
  body						\
  return obj2;					\
}

#define A								\
  if (!SG_PAIRP(obj2)) Sg_Error(UC("pair required but got: %S"), obj);	\
  obj2 = SG_CAR(obj2);

#define D								\
  if (!SG_PAIRP(obj2)) Sg_Error(UC("pair required but got: %S"), obj);	\
  obj2 = SG_CDR(obj2);

CXR(Sg_Car, "car", A)
CXR(Sg_Cdr, "cdr", D)
CXR(Sg_Caar, "caar", A A)
CXR(Sg_Cadr, "cadr", A D)
CXR(Sg_Cdar, "cdar", D A)
CXR(Sg_Cddr, "cddr", D D)
/* Maybe add cadr etc.*/

int Sg_Length(SgObject obj)
{
  SgObject slow = obj;
  int len = 0;
  for (;;) {
    if (SG_NULLP(obj)) break;
    if (!SG_PAIRP(obj)) return SG_LIST_DOTTED;

    obj = SG_CDR(obj);
    len++;
    if (SG_NULLP(obj)) break;
    if (!SG_PAIRP(obj)) return SG_LIST_DOTTED;

    obj = SG_CDR(obj);
    slow = SG_CDR(slow);
    if (obj == slow) return SG_LIST_CIRCULAR;
    len++;
  }
  return len;
}

SgObject Sg_CopyList(SgObject list)
{
  SgObject start = SG_NIL, last = SG_NIL;
  if (!SG_PAIRP(list)) return list;

  SG_FOR_EACH(list, list) {
    SG_APPEND1(start, last, SG_CAR(list));
  }
  if (!SG_NULLP(list)) SG_SET_CDR(last, list);
  return start;
}

SgObject Sg_Append2X(SgObject list, SgObject obj)
{
  SgObject cp;
  SG_FOR_EACH(cp, list) {
    if (SG_NULLP(SG_CDR(cp))) {
      SG_SET_CDR(cp, obj);
      return list;
    }
  }
  return obj;
}

SgObject Sg_Append2(SgObject list, SgObject obj)
{
  SgObject start = SG_NIL, last = SG_NIL;
  if (!SG_PAIRP(list)) return obj;

  SG_FOR_EACH(list, list) {
    SG_APPEND1(start, last, SG_CAR(list));
  }
  SG_SET_CDR(last, obj);
  return start;
}

SgObject Sg_Append(SgObject args)
{
  SgObject start = SG_NIL, last = SG_NIL, cp;
  SG_FOR_EACH(cp, args) {
    if (!SG_PAIRP(SG_CDR(cp))) {
      if (SG_NULLP(start)) return SG_CAR(cp);
      SG_SET_CDR(last, SG_CAR(cp));
      break;
    } else if (SG_NULLP(SG_CAR(cp))) {
      continue;
    } else if (!SG_PAIRP(SG_CAR(cp))) {
      Sg_Error(UC("pair required, but got %S"), SG_CAR(cp));
    } else {
      SG_APPEND(start, last, Sg_CopyList(SG_CAR(cp)));
    }
  }
  return start;
}

SgObject Sg_ReverseX(SgObject list)
{
  SgObject first, next, result = SG_NIL;
  if (!SG_PAIRP(list)) return list;
  for (first = list; SG_PAIRP(first); first = next) {
    next = SG_CDR(first);
    SG_SET_CDR(first, result);
    result = first;
  }
  return result;
}

SgObject Sg_Reverse(SgObject list)
{
  SgObject cp, result;
  SgPair *p;

  if (!SG_PAIRP(list)) return list;

  p = make_pair();
  SG_SET_CAR(p, SG_NIL);
  SG_SET_CDR(p, SG_NIL);
  result = SG_OBJ(p);
  SG_FOR_EACH(cp, list) {
    SG_SET_CAR(result, SG_CAR(cp));
    p = make_pair();
    SG_SET_CAR(p, SG_NIL);
    SG_SET_CDR(p, result);
    result = SG_OBJ(p);
  }
  return SG_CDR(result);
}

SgObject Sg_LastPair(SgObject list)
{
  SgObject cp;
  if (!SG_PAIRP(list)) Sg_Error(UC("pair required, but got %S"), list);

  SG_FOR_EACH(cp, list) {
    SgObject cdr = SG_CDR(cp);
    if (!SG_PAIRP(cdr)) return cp;
  }
  return SG_UNDEF; /* never reached */
}

SgObject Sg_ListTail(SgObject list, int i, SgObject fallback)
{
  int count = i;
  if (i < 0) goto err;
  while (count-- > 0) {
    if (!SG_PAIRP(list)) goto err;
    list = SG_CDR(list);
  }
  return list;
 err:
  if (SG_UNBOUNDP(fallback)) Sg_Error(UC("argument out of range: %d"), i);
  return fallback;
}

SgObject Sg_ListRef(SgObject list, int i, SgObject fallback)
{
  int k;
  if (i < 0) goto err;
  for (k = 0; k < i; k++) {
    if (!SG_PAIRP(list)) goto err;
    list = SG_CDR(list);
  }
  if (!SG_PAIRP(list)) goto err;
  return SG_CAR(list);
 err:
  if (SG_UNBOUNDP(fallback)) Sg_Error(UC("argument out of range: %d"), i);
  return fallback;
}

SgObject Sg_Memq(SgObject obj, SgObject list)
{
  SG_FOR_EACH(list, list) {
    if (SG_EQ(obj, SG_CAR(list))) return list;
  }
  return SG_FALSE;
}

SgObject Sg_Memv(SgObject obj, SgObject list)
{
  SG_FOR_EACH(list, list) {
    if (Sg_EqvP(obj, SG_CAR(list))) return list;
  }
  return SG_FALSE;
}
/*
SgObject Sg_Member(SgObject obj, SgObject list)
{
  SG_FOR_EACH(list, list) {
    if (Sg_EqualP(obj, SG_CAR(list))) return list;
  }
  return SG_FALSE;
}
*/

SgObject Sg_Assq(SgObject obj, SgObject alist)
{
  SgObject cp;
  if (!SG_LISTP(alist)) Sg_Error(UC("assq: list requried, but got %S"), alist);
  SG_FOR_EACH(cp, alist) {
    SgObject entry = SG_CAR(cp);
    if (!SG_PAIRP(entry)) continue;
    if (SG_EQ(obj, SG_CAR(entry))) return entry;
  }
  return SG_FALSE;
}

SgObject Sg_Assv(SgObject obj, SgObject alist)
{
  SgObject cp;
  if (!SG_LISTP(alist)) Sg_Error(UC("assv: list requried, but got %S"), alist);
  SG_FOR_EACH(cp, alist) {
    SgObject entry = SG_CAR(cp);
    if (!SG_PAIRP(entry)) continue;
    if (Sg_EqvP(obj, SG_CAR(entry))) return entry;
  }
  return SG_FALSE;
}

/*
SgObject Sg_Assoc(SgObject obj, SgObject alist)
{
  SgObject cp;
  if (!SG_LISTP(alist)) Sg_Error(UC("assv: list requried, but got %S"), alist);
  SG_FOR_EACH(cp, alist) {
    SgObject entry = SG_CAR(cp);
    if (!SG_PAIRP(entry)) continue;
    if (Sg_EqualP(obj, SG_CAR(entry))) return entry;
  }
  return SG_FALSE;
}
*/
  
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
