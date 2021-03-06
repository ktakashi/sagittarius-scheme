/* pair.c                                          -*- mode:c; coding:utf-8; -*-
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
#include "sagittarius/private/pair.h"
#include "sagittarius/private/collection.h"
#include "sagittarius/private/compare.h"
#include "sagittarius/private/error.h"
#include "sagittarius/private/subr.h"
#include "sagittarius/private/string.h"
#include "sagittarius/private/symbol.h"
#include "sagittarius/private/library.h"
#include "sagittarius/private/vm.h"

static SgClass *list_cpl[] = {
  SG_CLASS_LIST,
  SG_CLASS_SEQUENCE,
  SG_CLASS_COLLECTION,
  SG_CLASS_TOP,
  NULL
};
SG_DEFINE_BUILTIN_CLASS(Sg_ListClass, NULL, NULL, NULL, NULL, list_cpl+1);
SG_DEFINE_BUILTIN_CLASS(Sg_PairClass, NULL, NULL, NULL, NULL, list_cpl);
SG_DEFINE_BUILTIN_CLASS(Sg_NullClass, NULL, NULL, NULL, NULL, list_cpl);

static inline SgPair* make_pair()
{
  SgPair *z = SG_NEW(SgPair);
  z->info = SG_NIL;
  return z;
}

SgObject Sg_Cons(SgObject car, SgObject cdr)
{
  SgPair *z = make_pair();
  SG_SET_CAR(z, car);
  SG_SET_CDR(z, cdr);
  return SG_OBJ(z);
}

/* Public API */
int  Sg_IsPair(SgObject obj)
{
  return SG_PAIRP(obj);
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
       obj = va_arg(elts, SgObject)) {
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

static inline SgObject array_to_list_with_tail(SgObject *array,
					       int nelts, SgObject tail)
{
  SgObject h = SG_NIL, t = SG_NIL;
  if (array) {
    int i;
    for (i = 0; i < nelts; i++) SG_APPEND1(h, t, *array++);
  }
  if (!SG_NULLP(tail)) SG_APPEND(h, t, tail);
  return h;
}

SgObject Sg_ArrayToList(SgObject *array, int nelts)
{
  return array_to_list_with_tail(array, nelts, SG_NIL);
}

SgObject Sg_ArrayToListWithTail(SgObject *array, int nelts, SgObject tail)
{
  return array_to_list_with_tail(array, nelts, tail);
}

static SgObject* list_to_array_rec(SgObject list, int nullTermP, long *rlen)
{
  SgObject *array, lp;
  long len = Sg_Length(list), i, offset = 0;;
  if (len < 0) Sg_Error(UC("proper list required, but got %S"), list);
  if (nullTermP) offset++;
  array = SG_NEW_ARRAY(SgObject, len+offset);
  for (i = 0, lp = list; i<len; i++, lp = SG_CDR(lp)) {
    array[i] = SG_CAR(lp);
  }
  /* just in case */
  if (nullTermP) array[len] = NULL;
  if (rlen) *rlen= len;
  return array;
}

SgObject* Sg_ListToArray(SgObject list, int nullTermP)
{
  return list_to_array_rec(list, nullTermP, NULL);
}

#define CXR(cname, sname, body)			\
SgObject cname (SgObject obj)			\
{						\
  static SgObject PROC_NAME = SG_FALSE;		\
  SgObject obj2 = obj;				\
  if (SG_FALSEP(PROC_NAME)) {			\
    PROC_NAME = SG_INTERN(sname);		\
  }						\
  body						\
  return obj2;					\
}

#define A							\
  if (!SG_PAIRP(obj2)) {					\
    Sg_WrongTypeOfArgumentViolation(PROC_NAME,			\
				    SG_INTERN("pair"),		\
				    obj2, obj);			\
  }								\
  obj2 = SG_CAR(obj2);

#define D							\
  if (!SG_PAIRP(obj2)) {					\
    Sg_WrongTypeOfArgumentViolation(PROC_NAME,			\
				    SG_INTERN("pair"),		\
				    obj2, obj);			\
  }								\
  obj2 = SG_CDR(obj2);

CXR(Sg_Car, "car", A)
CXR(Sg_Cdr, "cdr", D)
CXR(Sg_Caar, "caar", A A)
CXR(Sg_Cadr, "cadr", D A)
CXR(Sg_Cdar, "cdar", A D)
CXR(Sg_Cddr, "cddr", D D)
/* Maybe add cadr etc.*/

long Sg_Length(SgObject obj)
{
  SgObject slow = obj;
  long len = 0;
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

SgObject Sg_ListTail(SgObject list, long i, SgObject fallback)
{
  long count = i;
  SgObject oargs = list;
  if (i < 0) goto err;
  while (count-- > 0) {
    if (!SG_PAIRP(list)) goto err;
    list = SG_CDR(list);
  }
  return list;
 err:
  if (SG_UNBOUNDP(fallback)) {
    Sg_AssertionViolation(SG_INTERN("list-tail"),
			  SG_MAKE_STRING("argument out of range"),
			  SG_LIST2(oargs, SG_MAKE_INT(i)));
  }
  return fallback;
}

SgObject Sg_ListRef(SgObject list, long i, SgObject fallback)
{
  long k;
  SgObject oargs = list;
  if (i < 0) goto err;
  for (k = 0; k < i; k++) {
    if (!SG_PAIRP(list)) goto err;
    list = SG_CDR(list);
  }
  if (!SG_PAIRP(list)) goto err;
  return SG_CAR(list);
 err:
  if (SG_UNBOUNDP(fallback)) {
    Sg_AssertionViolation(SG_INTERN("list-ref"),
			  SG_MAKE_STRING("argument out of range"),
			  SG_LIST2(oargs, SG_MAKE_INT(i)));
  }
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

static SgObject assq_rec(SgObject obj, SgObject alist)
{
  SgObject cp;
  SG_FOR_EACH(cp, alist) {
    SgObject entry = SG_CAR(cp);
    if (!SG_PAIRP(entry)) continue;
    if (SG_EQ(obj, SG_CAR(entry))) return entry;
  }
  return SG_FALSE;
}

SgObject Sg_Assq(SgObject obj, SgObject alist)
{
  if (!SG_LISTP(alist)) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("assq"),
				    SG_MAKE_STRING("list"),
				    alist, SG_NIL);
  }
  return assq_rec(obj, alist);
}

SgObject Sg_Assv(SgObject obj, SgObject alist)
{
  SgObject cp;
  if (!SG_LISTP(alist)) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("assv"),
				    SG_MAKE_STRING("list"),
				    alist, SG_NIL);
  }
  SG_FOR_EACH(cp, alist) {
    SgObject entry = SG_CAR(cp);
    if (!SG_PAIRP(entry)) continue;
    if (Sg_EqvP(obj, SG_CAR(entry))) return entry;
  }
  return SG_FALSE;
}

SgObject Sg_GetPairAnnotation(SgObject pair, SgObject name)
{
  SgObject s;
  if (!SG_PAIRP(pair)) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("pair-annotation"),
				    SG_MAKE_STRING("pair"),
				    pair, SG_NIL);
  }
  s = assq_rec(name, SG_PAIR(pair)->info);
  if (SG_FALSEP(s)) return SG_FALSE;
  return SG_CDR(s);
}

SgObject Sg_SetPairAnnotation(SgObject pair, SgObject name, SgObject v)
{
  SgObject s;
  if (!SG_PAIRP(pair)) {
    Sg_WrongTypeOfArgumentViolation(SG_INTERN("pair-annotation"),
				    SG_MAKE_STRING("pair"),
				    pair, SG_NIL);
  }
  s = assq_rec(name, SG_PAIR(pair)->info);
  if (SG_FALSEP(s)) {
    SG_PAIR(pair)->info = Sg_Acons(name, v, SG_PAIR(pair)->info);
  } else {
    SG_SET_CDR(s, v);
  }
  return pair;
}

/* from Ypsilon */
static SgObject do_transpose(long shortest_len, SgObject args[])
{
  SgObject ans = SG_NIL, tail = SG_NIL;
  long i, n, argc;
  SgObject *rest = list_to_array_rec(args[1], FALSE, &argc);

  for (i = 0; i < shortest_len; i++) {
    SgObject elt = SG_NIL, elt_tail = SG_NIL;
    
    SG_APPEND1(elt, elt_tail, SG_CAR(args[0]));
    args[0] = SG_CDR(args[0]);
    for (n = 0; n < argc; n++) {
      SG_APPEND1(elt, elt_tail, SG_CAR(rest[n]));
      rest[n] = SG_CDR(rest[n]);
    }
    SG_APPEND1(ans, tail, elt);
  }
  return ans;
}

static void improper_list_error(SgObject name, SgObject v, SgObject irr)
{
  Sg_WrongTypeOfArgumentViolation(name, SG_MAKE_STRING("proper list"), v, irr);
}

static SgObject list_transpose_s(SgObject *args, int argc, void *data)
{
  SgObject v;
  if (argc < 1) {
    Sg_WrongNumberOfArgumentsAtLeastViolation(SG_INTERN("list-transpose*"),
					      1, argc, SG_NIL);
  }
  /* since 0.3.4, optional arguments are packed to list.
     so argc is always 2.
   */
  v = args[0];
  if (SG_LISTP(args[0])) {
    long each_len = Sg_Length(args[0]);
    SgObject cp;
    if (each_len < 0 && each_len != SG_LIST_CIRCULAR) goto err;
    SG_FOR_EACH(cp, args[1]) {
      v = SG_CAR(cp);
      if (SG_LISTP(v)) {
	long len = Sg_Length(v);
	if (len < 0 && len != SG_LIST_CIRCULAR) goto err;
	if (len >= 0) {
	  if (len < each_len) each_len = len;
	  else if (each_len < 0) each_len = len;
	}
	continue;
      }
      goto err;
    }
    return do_transpose(each_len, args);
  }
 err:
  improper_list_error(SG_INTERN("list-transpose*"), v, 
		      Sg_ArrayToList(args, argc));
  return SG_UNDEF;
}

static SG_DEFINE_SUBR(list_transpose_s_stub, 1, 1, list_transpose_s,
		      SG_FALSE, NULL);

static SgObject list_transpose_p(SgObject *args, int argc, void *data)
{
  SgObject v;
  if (argc < 1) {
    Sg_WrongNumberOfArgumentsAtLeastViolation(SG_INTERN("list-transpose+"),
					      1, argc, SG_NIL);
  }
  v = args[0];
  if (SG_LISTP(args[0])) {
    long each_len = Sg_Length(args[0]);
    SgObject cp;
    if (each_len < 0) goto err;
    SG_FOR_EACH(cp, args[1]) {
      v = SG_CAR(cp);
      if (SG_LISTP(v)) {
	long len = Sg_Length(v);
	if (len < 0) goto err;
	if (len != each_len) return SG_FALSE;
	continue;
      }
      return SG_FALSE;
    }
    return do_transpose(each_len, args);
  }
 err:
  improper_list_error(SG_INTERN("list-transpose+"), v, 
		      Sg_ArrayToList(args, argc));
  return SG_UNDEF;
}

static SG_DEFINE_SUBR(list_transpose_p_stub, 1, 1, list_transpose_p,
		      SG_FALSE, NULL);

void Sg__InitPair()
{
  SgLibrary *lib = Sg_FindLibrary(SG_INTERN("(sagittarius)"), FALSE);
  SG_PROCEDURE_NAME(&list_transpose_s_stub) = SG_MAKE_STRING("list-transpose*");
  SG_PROCEDURE_TRANSPARENT(&list_transpose_s_stub) = SG_PROC_TRANSPARENT;
  Sg_InsertBinding(lib, SG_INTERN("list-transpose*"),
		   SG_OBJ(&list_transpose_s_stub));
  SG_PROCEDURE_NAME(&list_transpose_p_stub) = SG_MAKE_STRING("list-transpose+");
  SG_PROCEDURE_TRANSPARENT(&list_transpose_p_stub) = SG_PROC_TRANSPARENT;
  Sg_InsertBinding(lib, SG_INTERN("list-transpose+"),
		   SG_OBJ(&list_transpose_p_stub));
}
  
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
