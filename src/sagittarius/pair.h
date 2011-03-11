/* -*- C -*- */
/*
 * pair.h
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
#ifndef SAGITTARIUS_PAIR_HPP_
#define SAGITTARIUS_PAIR_HPP_

#include "sagittariusdefs.h"

struct SgPairRec
{
  SgObject car;
  SgObject cdr;
  /* source info which contains file name and line */
  SgObject sourceInfo;
};

#define SG_PAIRP(obj) 	  	 (SG_PTRP(obj) && (SG_HDR(obj) & 0xf) != 0x7)
#define SG_PAIR(obj)  	  	 ((SgPair*)obj)
#define SG_CAR(obj)   	  	 (SG_PAIR(obj)->car)
#define SG_CDR(obj)   	  	 (SG_PAIR(obj)->cdr)
#define SG_CAAR(obj)   	  	 (SG_CAR(SG_CAR(obj)))
#define SG_CADR(obj)   	  	 (SG_CAR(SG_CDR(obj)))
#define SG_CDAR(obj)   	  	 (SG_CDR(SG_CAR(obj)))
#define SG_CDDR(obj)   	  	 (SG_CDR(SG_CDR(obj)))
#define SG_SET_CAR(obj, value)   (SG_CAR(obj) = (value))
#define SG_SET_CDR(obj, value)   (SG_CDR(obj) = (value))
#define SG_SOURCE_INFO(obj)      (SG_PAIR(obj)->sourceInfo)

#define SG_LISTP(obj)            (SG_NULLP(obj) || SG_PAIRP(obj))

#define SG_FOR_EACH(p, list)				\
  for ((p) = (list); SG_PAIRP(p); (p) = SG_CDR(p))

#define SG_APPEND1(start, last, obj)			\
  do {							\
    if (SG_NULLP(start)) {				\
      (start) = (last) = Sg_Cons((obj), SG_NIL);	\
    } else {						\
      SG_SET_CDR((last), Sg_Cons((obj), SG_NIL));	\
      (last) = SG_CDR(last);				\
    }							\
  } while(0)						\

#define SG_APPEND(start, last, obj)			\
  do {							\
    SgObject list_SCM_GLS = (obj);			\
    if (SG_NULLP(start)) {				\
      (start) = (list_SCM_GLS);				\
      if (!SG_NULLP(list_SCM_GLS)) {			\
	(last) = Sg_LastPair(list_SCM_GLS);		\
      }							\
    } else {						\
      SG_SET_CDR((last), (list_SCM_GLS));		\
      (last) = Sg_LastPair(last);			\
    }							\
  } while(0)						\


#define SG_LIST1(a)              Sg_Cons(a, SG_NIL)
#define SG_LIST2(a,b)            Sg_Cons(a, SG_LIST1(b))
#define SG_LIST3(a,b,c)          Sg_Cons(a, SG_LIST2(b, c))
#define SG_LIST4(a,b,c,d)        Sg_Cons(a, SG_LIST3(b, c, d))
#define SG_LIST5(a,b,c,d,e)      Sg_Cons(a, SG_LIST4(b, c, d, e))

enum {
  SG_LIST_DOTTED = -1,  /* dotted list */
  SG_LIST_CIRCULAR = -2 /* circular list */
};

#define SG_PROPER_LISTP(obj)     (Sg_Length(obj) >= 0)
#define SG_DOTTED_LISTP(obj)     (Sg_Length(obj) == SG_LIST_DOTTED)
#define SG_CIRCULAR_LISTP(obj)   (Sg_Length(obj) == SG_LIST_CIRCULAR)

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_Cons(SgObject car, SgObject cdr);
SG_EXTERN SgObject Sg_Acons(SgObject caar, SgObject cdar, SgObject cdr);
SG_EXTERN SgObject Sg_List(SgObject elt, ...);
SG_EXTERN SgObject Sg_VaList(va_list elts);
SG_EXTERN SgObject Sg_ArrayToList(SgObject *array, int nelts);
SG_EXTERN SgObject Sg_ArrayToListWithTail(SgObject *array, int nelts, SgObject tail);

SG_EXTERN SgObject Sg_Car(SgObject obj);
SG_EXTERN SgObject Sg_Cdr(SgObject obj);
SG_EXTERN SgObject Sg_Caar(SgObject obj);
SG_EXTERN SgObject Sg_Cadr(SgObject obj);
SG_EXTERN SgObject Sg_Cdar(SgObject obj);
SG_EXTERN SgObject Sg_Cddr(SgObject obj);
SG_EXTERN int      Sg_Length(SgObject obj);
SG_EXTERN SgObject Sg_CopyList(SgObject list);
SG_EXTERN SgObject Sg_Append2X(SgObject list, SgObject obj);
SG_EXTERN SgObject Sg_Append2(SgObject list, SgObject obj);
SG_EXTERN SgObject Sg_Append(SgObject args);
SG_EXTERN SgObject Sg_ReverseX(SgObject list);
SG_EXTERN SgObject Sg_Reverse(SgObject list);
SG_EXTERN SgObject Sg_LastPair(SgObject list);
SG_EXTERN SgObject Sg_ListTail(SgObject list, int i, SgObject fallback);

SG_EXTERN SgObject Sg_Memq(SgObject obj, SgObject list);
SG_EXTERN SgObject Sg_Memv(SgObject obj, SgObject list);
SG_EXTERN SgObject Sg_Member(SgObject obj, SgObject list);
SG_EXTERN SgObject Sg_Assq(SgObject obj, SgObject alist);
SG_EXTERN SgObject Sg_Assv(SgObject obj, SgObject alist);
SG_EXTERN SgObject Sg_Assoc(SgObject obj, SgObject alist);

SG_CDECL_END

#endif /* SAGITTARIUS_PAIR_HPP_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
