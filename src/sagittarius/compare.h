/* compare.h                                       -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2014  Takashi Kato <ktakashi@ymail.com>
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
#ifndef SAGITTARIUS_COMPARE_H_
#define SAGITTARIUS_COMPARE_H_

#include "sagittariusdefs.h"
#include "clos.h"

SG_CLASS_DECL(Sg_ComparatorClass);
#define SG_CLASS_COMPARATOR (&Sg_ComparatorClass)

/* 
   <comparator>
    - type-test
    - equality
    - comparison
    - hash
    - comparison?
    - hash?
 */
struct SgComparatorRec
{
  SG_HEADER;
  SgObject name;		/* for debugging */
  SgObject typeFn;		/* type-test */
  SgObject eqFn;		/* equality */
  SgObject compFn;		/* comparison */
  SgObject hashFn;		/* hash*/
  unsigned long flags;		/* comparison?, hash? and so */
};

#define SG_COMPARATORP(obj) SG_XTYPEP(obj, SG_CLASS_COMPARATOR)
#define SG_COMPARATOR(obj)  ((SgComparator *)obj)

enum SgComparatorFlags {
  SG_COMPARATOR_NO_ORDER  = (1L << 0), /* no comparison procedure */
  SG_COMPARATOR_NO_HASH   = (1L << 1), /* no hash procedure */
  SG_COMPARATOR_ANY_TYPE  = (1L << 2), /* type-test returns always #t */
};

enum {
    SG_CMP_EQ,
    SG_CMP_EQV,
    SG_CMP_EQUAL
};

#define SG_EQ(x, y) ((x) == (y))

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeComparator(SgObject typeFn, SgObject eqFn,
				     SgObject compFn, SgObject hashFn,
				     SgObject name,   unsigned long flags);
/* pre-defined comparators.
   The returning value should be const SgObject but I'm lazy...
 */
SG_EXTERN SgObject Sg_EqComparator();
SG_EXTERN SgObject Sg_EqvComparator();
SG_EXTERN SgObject Sg_EqualComparator();

SG_EXTERN int Sg_EqP(SgObject x, SgObject y);
SG_EXTERN int Sg_EqvP(SgObject x, SgObject y);
SG_EXTERN int Sg_EqualP(SgObject x, SgObject y);
SG_EXTERN int Sg_EqualM(SgObject x, SgObject y, int mode);
SG_EXTERN int Sg_Compare(SgObject x, SgObject y);

SG_CDECL_END

#endif /* SAGITTARIUS_COMPARE_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
