/* -*- C -*- */
/*
 * exceptions.h
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
#ifndef SAGITTARIUS_EXCEPTIONS_H_
#define SAGITTARIUS_EXCEPTIONS_H_

#include "sagittariusdefs.h"
/* 
   r6rs standard conditions
   hierarchy

   &condition
     +- &warning
     +- &serious
     |	  +- &error
     |	  +- &violation
     |	       +- &assertion
     |	       +- &non-continuable
     |	       +- &implementation-restriction
     |	       +- &lexical
     |	       +- &syntax
     |	       +- &undefined
     +- &message
     +- &irritants

   we implement these standard conditions in C.
 */

/* macros to define C level exceptions */

#define SG_DECLARE_EXCEPTIONS(libname, create)				\
  SgObject rtd__, rcd__, ctr__, pred__, accessor__;			\
  SgObject lib__ = Sg_FindLibrary(SG_INTERN(libname), (create))

#define SG_INTERN__CONDITION(cname, sname, prtd, prcd, uid, sealed, opaque, fvec, protocol) \
  do {									\
    rtd__ = Sg_MakeRecordTypeDescriptor(SG_INTERN(#sname),		\
					(prtd), (uid), (sealed),	\
					(opaque), (fvec));		\
    rcd__ = Sg_MakeRecordConstructorDescriptor(rtd__, (prcd), (protocol)); \
    SG_INIT_RECORD_TYPE(cname, SG_INTERN(#sname), rtd__, rcd__);	\
    Sg_InsertBinding(lib__, SG_INTERN(#sname), cname);			\
  } while (0)

#define SG_INTERN__CONDITION_SIMPLE(cname, sname, prtd, prcd, fvec)	\
  SG_INTERN__CONDITION(cname, sname, prtd, prcd, SG_FALSE, FALSE, FALSE, fvec, SG_FALSE)

#define SG_INTERN__CONDITION_CTR(cname, method)			\
  do {								\
    ctr__ = Sg_RecordConstructor(SG_RECORD_TYPE_RCD(cname));	\
    Sg_InsertBinding(lib__, SG_INTERN(#method), ctr__);		\
  } while (0)

#define SG_INTERN__CONDITION_PRED(cname, method)		\
  do {								\
    pred__ = Sg_RecordPredicate(SG_RECORD_TYPE_RTD(cname));	\
    Sg_InsertBinding(lib__, SG_INTERN(#method), pred__);	\
  } while (0)

#define SG_INTERN__CONDITION_ACCESSOR(cname, rmethod, cmethod, pos)	\
  do {									\
    accessor__ = Sg_RecordAccessor(SG_RECORD_TYPE_RTD(cname), pos);	\
    Sg_InsertBinding(lib__, SG_INTERN(#rmethod), accessor__);		\
    Sg_InsertBinding(lib__, SG_INTERN(#cmethod),			\
		     Sg_ConditionAccessor(SG_RECORD_TYPE_RTD(cname), accessor__)); \
  } while (0)

#define SG_SET_CONSTRUCTOR(t) (t) = ctr__;

SG_CDECL_BEGIN

/* constructor */
SG_EXTERN SgObject Sg_Condition(SgObject components);

/* converter */
SG_EXTERN SgObject Sg_SimpleConditions(SgObject obj);
SG_EXTERN SgObject Sg_CompoundConditionComponent(SgObject obj);

/* predicate */
SG_EXTERN int      Sg_CompoundConditionP(SgObject obj);
SG_EXTERN int      Sg_SimpleConditionP(SgObject obj);
SG_EXTERN int      Sg_ConditionP(SgObject obj);

/* generator */
SG_EXTERN SgObject Sg_ConditionPredicate(SgObject rtd);
SG_EXTERN SgObject Sg_ConditionAccessor(SgObject rtd, SgObject proc);

/* for c use constructor */
SG_EXTERN SgObject Sg_MakeNonContinuableViolation();
SG_EXTERN SgObject Sg_MakeWhoCondition(SgObject who);
SG_EXTERN SgObject Sg_MakeMessageCondition(SgObject msg);
SG_EXTERN SgObject Sg_MakeIrritantsCondition(SgObject irritants);
SG_EXTERN SgObject Sg_MakeWarning();
SG_EXTERN SgObject Sg_MakeReaderCondition(SgObject msg);


SG_EXTERN SgObject Sg_DescribeCondition(SgObject con);

SG_CDECL_END

#endif /* SAGITTARIUS_EXCEPTIONS_H_ */
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
