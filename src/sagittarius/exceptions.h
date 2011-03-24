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

SG_CDECL_END

#endif /* SAGITTARIUS_EXCEPTIONS_H_ */
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
