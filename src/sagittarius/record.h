/* -*- C -*- */
/*
 * record.h
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
#ifndef SAGITTARIUS_RECORD_H_
#define SAGITTARIUS_RECORD_H_

#include "sagittariusdefs.h"

/*
  R6RS record
  memo:
  Record is a really huge library for R6RS (I think it's too huge, but anyway)
  Record has roughly 3 parts to use.
  1st one is record type.
  2nd one is record constructor descriptor(rcd).
  3rd one is record type descriptor(rtd).
  Record type has both rcd and rtd, and rcd contains rtd, so basic information
  for a record is rtd.

  so basic use for record is like this:
  ;; simple condition.
  (define &warning
    (let* ((rtd (make-record-type-descriptor '&warning #f #f #f '#()))
           (rcd (make-record-constructor-descriptor rtd #f #f)))
      (make-record type '&warning rtd rcd)))
  
  To represent these huge horrible thing we use our generic mechanism for rtd
  and rcd.

  TODO: how to implements default-protocol?
 */

struct SgRecordTypeRec {
  SG_HEADER;
  SgObject name;		/* record type name */
  SgObject rtd;			/* record type descriptor */
  SgObject rcd;			/* record constructor descriptor */
};

#define SG_RECORD_TYPE(obj)    	((SgRecordType*)obj)
#define SG_RECORD_TYPEP(obj)   	(SG_PTRP(obj) && IS_TYPE(obj, TC_RECORD_TYPE))
#define SG_RECORD_TYPE_RTD(obj) (SG_RECORD_TYPE(obj)->rtd)
#define SG_RECORD_TYPE_RCD(obj) (SG_RECORD_TYPE(obj)->rcd)

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeRecordType(SgObject name, SgObject rtd, SgObject rcd);
SG_EXTERN SgObject Sg_MakeRecordTypeDescriptor(SgSymbol *name, SgObject parent, SgObject uid,
					       int sealedP, int opaqueP, SgVector *fields);
SG_EXTERN SgObject Sg_MakeRecordConstructorDescriptor(SgObject rtd, SgObject parent, SgObject protocol);
SG_EXTERN SgObject Sg_RecordConstructor(SgObject rcd);
SG_EXTERN SgObject Sg_RecordPredicate(SgObject rtd);
SG_EXTERN SgObject Sg_RecordAccessor(SgObject rtd, int k);
SG_EXTERN SgObject Sg_RecordMutator(SgObject rtd, int k);

/* predicates */
SG_EXTERN int      Sg_RecordTypeDescriptorP(SgObject obj);
SG_EXTERN int      Sg_RecordConstructorDescriptorP(SgObject obj);

/* accessor */
/* TODO these accessor is only for (core base), so i don't want it to be public */
SG_EXTERN SgObject Sg_RtdParent(SgObject rtd);
SG_EXTERN SgObject Sg_RtdFields(SgObject rtd);
SG_EXTERN SgObject Sg_RcdParent(SgObject rcd);
SG_EXTERN SgObject Sg_RcdProtocol(SgObject rcd);

/* utilities */
SG_EXTERN int      Sg_RtdTotalFieldCount(SgObject rtd);
SG_EXTERN int      Sg_RtdInheritedFieldCount(SgObject rtd);

SG_CDECL_END

#endif /* SAGITTARIUS_RECORD_H_ */
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
