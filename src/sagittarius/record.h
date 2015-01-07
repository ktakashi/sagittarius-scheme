/* record.h                                        -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2015  Takashi Kato <ktakashi@ymail.com>
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
#include "clos.h"

/*
  R6RS record

  Class hierarchy and structure;
  <class>
    +- <record-type-meta>
         * rtd: <record-type-descriptor>
         * rcd: <record-constructor-descriptor>

  On C code, it's just meta class so that we can define conditions in C.
  The initialisation of the conditions and it's class are done in Scheme.
  All other code are defined in Scheme.
 */

SG_CLASS_DECL(Sg_RecordTypeMetaClass);

#define SG_CLASS_RECORD_TYPE_META (&Sg_RecordTypeMetaClass)
/* this is only for mark.. */
typedef SgClass SgRecordTypeMeta;

#define SG_RECORD_TYPE_META(obj)     ((SgRecordTypeMeta*)obj)
#define SG_RECORD_TYPE_METAP(obj)    SG_XTYPEP(obj, SG_CLASS_RECORD_TYPE_META)

SG_CDECL_BEGIN

SG_EXTERN int      Sg_RecordP(SgObject o);
SG_EXTERN SgObject Sg_AllocateRecordTypeMeta(SgClass *klass, SgObject initargs);

SG_CDECL_END

#endif /* SAGITTARIUS_RECORD_H_ */
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
