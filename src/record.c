/* record.c                                        -*- mode:c; coding:utf-8; -*-
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
#define LIBSAGITTARIUS_BODY
#include "sagittarius/record.h"
#include "sagittarius/clos.h"
#include "sagittarius/library.h"
#include "sagittarius/port.h"
#include "sagittarius/symbol.h"

static SgClass *Sg_RTMCPL[] = {
  SG_CLASS_CLASS,
  SG_CLASS_OBJECT,
  SG_CLASS_TOP,
  NULL
};

extern SgObject Sg_ClassAllocate(SgClass *klass, SgObject initargs);

static void rtm_printer(SgObject o, SgPort *p, SgWriteContext *ctx)
{
  Sg_Putuz(p, UC("#<record-type-meta>"));
}

SG_DEFINE_BASE_CLASS(Sg_RecordTypeMetaClass, SgRecordTypeMeta,
		     rtm_printer, NULL, NULL, Sg_ClassAllocate, Sg_RTMCPL);

static SgObject rtm_rtd(SgRecordTypeMeta *rtm)
{
  return rtm->rtd;
}
static void rtm_rtd_set(SgRecordTypeMeta *rtm, SgObject rtd)
{
  rtm->rtd = rtd;
}
static SgObject rtm_rcd(SgRecordTypeMeta *rtm)
{
  return rtm->rcd;
}
static void rtm_rcd_set(SgRecordTypeMeta *rtm, SgObject rcd)
{
  rtm->rcd = rcd;
}

static SgSlotAccessor rtm_slots[] = {
  SG_CLASS_SLOT_SPEC("rtd", 0, rtm_rtd, rtm_rtd_set),
  SG_CLASS_SLOT_SPEC("rcd", 0, rtm_rcd, rtm_rcd_set),
  { { NULL } }
};

int Sg_RecordP(SgObject o)
{
  SgClass *c = Sg_ClassOf(o);
  return SG_ISA(c, SG_CLASS_RECORD_TYPE_META);
}

SgObject Sg_AllocateRecordTypeMeta(SgClass *klass, SgObject initargs)
{
  SgObject m = klass->allocate(klass, initargs);
  SG_SET_CLASS(m, klass);
  /* ok we need to initialise the metaclass by hand */
  SG_CLASS(m)->cpa = Sg_RTMCPL;
  Sg_InitStaticClass(SG_CLASS(m), NULL, NULL, rtm_slots, 0);
  SG_RECORD_TYPE_META(m)->rtd = SG_FALSE;
  SG_RECORD_TYPE_META(m)->rcd = SG_FALSE;
  return m;
}

void Sg__InitRecord()
{
  SgLibrary *lib = Sg_FindLibrary(SG_INTERN("(sagittarius clos)"), TRUE);
  Sg_InitStaticClass(SG_CLASS_RECORD_TYPE_META, UC("<record-type-meta>"),
		     lib, rtm_slots, 0);
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
