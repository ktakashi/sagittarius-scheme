/* -*- C -*- */
/*
 * time.h: srfi-19 time library
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
#ifndef SAGITTARIUS_TIME_H_
#define SAGITTARIUS_TIME_H_

#include <sagittarius.h>
#include <time.h>

typedef struct SgTimeRec
{
  SG_HEADER;
  SgObject type;
  int64_t  sec;
  unsigned long nsec;
} SgTime;

SG_CLASS_DECL(Sg_TimeClass);
#define SG_CLASS_TIME  (&Sg_TimeClass)
#define SG_TIME(obj)  ((SgTime *)obj)
#define SG_TIMEP(obj) SG_XTYPEP(obj, SG_CLASS_TIME)


typedef struct SgDateRec
{
  SG_HEADER;
  unsigned long nsec;
  int     nanosecond;
  int     second;
  int     minute;
  int     hour;
  int     day;
  int     month;
  int     year;
  int64_t zoneOffset;
} SgDate;

SG_CLASS_DECL(Sg_DateClass);
#define SG_CLASS_DATE   (&Sg_DateClass)
#define SG_DATE(obj)   ((SgDate *)obj)
#define SG_DATEP(obj) SG_XTYPEP(obj, SG_CLASS_DATE)

#define TM_NANO  1.0e9
#define TM_SID   86400
#define TM_SIDH  43200
#define TM_TAI_EPOCH_IN_JD (double)(4881175/2)

SG_CDECL_BEGIN

SgObject Sg_MakeTime(SgObject type, int64_t sec, unsigned long nsec);
SgObject Sg_CurrentTime(SgObject type);
SgObject Sg_SecondsToTime(int64_t sec);
SgObject Sg_TimeToSeconds(SgTime *time);
SgObject Sg_TimeDifference(SgTime *x, SgTime *y, SgTime *r);
SgObject Sg_AddDuration(SgTime *x, SgTime *y, SgTime *r);
SgObject Sg_SubDuration(SgTime *x, SgTime *y, SgTime *r);

SgObject Sg_MakeDate(int nano, int sec, int min, int hour, int day, int mon, int year, int64_t zone);
SgObject Sg_LocalTzOffset();

SG_CDECL_END

#endif /* SAGITTARIUS_TIME_H_ */
