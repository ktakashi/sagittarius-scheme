/* -*- C -*- */
/*
 * time.c: srfi-19 time library
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
#include <math.h>
#include <sagittarius.h>
#define LIBSAGITTARIUS_BODY
#include <sagittarius/extend.h>
#include "time.h"

static void time_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  SgTime *t = SG_TIME(self);
  Sg_Printf(port, UC("#<%S %S.%09lu>"), t->type, Sg_MakeIntegerFromS64(t->sec), t->nsec);
}

static int time_compare(SgObject x, SgObject y, int equalp)
{
  SgTime *tx, *ty;
  /* it could be not the save meta object. see compare.c */
  if (!SG_TIMEP(x) || !SG_TIMEP(y)) return FALSE;
  tx = SG_TIME(x);
  ty = SG_TIME(y);
  if (equalp) {
    if (SG_EQ(tx->type, ty->type) &&
	tx->sec == ty->sec &&
	tx->nsec == ty->nsec) {
      return 0;
    } else {
      return -1;
    }
  } else {
    if (!SG_EQ(tx->type, ty->type)) {
      Sg_Error(UC("cannot compare different types of time objects: %S vs %S"), x, y);
    }
    if (tx->sec < ty->sec) return -1;
    if (tx->sec == ty->sec) {
      if (tx->nsec < ty->nsec) return -1;
      if (tx->nsec == ty->nsec) return 0;
      return 1;
    } else {
      return 1;
    }
  }
}

SG_DEFINE_BUILTIN_CLASS(Sg_TimeClass, time_printer, time_compare,
			NULL, NULL, NULL);

static SgObject time_utc = SG_UNDEF;
static SgObject time_tai = SG_UNDEF;
static SgObject time_monotonic = SG_UNDEF;
static SgObject time_duration = SG_UNDEF;
static SgObject time_process = SG_UNDEF;
static SgObject time_thread = SG_UNDEF;

static SgTime* make_time_int(SgObject type)
{
  SgTime *t = SG_NEW(SgTime);
  SG_SET_CLASS(t, SG_CLASS_TIME);
  t->type = SG_FALSEP(type) ? time_utc : type;
  return t;
}

SgObject Sg_MakeTime(SgObject type, int64_t sec, unsigned long nsec)
{
  SgTime *t = make_time_int(type);
  t->sec = sec;
  t->nsec = nsec;
  return SG_OBJ(t);
}

static struct 
{
  uint32_t utc_second;
  uint32_t second;
} leap_second_table[] = {
  {1136073600, 33},
  {915148800, 32},
  {867715200, 31},
  {820454400, 30},
  {773020800, 29},
  {741484800, 28},
  {709948800, 27},
  {662688000, 26},
  {631152000, 25},
  {567993600, 24},
  {489024000, 23},
  {425865600, 22},
  {394329600, 21},
  {362793600, 20},
  {315532800, 19},
  {283996800, 18},
  {252460800, 17},
  {220924800, 16},
  {189302400, 15},
  {157766400, 14},
  {126230400, 13},
  {94694400, 12},
  {78796800, 11},
  {63072000, 10}
};

static unsigned long leap_second_delta(unsigned long utcsec)
{
  if (utcsec < (1972 - 1970) * 365 * TM_SID) {
    return 0;
  } else {
    const int size = array_sizeof(leap_second_table);
    int i;
    for (i = 0; i < size; i++) {
      if (utcsec >= leap_second_table[i].utc_second) return leap_second_table[i].second;
    }
  }
  return 0;			/* dummy */
}

SgObject Sg_CurrentTime(SgObject type)
{
  unsigned long sec, usec;
  Sg_GetTimeOfDay(&sec, &usec);
  if (SG_EQ(type, time_utc)) {
    return Sg_MakeTime(time_utc, sec, usec * 10);
  } else if (SG_EQ(type, time_tai)) {
    return Sg_MakeTime(time_tai, sec + leap_second_delta(sec),
		       usec * 10);
  } else if (SG_EQ(type, time_monotonic)) {
    return Sg_MakeTime(time_monotonic, sec + leap_second_delta(sec),
		       usec * 10);
  } else if (SG_EQ(type, time_process)) {
    unsigned long vsec, vusec;
    Sg_VMProcessTime(&vsec, &vusec);
    return Sg_MakeTime(type, sec - vsec, (usec - vusec) * 10);
  } else if (SG_EQ(type, time_thread)) {
    SgVM *vm = Sg_VM();
    return Sg_MakeTime(type, sec - vm->uptimeSec, (usec - vm->uptimeUsec) * 10);
  } else {
    Sg_Error(UC("TIME-ERROR type current-time: invalid-clock-type %S"), type);
    return SG_UNDEF;		/* dummy */
  }
}

SgObject Sg_SecondsToTime(int64_t sec)
{
  return Sg_MakeTime(time_utc, sec, 0);
}

SgObject Sg_TimeToSeconds(SgTime *t)
{
  if (t->nsec) {
    return Sg_MakeFlonum((double)t->sec + (double)t->nsec/TM_NANO);
  } else {
    return Sg_MakeIntegerFromS64(t->sec);
  }
}

SgObject Sg_TimeDifference(SgTime *x, SgTime *y, SgTime *r)
{
  if (!SG_EQ(x->type, y->type)) {
    Sg_Error(UC("TIME-ERROR time-differece: imcompatible-time-types %S vs %S"), x, y);
  }
  r->type = time_duration;
  if (SG_CLASS_OF(x)->compare(x, y, FALSE) == 0) {
    r->sec = 0;
    r->nsec = 0;
  } else {
    double nano = (x->sec * TM_NANO + x->nsec) - (y->sec * TM_NANO + y->nsec);
    unsigned long nanos = abs(fmod(nano, TM_NANO));
    int64_t secs = nano / TM_NANO;
    r->sec = secs;
    r->nsec = nanos;
  }
  return r;
}

SgObject Sg_AddDuration(SgTime *x, SgTime *y, SgTime *r)
{
  int64_t sec_plus;
  unsigned long nsec_plus, rr, q;
  
  if (!SG_EQ(y->type, time_duration)) {
    Sg_Error(UC("TIME-ERROR time-differece: no-duration %S"), y);
  }
  sec_plus = x->sec + y->sec;
  nsec_plus = x->nsec + y->nsec;
  rr = (unsigned long)fmod(nsec_plus, TM_NANO);
  q = nsec_plus / TM_NANO;
  if (rr < 0) {
    r->sec = sec_plus + q + -1;
    r->nsec = TM_NANO + rr;
  } else {
    r->sec = sec_plus + q;
    r->nsec = rr;
  }
  return r;
}

SgObject Sg_SubDuration(SgTime *x, SgTime *y, SgTime *r)
{
  int64_t sec_minus;
  unsigned long nsec_minus, rr, q;
  
  if (!SG_EQ(y->type, time_duration)) {
    Sg_Error(UC("TIME-ERROR time-differece: no-duration %S"), y);
  }
  sec_minus = x->sec - y->sec;
  nsec_minus = x->nsec - y->nsec;
  rr = (unsigned long)fmod(nsec_minus, TM_NANO);
  q = nsec_minus / TM_NANO;
  if (r < 0) {
    r->sec = sec_minus - q - 1;
    r->nsec = TM_NANO + rr;
  } else {
    r->sec = sec_minus - q;
    r->nsec = rr;
  }
  return r;
}

extern void Sg__Init_sagittarius_time_impl();
extern void Sg__Init_sagittarius_date_impl();

SG_EXTENSION_ENTRY void Sg_Init_sagittarius__time()
{
  SG_INIT_EXTENSION(sagittarius__time);
  time_utc = SG_INTERN("time-utc");
  time_tai = SG_INTERN("time-tai");
  time_monotonic = SG_INTERN("time-monotonic");
  time_duration = SG_INTERN("time-duration");
  time_process = SG_INTERN("time-process");
  time_thread = SG_INTERN("time-thread");
  Sg__Init_sagittarius_time_impl();
  Sg__Init_sagittarius_date_impl();
}

