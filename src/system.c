/* system.c                                        -*- mode:c; coding:utf-8; -*-
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
#include <float.h>
#include <math.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/system.h"
#include "sagittarius/clos.h"
#include "sagittarius/error.h"
#include "sagittarius/number.h"
#include "sagittarius/library.h"
#include "sagittarius/symbol.h"
#include "sagittarius/writer.h"
#include "sagittarius/values.h"

static void time_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  SgTime *t = SG_TIME(self);
  Sg_Printf(port, UC("#<%S %S.%09lu>"), t->type, 
	    Sg_MakeIntegerFromS64(t->sec), t->nsec);
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
      Sg_Error(UC("cannot compare different types of time objects: %S vs %S"),
	       x, y);
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

/* slot accessor for time and date */
#define DEFINE_GETTER(type, slot, ctype, conv)				\
  static SgObject type##_##slot (ctype *i) {				\
    return conv(i -> slot);						\
  }
#define DEFINE_SETTER(type, slot, ctype, check, conv)			\
  static void type##_##slot##_set (ctype *i, SgObject v) {		\
    if (!check(v)) {							\
      Sg_Error(UC("Type error: "#type" '"#slot " %S"), v);		\
    }									\
    (i -> slot) = conv(v);						\
  }
#define DEFINE_ACCESSOR(type, slot, ctype, getter_conv, check, setter_conv) \
  DEFINE_GETTER(type, slot, ctype, getter_conv)				\
  DEFINE_SETTER(type, slot, ctype, check, setter_conv)

#define GET_INT64(x) Sg_GetIntegerS64Clamp(x, SG_CLAMP_BOTH, NULL)
#define ID(x) x
DEFINE_ACCESSOR(time, type, SgTime, ID, SG_SYMBOLP, ID);
DEFINE_ACCESSOR(time, sec, SgTime, 
		Sg_MakeIntegerFromS64, SG_EXACT_INTP, GET_INT64);
DEFINE_ACCESSOR(time, nsec, SgTime, Sg_MakeIntegerU, SG_EXACT_INTP,
		Sg_GetUInteger);

static SgSlotAccessor time_slots[] = {
  SG_CLASS_SLOT_SPEC("type",       0, time_type, time_type_set),
  SG_CLASS_SLOT_SPEC("nanosecond", 1, time_nsec, time_nsec_set),
  SG_CLASS_SLOT_SPEC("second",     2, time_sec,  time_sec_set),
  { { NULL } }
};

struct timespec* Sg_GetTimeSpec(SgObject t, struct timespec *spec)
{
  if (SG_FALSEP(t)) return NULL;
  if (SG_TIMEP(t)) {
    spec->tv_sec = SG_TIME(t)->sec;
    spec->tv_nsec = SG_TIME(t)->nsec;
  } else if (SG_REALP(t)) {
    unsigned long sec, usec;
    Sg_GetTimeOfDay(&sec, &usec);
    spec->tv_sec = sec;
    spec->tv_nsec = usec * 1000;
    if (SG_INTP(t)) {
      spec->tv_sec += Sg_GetUInteger(t);
    } else {
      double s;
      spec->tv_nsec += (unsigned long)(modf(Sg_GetDouble(t), &s) * 1.0e9);
      spec->tv_sec += (unsigned long)s;
      while (spec->tv_nsec >= 1000000000) {
	spec->tv_nsec -= 1000000000;
	spec->tv_sec += 1;
      }
    }
  } else {
    Sg_Error(UC("bad timeout spec: <time> object or real number is required,"
		" but got %S"), t);
  }
  return spec;
}

static double usec2double(uint64_t time)
{
  /* sec*1000000 + usec */
  int64_t sec = time/1000000;
  int64_t usec = time%1000000;
  return (double)sec + usec / (double)1000000.0;
}

SgObject Sg_VMTimeUsage()
{
  uint64_t r, u, s;
  int rr = Sg_TimeUsage(&r, &u, &s);
  if (rr < 0) {
    SgObject zero = Sg_MakeFlonum(0);
    return Sg_Values3(zero, zero, zero);
  }
  return Sg_Values3(Sg_MakeFlonum(usec2double(r)),
		    Sg_MakeFlonum(usec2double(u)),
		    Sg_MakeFlonum(usec2double(s)));
}


void Sg__InitBaseSystem()
{
  SgLibrary *lib = Sg_FindLibrary(SG_INTERN("(sagittarius clos)"), FALSE);
  Sg_InitStaticClassWithMeta(SG_CLASS_TIME, UC("<time>"), lib, NULL,
			     SG_FALSE, time_slots, 0);
}
