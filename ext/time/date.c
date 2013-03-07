/* -*- C -*- */
/*
 * date.c
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
#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "sagittarius-time.h"

static void date_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  SgDate *d = SG_DATE(self);
  Sg_Printf(port, UC("#<date %d/%d/%d %d:%d:%d.%d>"),
	    d->day, d->month, d->year,
	    d->hour, d->minute, d->second, d->nanosecond);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_DateClass, date_printer);

SgObject Sg_MakeDate(int nano, int sec, int min, int hour, int day, 
		     int mon, int year, int64_t zone)
{
  SgDate *d = SG_NEW(SgDate);
  SG_SET_CLASS(d, SG_CLASS_DATE);
  d->nanosecond = nano;
  d->second = sec;
  d->minute = min;
  d->hour = hour;
  d->day = day;
  d->month = mon;
  d->year = year;
  d->zoneOffset = zone;
  return SG_OBJ(d);
}

SgObject Sg_LocalTzOffset()
{
  struct tm localTime;
  struct tm utcTime;
  time_t current = time(NULL);
  time_t l;
#ifdef _WIN32
  localtime_s(&localTime, &current);
  l = mktime(&localTime);
  gmtime_s(&utcTime, &l);
#else
  localtime_r(&current, &localTime);
  l = mktime(&localTime);
  gmtime_r(&l, &utcTime);
#endif
  return Sg_MakeIntegerFromU64((uint64_t)mktime(&localTime)
			       - (uint64_t)mktime(&utcTime));
}
