/* -*- C -*- */
/*
 * date.c
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
#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "sagittarius-time.h"

#ifdef _WIN32
#define tzname _tzname
#define tzset _tzset
#endif

SgObject Sg_Timezones()
{
  SgObject zone = Sg_MakeStringC(tzname[0]);
  SgObject dst = Sg_MakeStringC(tzname[1]);
  return Sg_Values2(zone, dst);
}

SgObject Sg_Timezone(SgObject when)
{
  if (Sg_DaylightP(when)) {
    return Sg_MakeStringC(tzname[1]);
  } else {
    return Sg_MakeStringC(tzname[0]);
  }
}

void Sg_SetTimezone(SgString *zone)
{
  /* TODO check if the given zone is a valid timezone name */
  if (zone) {
    Sg_Setenv(UC("TZ"), SG_STRING_VALUE(zone));
  } else {
    Sg_Setenv(UC("TZ"), NULL);
  }
  tzset();
}

SgObject Sg_TimezoneOffset(SgObject t)
{
  struct tm localTime;
  struct tm utcTime;
  time_t current, l;
  struct timespec spec, *tmp;

  tmp = Sg_GetTimeSpec(t, &spec);
  if (tmp) current = tmp->tv_sec;
  else  current = time(NULL);

#ifdef _WIN32
  localtime_s(&localTime, &current);
  l = mktime(&localTime);
  gmtime_s(&utcTime, &l);
#else
  localtime_r(&current, &localTime);
  l = mktime(&localTime);
  gmtime_r(&l, &utcTime);
#endif
  localTime.tm_isdst = 0;	/* set to 0 so that mktime consider DST */
  return Sg_MakeIntegerFromS64((int64_t)mktime(&localTime)
			       - (int64_t)mktime(&utcTime));
}

int Sg_DaylightP(SgObject t)
{
  struct tm localTime;
  time_t current;
  struct timespec spec, *tmp;

  tmp = Sg_GetTimeSpec(t, &spec);
  if (tmp) current = tmp->tv_sec;
  else  current = time(NULL);

#ifdef _WIN32
  localtime_s(&localTime, &current);
#else
  localtime_r(&current, &localTime);
#endif
  return localTime.tm_isdst;  
}
