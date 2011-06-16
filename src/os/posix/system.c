/* -*- C -*- */
/*
 * systam.c
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
#include <io.h>
#include <unistd.h>
#include <time.h>
#include <errno.h>
#ifdef HAVE_SCHED_H
# include <sched.h>
#endif
#define LIBSAGITTARIUS_BODY
#include <sagittarius/system.h>
#include <sagittarius/file.h>
#include <sagittarius/unicode.h>
#include <sagittarius/string.h>
#include <sagittarius/pair.h>
#include <sagittarius/error.h>

/* os dependent values */
const SgChar* Sg_NativeFileSeparator()
{
  return UC("/");
}

SgObject Sg_GetLastErrorMessageWithErrorCode(int code)
{
  return Sg_MakeStringC(strerror(code));
}

SgObject Sg_GetLastErrorMessage()
{
  return Sg_GetLastErrorMessageWithErrorCode(errno);
}

SgObject Sg_GetDefaultLoadPath()
{
  return SG_LIST2(Sg_MakeString(UC(SAGITTARIUS_SITE_LIB_PATH), SG_LITERAL_STRING),
		  Sg_MakeString(UC(SAGITTARIUS_SHARE_LIB_PATH), SG_LITERAL_STRING));
}

SgObject Sg_GetDefaultDynamicLoadPath()
{
  return SG_LIST1(Sg_MakeString(UC(SAGITTARIUS_DYNLIB_PATH), SG_LITERAL_STRING));
}

int Sg_GetTimeOfDay(unsigned long *sec, unsigned long *usec)
{
#if defined(HAVE_CLOCK_GETTIME)
  struct timespec ts;
  int r;
  r = clock_gettime(CLOCK_REALTIME, &ts);
  /* TODO do we need system error? */
  if (r < 0) Sg_Error(UC("clock_gettime failed"));
  *sec = (unsigned long)ts.tv_sec;
  *usec = (unsigned long)ts.tv_nsec / 1000;
  return r;
#elif defined(HAVE_GETTIMEOFDAY)
  struct timeval tv;
  int r;
  r = gettimeofday(&tv, NULL);
  /* TODO do we need system error? */
  if (r < 0) Sg_Error(UC("gettimeofday failed"));
  *sec = (unsigned long)tv.tv_sec;
  *usec = (unsigned long)tv.tv_usec;
  return r;
#else
  /* Last resort */
  /* If the platform is POSIX, it must have either clock_gettime or gettimeofday.
     Do we still need this? */
  *sec = (unsigned long)time(NULL);
  *usec = 0;
  return 0;
#endif
}

void Sg_YieldCPU()
{
#if defined(HAVE_SCHED_YIELD)
    sched_yield();
#elif defined(HAVE_NANOSLEEP)
    struct timespec spec;
    spec.tv_sec = 0;
    spec.tv_nsec = 1;
    nanosleep(&spec, NULL);
#elif defined(HAVE_SELECT)
    struct timeval tv;
    tv.tv_sec = 0;
    tv.tv_usec = 1;
    select(0, NULL, NULL, NULL, &tv);
#else /* the last resort */
    sleep(1);
#endif
}
