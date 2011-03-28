/* -*- C -*- */
/*
 * profiler.h
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
#ifndef SAGITTARIUS_PROFILER_H_
#define SAGITTARIUS_PROFILER_H_

#include "sagittariusdefs.h"

/* profiler */
typedef enum {
  SG_PROFILER_INACTIVE,
  SG_PROFILER_RUNNING,
  SG_PROFILER_PAUSING
} SgVMProfilerState;

typedef struct SgProfSampleRec
{
  SgObject  func;		/* code builder(closure), or subr */
  SgWord   *pc;
} SgProfSample;

#define SG_PROF_SAMPLES 6000

typedef struct SgProfCountRec
{
  SgObject func;		/* called function */
} SgProfCount;

#define SG_PROF_COUNTER 12000

struct SgVMProfilerRec
{
  SgVMProfilerState state;		/* profiler state */
  int               currentSample;	/* index of the current sample */
  int               totalSamples;	/* total # of samples */
  int               errorOccurred;	/* TRUE if error has occurred */
  int               currentCount;	/* index of the current counter */
  SgHashTable      *statHash;		/* hashtable for collected data.
					   value is a pair of integers,
					   (<call-count> . <sapmle-hits>) */
  SgProfSample      samples[SG_PROF_SAMPLES];
  SgProfCount       counts[SG_PROF_COUNTER];
};

#ifdef SAGITTARIUS_PROFILE
#define SG_PROF_COUNT_CALL(vm, obj)					\
  do {									\
    if ((vm)->profilerRunning) {					\
      if ((vm)->profiler->currentCount == SG_PROF_COUNTER) {		\
	Sg_ProfilerCountBufferFlush(vm);				\
      }									\
      (vm)->profiler->counts[(vm)->profiler->currentCount++].func = obj; \
    }									\
  } while (0)
#else
#define SG_PROF_COUNT_CALL(vm, obj) /* empty */
#endif

SG_CDECL_BEGIN

/* profiler */
SG_EXTERN void     Sg_ProfilerStart();
SG_EXTERN int      Sg_ProfilerStop();
SG_EXTERN void     Sg_ProfilerReset();
SG_EXTERN SgObject Sg_ProfilerRawResult();
SG_EXTERN void     Sg_ProfilerCountBufferFlush(SgVM *vm);

SG_CDECL_END

#endif /* SAGITTARIUS_PROFILER_H_ */
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/

