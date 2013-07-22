/* -*- C -*- */
/*
 * profiler.c
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
#define LIBSAGITTARIUS_BODY
#include "sagittarius/profiler.h"
#include "sagittarius/error.h"
#include "sagittarius/hashtable.h"
#include "sagittarius/instruction.h"
#include "sagittarius/pair.h"
#include "sagittarius/vm.h"
#include "sagittarius/writer.h"

/* for some reason BB10 doesn't have SA_RESTART */
#if defined(SAGITTARIUS_PROFILE) && defined(SA_RESTART)
#include <sys/time.h>
#include <time.h>
#include <signal.h>

#define SAMPLING_PERIOD 10000

#ifdef __CYGWIN__
#undef ITIMER_PROF
#undef SIGPROF
#define ITIMER_PROF ITIMER_REAL
#define SIGPROF     SIGALRM
#endif

#define ITIMER_START()                                  \
    do {                                                \
        struct itimerval tval, oval;                    \
        tval.it_interval.tv_sec = 0;                    \
        tval.it_interval.tv_usec = SAMPLING_PERIOD;     \
        tval.it_value.tv_sec = 0;                       \
        tval.it_value.tv_usec = SAMPLING_PERIOD;        \
        setitimer(ITIMER_PROF, &tval, &oval);           \
    } while (0)

#define ITIMER_STOP()                           \
    do {                                        \
        struct itimerval tval, oval;            \
        tval.it_interval.tv_sec = 0;            \
        tval.it_interval.tv_usec = 0;           \
        tval.it_value.tv_sec = 0;               \
        tval.it_value.tv_usec = 0;              \
        setitimer(ITIMER_PROF, &tval, &oval);   \
    } while (0)


static void sampler_sample(int sig)
{
  SgVM *vm = Sg_VM();
  int i;
  
  if (!vm) return;		/* how could this happen? */
  if (vm->profiler == NULL) return;
  if (vm->profiler->state != SG_PROFILER_RUNNING) return;
  /* we don't need compiler profiler at this moment. */
  if (vm->state != RUNNING) return;

  if (vm->profiler->currentSample >= SG_PROF_SAMPLES) {
    Sg_Printf(Sg_CurrentErrorPort(), UC("*STOP* profiler buffer is full."));
    ITIMER_STOP();
    return;
  }
  i = vm->profiler->currentSample++;
  if (vm->cl) {
    if (vm->pc && INSN(*vm->pc) == RET && SG_SUBRP(vm->ac)) {
      vm->profiler->samples[i].func = vm->ac;
      vm->profiler->samples[i].pc = NULL;
    } else {
      vm->profiler->samples[i].func = vm->cl;
      vm->profiler->samples[i].pc = vm->pc;
    }
  } else {
    vm->profiler->samples[i].func = SG_FALSE;
    vm->profiler->samples[i].pc = NULL;
  }
  vm->profiler->totalSamples++;
}

static void collect_samples(SgVMProfiler *prof)
{
  int i, count;
  for (i = 0; i < prof->currentSample; i++) {
    SgObject e = Sg_HashTableRef(prof->statHash,
				 prof->samples[i].func, SG_UNBOUND);
    if (SG_UNBOUNDP(e)) {
      Sg_Warn(UC("profiler: uncounted object appeared in a sample: %p (%S)\n"),
	      prof->samples[i].func, prof->samples[i].func);
    } else {
      ASSERT(SG_PAIRP(e));
      count = SG_INT_VALUE(SG_CDR(e)) + 1;
      SG_SET_CDR(e, SG_MAKE_INT(count));
    }
  }
}

void Sg_ProfilerStart()
{
  struct sigaction act;
  SgVM *vm = Sg_VM();

  if (!vm->profiler) {
    vm->profiler = SG_NEW(SgVMProfiler);
    vm->profiler->state = SG_PROFILER_INACTIVE;
    vm->profiler->currentSample = 0;
    vm->profiler->totalSamples = 0;
    vm->profiler->errorOccurred = FALSE;
    vm->profiler->currentCount = 0;
    vm->profiler->statHash = 
      SG_HASHTABLE(Sg_MakeHashTableSimple(SG_HASH_EQ, 0));
  }

  if (vm->profiler->state == SG_PROFILER_RUNNING) return;
  vm->profiler->state = SG_PROFILER_RUNNING;
  vm->profilerRunning = TRUE;

  act.sa_handler = sampler_sample;
  sigfillset(&act.sa_mask);
  act.sa_flags = SA_RESTART;
  if (sigaction(SIGPROF, &act, NULL) < 0) {
    /* TODO must be system error */
    Sg_Error(UC("sigaction failed"));
  }
  ITIMER_START();
}

int Sg_ProfilerStop()
{
  SgVM *vm = Sg_VM();
  if (vm->profiler == NULL) return 0;
  if (vm->profiler->state != SG_PROFILER_RUNNING) return 0;
  ITIMER_STOP();
  vm->profiler->state = SG_PROFILER_PAUSING;
  vm->profilerRunning = FALSE;
  return vm->profiler->totalSamples;
}

void Sg_ProfilerReset()
{
  SgVM *vm = Sg_VM();
  if (vm->profiler == NULL) return;
  if (vm->profiler->state == SG_PROFILER_INACTIVE) return;
  if (vm->profiler->state == SG_PROFILER_RUNNING) Sg_ProfilerStop();

  vm->profiler->currentSample = 0;
  vm->profiler->totalSamples = 0;
  vm->profiler->errorOccurred = FALSE;
  vm->profiler->currentCount = 0;
  vm->profiler->statHash = 
    SG_HASHTABLE(Sg_MakeHashTableSimple(SG_HASH_EQ, 0));
  vm->profiler->state = SG_PROFILER_INACTIVE;
}

SgObject Sg_ProfilerRawResult()
{
  SgVM *vm = Sg_VM();
  if (vm->profiler == NULL) return SG_FALSE;
  if (vm->profiler->state == SG_PROFILER_INACTIVE) return SG_FALSE;
  if (vm->profiler->state == SG_PROFILER_RUNNING) Sg_ProfilerStop();

  Sg_ProfilerCountBufferFlush(vm);
  collect_samples(vm->profiler);

  vm->profiler->currentSample = 0;
  return SG_OBJ(vm->profiler->statHash);
}

void Sg_ProfilerCountBufferFlush(SgVM *vm)
{
  int i, ncounts;
  sigset_t set;

  if (vm->profiler == NULL) return; /* for safety */
  if (vm->profiler->currentCount == 0) return;

  sigemptyset(&set);
  sigaddset(&set, SIGPROF);
  sigprocmask(SIG_BLOCK, &set, NULL);

  ncounts = vm->profiler->currentCount;
  for (i = 0; i < ncounts; i++) {
    SgObject e, func;
    int count;
    
    func = vm->profiler->counts[i].func;
    e = Sg_HashTableSet(vm->profiler->statHash,
			func,
			SG_FALSE,
			SG_HASH_NO_OVERWRITE);
    if (SG_FALSEP(e)) {
      e = Sg_HashTableSet(vm->profiler->statHash,
			  func,
			  Sg_Cons(SG_MAKE_INT(0), SG_MAKE_INT(0)),
			  0);
    }
    ASSERT(SG_PAIRP(e));
    count = SG_INT_VALUE(SG_CAR(e)) + 1;
    SG_SET_CAR(e, SG_MAKE_INT(count));
  }
  vm->profiler->currentCount = 0;
  sigprocmask(SIG_UNBLOCK, &set, NULL);
}

#else
void Sg_ProfilerStart()
{
  Sg_Error(UC("profiler is not supported"));
}

int Sg_ProfilerStop()
{
  Sg_Error(UC("profiler is not supported"));
  return 0;
}

void Sg_ProfilerReset()
{
  Sg_Error(UC("profiler is not supported"));
}

SgObject Sg_ProfilerRawResult()
{
  Sg_Error(UC("profiler is not supported"));
  return SG_FALSE;
}

void Sg_ProfilerCountBufferFlush(SgVM *vm)
{
  Sg_Error(UC("profiler is not supported"));
}

#endif
