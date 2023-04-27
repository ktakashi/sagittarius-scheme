/* kernel.h                                        -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2021  Takashi Kato <ktakashi@ymail.com>
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
#ifndef SAGITTARIUS_PRIVATE_KERNEL_H_
#define SAGITTARIUS_PRIVATE_KERNEL_H_

#include "sagittariusdefs.h"
#include "clos.h"

/* For now, only internal use */
typedef struct SgDLinkNodeRec
{
  SgObject value;
  struct SgDLinkNodeRec *prev;
  struct SgDLinkNodeRec *next;
} SgDLinkNode;

SG_CLASS_DECL(Sg_KernelClass);
#define SG_CLASS_KERNEL (&Sg_KernelClass)

/* May expose to Scheme world, so make it a Scheme class */
typedef struct SgKernelRec
{
  SG_HEADER;
  SgDLinkNode *threads;		/* managed thread
				   The first entry must always be a main thread
				   of this kernel. */
  int          nThreads;	/* number of threads */
  SgInternalMutex lock;
} SgKernel;

#define SG_KERNEL(obj) ((SgKernel *)obj)
#define SG_KERNELP(obj) SG_XTYPEP(obj, SG_CLASS_KERNEL)


SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_NewKernel(SgVM *rootVM);
SG_EXTERN SgObject Sg_StartManagedThread(SgVM *vm, SgThreadEntryFunc func,
					 int daemonP);

SG_EXTERN SgObject Sg_KernelManagedThreads();
SG_EXTERN int Sg_KernelManagedCount();

SG_CDECL_END

#endif
