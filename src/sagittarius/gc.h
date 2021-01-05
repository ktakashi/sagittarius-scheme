/* gc.h                                     -*- mode:c; coding:utf-8; -*-
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
 */

#ifndef SAGITTARIUS_GC_H_
#define SAGITTARIUS_GC_H_

#include "sagittarius/platform.h"

/* alien thread invocation */
typedef void* (*SgAlienThreadInvokeFunc)(void *data);

SG_CDECL_BEGIN

/* 
   Adds data and bss ranges to GC roots

   @param data_start starting pointer of the data section
   @param data_end   end pointer of the data section
   @param bss_start  starting pointer of the bss section
   @param bss_end    end pointer of the bss section
 */
SG_EXTERN void 	Sg_RegisterDL(void *data_start, void *data_end,
			      void *bss_start, void *bss_end);
/* 
   Adds the given range of the memory section to GC roots

   @param start starting pointer
   @param end   end pointer
 */
SG_EXTERN void  Sg_AddGCRoots(void *start, void *end);

/*
  Invokes the given callback function in the managed GC memory space.

  @param func a callback function which accepts one arguments
  @param data user data which will be passed to the given `func`
 */
SG_EXTERN void* Sg_InvokeOnAlienThread(void * (*func)(void *data), void *data);


SG_CDECL_END


#endif	/* SAGITTARIUS_GC_H_ */
