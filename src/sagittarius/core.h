// -*- C -*-
/*
 * stub.h
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
#ifndef SAGITTARIUS_STUB_H_
#define SAGITTARIUS_STUB_H_

#include "sagittariusdefs.h"

/* finalizer proc */
typedef void (*SgFinalizerProc)(SgObject z, void *data);

SG_CDECL_BEGIN

SG_EXTERN void  Sg_Init();

SG_EXTERN void 	Sg_Exit(int code);
SG_EXTERN void 	Sg_Cleanup();
SG_EXTERN void*	Sg_AddCleanupHandler(void (*proc)(void *data), void *data);
SG_EXTERN void	Sg_DeleteCleanupHandler(void *handle);
SG_EXTERN void 	Sg_Panic(const char* msg, ...);
SG_EXTERN void 	Sg_Abort(const char* msg);
/* gc wrappers */
SG_EXTERN void 	Sg_GC();
SG_EXTERN void 	Sg_RegisterFinalizer(SgObject z, SgFinalizerProc finalizer, void *data);
SG_EXTERN void 	Sg_UnregisterFinalizer(SgObject z);
SG_EXTERN void 	Sg_RegisterDL(void *data_start, void *data_end,
			      void *bss_start, void *bss_end);
SG_EXTERN void 	Sg_RegisterDisappearingLink(void **p, void *value);
SG_EXTERN void 	Sg_UnregisterDisappearingLink(void **p);
SG_EXTERN void* Sg_GCBase(void *value);

/* experimental */
SG_EXTERN void  Sg_AddGCRoots(void *start, void *end);

/* cond-expand */
SG_EXTERN void  Sg_AddCondFeature(const SgChar *feature);
SG_EXTERN SgObject Sg_CondFeatures();

SG_CDECL_END

#endif /* SAGITTARIUS_STUB_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
