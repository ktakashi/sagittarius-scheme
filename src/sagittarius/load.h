/* -*- C -*- */
/*
 * load.h
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
#ifndef SAGITTARIUS_LOAD_H_
#define SAGITTARIUS_LOAD_H_

#include "sagittariusdefs.h"

SG_CDECL_BEGIN

SG_EXTERN int      Sg_Load(SgString *path);
SG_EXTERN SgObject Sg_VMLoad(SgString *path);
SG_EXTERN SgObject Sg_VMLoadFromPort(SgPort *port);

SG_EXTERN SgObject Sg_DynLoad(SgString *filename, SgObject initfn, unsigned long flags);
/* for FFI interface */
SG_EXTERN void*    Sg_OpenSharedObject(SgString *filename);
SG_EXTERN void*    Sg_LookupSharedObject(void *handle, const char *symbol);
SG_EXTERN void     Sg_CloseSharedObject(void *handle);
SG_EXTERN SgObject Sg_GetSharedError();

SG_CDECL_END

#endif /* SAGITTARIUS_LOAD_H_ */
