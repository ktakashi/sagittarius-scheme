/* system.h                                        -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2013  Takashi Kato <ktakashi@ymail.com>
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
#ifndef SAGITTARIUS_SYSTEM_H_
#define SAGITTARIUS_SYSTEM_H_

#include "sagittariusdefs.h"

SG_CDECL_BEGIN

SG_EXTERN const SgChar* Sg_NativeFileSeparator();

SG_EXTERN SgObject      Sg_GetLastErrorMessage();
SG_EXTERN SgObject      Sg_GetLastErrorMessageWithErrorCode(int code);

/* load path */
SG_EXTERN SgObject      Sg_GetDefaultLoadPath();
SG_EXTERN SgObject      Sg_GetDefaultDynamicLoadPath();

/* time */
SG_EXTERN int           Sg_GetTimeOfDay(unsigned long *sec, unsigned long *usec);
SG_EXTERN SgObject      Sg_TimeUsage();

/* for threading */
SG_EXTERN void          Sg_YieldCPU();

SG_EXTERN SgObject      Sg_Getenv(const SgChar *env);
SG_EXTERN void          Sg_Setenv(const SgChar *env, const SgChar *value);
SG_EXTERN SgObject      Sg_GetenvAlist();
SG_EXTERN SgObject      Sg_GetTemporaryDirectory();
/* returns a bytevector 
   TODO should this be in socket extra library?
 */
SG_EXTERN SgObject      Sg_GetMacAddress(int pos);
/* returns vector for SRFI-112 */
SG_EXTERN SgObject      Sg_Uname();

SG_EXTERN SgObject      Sg_SitelibPath();

/* for Cygwin environment 
   returns pid or handle
 */
SG_EXTERN uintptr_t Sg_SysProcessCall(SgObject name, SgObject args,
				      /* output parameters */
				      SgObject *inp, SgObject *outp,
				      SgObject *errp);
SG_EXTERN int       Sg_SysProcessWait(uintptr_t pid);

SG_CDECL_END

#endif /* SAGITTARIUS_SYSTEM_H_ */
