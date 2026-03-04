/* parameter.h                                      -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2026  Takashi Kato <ktakashi@ymail.com>
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
#ifndef SAGITTARIUS_PRIVATE_PARAMETER_H_
#define SAGITTARIUS_PRIVATE_PARAMETER_H_

#include "sagittariusdefs.h"
#include "clos.h"

SG_CLASS_DECL(Sg_ParameterizationClass)

#define SG_CLASS_PARAMETERIZATION (&Sg_ParameterizationClass)

typedef struct {
  SG_HEADER;
  SgObject cells;
} SgParameterization;

#define SG_PARAMETERIZATION(obj)  ((SgParameterization *)obj)
#define SG_PARAMETERIZATIONP(obj) SG_XTYPEP(obj, SG_CLASS_PARAMETERIZATION)
#define SG_PARAMETERIZATION_CELLS(o) SG_PARAMETERIZATION(o)->cells

typedef SgObject SgCoreParameterRef(SgObject);
typedef void SgCoreParameterSet(SgObject, SgObject);

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeParameterization(SgObject cells);
SG_EXTERN SgObject Sg_ParameterizationContinuationMarkKey();
SG_EXTERN SgObject Sg_CurrentParameterization();
SG_EXTERN SgObject Sg_ParameterizationRef(SgObject p, SgObject key);

SG_EXTERN SgObject Sg_MakeCoreParameter(SgObject name,
					SgObject initValue,
					SgCoreParameterRef ref,
					SgCoreParameterSet set);
SG_EXTERN int      Sg_CoreParameterP(SgObject o);
SG_CDECL_END

#endif
