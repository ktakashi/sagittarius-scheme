/* sagittarius-ec.h                              -*- mode: c; coding: utf-8; -*-
 *
 *   Copyright (c) 2022  Takashi Kato <ktakashi@ymail.com>
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

#ifndef SAGITTARIUS_EC_H_
#define SAGITTARIUS_EC_H_

#include <sagittarius.h>

typedef struct {
  SG_HEADER;
  SgObject p;
} SgEcFieldFp;

SG_CLASS_DECL(Sg_EcFieldFpClass);
#define SG_CLASS_EC_FIELD_FP   (&Sg_EcFieldFpClass)
#define SG_EC_FIELD_FP(obj)    ((SgEcFieldFp *) obj)
#define SG_EC_FIELD_FP_P(obj)  SG_XTYPEP(obj, SG_CLASS_EC_FIELD_FP)

typedef struct {
  SG_HEADER;
  int m;
  int k1;
  int k2;
  int k3;
} SgEcFieldF2m;

SG_CLASS_DECL(Sg_EcFieldF2mClass);
#define SG_CLASS_EC_FIELD_F2M   (&Sg_EcFieldF2mClass)
#define SG_EC_FIELD_F2M(obj)    ((SgEcFieldF2m *) obj)
#define SG_EC_FIELD_F2M_P(obj)  SG_XTYPEP(obj, SG_CLASS_EC_FIELD_F2M)
#define SG_EC_FIELD_F2M_M(obj)  SG_EC_FIELD_F2M(obj)->m
#define SG_EC_FIELD_F2M_K1(obj)  SG_EC_FIELD_F2M(obj)->k1
#define SG_EC_FIELD_F2M_K2(obj)  SG_EC_FIELD_F2M(obj)->k2
#define SG_EC_FIELD_F2M_K3(obj)  SG_EC_FIELD_F2M(obj)->k3

#define SG_EC_FIELD_F2M_PPB_P(obj)					\
  (SG_EC_FIELD_F2M_K2(obj) != 0 && SG_EC_FIELD_F2M_K3(obj) != 0)

SgObject Sg_MakeEcFieldFp(SgObject p);
SgObject Sg_MakeEcFieldF2m(int m, int k1, int k2, int k3);
SgObject Sg_F2mAdd(SgEcFieldF2m *f2m, SgObject x, SgObject y);
SgObject Sg_F2mMul(SgEcFieldF2m *f2m, SgObject x, SgObject y);
SgObject Sg_F2mDiv(SgEcFieldF2m *f2m, SgObject x, SgObject y);
SgObject Sg_F2mSquare(SgEcFieldF2m *f2m, SgObject x);
SgObject Sg_F2mInverse(SgEcFieldF2m *f2m, SgObject x);

void Sg_InitEc(SgLibrary *lib);

#endif	/* SAGITTARIUS_EC_H_ */
