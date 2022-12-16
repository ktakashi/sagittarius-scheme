/* sagittarius-ec.c                              -*- mode: c; coding: utf-8; -*-
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
#include <sagittarius.h>
#include <sagittarius/private.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "sagittarius-ec.h"

static void ec_field_fp_printer(SgObject o, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<ec-field-fp %A>"), SG_EC_FIELD_FP(o)->p);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_EcFieldFpClass, ec_field_fp_printer);

static void ec_field_f2m_printer(SgObject o, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<ec-field-f2m %d %d %d %d>"),
	    SG_EC_FIELD_F2M_M(o),
	    SG_EC_FIELD_F2M_K1(o),
	    SG_EC_FIELD_F2M_K2(o),
	    SG_EC_FIELD_F2M_K3(o));
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_EcFieldF2mClass, ec_field_f2m_printer);

SgObject Sg_MakeEcFieldFp(SgObject p)
{
  SgEcFieldFp *fp = SG_NEW(SgEcFieldFp);
  SG_SET_CLASS(fp, SG_CLASS_EC_FIELD_FP);
  fp->p = p;
  return SG_OBJ(fp);
}

SgObject Sg_MakeEcFieldF2m(int m, int k1, int k2, int k3)
{
  SgEcFieldF2m *f2m = SG_NEW(SgEcFieldF2m);
  SG_SET_CLASS(f2m, SG_CLASS_EC_FIELD_F2M);
  f2m->m = m;
  f2m->k1 = k1;
  f2m->k2 = k2;
  f2m->k3 = k3;
  return SG_OBJ(f2m);
}

/* 
 * Implementing f2m-* procedures in C for performance 
 * Even this simple migration improved the performance 100%...a
 */
SgObject Sg_F2mAdd(SgEcFieldF2m *f2m, SgObject x, SgObject y)
{
  if (Sg_ZeroP(y)) {
    return x;
  }
  return Sg_LogXor(x, y);
}

#define ONE (SG_MAKE_INT(1))

static SgObject mult_z_mod(int m, SgObject mm, SgObject k, SgObject a)
{
  SgObject az = Sg_Mul(a, SG_MAKE_INT(2));
  if (Sg_BitSetP(az, m)) {
    int bl = Sg_BitSize(az);
    SgObject bm = Sg_Sub(Sg_Ash(ONE, bl), mm);
    SgObject r = Sg_LogAnd(az, bm);
    return Sg_LogXor(Sg_LogXor(r, ONE), k);
  }
  return az;
}
SgObject Sg_F2mMul(SgEcFieldF2m *f2m, SgObject x, SgObject y)
{
  int i;
  int ppbP = SG_EC_FIELD_F2M_PPB_P(f2m);
  SgObject ax = x, bx = y, cz = Sg_BitSetP(x, 0)? y: SG_MAKE_INT(0);
  SgObject
    mm  = Sg_Ash(ONE, f2m->m),
    k1m = Sg_Ash(ONE, f2m->k1),
    k2m = Sg_Ash(ONE, f2m->k2),
    k3m = Sg_Ash(ONE, f2m->k3);
  SgObject mmp1 = Sg_Add(mm, ONE);
  SgObject k = ppbP? Sg_LogXor(Sg_LogXor(k1m, k2m), k3m): k1m;

  bx = mult_z_mod(f2m->m, mmp1, k, bx);
  for (i = 1; i <= f2m->m; i++) {
    if (Sg_BitSetP(ax, i)) cz = Sg_LogXor(cz, bx);
    bx = mult_z_mod(f2m->m, mmp1, k, bx);    
  }
  return cz;
}

SgObject Sg_F2mDiv(SgEcFieldF2m *f2m, SgObject x, SgObject y)
{
  return Sg_F2mMul(f2m, x, Sg_F2mInverse(f2m, y));
}
SgObject Sg_F2mSquare(SgEcFieldF2m *f2m, SgObject x)
{
  return Sg_F2mMul(f2m, x, x);
}
SgObject Sg_F2mInverse(SgEcFieldF2m *f2m, SgObject x)
{
  SgObject
    uz = x,
    vz = Sg_LogIor(Sg_LogIor(Sg_Ash(ONE, f2m->m), ONE), Sg_Ash(ONE, f2m->k1));
  SgObject g1z = ONE, g2z = SG_MAKE_INT(0);

  if (SG_EC_FIELD_F2M_PPB_P(f2m)) {
    vz = Sg_LogIor(vz, Sg_Ash(ONE, f2m->k2));
    vz = Sg_LogIor(vz, Sg_Ash(ONE, f2m->k3));
  }
  if (Sg_NegativeP(uz) || Sg_ZeroP(uz)) {
    Sg_AssertionViolation(SG_INTERN("f2m-inverse"),
			  SG_MAKE_STRING("x is zero or negative"),
			  SG_LIST1(x));
  }
  while (!Sg_ZeroP(uz)) {
    int j = Sg_BitSize(uz) - Sg_BitSize(vz);
    if (j < 0) {
      SgObject t1 = uz, t2 = g1z;
      uz = vz;
      vz = t1;
      g1z = g2z;
      g2z = t2;
      j = 0 - j;
    }
    uz = Sg_LogXor(uz, Sg_Ash(vz, j));
    g1z = Sg_LogXor(g1z, Sg_Ash(g2z, j));
  }
  return g2z;
}

extern void Sg__Init_ec_fields(SgLibrary *lib);

SG_EXTENSION_ENTRY void CDECL Sg_Init_sagittarius__ec()
{
  SgLibrary *lib;
  SG_INIT_EXTENSION(sagittarius__ec);
  lib = SG_LIBRARY(Sg_FindLibrary(SG_INTERN("(sagittarius crypto math ec fields)"), FALSE));
  Sg__Init_ec_fields(lib);
  Sg_InitStaticClass(SG_CLASS_EC_FIELD_FP, UC("<ec-field-fp>"), lib, NULL, 0);
  Sg_InitStaticClass(SG_CLASS_EC_FIELD_F2M, UC("<ec-field-f2m>"), lib, NULL, 0);
}
