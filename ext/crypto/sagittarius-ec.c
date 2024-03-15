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
#include <math.h>
#include <string.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "sagittarius-ec.h"

#undef min
#define min(x, y)   (((x) < (y))? (x) : (y))
#undef max
#define max(x, y)   (((x) > (y))? (x) : (y))


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

#define USE_MBIGNUM

#ifndef USE_MBIGNUM

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
  
  /* Sg_Printf(Sg_StandardErrorPort(), UC("f2m: %S\n"), f2m); */
  /* Sg_Printf(Sg_StandardErrorPort(), UC("  x: %S\n"), x); */
  /* Sg_Printf(Sg_StandardErrorPort(), UC("  y: %S\n"), y); */
  /* Sg_Printf(Sg_StandardErrorPort(), UC("  m: %S\n"), mmp1); */
  /* Sg_Printf(Sg_StandardErrorPort(), UC("  k: %S\n"), k); */
  /* Sg_Printf(Sg_StandardErrorPort(), UC("bx0: %S\n"), bx); */
  
  for (i = 1; i <= f2m->m; i++) {
    if (Sg_BitSetP(ax, i)) cz = Sg_LogXor(cz, bx);
    bx = mult_z_mod(f2m->m, mmp1, k, bx);    
  }

  /* Sg_Printf(Sg_StandardErrorPort(), UC(" bx: %S\n"), bx); */
  /* Sg_Printf(Sg_StandardErrorPort(), UC(" cz: %S\n"), cz); */
  return cz;
}

#else

static mbignum_t * multi_z_mod(mbignum_t *r,
			       int m,
			       mbignum_t *mm,
			       mbignum_t *k,
			       mbignum_t *a)
{
  mbignum_t *az = mbignum_lshift(r, a, 1);
  
  if (mbignum_bit_setp(az, m)) {
    int bl = mbignum_bit_size(az);
    long size = mbignum_left_shift_space(ONE, bl) + 1;
    mbignum_t *bm;
    alloc_temp_mbignum(bm, size);
    mbignum_one(bm);
    mbignum_ash(bm, bm, bl);
    mbignum_sub(bm, bm, mm);

    mbignum_logand(az, az, bm);
    mbignum_one(bm);		/* reuse */
    mbignum_logxor(az, az, bm);
    mbignum_logxor(az, az, k);
  }
  return az;
}

SgObject Sg_F2mMul(SgEcFieldF2m *f2m, SgObject x, SgObject y)
{
  int i;
  int ppbP = SG_EC_FIELD_F2M_PPB_P(f2m);

  SgObject ax = x;		/* no need to be mbignum */
  mbignum_t *mm, *k1m, *k2m, *k3m, *bx, *cz;
  mbignum_t *k;
  long mms, k1ms, k2ms, k3ms;
  mms  = mbignum_left_shift_space(ONE, f2m->m) + 1;
  k1ms = mbignum_left_shift_space(ONE, f2m->k1);
  k2ms = mbignum_left_shift_space(ONE, f2m->k2);
  k3ms = mbignum_left_shift_space(ONE, f2m->k3);

  mm  = make_mbignum(mms);  mbignum_one(mm);
  k1m = make_mbignum(k1ms); mbignum_one(k1m);
  k2m = make_mbignum(k2ms); mbignum_one(k2m);
  k3m = make_mbignum(k3ms); mbignum_one(k3m);

  mbignum_ash(mm,  mm,  f2m->m);
  mbignum_ash(k1m, k1m, f2m->k1);
  mbignum_ash(k2m, k2m, f2m->k2);
  mbignum_ash(k3m, k3m, f2m->k3);

  mbignum_add_si(mm, mm, 1);

  if (ppbP) {
    long ks = max(max(k1ms, k2ms), k3ms);
    k = make_mbignum(ks);
    k = mbignum_logxor(k, k1m, k2m);
    k = mbignum_logxor(k, k, k3m);
  } else {
    k = k1m;
  }
  /* size of bx == cz == mm */
  bx = number_to_mbignum(y, mms);
  if (Sg_BitSetP(x, 0)) {
    cz = number_to_mbignum(y, mms);
  } else {
    cz= make_mbignum(mms);
    mbignum_zero(cz);
  }

  bx = multi_z_mod(bx, f2m->m, mm, k, bx);
  /* Sg_Printf(Sg_StandardErrorPort(), UC("f2m: %S\n"), f2m); */
  /* Sg_Printf(Sg_StandardErrorPort(), UC("  x: %S\n"), x); */
  /* Sg_Printf(Sg_StandardErrorPort(), UC("  y: %S\n"), y); */
  /* Sg_Printf(Sg_StandardErrorPort(), UC("  m: %S\n"), mbignum_to_number(mm)); */
  /* Sg_Printf(Sg_StandardErrorPort(), UC("  k: %S\n"), mbignum_to_number(k)); */
  /* Sg_Printf(Sg_StandardErrorPort(), UC("bx0: %S\n"), mbignum_to_number(bx)); */
  for (i = 1; i <= f2m->m; i++) {
    if (Sg_BitSetP(ax, i)) cz = mbignum_logxor(cz, cz, bx);
    bx = multi_z_mod(bx, f2m->m, mm, k, bx);
  }

  
  /* Sg_Printf(Sg_StandardErrorPort(), UC(" bx: %S\n"), mbignum_to_number(bx)); */
  /* Sg_Printf(Sg_StandardErrorPort(), UC(" cz: %S\n"), mbignum_to_number(cz)); */
  
  return mbignum_to_number(cz);
}
#endif

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
#ifndef USE_MBIGNUM
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
  /* Sg_Printf(Sg_StandardErrorPort(), UC("f2m: %S\n"), f2m); */
  /* Sg_Printf(Sg_StandardErrorPort(), UC("  x: %S\n"), x); */
  /* Sg_Printf(Sg_StandardErrorPort(), UC(" uz: %S\n"), uz); */
  /* Sg_Printf(Sg_StandardErrorPort(), UC(" vz: %S\n"), vz); */
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
  /* Sg_Printf(Sg_StandardErrorPort(), UC(" g2z: %S\n"), g2z); */
  return g2z;
#else
  mbignum_t *uz, *vz, *mm, *k1m, one, *g1z, *g2z, *buf;
  long mms, k1ms, uzs;

  mbignum_one(&one);
  mms  = mbignum_left_shift_space(ONE, f2m->m);
  k1ms = mbignum_left_shift_space(ONE, f2m->k1);
  alloc_temp_mbignum(mm, mms);  mbignum_one(mm);
  alloc_temp_mbignum(k1m, k1ms); mbignum_one(k1m);
  mbignum_ash(mm,  mm,  f2m->m);
  mbignum_ash(k1m, k1m, f2m->k1);
  mbignum_logior(mm, mm, &one);

  uzs = (Sg_BitSize(x) + 7)/8 + 1;
  alloc_temp_mbignum(vz, uzs);
  alloc_temp_mbignum(g1z, uzs);
  alloc_temp_mbignum(g2z, uzs);
  alloc_temp_mbignum(buf, uzs);
  
  uz = number_to_mbignum(x, uzs);
  vz = mbignum_logior(vz, mm, k1m);
  mbignum_one(g1z);
  mbignum_zero(g2z);
  
  if (SG_EC_FIELD_F2M_PPB_P(f2m)) {
    long k2ms, k3ms;
    mbignum_t *k2m, *k3m;
    k2ms = mbignum_left_shift_space(ONE, f2m->k2);
    k3ms = mbignum_left_shift_space(ONE, f2m->k3);
    alloc_temp_mbignum(k2m, k2ms);
    alloc_temp_mbignum(k3m, k3ms);
    mbignum_ash(k2m, &one, f2m->k2);
    mbignum_ash(k3m, &one, f2m->k3);
    vz = mbignum_logior(vz, vz, k2m);
    vz = mbignum_logior(vz, vz, k3m);
  }
  if (vz->sign < 0 || mbignum_zerop(vz)) {
    Sg_AssertionViolation(SG_INTERN("f2m-inverse"),
			  SG_MAKE_STRING("x is zero or negative"),
			  SG_LIST1(x));
  }

  /* Sg_Printf(Sg_StandardErrorPort(), UC("f2m: %S\n"), f2m); */
  /* Sg_Printf(Sg_StandardErrorPort(), UC("  x: %S\n"), x); */
  /* Sg_Printf(Sg_StandardErrorPort(), UC(" uz: %S\n"), mbignum_to_number(uz)); */
  /* Sg_Printf(Sg_StandardErrorPort(), UC(" vz: %S\n"), mbignum_to_number(vz)); */
  
  while (!mbignum_zerop(uz)) {
    int j = mbignum_bit_size(uz) - mbignum_bit_size(vz);
    if (j < 0) {
      mbignum_t *t1 = uz, *t2 = g1z;
      uz = vz;
      vz = t1;
      g1z = g2z;
      g2z = t2;
      j = -j;
    }
    buf = mbignum_ash(buf, vz, j);
    uz = mbignum_logxor(uz, uz, buf);
    buf = mbignum_ash(buf, g2z, j);
    g1z = mbignum_logxor(g1z, g1z, buf);
  }
  /* Sg_Printf(Sg_StandardErrorPort(), UC(" g2z: %S\n"), mbignum_to_number(g2z)); */
  return mbignum_to_number(g2z);
#endif
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
