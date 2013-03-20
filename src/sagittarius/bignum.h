/* -*- C -*- */
/*
 * bignum.h
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
#ifndef SAGITTARIUS_BIGNUM_H_
#define SAGITTARIUS_BIGNUM_H_

#include "sagittariusdefs.h"

#define BIGNUM_SIZE(size) (sizeof(SgBignum)+((size)-1)*sizeof(long))

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeBignumFromSI(long value);
SG_EXTERN SgObject Sg_MakeBignumFromUI(unsigned long value);
SG_EXTERN SgObject Sg_MakeBignumFromS64(int64_t value);
SG_EXTERN SgObject Sg_MakeBignumFromU64(uint64_t value);
SG_EXTERN SgObject Sg_MakeBignumFromDouble(double value);
SG_EXTERN SgObject Sg_MakeBignumWithSize(int size, unsigned long init);
SG_EXTERN SgObject Sg_BignumCopy(SgBignum *b);
SG_EXTERN SgObject Sg_NormalizeBignum(SgBignum *b);
SG_EXTERN int      Sg_BignumCmp(SgBignum *lhs, SgBignum *rhs);

SG_EXTERN double   Sg_BignumToDouble(SgBignum *b);
SG_EXTERN SgObject Sg_BignumToInteger(SgBignum *b);
SG_EXTERN long          Sg_BignumToSI(SgBignum *b, int clamp, int *oor);
SG_EXTERN unsigned long Sg_BignumToUI(SgBignum *b, int clamp, int *oor);

SG_EXTERN int      Sg_BignumBitCount(SgBignum *b);
SG_EXTERN int      Sg_BignumBitSize(SgBignum *b);
SG_EXTERN int      Sg_BignumFirstBitSet(SgBignum *b);
SG_EXTERN int      Sg_BignumAbsCmp(SgBignum *bx, SgBignum *by);
SG_EXTERN int      Sg_BignumCmp3U(SgBignum *bx, SgBignum *off, SgBignum *by);

SG_EXTERN SgObject Sg_BignumToString(SgBignum *b, int radix, int use_upper);

/* i don't want to think about 128 bit CPU */
#if SIZEOF_LONG >= 8
SG_EXTERN int32_t  Sg_BignumToS32(SgBignum *b, int clamp, int *oor);
SG_EXTERN uint32_t Sg_BignumToU32(SgBignum *b, int clamp, int *oor);
#define Sg_BignumToS64 Sg_BignumToSI
#define Sg_BignumToU64 Sg_BignumToUI
#else
#define Sg_BignumToS32 Sg_BignumToSI
#define Sg_BignumToU32 Sg_BignumToUI
SG_EXTERN int64_t  Sg_BignumToS64(SgBignum *b, int clamp, int *oor);
SG_EXTERN uint64_t Sg_BignumToU64(SgBignum *b, int clamp, int *oor);
#endif

/* bignum arithmatics */
SG_EXTERN SgObject Sg_BignumComplement(SgBignum *bx);
SG_EXTERN SgObject Sg_BignumAsh(SgBignum *b, int count);
SG_EXTERN SgObject Sg_BignumShiftLeft(SgBignum *b, int count);
SG_EXTERN SgObject Sg_BignumShiftRight(SgBignum *b, int count);
SG_EXTERN SgObject Sg_BignumLogAnd(SgBignum *x, SgBignum *y);
SG_EXTERN SgObject Sg_BignumLogIor(SgBignum *x, SgBignum *y);
SG_EXTERN SgObject Sg_BignumLogXor(SgBignum *x, SgBignum *y);
SG_EXTERN SgObject Sg_BignumAdd(SgBignum *a, SgBignum *b);
SG_EXTERN SgObject Sg_BignumAddSI(SgBignum *a, long b);
SG_EXTERN SgObject Sg_BignumSub(SgBignum *a, SgBignum *b);
SG_EXTERN SgObject Sg_BignumSubSI(SgBignum *a, long b);
SG_EXTERN SgObject Sg_BignumMul(SgBignum *a, SgBignum *b);
SG_EXTERN SgObject Sg_BignumMulSI(SgBignum *a, long b);
SG_EXTERN SgObject Sg_BignumDivRem(SgBignum *a, SgBignum *b);
SG_EXTERN SgObject Sg_BignumDivSI(SgBignum *a, long b, long *rem);
SG_EXTERN SgObject Sg_BignumModulo(SgBignum *a, SgBignum *b, int remp);
SG_EXTERN SgObject Sg_BignumModuloSI(SgBignum *a, long b, int remp);
SG_EXTERN SgObject Sg_BignumSqrt(SgBignum *bn);
SG_EXTERN SgObject Sg_BignumAccMultAddUI(SgBignum *acc,
					 unsigned long coef,
					 unsigned long c);

SG_EXTERN SgObject Sg_BignumGcd(SgBignum *bx, SgBignum *by);
SG_EXTERN SgObject Sg_BignumModInverse(SgBignum *bx, SgBignum *bm);
SG_EXTERN SgObject Sg_BignumModExpt(SgBignum *bx, SgBignum *be, SgBignum *bm);
SG_EXTERN SgObject Sg_BignumExpt(SgBignum *b, int n);

SG_CDECL_END

#endif /* SAGITTARIUS_BIGNUM_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End
*/
