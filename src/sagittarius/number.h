/*  -*- C -*- */
/*
 * number.h
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
#ifndef SAGITTARIUS_NUMBER_H_
#define SAGITTARIUS_NUMBER_H_

#include "sagittariusdefs.h"

/*
  header:
  cccc cccc cccc cccc .... NZ-- ---- 0111: c: count, NZ: 01(positive) 11(negative) 00(zero)
 */
struct SgBignumRec
{
  SG_HEADER;
  unsigned long elements[1];
};

#define SG_BIGNUMP(obj) (SG_PTRP(obj) && IS_TYPE(obj, TC_BIGNUM))
#define SG_BIGNUM(obj)  ((SgBignum*)(obj))

#define BIGNUM_SIGN_SHIFT  10
#define BIGNUM_COUNT_SHIFT 16
#define BIGNUM_MAX_DIGITS  ((1UL<<(SIZEOF_INT*CHAR_BIT-2))-1)

#define SG_BIGNUM_SET_SIGN(obj, sign)		\
  SG_SET_HEADER_ATTRIBUTE(obj, SG_MAKEBITS((sign & 0x3), BIGNUM_SIGN_SHIFT))
#define SG_BIGNUM_SET_COUNT(obj, count)					\
  (SG_HDR(obj) = (MAKE_HDR_VALUE(TC_BIGNUM)				\
		  | (SG_MAKEBITS(count, BIGNUM_COUNT_SHIFT))		\
		  | (SG_HDR(obj) & SG_MAKEBITS(3, BIGNUM_SIGN_SHIFT))))

#define SG_BIGNUM_SET_ZERO(obj) SG_SET_HEADER(obj, TC_BIGNUM);

#define SG_BIGNUM_GET_SIGN(obj)   \
  ((((SG_HDR(obj)>>BIGNUM_SIGN_SHIFT)&0x03) == 0) ? 0 : (1 - (int)((SG_HDR(obj)>>BIGNUM_SIGN_SHIFT)&0x03)) | 1)
#define SG_BIGNUM_GET_COUNT(obj) (SG_HDR(obj)>>BIGNUM_COUNT_SHIFT)

struct SgComplexRec
{
  SG_HEADER;
  SgObject imag;
  SgObject real;
};

#define SG_COMPLEXP(obj)  (SG_PTRP(obj) && IS_TYPE(obj, TC_COMPLEX))
#define SG_COMPLEX(obj)   ((SgComplex*)(obj))


struct SgRationalRec
{
  SG_HEADER;
  SgObject numerator;
  SgObject denominator;
};

#define SG_RATIONALP(obj)  (SG_PTRP(obj) && IS_TYPE(obj, TC_RATIONAL))
#define SG_RATIONAL(obj)   ((SgRational*)(obj))

struct SgFlonumRec
{
  SG_HEADER;
  double value;
};

#define SG_FLONUMP(obj)   (SG_PTRP(obj) && IS_TYPE(obj, TC_FLONUM))
#define SG_FLONUM(obj)    ((SgFlonum*)(obj))

/* number type check */
#define SG_REALP(obj)     ((SG_INTP(obj)) || (SG_FLONUMP(obj)) || (SG_BIGNUMP(obj)) || (SG_RATIONALP(obj)))
#define SG_NUMBERP(obj)   (SG_REALP(obj) || SG_COMPLEXP(obj))

enum ScmClampMode {
  SG_CLAMP_ERROR = 0,       /* throws an error when out-of-range */
  SG_CLAMP_HI = 1,
  SG_CLAMP_LO = 2,
  SG_CLAMP_BOTH = 3,
  SG_CLAMP_NONE = 4         /* do not convert when out-of-range */
};

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeInteger(long x);
SG_EXTERN SgObject Sg_MakeIntegerU(unsigned long x);
SG_EXTERN SgObject Sg_MakeIntegerFromS64(int64_t x);
SG_EXTERN SgObject Sg_MakeIntegerFromU64(uint64_t x);

#if SIZEOF_LONG >= 8
/* if intptr_t is 64 bit, then there is no reason not to fit (u)int32_t */
#define Sg_MakeIntegerFromS32 SG_MAKE_INT
#define Sg_MakeIntegerFromU32 SG_MAKE_INT
#else
#define Sg_MakeIntegerFromS32 Sg_MakeInteger
#define Sg_MakeIntegerFromU32 Sg_MakeIntegerU
#endif

SG_EXTERN SgObject Sg_MakeRational(SgObject numerator, SgObject denominator);
SG_EXTERN SgObject Sg_MakeFlonum(double d);
SG_EXTERN SgObject Sg_MakeComplex(SgObject real, SgObject imag);
SG_EXTERN SgObject Sg_MakeComplexPolar(SgObject magnitude, SgObject angle);

SG_EXTERN long          Sg_GetIntegerClamp(SgObject obj, int clamp, int *oor);
SG_EXTERN unsigned long Sg_GetUIntegerClamp(SgObject obj, int clamp, int *oor);
#define Sg_GetInteger(x)  Sg_GetIntegerClamp(x, SG_CLAMP_BOTH, NULL)
#define Sg_GetUInteger(x) Sg_GetUIntegerClamp(x, SG_CLAMP_BOTH, NULL)

SG_EXTERN double   Sg_GetDouble(SgObject obj);
SG_EXTERN SgObject Sg_DecodeFlonum(double d, int *exp, int *sign);
SG_EXTERN SgObject Sg_ReduceRational(SgObject rational);

/* converter */
SG_EXTERN double   Sg_RationalToDouble(SgRational *r);

SG_EXTERN SgObject Sg_StringToNumber(SgString *str, int radix, int strict);
SG_EXTERN SgObject Sg_NumberToString(SgObject num, int radix, int use_upper);
SG_EXTERN int      Sg_ZeroP(SgObject obj);
SG_EXTERN int      Sg_IntegerP(SgObject obj);
SG_EXTERN SgObject Sg_Negate(SgObject obj);
SG_EXTERN int      Sg_NegativeP(SgObject obj);
SG_EXTERN int      Sg_PositiveP(SgObject obj);
SG_EXTERN SgObject Sg_Exact(SgObject obj);
SG_EXTERN SgObject Sg_Inexact(SgObject obj);
SG_EXTERN int      Sg_ExactP(SgObject obj);
SG_EXTERN int      Sg_InexactP(SgObject obj);
SG_EXTERN int      Sg_OddP(SgObject obj);

SG_EXTERN SgObject Sg_Inverse(SgObject obj);

SG_EXTERN SgObject Sg_Ash(SgObject x, int count);
SG_EXTERN SgObject Sg_Add(SgObject x, SgObject y);
SG_EXTERN SgObject Sg_Sub(SgObject x, SgObject y);
SG_EXTERN SgObject Sg_Mul(SgObject x, SgObject y);
SG_EXTERN SgObject Sg_Div(SgObject x, SgObject y);
SG_EXTERN SgObject Sg_Quotient(SgObject x, SgObject y, SgObject *remp);
SG_EXTERN SgObject Sg_Modulo(SgObject x, SgObject y, int reminderp);
SG_EXTERN SgObject Sg_Expt(SgObject x, SgObject y);
SG_EXTERN SgObject Sg_Exp(SgObject obj);
SG_EXTERN SgObject Sg_Abs(SgObject obj);
SG_EXTERN SgObject Sg_Sqrt(SgObject obj);
SG_EXTERN int      Sg_Sign(SgObject obj);
SG_EXTERN SgObject Sg_Gcd(SgObject x, SgObject y);
SG_EXTERN SgObject Sg_Magnitude(SgObject obj);
SG_EXTERN SgObject Sg_Log(SgObject obj);

SG_EXTERN int      Sg_NumEq(SgObject x, SgObject y);
SG_EXTERN int      Sg_NumCmp(SgObject x, SgObject y);
SG_EXTERN int      Sg_NumGt(SgObject x, SgObject y);
SG_EXTERN int      Sg_NumGe(SgObject x, SgObject y);
SG_EXTERN int      Sg_NumLt(SgObject x, SgObject y);
SG_EXTERN int      Sg_NumLe(SgObject x, SgObject y);

SG_CDECL_END

#endif /* SAGITTARIUS_NUMBER_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
