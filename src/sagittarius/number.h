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
#include "clos.h"

/* classes */
SG_CLASS_DECL(Sg_NumberClass);
SG_CLASS_DECL(Sg_ComplexClass);
SG_CLASS_DECL(Sg_RealClass);
SG_CLASS_DECL(Sg_RationalClass);
SG_CLASS_DECL(Sg_IntegerClass);

#define SG_CLASS_NUMBER      (&Sg_NumberClass)
#define SG_CLASS_COMPLEX     (&Sg_ComplexClass)
#define SG_CLASS_REAL        (&Sg_RealClass)
#define SG_CLASS_RATIONAL    (&Sg_RationalClass)
#define SG_CLASS_INTEGER     (&Sg_IntegerClass)

SG_CDECL_BEGIN

struct SgBignumRec
{
  SG_HEADER;
  int sign : 2;
  unsigned int size: (SIZEOF_INT*CHAR_BIT-2);
  unsigned long elements[1];
};

#define SG_BIGNUMP(obj) SG_XTYPEP(obj, SG_CLASS_INTEGER)
#define SG_BIGNUM(obj)  ((SgBignum*)(obj))

#define BIGNUM_MAX_DIGITS  ((1UL<<(SIZEOF_INT*CHAR_BIT-2))-1)

#define SG_BIGNUM_SET_SIGN(obj, s)   (SG_BIGNUM(obj)->sign=(s))

#define SG_BIGNUM_SET_COUNT(obj, count) (SG_BIGNUM(obj)->size=(count))

#define SG_BIGNUM_SET_ZERO(obj)			\
  (SG_BIGNUM_SET_SIGN(obj, 0), SG_BIGNUM_SET_COUNT(obj, 0))

#define SG_BIGNUM_GET_SIGN(obj)  (SG_BIGNUM(obj)->sign)
#define SG_BIGNUM_GET_COUNT(obj) (SG_BIGNUM(obj)->size)

struct SgComplexRec
{
  SG_HEADER;
  SgObject imag;
  SgObject real;
};

#define SG_COMPLEXP(obj)  SG_XTYPEP(obj, SG_CLASS_COMPLEX)
#define SG_COMPLEX(obj)   ((SgComplex*)(obj))


struct SgRationalRec
{
  SG_HEADER;
  SgObject numerator;
  SgObject denominator;
};

#define SG_RATIONALP(obj)  SG_XTYPEP(obj, SG_CLASS_RATIONAL)
#define SG_RATIONAL(obj)   ((SgRational*)(obj))

struct SgFlonumRec
{
  SG_HEADER;
  double value;
};

#ifdef USE_IMMEDIATE_FLONUM
typedef union SgIFlonumRec
{
#if SIZEOF_VOIDP == 8
  double    f;
#else
  float     f;
#endif
  uintptr_t i;
} SgIFlonum;
#define SG_FLONUMP(obj)      (SG_IFLONUMP(obj) || SG_XTYPEP(obj, SG_CLASS_REAL))
#define SG_FLONUM(obj)    	/* don't use */
#ifdef __GNUC__
#define SG_FLONUM_VALUE(obj)						\
  (SG_IFLONUMP(obj)							\
   ? ((double)(((SgIFlonum)((uintptr_t)obj&~SG_IFLONUM_MASK)).f))	\
   : ((SgFlonum*)(obj))->value)
#else
#define SG_FLONUM_VALUE(obj) Sg_FlonumValue(obj)
#endif

SG_EXTERN double Sg_FlonumValue(SgObject obj);
#else
#define SG_FLONUMP(obj)      SG_XTYPEP(obj, SG_CLASS_REAL)
#define SG_FLONUM(obj)       ((SgFlonum*)(obj))
#define SG_FLONUM_VALUE(obj) (SG_FLONUM(obj)->value)
#endif	/* USE_IMMEDIATE_FLONUM */

/* number type check */
#define SG_EXACT_INTP(obj) ((SG_INTP(obj)) || (SG_BIGNUMP(obj)))
#define SG_REALP(obj)      ((SG_EXACT_INTP(obj)) || (SG_FLONUMP(obj)) || (SG_RATIONALP(obj)))
#define SG_NUMBERP(obj)    (SG_REALP(obj) || SG_COMPLEXP(obj))

enum ScmClampMode {
  SG_CLAMP_ERROR = 0,       /* throws an error when out-of-range */
  SG_CLAMP_HI = 1,
  SG_CLAMP_LO = 2,
  SG_CLAMP_BOTH = 3,
  SG_CLAMP_NONE = 4         /* do not convert when out-of-range */
};

#ifdef _MSC_VER
#define isinf(x) (!_finite(x) && !_isnan(x))
#define isnan(x) _isnan(x)
#endif

SG_EXTERN SgObject Sg_MakeInteger(long x);
SG_EXTERN SgObject Sg_MakeIntegerU(unsigned long x);
SG_EXTERN SgObject Sg_MakeIntegerFromS64(int64_t x);
SG_EXTERN SgObject Sg_MakeIntegerFromU64(uint64_t x);

#if SIZEOF_LONG >= 8
/* if intptr_t is 64 bit, then there is no reason not to fit (u)int32_t */
#define Sg_MakeIntegerFromS32 SG_MAKE_INT
#define Sg_MakeIntegerFromU32 SG_MAKE_INT
#define Sg_GetIntegerS64Clamp Sg_GetIntegerClamp
#define Sg_GetIntegerU64Clamp Sg_GetUIntegerClamp
#else
#define Sg_MakeIntegerFromS32 Sg_MakeInteger
#define Sg_MakeIntegerFromU32 Sg_MakeIntegerU
SG_EXTERN int64_t  Sg_GetIntegerS64Clamp(SgObject obj, int clamp, int *oor);
SG_EXTERN uint64_t Sg_GetIntegerU64Clamp(SgObject obj, int clamp, int *oor);
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
SG_EXTERN SgObject Sg_Numerator(SgObject x);
SG_EXTERN SgObject Sg_Denominator(SgObject x);
SG_EXTERN SgObject Sg_Rationalize(SgObject x, SgObject e);

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
SG_EXTERN int      Sg_FiniteP(SgObject obj);
SG_EXTERN int      Sg_InfiniteP(SgObject obj);
SG_EXTERN int      Sg_NanP(SgObject obj);
SG_EXTERN int      Sg_RationalP(SgObject obj);
SG_EXTERN int      Sg_RealValuedP(SgObject n);
SG_EXTERN int      Sg_RationalValuedP(SgObject n);
SG_EXTERN int      Sg_IntegerValuedP(SgObject n);

SG_EXTERN SgObject Sg_Inverse(SgObject obj);

SG_EXTERN int      Sg_IntegerLength(SgObject n);
SG_EXTERN SgObject Sg_Ash(SgObject x, int count);
SG_EXTERN SgObject Sg_LogNot(SgObject x);
SG_EXTERN SgObject Sg_LogAnd(SgObject x, SgObject y);
SG_EXTERN SgObject Sg_LogIor(SgObject x, SgObject y);
SG_EXTERN SgObject Sg_LogXor(SgObject x, SgObject y);
SG_EXTERN int      Sg_BitCount(SgObject x);
SG_EXTERN int      Sg_BitSize(SgObject x);
SG_EXTERN int      Sg_FirstBitSet(SgObject x);

SG_EXTERN SgObject Sg_Add(SgObject x, SgObject y);
SG_EXTERN SgObject Sg_Sub(SgObject x, SgObject y);
SG_EXTERN SgObject Sg_Mul(SgObject x, SgObject y);
SG_EXTERN SgObject Sg_Div(SgObject x, SgObject y);
SG_EXTERN SgObject Sg_Quotient(SgObject x, SgObject y, SgObject *remp);
SG_EXTERN SgObject Sg_Modulo(SgObject x, SgObject y, int reminderp);
SG_EXTERN SgObject Sg_Expt(SgObject x, SgObject y);
SG_EXTERN SgObject Sg_Exp(SgObject obj);
SG_EXTERN SgObject Sg_Sin(SgObject obj);
SG_EXTERN SgObject Sg_Cos(SgObject obj);
SG_EXTERN SgObject Sg_Tan(SgObject obj);
SG_EXTERN SgObject Sg_Asin(SgObject obj);
SG_EXTERN SgObject Sg_Acos(SgObject obj);
SG_EXTERN SgObject Sg_Atan(SgObject obj);
SG_EXTERN SgObject Sg_Atan2(SgObject x, SgObject y);
SG_EXTERN SgObject Sg_Abs(SgObject obj);
SG_EXTERN SgObject Sg_Sqrt(SgObject obj);
SG_EXTERN SgObject Sg_ExactIntegerSqrt(SgObject obj);
SG_EXTERN int      Sg_Sign(SgObject obj);
SG_EXTERN SgObject Sg_Gcd(SgObject x, SgObject y);
SG_EXTERN SgObject Sg_Magnitude(SgObject obj);
SG_EXTERN SgObject Sg_Angle(SgObject obj);
SG_EXTERN SgObject Sg_Log(SgObject obj);
SG_EXTERN void     Sg_MinMax(SgObject arg0, SgObject args, SgObject *min, SgObject *max);
SG_EXTERN SgObject Sg_IntegerDiv(SgObject x, SgObject y);
SG_EXTERN SgObject Sg_IntegerDiv0(SgObject x, SgObject y);
SG_EXTERN SgObject Sg_IntegerMod(SgObject x, SgObject y);
SG_EXTERN SgObject Sg_IntegerMod0(SgObject x, SgObject y);

enum SgRoundMode {
  SG_ROUND_FLOOR,
  SG_ROUND_CEIL,
  SG_ROUND_TRUNC,
  SG_ROUND_ROUND
};
SG_EXTERN SgObject Sg_Round(SgObject num, int mode);

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
