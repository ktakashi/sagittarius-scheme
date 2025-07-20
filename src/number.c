/* number.c                                        -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2021  Takashi Kato <ktakashi@ymail.com>
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
#include <string.h>
#include <float.h>
#include <math.h>
#include <ctype.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/private/number.h"
#include "sagittarius/private/bignum.h"
#include "sagittarius/private/string.h"
#include "sagittarius/private/symbol.h"
#include "sagittarius/private/pair.h"
#include "sagittarius/private/port.h"
#include "sagittarius/private/core.h"
#include "sagittarius/private/exceptions.h"
#include "sagittarius/private/clos.h"
#include "sagittarius/private/unicode.h"
#include "sagittarius/private/error.h"
#include "sagittarius/private/numconst.h"
#include "sagittarius/private/arith.h"
#include "sagittarius/private/bits.h"
#include "sagittarius/private/values.h"
#include "sagittarius/private/vm.h"
#include "sagittarius/private/writer.h"
#include "sagittarius/private/library.h"

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

/* Solaris doesn't have isinf */
#if defined(__SVR4) && defined(__sun)
# ifndef isinf
#  include <ieeefp.h>
#  define isinf(x) (!finite((x)) && (x) == (x))
# endif
#endif

#include "number.inc"

/* wrong type of argument */
#define wte4(who, t, g, irr)						\
  Sg_WrongTypeOfArgumentViolation(who, SG_MAKE_STRING(t), g, irr)
#define wte(who, t, g)	wte4(who, t, g, g)

struct numread_packet {
    const SgChar *buffer;       /* original buffer */
    int radix;                  /* radix */
    int exactness;              /* exactness; see enum below */
    int padread;                /* '#' padding has been read */
    int strict;                 /* when true, reports an error if the
                                   input violates implementation limitation;
                                   otherwise, the routine returns #f. */
};

enum { /* used in the exactness flag */
    NOEXACT, EXACT, INEXACT
};

#define IS_EXACT(ctx) ((ctx)->exactness == EXACT)
#define IS_INEXACT(ctx) ((ctx)->exactness == INEXACT)

#define RADIX_MIN        2
#define RADIX_MAX        36
#define P_EXP10          22  /* (floor (* 53 (log 2 5))) */
#define MAX_EXPONENT     325
#define MAX_EXACT_10_EXP (P_EXP10 + 1)

static const int64_t iexpt_2n52 = 0x10000000000000LL; // 2^(53-1)
static const int64_t iexpt_2n53 = 0x20000000000000LL; // 2^53

#include "roundeven.inc"

/* classes */
static SgClass *numeric_cpl[] = {
  /* SG_CLASS_NUMBER, */
  /* SG_CLASS_COMPLEX, */
  /* SG_CLASS_REAL, */
  /* SG_CLASS_RATIONAL, */
  /* SG_CLASS_INTEGER, */
  SG_CLASS_RATIONAL,
  SG_CLASS_REAL,
  SG_CLASS_COMPLEX,
  SG_CLASS_NUMBER,
  SG_CLASS_TOP,
  NULL
};

static void number_print(SgObject obj, SgPort *port, SgWriteContext *ctx);

SG_DEFINE_BUILTIN_CLASS(Sg_NumberClass, number_print, NULL, NULL, NULL,
			numeric_cpl+4);
SG_DEFINE_BUILTIN_CLASS(Sg_ComplexClass, number_print, NULL, NULL, NULL,
			numeric_cpl+3);
SG_DEFINE_BUILTIN_CLASS(Sg_RealClass, number_print, NULL, NULL, NULL,
			numeric_cpl+2);
SG_DEFINE_BUILTIN_CLASS(Sg_RationalClass, number_print, NULL, NULL, NULL,
			numeric_cpl+1);
SG_DEFINE_BUILTIN_CLASS(Sg_IntegerClass, number_print, NULL, NULL, NULL,
			numeric_cpl);

static inline unsigned long ipow(int r, long n)
{
  unsigned long k;
  for (k = 1; n > 0; n--) k *= r;
  return k;
}

static double pow10n(double x, long n)
{
  static double dpow10[] = { 1.0, 1.0e1, 1.0e2, 1.0e3, 1.0e4,
			     1.0e5, 1.0e6, 1.0e7, 1.0e8, 1.0e9,
			     1.0e10, 1.0e11, 1.0e12, 1.0e13, 1.0e14,
			     1.0e15, 1.0e16, 1.0e17, 1.0e18, 1.0e19,
			     1.0e20, 1.0e21, 1.0e22, 1.0e23 };
  if (n >= 0) {
    while (n > 23) {
      x *= 1.0e24;
      n -= 24;
    }
    return x*dpow10[n];
  } else {
    while (n < -23) {
      x /= 1.0e24;
      n += 24;
    }
    return x/dpow10[-n];
  }

}

static SgObject int64_to_integer(int64_t value)
{
  if ((value <= SG_INT_MAX) && (value >= SG_INT_MIN)) return SG_MAKE_INT(value);
  return Sg_MakeBignumFromS64(value);
}

static SgObject oprtr_norm_integer(SgObject obj)
{
  ASSERT(SG_INTP(obj) || SG_BIGNUMP(obj));
  if (SG_BIGNUMP(obj)) return Sg_BignumToInteger(SG_BIGNUM(obj));
  return obj;
}

static SgObject oprtr_norm_complex(SgObject real, SgObject imag)
{
  ASSERT(!SG_COMPLEXP(real));
  ASSERT(!SG_COMPLEXP(imag));
  if (SG_INTP(imag) && SG_EQ(imag, SG_MAKE_INT(0))) return real;
  if (SG_BIGNUMP(imag) && SG_BIGNUM_GET_SIGN(SG_BIGNUM(imag)) == 0) return real;
  if (SG_FLONUMP(real) || SG_FLONUMP(imag)) {
    return Sg_MakeComplex(Sg_Inexact(real), Sg_Inexact(imag));
  }
  return Sg_MakeComplex(real, imag);
}

static SgObject oprtr_expt(SgObject lhs, long n)
{
  SgObject r;
  if (n == 0) return SG_MAKE_INT(1);
  if (n == 1) return lhs;
  if (n < 0) return Sg_Inverse(oprtr_expt(lhs, -n));
  if (!SG_COMPLEXP(lhs) && Sg_NegativeP(lhs)) {
    SgObject ans = oprtr_expt(Sg_Negate(lhs), n);
    if (n & 1) return Sg_Negate(ans);
    return ans;
  }
  if (lhs == SG_MAKE_INT(0)) return lhs;
  if (lhs == SG_MAKE_INT(1)) return lhs;
  if (lhs == SG_MAKE_INT(2)) {
    if (n + 1 <= SG_INT_SIZE) return SG_MAKE_INT(1UL << n);
    return Sg_Ash(SG_MAKE_INT(1), n);
  }
  
  if (SG_RATIONALP(lhs)) {
    return Sg_MakeRational(oprtr_expt(SG_RATIONAL(lhs)->numerator, n),
			   oprtr_expt(SG_RATIONAL(lhs)->denominator, n));
  }
  /* bignum */
  if (SG_BIGNUMP(lhs)) {
    return Sg_BignumExpt(SG_BIGNUM(lhs), n);
  }
  r = SG_MAKE_INT(1);
  while (TRUE) {
    if (n & 1) {
      if (r == SG_MAKE_INT(1)) r = lhs;
      else r = Sg_Mul(r, lhs);
      if (n == 1) return r;
    }
    lhs = Sg_Mul(lhs, lhs);
    n >>= 1;
  }
}

static int64_t decode_double(double n, int *exp, int *sign)
{
  union { double f64; uint64_t u64; } datum;
  uint64_t bits;
  uint64_t mant_bits;
  uint32_t sign_bits;
  uint32_t exp_bits;
  datum.f64 = n;
  bits = datum.u64;
  mant_bits = bits & (iexpt_2n52 - 1);
  sign_bits = bits >> 63;
  exp_bits = (bits >> 52) & 0x7ff;

  if (n == 0.0) {
    *exp = 0;
    *sign = sign_bits ? -1 : 1;
    return 0;
  }
  if (isnan(n)) {
    *exp = 972;
    *sign = 1;
    return 0x18000000000000LL; /* (uint64_t)0x180000 << 32; */
  }
  if (isinf(n)) {
    *exp = 972;
    *sign = sign_bits ? -1 : 1;
    return 0x10000000000000LL; /* (uint64_t)0x100000 << 32; */
  }
  ASSERT(exp_bits != 0x7ff);
  *exp = (exp_bits ? (int)exp_bits - 1023 : -1022) - 52;
  *sign = sign_bits ? -1 : 1;
  if (exp_bits) mant_bits |= iexpt_2n52;
  return mant_bits;
}

static SgObject integer_init_n_alloc(int64_t m, int shift_left)
{
  ASSERT(m >= 0);
  if (m == 0) return SG_MAKE_INT(0);
  else {
    SgObject b = Sg_MakeBignumFromS64(m);
    return Sg_BignumShiftLeft(SG_BIGNUM(b), shift_left);
  }
}

static double nextfloat(double z)
{
  int k, sign;
  int64_t m = decode_double(z, &k, &sign);
  ASSERT(sign >= 0);
  if (m == iexpt_2n53 - 1) return ldexp((double)iexpt_2n52, k + 1);
  return ldexp((double)(m + 1), k);
}

static double prevfloat(double z)
{
  int k, sign;
  int64_t m = decode_double(z, &k, &sign);
  ASSERT(sign >= 0);
  if (k > -1074 && m == iexpt_2n52) return ldexp((double)(iexpt_2n53 - 1), k - 1);
  return ldexp((double)(m - 1), k);
}

/* 
//  Reference:
//  William D. Clinger.
//  How to read floating point numbers accurately
//  Proceedings of the ACM SIGPLAN 1990 conference on Programming language design and implementation, p.92-101, June 1990
*/
static double algorithmR(SgObject f, const long e, const double z0)
{
  double z = z0;
  SgObject x0, pow10e;
  if (e >= 0) {
    x0 = Sg_Mul(f, Sg_Expt(SG_MAKE_INT(10), SG_MAKE_INT(e)));
    pow10e = SG_UNDEF;
  } else {
    x0 = SG_UNDEF;
    pow10e = Sg_Expt(SG_MAKE_INT(10), SG_MAKE_INT(-e));
  }
  while (1) {
    int k, sign, test;
    int64_t m;
    SgObject x, y, D, D2;
    int negP;
    if (isinf(z)) return z;
    m = decode_double(z, &k, &sign);
    ASSERT(sign >= 0);
    if (e >= 0) {
      if (k >= 0) {
	x = x0;
	y = integer_init_n_alloc(m, k);
      } else {
	x = Sg_Ash(x0, -k);
	y = int64_to_integer(m);
      }
    } else {
      if (k >= 0) {
	x = f;
	y = Sg_Mul(integer_init_n_alloc(m, k), pow10e);
      } else {
	x = Sg_Ash(f, -k);
	y = Sg_Mul(int64_to_integer(m), pow10e);
      }
    }
    D = Sg_Sub(x, y);
    D2 = Sg_Mul(int64_to_integer(m + m), D);
    negP = Sg_NegativeP(D);
    if (negP) {
      if (SG_BIGNUMP(D2)) SG_BIGNUM_SET_SIGN(SG_BIGNUM(D2), -SG_BIGNUM_GET_SIGN(D2));
      else D2 = SG_MAKE_INT(-SG_INT_VALUE(D2));
    }
    test = Sg_NumCmp(D2, y);
    if (test < 0) {
      if (negP && 
	  m == iexpt_2n52 &&
	  k > -1074 &&
	  Sg_NumGt(Sg_Ash(D2, 1), y)) {
	z = prevfloat(z);
	continue;
      }
      return z;
    }
    if (test == 0) {
      if ((m & 1) == 0) {
	if (negP && m == iexpt_2n52) {
	  z = prevfloat(z);
	  continue;
	}
	return z;
      }
      return negP ? prevfloat(z) : nextfloat(z);
    }
    z = negP ? prevfloat(z) : nextfloat(z);;
  }
}

static SgObject number_read_error(const SgChar *msg, 
				  struct numread_packet *context)
{
  if (context->strict) {
    SgObject irr = Sg_HeapString(context->buffer);
    SgObject m   = Sg_Sprintf(UC("%s: %A"), msg, irr);
    /* FIXME wasting memory. */
    SgObject err = Sg_SimpleConditions(Sg_MakeReaderCondition(m));
    err = Sg_Cons(Sg_MakeIrritantsCondition(irr), err);
    err = Sg_Cons(Sg_MakeWhoCondition(SG_INTERN("read")), err);
    err = Sg_Cons(Sg_MakeImplementationRestrictionViolation(), err);
    Sg_Raise(Sg_Condition(err), FALSE);
  }
  return SG_FALSE;
}


static long longdigs[RADIX_MAX - RADIX_MIN + 1] = {0};
static unsigned long longlimit[RADIX_MAX - RADIX_MIN + 1] = {0};
static unsigned long bigdig[RADIX_MAX - RADIX_MIN + 1] = {0};

static SgObject read_uint(const SgChar **strp, long *lenp,
			  struct numread_packet *ctx,
			  SgObject initval)
{
  const SgChar *str = *strp;
  int digread = FALSE;
  long len = *lenp;
  int radix = ctx->radix;
  long digits = 0, diglimit = longdigs[radix - RADIX_MIN];
  unsigned long limit = longlimit[radix - RADIX_MIN],
                bdig = bigdig[radix - RADIX_MIN];
  unsigned long value_int = 0;
  SgBignum *value_big = NULL;
  SgChar c;
  static const char tab[] = "0123456789abcdefghijklmnopqrstuvwxyz";
  const char *ptab;

  if (!SG_FALSEP(initval)) {
    if (SG_INTP(initval)) {
      if ((unsigned long)SG_INT_VALUE(initval) > limit) {
	value_big = Sg_MakeBignumWithSize(4, SG_INT_VALUE(initval));
      } else {
	value_int = SG_INT_VALUE(initval);
      }
    } else if (SG_BIGNUMP(initval)) {
      value_big = SG_BIGNUM(Sg_BignumCopy(SG_BIGNUM(initval)));
    }
    digread = TRUE;
  } else if (*str == '0') {
    /* Ignore leading 0's, to avoid unnecessary bignum operations. */
    while (len > 0 && *str == '0') { str++; len--; }
    digread = TRUE;
  }

  while (len--) {
    int digval = -1;
    c = tolower(*str++);
    if (ctx->padread) {
      if (c == '#') digval = 0;
      else break;
    } else if (digread && c == '#') {
      digval = 0;
      ctx->padread = TRUE;
      if (ctx->exactness == NOEXACT) {
	ctx->exactness = INEXACT;
      }
    } else {
      for (ptab = tab; ptab < tab + radix; ptab++) {
	if (c == *ptab) {
	  digval = (int)(ptab - tab);
	  digread = TRUE;
	  break;
	}
      }
    }

    if (digval < 0) break;
    value_int = value_int * radix + digval;
    digits++;
    if (value_big == NULL) {
      if (value_int >= limit) {
	value_big = Sg_MakeBignumWithSize(4, value_int);
	value_int = digits = 0;
      }
    } else if (digits > diglimit) {
      value_big = Sg_BignumAccMultAddUI(value_big, bdig, value_int);
      value_int = digits = 0;
    }
  }
  *strp = str - 1;
  *lenp = len + 1;
  if (value_big == NULL) return Sg_MakeInteger(value_int);
  if (digits > 0) {
    value_big = Sg_BignumAccMultAddUI(value_big, 
				      ipow(radix, digits),
				      value_int);
  }
  return Sg_NormalizeBignum(value_big);
}

static SgObject read_real(const SgChar **strp, long *lenp,
			  struct numread_packet *ctx)
{
  int minusp = FALSE, exp_minusp = FALSE, exp_overflow = FALSE;
  int sign_seen = FALSE, has_fraction = FALSE, has_exponent = FALSE;
  long fracdigs = 0;
  long exponent = 0;
  SgObject intpart, fraction;
  const SgChar *mark;

  switch (**strp) {
  case '-': minusp = TRUE;
  case '+':
    (*strp)++; (*lenp)--; sign_seen = TRUE;
  }
  if ((*lenp) <= 0) return SG_FALSE;
  mark = *strp;
  /* recognize specials */
  if (sign_seen && (*lenp) >= 5) {
    if (ustrncmp(*strp, "inf.0", 5) == 0) {
      (*strp) += 5; (*lenp) -=5;
      return minusp ? SG_NEGATIVE_INFINITY : SG_POSITIVE_INFINITY;
    }
    if (ustrncmp(*strp, "nan.0", 5) == 0) {
      (*strp) += 5; (*lenp) -=5;
      return SG_NAN;
    }
  }
  /* read integral part */
  if (**strp != '.') {
    intpart = read_uint(strp, lenp, ctx, SG_FALSE);
    if ((*lenp) <= 0) {
      if (minusp) intpart = Sg_Negate(intpart);
      if (IS_INEXACT(ctx)) {
	return Sg_Inexact(intpart);
      } else {
	return intpart;
      }
    }
    if (**strp == '/') {
      /* possibly rational */
      SgObject denom;
      long lensave;
      if ((*lenp) <= 1 || mark == *strp) return SG_FALSE;
      (*strp)++; (*lenp)--;
      lensave = *lenp;
      denom = read_uint(strp, lenp, ctx, SG_FALSE);
      if (SG_FALSEP(denom)) return SG_FALSE;
      if (SG_MAKE_INT(0) == denom) {
	if (lensave > *lenp) {
	  if (!IS_INEXACT(ctx)) {
	    return number_read_error(UC("exact infinity/nan is not supported."),
				     ctx);
	  }
	  if (!SG_VM_IS_SET_FLAG(Sg_VM(), SG_R6RS_MODE)) {
	    if (SG_MAKE_INT(0) == intpart) return SG_NAN;
	    return minusp ? SG_NEGATIVE_INFINITY : SG_POSITIVE_INFINITY;
	  } else {
	    return SG_FALSE;
	  }
	} else {
	  return SG_FALSE;
	}
      }
      if (minusp) intpart = Sg_Negate(intpart);
      if (IS_INEXACT(ctx)) {return Sg_Inexact(Sg_Div(intpart, denom));
      } else {
	return Sg_MakeRational(intpart, denom);
      }
    }
  } else {
    intpart = SG_FALSE;		/* indicate there was no intpart */
  }

  /* read fractional part */
  if (**strp == '.') {
    long lensave;
    if (ctx->radix != 10) {
      return number_read_error(UC("only 10-based fraction is supported"), ctx);
    }
    (*strp)++; (*lenp)--;
    lensave = *lenp;
    fraction = read_uint(strp, lenp, ctx, intpart);
    fracdigs = lensave - *lenp;
    has_fraction = TRUE;
  } else {
    fraction = intpart;
  }

  if (SG_FALSEP(intpart)) {
    if (fracdigs == 0) return SG_FALSE; /* input was '.' */
  }
  if (mark == *strp) return SG_FALSE;
  /* read exponent */
  if (*lenp > 0 && strchr("eEsSfFdDlL", (int)**strp)) {
    (*strp)++;
    if (**strp == '|') return SG_FALSE; /* `10.2e|43 */
    if (--(*lenp) <= 0) return SG_FALSE;
    switch (**strp) {
    case '-': exp_minusp = TRUE;
    case '+':
      (*strp)++;
      if (--(*lenp) <= 0) return SG_FALSE;
    }
    while (*lenp > 0) {
      SgChar c = **strp;
      if (!isdigit(c)) break;
      (*strp)++, (*lenp)--;
      if (isdigit(c) && !exp_overflow) {
	exponent = exponent * 10 + (c - '0');
	if (exponent >= MAX_EXPONENT) exp_overflow = TRUE;
      }
    }
    if (exp_minusp) exponent = -exponent;
    has_exponent = TRUE;
  }
  /* parse precision */
  if (**strp == '|') {
    SgObject pre;
    (*strp)++, (*lenp)--;
    /* just ignore */
    pre = read_uint(strp, lenp, ctx, SG_FALSE);
    if (!SG_FALSEP(pre)) ctx->exactness = INEXACT;
  }

  if (exp_overflow && IS_INEXACT(ctx)) {
    if (exp_minusp) {
      return Sg_MakeFlonum(0.0);
    } else {
      return minusp ? SG_NEGATIVE_INFINITY : SG_POSITIVE_INFINITY;
    }
  }
  if (IS_EXACT(ctx)) {
    /* explicit exact number. */
    SgObject n = Sg_MakeInteger(exponent - fracdigs);
    SgObject exp = Sg_Expt(SG_MAKE_INT(10), n);
    SgObject e = Sg_Mul(fraction, exp);
    if (minusp) return Sg_Negate(e);
    else        return e;
  } else if (ctx->exactness == NOEXACT &&
	     !has_fraction && !has_exponent) {
    return (minusp) ? Sg_Negate(intpart) : intpart;
  } else {
    double realnum = Sg_GetDouble(fraction);
    realnum = pow10n(realnum, exponent - fracdigs);
    if (isinf(realnum)) {
      return minusp ? SG_NEGATIVE_INFINITY : SG_POSITIVE_INFINITY;
    }
    if (realnum > 0.0
	&& (Sg_NumCmp(fraction, SG_2_52) > 0
	    || exponent - fracdigs > MAX_EXACT_10_EXP
	    || exponent - fracdigs < -MAX_EXACT_10_EXP)) {
      realnum = algorithmR(fraction, exponent - fracdigs, realnum);
    }
    if (minusp) realnum = -realnum;
    return Sg_MakeFlonum(realnum);
  }
}
/*
 * Number Parser
 *
 *  <number> : <prefix> <complex>
 *  <prefix> : <radix> <exactness> | <exactness> <radix>
 *  <radix>  : <empty> | '#b' | '#o' | '#d' | '#x'
 *  <exactness> : <empty> | '#e' | '#i'
 *  <complex> : <real>
 *            | <real> '@' <real>
 *            | <real> '+' <ureal> 'i'
 *            | <real> '-' <ureal> 'i'
 *            | <real> '+' 'i'
 *            | <real> '-' 'i'
 *            | '+' <ureal> 'i'
 *            | '-' <ureal> 'i'
 *            | '+' 'i'
 *            | '-' 'i'
 *  <real>   : <sign> <ureal>
 *  <sign>   : <empty> | '+' | '-'
 *  <ureal>  : <uinteger>
 *           | <uinteger> '/' <uinteger>
 *           | <decimal>
 *  <uinteger> : <digit>+ '#'*
 *  <decimal> : <digit10>+ '#'* <suffix>
 *            | '.' <digit10>+ '#'* <suffix>
 *            | <digit10>+ '.' <digit10>+ '#'* <suffix>
 *            | <digit10>+ '#'+ '.' '#'* <suffix>
 *  <suffix>  : <empty> | <exponent-marker> <sign> <digit10>+
 *  <exponent-marker> : 'e' | 's' | 'f' | 'd' | 'l'
 *
 * The parser reads characters from on-memory buffer.
 * Multibyte strings are filtered out in the early stage of
 * parsing, so the subroutines assume the buffer contains
 * only ASCII chars.
 */
/* entry point */
static SgObject read_number(const SgChar *str, long len, int radix, int strict)
{
  struct numread_packet ctx;
  int radix_seen = 0, exactness_seen = 0, sign_seen = 0;
  SgObject realpart;

  ctx.buffer = str;
  ctx.exactness = NOEXACT;
  ctx.padread = FALSE;
  ctx.strict = strict;

  if (radix <= 1 || radix > 36) return SG_FALSE;
  ctx.radix = radix;
  /* read radix and prefix*/
  for (; len >= 0; len -= 2) {
    if (*str != '#') break;
    str++;
    switch (*str++) {
    case 'x':; case 'X':;
      if (radix_seen) return SG_FALSE;
      ctx.radix = 16; radix_seen++;
      continue;
    case 'o':; case 'O':;
      if (radix_seen) return SG_FALSE;
      ctx.radix = 8; radix_seen++;
      continue;
    case 'b':; case 'B':;
      if (radix_seen) return SG_FALSE;
      ctx.radix = 2; radix_seen++;
      continue;
    case 'd':; case 'D':;
      if (radix_seen) return SG_FALSE;
      ctx.radix = 10; radix_seen++;
      continue;
    case 'e':; case 'E':;
      if (exactness_seen) return SG_FALSE;
      ctx.exactness = EXACT; exactness_seen++;
      continue;
    case 'i':; case 'I':;
      if (exactness_seen) return SG_FALSE;
      ctx.exactness = INEXACT; exactness_seen++;
      continue;
    }
    return SG_FALSE;
  }
  if (len <= 0) return SG_FALSE;

  if (*str == '+' || *str == '-') {
    if (len == 1) return SG_FALSE;
    if (len == 2 && (str[1] == 'i' || str[1] == 'I')) {
      if (IS_INEXACT(&ctx)) {
	return Sg_MakeComplex(Sg_MakeFlonum(0.0),
			      Sg_MakeFlonum((*str == '+') ? 1.0 : -1.0));
      } else {
	return Sg_MakeComplex(SG_MAKE_INT(0),
			      SG_MAKE_INT((*str == '+') ? 1 : -1));
      }
    }
    sign_seen = TRUE;
  }

  realpart = read_real(&str, &len, &ctx);
  if (SG_FALSEP(realpart) || len == 0) return realpart;

  switch (*str) {
  case '@':
    /* polar representation of complex */
    if (len <= 1) {
      return SG_FALSE;
    } else {
      SgObject angle;
      str++; len--;
      angle = read_real(&str, &len, &ctx);
      if (SG_FALSEP(angle) || len != 0) return SG_FALSE;
      if (IS_EXACT(&ctx)) {
	/* follow the decision of Chez, it does make sense. 
	   c.f. #e1@1 can't be represent as exact number but
	        it should not return inexact number since the
		prefix specifies exact number. so just raise
		an exception.
	 */
	return number_read_error(UC("cannot represent given number"), &ctx);
      } else {
	return Sg_MakeComplexPolar(realpart, angle);
      }
    }
  case '+':
  case '-':
    /* rectangular representation of complex */
    if (len <= 1) {
      return SG_FALSE;
    } else if (len == 2 && (str[1] == 'i' || str[1] == 'I')) {
      if (IS_INEXACT(&ctx)) {
	return Sg_MakeComplex(realpart,
			      Sg_MakeFlonum((*str == '+') ? 1.0 : -1.0));
      } else {
	return Sg_MakeComplex(realpart,
			      SG_MAKE_INT((*str == '+') ? 1 : -1));
      }
    } else {
      SgObject imagpart;
      ctx.exactness = NOEXACT;
      imagpart = read_real(&str, &len, &ctx);
      if (SG_FALSEP(imagpart) || len != 1 || (*str != 'i' && *str != 'I')) {
	return SG_FALSE;
      }
      /* if (Sg_Sign(imagpart) == 0) return realpart; */
      return Sg_MakeComplex(realpart, imagpart);
    }
  case 'i':
  case 'I':
    /* '+' <ureal> 'i' or '-' <ureal> 'i' */
    if (!sign_seen || len != 1) return SG_FALSE;
    if (Sg_Sign(realpart) == 0) return realpart;
    else {
      if (IS_INEXACT(&ctx)) return Sg_MakeComplex(Sg_MakeFlonum(0.0), realpart);
      else return Sg_MakeComplex(SG_MAKE_INT(0), realpart);
    }
  default:
    return SG_FALSE;
  }
}

SgObject Sg_MakeInteger(long x)
{
  if (x >= SG_INT_MIN && x <= SG_INT_MAX) {
    return SG_MAKE_INT(x);
  } else {
    return Sg_MakeBignumFromSI(x);
  }
}

SgObject Sg_MakeIntegerU(unsigned long x)
{
  if (x <= (unsigned long)SG_INT_MAX) {
    return SG_MAKE_INT(x);
  } else {
    return Sg_MakeBignumFromUI(x);
  }
}

SgObject Sg_MakeIntegerFromS64(int64_t x)
{
  if ((x <= SG_INT_MAX) && (x >= SG_INT_MIN)) return SG_MAKE_INT(x);
  return Sg_MakeBignumFromS64(x);
}

SgObject Sg_MakeIntegerFromU64(uint64_t x)
{
  if (x <= SG_INT_MAX) return SG_MAKE_INT(x);
  return Sg_MakeBignumFromU64(x);
}

static void range_err(SgObject obj, int clamp, int *oor)
{
  if (clamp == SG_CLAMP_NONE && oor != NULL) {
    *oor = TRUE;
  } else {
    /* TODO should we make invalid argument error? */
    Sg_Error(UC("argument out of range: %S"), obj);
  }
}

long Sg_GetIntegerClamp(SgObject obj, int clamp, int *oor)
{
  double v = 0.0;
  if (clamp == SG_CLAMP_NONE && oor != NULL) *oor = FALSE;
  if (SG_INTP(obj)) return SG_INT_VALUE(obj);
  else if (SG_BIGNUMP(obj)) {
    return Sg_BignumToSI(SG_BIGNUM(obj), clamp, oor);
  }
  else if (SG_FLONUMP(obj)) {
    v = SG_FLONUM_VALUE(obj);
    goto flonum;
  }
  else if (SG_RATIONALP(obj)) {
    v = Sg_GetDouble(obj);
    goto flonum;
  }
  else {
    goto err;
  }
 flonum:
  if (v > (double)LONG_MAX) {
    if (clamp & SG_CLAMP_HI) return LONG_MAX;
    else goto err;
  }
  if (v < (double)LONG_MIN) {
    if (clamp & SG_CLAMP_LO) return LONG_MIN;
    else goto err;
  }
  return (long)v;
 err:
  range_err(obj, clamp, oor);
  return 0;
}

unsigned long Sg_GetUIntegerClamp(SgObject obj, int clamp, int *oor)
{
  double v = 0.0;
  if (clamp == SG_CLAMP_NONE && oor != NULL) *oor = FALSE;
  if (SG_INTP(obj)) {
    if (SG_INT_VALUE(obj) < 0) {
      if (clamp & SG_CLAMP_LO) return 0;
      else goto err;
    }
    return SG_INT_VALUE(obj);
  }
  else if (SG_BIGNUMP(obj)) {
    return Sg_BignumToUI(SG_BIGNUM(obj), clamp, oor);
  }
  else if (SG_FLONUMP(obj)) {
    v = SG_FLONUM_VALUE(obj);
    goto flonum;
  }
  else if (SG_RATIONALP(obj)) {
    v = Sg_GetDouble(obj);
    goto flonum;
  }
  else {
    goto err;
  }
 flonum:
  if (v > (double)ULONG_MAX) {
    if (clamp & SG_CLAMP_HI) return ULONG_MAX;
    else goto err;
  }
  if (v < 0.0) {
    if (clamp & SG_CLAMP_LO) return 0;
    else goto err;
  }
  return (unsigned long)v;
 err:
  range_err(obj, clamp, oor);
  return 0;
}

#if SIZEOF_LONG < 8
#define SG_SET_INT64_MAX(v64)  \
    ((v64) = ((((int64_t)LONG_MAX)<<32) + (int64_t)ULONG_MAX))
#define SG_SET_INT64_MIN(v64)  \
    ((v64) = (((int64_t)LONG_MAX + 1) << 32))
#define SG_SET_UINT64_MAX(v64) \
    ((v64) = ((((uint64_t)ULONG_MAX) << 32) + (uint64_t)ULONG_MAX))

int64_t  Sg_GetIntegerS64Clamp(SgObject obj, int clamp, int *oor)
{
  int64_t r = 0;
  if (clamp == SG_CLAMP_NONE && oor != NULL) *oor = FALSE;
  if (SG_INTP(obj)) return (int64_t)SG_INT_VALUE(obj);
  if (SG_BIGNUMP(obj)) {
    return Sg_BignumToS64(SG_BIGNUM(obj), clamp, oor);
  }
  if (SG_RATIONALP(obj)) {
    obj = Sg_Inexact(obj);
    /* fall through */
  }
  if (SG_FLONUMP(obj)) {
    int64_t maxval, minval;
    double v;
    SG_SET_INT64_MAX(maxval);
    SG_SET_INT64_MIN(minval);
    v = SG_FLONUM_VALUE(obj);
    if (v > (double)maxval) {
      if (!(clamp & SG_CLAMP_HI)) goto err;
      return maxval;
    } else if (v < (double)minval) {
      if (!(clamp & SG_CLAMP_LO)) goto err;
      return minval;
    } else {
      return (int64_t)v;
    }
  }
 err:
  range_err(obj, clamp, oor);
  return r;
}

uint64_t Sg_GetIntegerU64Clamp(SgObject obj, int clamp, int *oor)
{
  uint64_t r = 0;
  if (clamp == SG_CLAMP_NONE && oor != NULL) *oor = FALSE;
  if (SG_INTP(obj)) {
    long v = SG_INT_VALUE(obj);
    if (v < 0) {
      if (!(clamp & SG_CLAMP_LO)) goto err;
      return 0;
    } else {
      return (uint64_t)v;
    }
  }
  if (SG_BIGNUMP(obj)) {
    return Sg_BignumToU64(SG_BIGNUM(obj), clamp, oor);
  }
  if (SG_RATIONALP(obj)) {
    obj = Sg_Inexact(obj);
    /* fall through */
  }
  if (SG_FLONUMP(obj)) {
    double v = SG_FLONUM_VALUE(obj);
    uint64_t maxval;
    if (v < 0) {
      if (!(clamp & SG_CLAMP_LO)) goto err;
      return 0;
    }

    SG_SET_UINT64_MAX(maxval);

    if (v > (double)maxval) {
      if (!(clamp & SG_CLAMP_HI)) goto err;
      return maxval;
    } else {
      return (uint64_t)v;
    }
  }
 err:
  range_err(obj, clamp, oor);
  return r;
}
#endif


static SgRational* make_rational(SgObject nume, SgObject deno)
{
  SgRational *z;
  z = SG_NEW(SgRational);
  SG_SET_CLASS(z, SG_CLASS_RATIONAL);
  z->numerator = nume;
  z->denominator = deno;
  return z;
}

SgObject Sg_MakeRational(SgObject numerator, SgObject denominator)
{
  SgRational *z;
  if(!Sg_ExactP(numerator)) {
    Sg_AssertionViolation(
      SG_FALSE, 
      Sg_Sprintf(UC("numerator must be an exact integer, but got %S"), 
		 numerator),
      numerator);
  }
  if(!Sg_ExactP(denominator)) {
    Sg_AssertionViolation(
      SG_FALSE, 
      Sg_Sprintf(UC("denominator must be an exact integer, but got %S"), 
		 denominator),
      denominator);
  }
  if (denominator == SG_MAKE_INT(0)) {
    Sg_AssertionViolation(SG_FALSE, SG_MAKE_STRING("undefined for 0"),
			  SG_LIST2(numerator, denominator));
  }

  if (denominator == SG_MAKE_INT(1)) return numerator;
  if (numerator == SG_MAKE_INT(0)) return SG_MAKE_INT(0);

  z = make_rational(numerator, denominator);
  return Sg_ReduceRational(z);
}

SgObject Sg_ReduceRational(SgObject rational)
{
  SgObject numer, denom;
  SgObject common;
  int negated = FALSE;

  if (SG_INTP(rational) || SG_BIGNUMP(rational)) return rational;
  if (!SG_RATIONALP(rational)) {
    wte(SG_FALSE, "exect rational number", rational);
  }
  numer = SG_RATIONAL(rational)->numerator;
  denom = SG_RATIONAL(rational)->denominator;

  if (Sg_Sign(denom) < 0) {
    numer = Sg_Negate(numer);
    denom = Sg_Negate(denom);
    negated = TRUE;
  }

  if (denom == SG_MAKE_INT(1)) return numer;
  if (denom == SG_MAKE_INT(0)) {
    int s = Sg_Sign(numer);
    if (s > 0) return SG_POSITIVE_INFINITY;
    if (s < 0) return SG_NEGATIVE_INFINITY;
    return SG_NAN;
  }

  common = Sg_Gcd(numer, denom);
  if (SG_MAKE_INT(1) == common) {
    if (negated) {
      return make_rational(numer, denom);
    } else {
      return rational;
    }
  } else {
    numer = Sg_Quotient(numer, common, NULL);
    denom = Sg_Quotient(denom, common, NULL);
    if (SG_EQ(denom, SG_MAKE_INT(1))) {
      return numer;
    } else {
      return make_rational(numer, denom);
    }
  }
}

#define set_nume_deno(r, n, d)			\
  do {						\
    if (SG_RATIONALP(r)) {			\
      (n) = SG_RATIONAL(r)->numerator;		\
      (d) = SG_RATIONAL(r)->denominator;	\
    } else {					\
      (n) = (r);				\
      (d) = SG_MAKE_INT(1);			\
    }						\
  } while (0)


SgObject Sg_RationalAddSub(SgObject x, SgObject y, int subtract)
{
  SgObject nx, dx, ny, dy;
  SgObject gcd, fx, fy, nr, dr;

  set_nume_deno(x, nx, dx);
  set_nume_deno(y, ny, dy);

  if (Sg_NumEq(dx, dy)) {
    dr = dx;
    goto finish;
  }
  
  if (SG_MAKE_INT(1) == dx || SG_MAKE_INT(1) == dy) gcd = SG_MAKE_INT(1);
  else gcd = Sg_Gcd(dx, dy);
  if (Sg_NumEq(dx, gcd)) {
    /* only factor x */
    nx = Sg_Mul(Sg_Quotient(dy, dx, NULL), nx);
    dr = dy;
    goto finish;
  }
  if (Sg_NumEq(dy, gcd)) {
    /* only factor y */
    ny = Sg_Mul(Sg_Quotient(dx, dy, NULL), ny);
    dr = dx;
    goto finish;
  }
  /* general case */
  fx = Sg_Quotient(dx, gcd, NULL);
  fy = Sg_Quotient(dy, gcd, NULL);
  nx = Sg_Mul(nx, fy);
  ny = Sg_Mul(ny, fx);
  dr = Sg_Mul(dx, fy);
 finish:
  nr = (subtract ? Sg_Sub(nx, ny) : Sg_Add(nx, ny));
  return Sg_MakeRational(nr, dr);
}

SgObject Sg_RationalMulDiv(SgObject x, SgObject y, int divide)
{
  SgObject nx, dx, ny, dy;

  set_nume_deno(x, nx, dx);
  set_nume_deno(y, ny, dy);

  if (divide) {
    /* swap */
    SgObject t = ny; ny = dy; dy = t;
  }
  return Sg_MakeRational(Sg_Mul(nx, ny),
			 Sg_Mul(dx, dy));
}

#define Sg_RationalAdd(x, y) Sg_RationalAddSub(x, y, FALSE)
#define Sg_RationalSub(x, y) Sg_RationalAddSub(x, y, TRUE)
#define Sg_RationalMul(x, y) Sg_RationalMulDiv(x, y, FALSE)
#define Sg_RationalDiv(x, y) Sg_RationalMulDiv(x, y, TRUE)


static SgFlonum* make_flonum(double d)
{
  SgFlonum *f;
#ifdef USE_IMMEDIATE_FLONUM
  SgIFlonum ifl;
  /* check if flonum can be immediate */
# if SIZEOF_VOIDP == 8
  ifl.f = d;
  if ((ifl.i & SG_IFLONUM_MASK) == 0) {
    ifl.i |= SG_IFLONUM_TAG;
    return SG_OBJ(ifl.i);
  }
# else
  if (FLT_MIN <= d && d <= FLT_MAX) {
    ifl.f = (float)d;
    /* for MSVC we need to make scope... */
    {
      /* To keep calculation better
	 TODO:
	  this actually does not allow to make most of flonums immediate
	  value except the numbers which have its fraction multiple of 5.
	  So, this makes only gambit benchmarks sumfp and fibfp a bit faster.

	 IEEE single precision
	 s eeeeeeee fffffffffffffffffffffff
	 s = sign
	 e = exponent ( 8 bits)
	 f = fraction (23 bits)
	 
       */
      int e = ifl.i >> 23;
      e &= 0xFF;			/* drop sign bit */
      if ((ifl.i & SG_IFLONUM_MASK) == 0 &&
	  /* To make as less error as possible, we allow
	     only exponent less than 15 or all 1. */
	  (e == 0xFF || e <= 0xE)) {
	ifl.i |= SG_IFLONUM_TAG;
	return SG_OBJ(ifl.i);
      }
    }
  }
# endif	 /* SIZEOF_VOIDP == 8 */
#endif	/* USE_IMMEDIATE_FLONUM */
  f = SG_NEW_ATOMIC(SgFlonum);
  SG_SET_CLASS(f, SG_CLASS_REAL);
  f->value = d;
  return SG_OBJ(f);
}

SgObject Sg_MakeFlonum(double d)
{
  if (d == 0.0) {
    union { double f64; int64_t i64; } datum;
    datum.f64 = d;
    if (datum.i64 < 0) {
      return SG_FL_NEGATIVE_ZERO;
    } else {
      return SG_FL_POSITIVE_ZERO;
    }
  } else if (isnan(d)) return SG_NAN;
  else return make_flonum(d);
}

#ifdef USE_IMMEDIATE_FLONUM
double Sg_FlonumValue(SgObject obj)
{
  /* MSVC does not allow me to cast */
#ifndef __GNUC__
  if (SG_IFLONUMP(obj)) {
    void *p = (void *)((uintptr_t)obj&~SG_IFLONUM_MASK);
    return (double)((SgIFlonum *)&p)->f;
  } else {
    return ((SgFlonum *)(obj))->value;
  }
#else
  return SG_FLONUM_VALUE(obj);
#endif
}
#endif /* USE_IMMEDIATE_FLONUM */

static inline SgObject make_complex(SgObject real, SgObject imag)
{
  SgComplex *c;
  ASSERT(!SG_COMPLEXP(real));
  ASSERT(!SG_COMPLEXP(imag));
  c = SG_NEW(SgComplex);
  SG_SET_CLASS(c, SG_CLASS_COMPLEX);
  c->real = real;
  c->imag = imag;
  return SG_OBJ(c);  
}

SgObject Sg_MakeComplex(SgObject real, SgObject imag)
{
  if (!SG_FLONUMP(imag) && Sg_ZeroP(imag)) return real;
  else if (SG_FLONUMP(real) || SG_FLONUMP(imag)) {
    return make_complex(Sg_Inexact(real), Sg_Inexact(imag));
  }
  else
    return make_complex(real, imag);
}

SgObject Sg_MakeComplexPolar(SgObject magnitude, SgObject angle)
{
  double r, a;
  if (angle == SG_MAKE_INT(0)) return magnitude;
  r = Sg_GetDouble(magnitude);
  a = Sg_GetDouble(angle);
  return Sg_MakeComplex(Sg_MakeFlonum(r * cos(a)), Sg_MakeFlonum(r * sin(a)));
}

double Sg_GetDouble(SgObject obj)
{
  if (SG_FLONUMP(obj)) return SG_FLONUM_VALUE(obj);
  else if (SG_INTP(obj)) return (double)SG_INT_VALUE(obj);
  else if (SG_BIGNUMP(obj)) return Sg_BignumToDouble(SG_BIGNUM(obj));
  else if (SG_RATIONAL(obj)) return Sg_RationalToDouble(obj);
  else if (SG_COMPLEXP(obj)) {
    SgComplex *c = SG_COMPLEX(obj);
    if (Sg_ZeroP(c->imag)) return Sg_GetDouble(c->real);
    else return 0.0;
  }
  else return 0.0; 		/* should this be error? */
}

SgObject Sg_DecodeFlonum(double d, int *exp, int *sign)
{
  SgObject f;
  int exp0, sign0;
  int64_t mant = decode_double(d, &exp0, &sign0);
  f = int64_to_integer(mant);
  *exp = exp0;
  *sign = sign0;
  return f;
}

double Sg_RationalToDouble(SgRational *obj)
{
  const int BITSIZE_TH = 96;
  double nume = Sg_GetDouble(obj->numerator);
  double deno = Sg_GetDouble(obj->denominator);
  if (isinf(nume) || isinf(deno)) {
    if (isinf(nume) && isinf(deno)) {
      long nume_bitsize = Sg_BignumBitSize(obj->numerator);
      long deno_bitsize = Sg_BignumBitSize(obj->denominator);
      long shift = (nume_bitsize > deno_bitsize)
	? nume_bitsize - BITSIZE_TH : deno_bitsize - BITSIZE_TH;
      if (shift < 1) shift = 1;
      nume = Sg_GetDouble(Sg_BignumShiftRight(obj->numerator, shift));
      deno = Sg_GetDouble(Sg_BignumShiftRight(obj->denominator, shift));
    } else if (isinf(deno)) {
      long deno_bitsize = Sg_BignumBitSize(obj->denominator);
      long shift = deno_bitsize - BITSIZE_TH;
      if (shift < 1) shift = 1;
      nume = ldexp(nume, (int)-shift);
      deno = Sg_GetDouble(Sg_BignumShiftRight(obj->denominator, shift));
    } else {
      long nume_bitsize = Sg_BignumBitSize(obj->numerator);
      long shift = nume_bitsize - BITSIZE_TH;
      if (shift < 1) shift = 1;
      nume = Sg_GetDouble(Sg_BignumShiftRight(obj->numerator, shift));
      deno = ldexp(deno, (int)-shift);
    }
  }
  return nume / deno;
}

SgObject Sg_Numerator(SgObject x)
{
  int inexact = FALSE;
  SgObject obj;
  if (!SG_NUMBERP(x)) wte(SG_INTERN("numerator"), "number", x);
  if (SG_FLONUMP(x)) {
    double d = SG_FLONUM_VALUE(x);
    if (d == 0.0) return x;
    if (isinf(d) || isnan(d)) return x;
    inexact = TRUE;
  }
  obj = Sg_Exact(x);
  if (SG_RATIONALP(obj)) {
    if (inexact) return Sg_Inexact(SG_RATIONAL(obj)->numerator);
    return SG_RATIONAL(obj)->numerator;
  }
  return inexact ? Sg_Inexact(obj) : obj;
}

SgObject Sg_Denominator(SgObject x)
{
  int inexact = FALSE;
  SgObject obj;
  if (!SG_NUMBERP(x)) wte(SG_INTERN("denominator"), "number", x);
  if (SG_FLONUMP(x)) {
    double d = SG_FLONUM_VALUE(x);
    if (isinf(d)) return Sg_MakeFlonum(1.0);
    if (isnan(d)) return SG_NAN;
    inexact = TRUE;
  }
  obj = Sg_Exact(x);
  if (SG_RATIONALP(obj)) {
    if (inexact) return Sg_Inexact(SG_RATIONAL(obj)->denominator);
    return SG_RATIONAL(obj)->denominator;
  }
  return inexact ? Sg_MakeFlonum(1.0) : SG_MAKE_INT(1);
}

static inline SgObject rationalize_rec(SgObject bottom, SgObject top)
{
  if (Sg_NumCmp(bottom, top) == 0) return bottom;
  else {
    SgObject x = Sg_Round(bottom, SG_ROUND_CEIL);
    if (Sg_NumCmp(x, top) < 0) return x;
    else {
      SgObject one = SG_MAKE_INT(1);
      SgObject a = Sg_Sub(x, one);
      return Sg_Add(a, Sg_Div(one,
			      rationalize_rec(Sg_Div(one,
						     Sg_Sub(top, a)),
					      Sg_Div(one,
						     Sg_Sub(bottom, a)))));
    }
  }
}

SgObject Sg_Rationalize(SgObject x, SgObject e)
{
  if (Sg_InfiniteP(e)) {
    if (Sg_InfiniteP(x)) return SG_NAN;
    else return Sg_MakeFlonum(0.0);
  } else if (Sg_ZeroP(x)) return x;
  else if (Sg_NumCmp(x, e) == 0) return Sg_Sub(x, e);
  else if (Sg_NegativeP(x)) return Sg_Negate(Sg_Rationalize(Sg_Negate(x), e));
  else {
    SgObject bottom, top;
    e = Sg_Abs(e);
    bottom = Sg_Sub(x, e);
    top = Sg_Add(x, e);
    return rationalize_rec(bottom, top);
  }
}

SgObject Sg_StringToNumber(SgString *str, int radix, int strict)
{
  return read_number(str->value, str->size, radix, strict);
}

int Sg_ZeroP(SgObject obj)
{
  if (SG_INTP(obj)) return obj == SG_MAKE_INT(0);
  if (SG_FLONUMP(obj)) return SG_FLONUM_VALUE(obj) == 0.0;
  if (SG_BIGNUMP(obj)) {
    /* ASSERT(SG_BIGNUM_GET_SIGN(obj) != 0); */
    return SG_BIGNUM_GET_SIGN(obj) == 0;
  }
  if (SG_RATIONALP(obj)) {
    ASSERT(Sg_ZeroP(SG_RATIONAL(obj)->numerator) == FALSE);
    return FALSE;
  }
  if (SG_COMPLEXP(obj)) {
    SgComplex *c = SG_COMPLEX(obj);
    return Sg_ZeroP(c->real) && Sg_ZeroP(c->imag);
  }
  wte(SG_INTERN("zero?"), "number", obj);
  return -1;			/* dummy */
}

int Sg_IntegerP(SgObject obj)
{
  if (SG_INTP(obj) || SG_BIGNUMP(obj)) return TRUE;
  if (SG_RATIONALP(obj)) return FALSE; /* normalized ratnum never be integer */
  if (SG_FLONUMP(obj)) {
    double d = SG_FLONUM_VALUE(obj);
    double f, i;
    if (Sg_InfiniteP(obj)) return FALSE;
    if ((f = modf(d, &i)) == 0.0) return TRUE;
    return FALSE;
  }
  if (SG_COMPLEXP(obj)) return FALSE;
  return FALSE;
}

int Sg_OddP(SgObject obj)
{
  if (SG_INTP(obj)) {
    return (SG_INT_VALUE(obj) & 1);
  }
  if (SG_BIGNUMP(obj)) {
    return (SG_BIGNUM(obj)->elements[0] & 1);
  }
  if (SG_FLONUMP(obj) && Sg_IntegerP(obj)) {
    return (fmod(SG_FLONUM_VALUE(obj), 2.0) != 0.0);
  }
  wte(SG_INTERN("odd?"), "integer", obj);
  return FALSE;			/* dummy */
}

int Sg_FiniteP(SgObject obj)
{
  return !Sg_InfiniteP(obj) && !Sg_NanP(obj);
}

int Sg_InfiniteP(SgObject obj)
{
  if (SG_FLONUMP(obj)) {
    double v = SG_FLONUM_VALUE(obj);
    return isinf(v);
  } else if (SG_COMPLEXP(obj)) {
    SgObject r = SG_COMPLEX(obj)->real;
   SgObject i = SG_COMPLEX(obj)->imag;
    return Sg_InfiniteP(r) || Sg_InfiniteP(i);
  } else if (!SG_NUMBERP(obj)) {
    wte(SG_INTERN("infinite?"), "number", obj);
  }
  return FALSE;
}

int Sg_NanP(SgObject obj)
{
  if (SG_FLONUMP(obj)) {
    double v = SG_FLONUM_VALUE(obj);
    return isnan(v);
  } else if (SG_COMPLEXP(obj)) {
    SgObject r = SG_COMPLEX(obj)->real;
   SgObject i = SG_COMPLEX(obj)->imag;
    return Sg_NanP(r) || Sg_NanP(i);
  } else if (!SG_NUMBERP(obj)) {
    wte(SG_INTERN("nan?"), "number", obj);
  }
  return FALSE;
}

/* TODO: the name is conflicted */
int Sg_RationalP(SgObject n)
{
  if (SG_EXACT_INTP(n) || SG_RATIONALP(n)) return TRUE;
  if (SG_FLONUMP(n)) {
    if (Sg_InfiniteP(n)) return FALSE;
    if (Sg_NanP(n)) return FALSE;
    return TRUE;
  }
  return FALSE;
}

int Sg_RealValuedP(SgObject n)
{
  if (SG_REALP(n)) return TRUE;
  if (SG_COMPLEXP(n)) {
    return Sg_ZeroP(SG_COMPLEX(n)->imag);
  }
  return FALSE;
}

int Sg_RationalValuedP(SgObject n)
{
  if (SG_RATIONALP(n)) return TRUE;
  if (SG_EXACT_INTP(n)) return TRUE;
  if (SG_FLONUMP(n)) {
    if (Sg_InfiniteP(n)) return FALSE;
    if (Sg_NanP(n)) return FALSE;
    return TRUE;
  }
  if (SG_COMPLEXP(n)) {
    return Sg_ZeroP(SG_COMPLEX(n)->imag) &&
      Sg_RationalP(SG_COMPLEX(n)->real);
  }
  return FALSE;
}

int Sg_IntegerValuedP(SgObject n)
{
  if (Sg_IntegerP(n)) return TRUE;
  if (SG_COMPLEXP(n)) {
    return Sg_ZeroP(SG_COMPLEX(n)->imag) &&
      Sg_IntegerValuedP(SG_COMPLEX(n)->real);
  }
  return FALSE;
}

SgObject Sg_Negate(SgObject obj)
{
  if (SG_INTP(obj)) {
    long n = SG_INT_VALUE(obj);
    if (n == SG_INT_MIN) return Sg_MakeInteger(-n);
    return SG_MAKE_INT(-n);
  }
  if (SG_FLONUMP(obj)) {
    return Sg_MakeFlonum(-SG_FLONUM_VALUE(obj));
  }
  if (SG_BIGNUMP(obj)) {
    SgBignum *b = Sg_BignumCopy(obj);
    SG_BIGNUM_SET_SIGN(b, -SG_BIGNUM_GET_SIGN(obj));
    return Sg_NormalizeBignum(b);
  }
  if (SG_RATIONALP(obj)) {
    SgRational *r = SG_RATIONAL(obj);
    return Sg_MakeRational(Sg_Negate(r->numerator), r->denominator);
  }
  if (SG_COMPLEXP(obj)) {
    SgComplex *c = SG_COMPLEX(obj);
    return Sg_MakeComplex(Sg_Negate(c->real), Sg_Negate(c->imag));
  }
  wte(SG_INTERN("negate"), "number", obj);
  return SG_UNDEF;		/* dummy */
}

int Sg_NegativeP(SgObject obj)
{
  if (SG_INTP(obj)) return SG_INT_VALUE(obj) < 0;
  if (SG_BIGNUMP(obj)) return SG_BIGNUM_GET_SIGN(obj) < 0;
  if (SG_FLONUMP(obj)) return SG_FLONUM_VALUE(obj) < 0.0;
  if (SG_RATIONALP(obj)) return Sg_NegativeP(SG_RATIONAL(obj)->numerator);
  if (SG_COMPLEXP(obj)) return Sg_NegativeP(SG_COMPLEX(obj)->real);
  wte(SG_INTERN("negative?"), "number", obj);
  return FALSE;			/* dummy */
}

int Sg_PositiveP(SgObject obj)
{
  if (SG_INTP(obj)) return SG_INT_VALUE(obj) > 0;
  if (SG_BIGNUMP(obj)) return SG_BIGNUM_GET_SIGN(obj) > 0;
  if (SG_FLONUMP(obj)) {
#if __WATCOMC__
    /* on Watcom, +nan.0 is bigger than 0 */
    if (isnan(SG_FLONUM_VALUE(obj))) return FALSE;
    else return SG_FLONUM_VALUE(obj) > 0.0;
#else
    return SG_FLONUM_VALUE(obj) > 0.0;
#endif
  }
  if (SG_RATIONALP(obj)) return Sg_PositiveP(SG_RATIONAL(obj)->numerator);
  if (SG_COMPLEXP(obj)) return Sg_PositiveP(SG_COMPLEX(obj)->real);
  wte(SG_INTERN("positive?"), "number", obj);
  return FALSE;			/* dummy */
}

SgObject Sg_Exact(SgObject obj)
{
  if (SG_FLONUMP(obj)) {
    double d = SG_FLONUM_VALUE(obj);
    double f, i;
    /* inf.0 or nan.0 doesn't have exact value. */
    
    if (isinf(d) || isnan(d)) {
      Sg_AssertionViolation(SG_INTERN("exact"),
	    Sg_Sprintf(UC("no exact representation for %S"), obj),
	    SG_LIST1(obj));
    }
    if ((f = modf(d, &i)) == 0.0) {
      if (d < (double)SG_INT_MIN || d > (double)SG_INT_MAX) {
	obj = Sg_MakeBignumFromDouble(d);
      } else {
	obj = SG_MAKE_INT((long)d);
      }
    } else {
      SgObject m;
      int exp, sign;
      m = Sg_DecodeFlonum(d, &exp, &sign);
      ASSERT(exp < 0); /* exp >= case should be handled above */
      obj = Sg_Div(m, Sg_Ash(SG_MAKE_INT(1), -exp));
      if (sign < 0) obj = Sg_Negate(obj);
    }
    return obj;
  } else if (SG_COMPLEXP(obj)) {
    SgComplex *c = SG_COMPLEX(obj);
    if (SG_FLONUMP(c->real) || SG_FLONUMP(c->imag)) {
      return oprtr_norm_complex(Sg_Exact(c->real), Sg_Exact(c->imag));
    }
    return obj;
  }
  if (SG_INTP(obj) || SG_BIGNUMP(obj) || SG_RATIONALP(obj)) return obj;
  wte(SG_INTERN("exact"), "number", obj);
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_Inexact(SgObject obj)
{
  if (SG_INTP(obj)) {
    double z = (double)SG_INT_VALUE(obj);
    return Sg_MakeFlonum(z);
  } else if (SG_BIGNUMP(obj)) {
    double z = Sg_BignumToDouble(SG_BIGNUM(obj));
    return Sg_MakeFlonum(z);
  } else if (SG_RATIONALP(obj)) {
    double z = Sg_GetDouble(obj);
    return Sg_MakeFlonum(z);
  } else if (SG_FLONUMP(obj)) {
    return obj;
  } else if (SG_COMPLEXP(obj)) {
    SgComplex *c = SG_COMPLEX(obj);
    if (SG_FLONUMP(c->real) & SG_FLONUMP(c->imag)) return obj;
    return Sg_MakeComplex(Sg_Inexact(c->real), Sg_Inexact(c->imag));
  } else {
    wte(SG_INTERN("inexact"), "number", obj);
  }
  return SG_UNDEF; 		/* dummy */
}


int Sg_ExactP(SgObject obj)
{
  if (SG_INTP(obj) || SG_BIGNUMP(obj) || SG_RATIONALP(obj)) return TRUE;
  if (SG_FLONUMP(obj)) return FALSE;
  if (SG_COMPLEXP(obj)) {
    SgComplex *c = SG_COMPLEX(obj);
    return Sg_ExactP(c->real) && Sg_ExactP(c->imag);
  }
  wte(SG_INTERN("exact?"), "number", obj);
  return FALSE;
}

int Sg_InexactP(SgObject obj)
{
  if (SG_INTP(obj) || SG_BIGNUMP(obj) || SG_RATIONALP(obj)) return FALSE;
  if (SG_FLONUMP(obj)) return TRUE;
  if (SG_COMPLEXP(obj)) {
    SgComplex *c = SG_COMPLEX(obj);
    /* TODO is it correct? */
    return Sg_InexactP(c->real) || Sg_InexactP(c->imag);
  }
  wte(SG_INTERN("inexact?"), "number", obj);
  return FALSE;
}

SgObject Sg_Inverse(SgObject obj)
{
  if (SG_INTP(obj)) {
    if (SG_INT_VALUE(obj) == 0) wte(SG_INTERN("inverse"), "non 0 number", obj);
    if (SG_INT_VALUE(obj) > 0) {
      if (SG_INT_VALUE(obj) == 1) return obj;
      return Sg_MakeRational(SG_MAKE_INT(1), obj);
    }
    if (obj == SG_MAKE_INT(-1)) return obj;
    return Sg_MakeRational(SG_MAKE_INT(-1), Sg_Negate(obj));
  }
  if (SG_FLONUMP(obj)) return Sg_MakeFlonum(1.0 / SG_FLONUM_VALUE(obj));
  if (SG_BIGNUMP(obj)) {
    if (SG_BIGNUM_GET_SIGN(obj) == 0) wte(SG_INTERN("inverse"), "non 0 number", obj);
    if (SG_BIGNUM_GET_SIGN(obj) > 0) {
      return Sg_MakeRational(SG_MAKE_INT(1), obj);
    }
    return Sg_MakeRational(SG_MAKE_INT(-1), Sg_Negate(obj));
  }
  if (SG_RATIONALP(obj)) {
    if (!Sg_NegativeP(SG_RATIONAL(obj)->numerator)) {
      if (SG_RATIONAL(obj)->numerator == SG_MAKE_INT(1)) {
	return oprtr_norm_integer(SG_RATIONAL(obj)->denominator);
      }
      return Sg_MakeRational(SG_RATIONAL(obj)->denominator, SG_RATIONAL(obj)->numerator);
    }
    if (SG_RATIONAL(obj)->numerator == SG_MAKE_INT(-1)) {
      return Sg_Negate(SG_RATIONAL(obj)->denominator);
    }
    return Sg_MakeRational(Sg_Negate(SG_RATIONAL(obj)->denominator),
			   Sg_Negate(SG_RATIONAL(obj)->numerator));
  }
  if (SG_COMPLEXP(obj)) return Sg_Div(SG_MAKE_INT(1), obj);
  wte(SG_INTERN("inverse"), "number", obj);
  return SG_UNDEF;		/* dummy */
}

static inline long integer_length_rec(SgObject n)
{
  long n2;
  SgObject n3;
  if (SG_INTP(n)) {
    switch (SG_INT_VALUE(n)) {
    case 0: case -1:
      return 0;
    case 1: case -2:
      return 1;
    case 2: case 3: case -3: case -4:
      return 2;
    case 4: case 5: case 6: case 7:
    case -5: case -6: case -7: case -8:
      return 3;
    }
    /* fall through */
  }
  n2 = integer_length_rec(Sg_Ash(n, -4));
  n3 = Sg_Add(SG_MAKE_INT(4),
	      SG_MAKE_INT(n2));
  ASSERT(SG_INTP(n3));
  return SG_INT_VALUE(n3);
}

long Sg_IntegerLength(SgObject n)
{
  if (!Sg_IntegerP(n)) wte(SG_INTERN("integer-length"), "integer", n);
  return integer_length_rec(n);
}

SgObject Sg_Ash(SgObject x, long count)
{
  if (SG_INTP(x)) {
    long ix = SG_INT_VALUE(x);
    if (x == 0) {
      return SG_MAKE_INT(0);
    } else if (count <= -(SIZEOF_LONG * 8)) {
      ix = (ix < 0) ? -1 : 0;
      return Sg_MakeInteger(ix);
    } else if (count < 0) {
      if (ix < 0) {
	ix = ~((~ix) >> (-count));
      } else {
	ix >>= -count;
      }
      return Sg_MakeInteger(ix);
    } else if (count < SG_INT_SIZE) {
      if (ix < 0) {
	if (-ix < (SG_INT_MAX >> count)) {
	  ix <<= count;
	  return Sg_MakeInteger(ix);
	}
      } else {
	if (ix < (SG_INT_MAX >> count)) {
	  ix <<= count;
	  return Sg_MakeInteger(ix);
	}
      }
    }
    return Sg_BignumAshSI(ix, count);
  } else if (SG_BIGNUMP(x)) {
    return Sg_BignumAsh(SG_BIGNUM(x), count);
  }
  wte(SG_INTERN("bitwise-arithmetic-shift"), "exact integer", x);
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_LogNot(SgObject x)
{
  if (!SG_EXACT_INTP(x)) wte(SG_INTERN("bitwise-not"), "exact integer", x);
  if (SG_INTP(x)) {
    /* this won't cause an overflow */
    return SG_MAKE_INT(~SG_INT_VALUE(x));
  } else {
    return Sg_Negate(Sg_BignumAddSI(SG_BIGNUM(x), 1));
  }
}

SgObject Sg_LogAnd(SgObject x, SgObject y)
{
  if (!SG_EXACT_INTP(x)) wte(SG_INTERN("bitwise-and"), "exact integer", x);
  if (!SG_EXACT_INTP(y)) wte(SG_INTERN("bitwise-and"), "exact integer", y);
  if (SG_INTP(x)) {
    if (SG_INTP(y)) {
      return SG_MAKE_INT(SG_INT_VALUE(x) & SG_INT_VALUE(y));
    } else if (SG_INT_VALUE(x) >= 0) {
      if (SG_BIGNUM_GET_SIGN(y) > 0) {
	return Sg_MakeInteger(SG_INT_VALUE(x) & SG_BIGNUM(y)->elements[0]);
      } else if (SG_BIGNUM_GET_SIGN(y) == 0) {
	return SG_MAKE_INT(0);
      }
    }
    /* x = Sg_MakeBignumFromSI(SG_INT_VALUE(x)); */
    return Sg_BignumLogAndSI(SG_BIGNUM(y), SG_INT_VALUE(x));
  } else if (SG_INTP(y)) {
    if (SG_INT_VALUE(y) >= 0) {
      if (SG_BIGNUM_GET_SIGN(x) > 0) {
	return Sg_MakeInteger(SG_INT_VALUE(y) & SG_BIGNUM(x)->elements[0]);
      } else if (SG_BIGNUM_GET_SIGN(x) == 0) {
	return SG_MAKE_INT(0);
      }
    }
    /* y = Sg_MakeBignumFromSI(SG_INT_VALUE(y)); */
    return Sg_BignumLogAndSI(SG_BIGNUM(x), SG_INT_VALUE(y));
  }
  return Sg_BignumLogAnd(SG_BIGNUM(x), SG_BIGNUM(y));
}

SgObject Sg_LogIor(SgObject x, SgObject y)
{
  if (!SG_EXACT_INTP(x)) wte(SG_INTERN("bitwise-ior"), "exact integer", x);
  if (!SG_EXACT_INTP(y)) wte(SG_INTERN("bitwise-ior"), "exact integer", y);
  if (SG_INTP(x)) {
    if (SG_INTP(y)) {
      return SG_MAKE_INT(SG_INT_VALUE(x) | SG_INT_VALUE(y));
    } else {
      /* x = Sg_MakeBignumFromSI(SG_INT_VALUE(x)); */
      return Sg_BignumLogIorSI(SG_BIGNUM(y), SG_INT_VALUE(x));
    }
  } else {
    if (SG_INTP(y)) {
      /* y = Sg_MakeBignumFromSI(SG_INT_VALUE(y)); */
      return Sg_BignumLogIorSI(SG_BIGNUM(x), SG_INT_VALUE(y));
    }
  }
  return Sg_BignumLogIor(SG_BIGNUM(x), SG_BIGNUM(y));
}

SgObject Sg_LogXor(SgObject x, SgObject y)
{
  if (!SG_EXACT_INTP(x)) wte(SG_INTERN("bitwise-xor"), "exact integer", x);
  if (!SG_EXACT_INTP(y)) wte(SG_INTERN("bitwise-xor"), "exact integer", y);
  if (SG_INTP(x)) {
    if (SG_INTP(y)) {
      return SG_MAKE_INT(SG_INT_VALUE(x) ^ SG_INT_VALUE(y));
    } else {
      /* x = Sg_MakeBignumFromSI(SG_INT_VALUE(x)); */
      return Sg_BignumLogXorSI(y, SG_INT_VALUE(x));
    }
  } else {
    if (SG_INTP(y)) {
      /* y = Sg_MakeBignumFromSI(SG_INT_VALUE(y)); */
      return Sg_BignumLogXorSI(x, SG_INT_VALUE(y));
    }
  }
  return Sg_BignumLogXor(SG_BIGNUM(x), SG_BIGNUM(y));
}

long Sg_BitCount(SgObject x)
{
  if (!SG_EXACT_INTP(x)) wte(SG_INTERN("bitwise-bit-count"), "exact integer", x);
  if (SG_INTP(x)) {
    long n = SG_INT_VALUE(x);
    if (n >= 0) {
      return nbits(n);
    } else {
      return ~nbits(~n);
    }
  } else {
    return Sg_BignumBitCount(SG_BIGNUM(x));
  }
}

long Sg_BitSize(SgObject x)
{
  if (!SG_EXACT_INTP(x)) wte(SG_INTERN("bitwise-length"), "exact integer", x);
  if (SG_INTP(x)) {
    long n = SG_INT_VALUE(x), n2;
    if (n == 0) return 0;
    n2 = (n < 0) ? ~n : n;
    return WORD_BITS - nlz(n2);
  } else {
    if (SG_BIGNUM_GET_SIGN(x) > 0) return Sg_BignumBitSize(SG_BIGNUM(x));
    else return Sg_BitSize(Sg_LogNot(x));
  }  
}

long Sg_FirstBitSet(SgObject x)
{
  if (!SG_EXACT_INTP(x)) {
    wte(SG_INTERN("bitwise-first-bit-set"), "exact integer", x);
  }
  if (SG_INTP(x)) {
    long n = SG_INT_VALUE(x);
    int bit;
    if (n == 0) return -1;
    bit = 0;
    bit += ntz(n);
    return bit;
  } else {
    return Sg_BignumFirstBitSet(SG_BIGNUM(x));
  }
}

int Sg_BitSetP(SgObject x, long n)
  
{
  if (!SG_EXACT_INTP(x)) {
    wte(SG_INTERN("bitwise-bit-set?"), "exact integer", x);
  }
  if (n < 0) {
    wte(SG_INTERN("bitwise-bit-set?"), "non negative integer", SG_MAKE_INT(n));
  }

  if (SG_INTP(x)) {
    long ix = SG_INT_VALUE(x);
    if (n >= SG_INT_SIZE) {
      return (ix < 0);
    }
    return (ix>>n)&1;
  }
  return Sg_BignumBitSetP(SG_BIGNUM(x), n);
}

SgObject Sg_Add(SgObject x, SgObject y)
{
  if (SG_INTP(x)) {
    if (SG_INTP(y)) {
      long r = SG_INT_VALUE(x) + SG_INT_VALUE(y);
      return Sg_MakeInteger(r);
    }
    else if (SG_BIGNUMP(y)) {
      if (x == SG_MAKE_INT(0)) return y;
      else return Sg_BignumAddSI(SG_BIGNUM(y), SG_INT_VALUE(x));
    }
    else if (SG_RATIONALP(y)) {
      if (x == SG_MAKE_INT(0)) return y;
      else return Sg_RationalAdd(SG_RATIONAL(y), x);
    }
    else if (SG_FLONUMP(y)) {
      return Sg_MakeFlonum((double)SG_INT_VALUE(x) + SG_FLONUM_VALUE(y));
    }
    else if (SG_COMPLEXP(y)) {
      if (x == SG_MAKE_INT(0)) return y;
      else return Sg_MakeComplex(Sg_Add(SG_COMPLEX(y)->real, x),
				 SG_COMPLEX(y)->imag);
    }
  }
  else if (SG_FLONUMP(x)) {
    if (SG_INTP(y)) {
      if (y == SG_MAKE_INT(0)) return x;
      else {
	double z = SG_FLONUM_VALUE(x) + (double)SG_INT_VALUE(y);
	return Sg_MakeFlonum(z);
      }
    }
    else if (SG_BIGNUMP(y) || SG_RATIONALP(y)) {
      return Sg_MakeFlonum(SG_FLONUM_VALUE(x) + Sg_GetDouble(y));
    }
    else if (SG_FLONUMP(y)) {
      return Sg_MakeFlonum(SG_FLONUM_VALUE(x) + SG_FLONUM_VALUE(y));
    }
    else if (SG_COMPLEXP(y)) {
      return Sg_MakeComplex(Sg_Add(SG_COMPLEX(y)->real, x),
			    SG_COMPLEX(y)->imag);
    }
  }
  else if (SG_BIGNUMP(x)) {
    if (SG_INTP(y)) {
      if (y == SG_MAKE_INT(0)) return x;
      else return Sg_BignumAddSI(SG_BIGNUM(x), SG_INT_VALUE(y));
    }
    else if (SG_BIGNUMP(y)) {
      return Sg_BignumAdd(SG_BIGNUM(x), SG_BIGNUM(y));
    }
    else if (SG_RATIONALP(y)) {
      return Sg_RationalAdd(x, y);
    }
    else if (SG_FLONUMP(y)) {
      return Sg_MakeFlonum(Sg_BignumToDouble(x) + SG_FLONUM_VALUE(y));
    }
    else if (SG_COMPLEXP(y)) {
      return Sg_MakeComplex(Sg_Add(SG_COMPLEX(y)->real, x), SG_COMPLEX(y)->imag);
    }
  }
  else if (SG_RATIONALP(x)) {
    if (SG_INTP(y)) {
      if (y == SG_MAKE_INT(0)) return x;
      else return Sg_RationalAdd(x, y);
    }
    else if (SG_BIGNUMP(y) || SG_RATIONALP(y)) {
      return Sg_RationalAdd(x, y);
    }
    else if (SG_FLONUMP(y)) {
      return Sg_MakeFlonum(Sg_GetDouble(x) + SG_FLONUM_VALUE(y));
    }
    else if (SG_COMPLEXP(y)) {
      return Sg_MakeComplex(Sg_Add(SG_COMPLEX(y)->real, x),
			    SG_COMPLEX(y)->imag);
    }
  }
  else if (SG_COMPLEXP(x)) {
    if (SG_INTP(y)) {
      if (y == SG_MAKE_INT(0)) return x;
      else return Sg_MakeComplex(Sg_Add(SG_COMPLEX(x)->real, y),
				 SG_COMPLEX(x)->imag);
    }
    else if (SG_BIGNUMP(y) || SG_RATIONALP(y)) {
      return Sg_MakeComplex(Sg_Add(SG_COMPLEX(x)->real, y),
			    SG_COMPLEX(x)->imag);
      
    }
    else if (SG_FLONUMP(y)) {
      return Sg_MakeComplex(Sg_Add(SG_COMPLEX(x)->real, y),
			    Sg_Inexact(SG_COMPLEX(x)->imag));
    }
    else if (SG_COMPLEXP(y)) {
      SgObject real = Sg_Add(SG_COMPLEX(x)->real, SG_COMPLEX(y)->real);
      SgObject imag = Sg_Add(SG_COMPLEX(x)->imag, SG_COMPLEX(y)->imag);
      return oprtr_norm_complex(real, imag);
    }
  }
  wte(SG_INTERN("+"), "number", SG_LIST2(x, y));
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_Sub(SgObject x, SgObject y)
{
  if (SG_INTP(x)) {
    if (SG_INTP(y)) {
      long r = SG_INT_VALUE(x) - SG_INT_VALUE(y);
      return Sg_MakeInteger(r);
    }
    else if (SG_BIGNUMP(y)) {
      SgObject big = Sg_MakeBignumFromSI(SG_INT_VALUE(x));
      return Sg_BignumSub(SG_BIGNUM(big), SG_BIGNUM(y));
    }
    else if (SG_RATIONALP(y)) {
      if (x == SG_MAKE_INT(0)) return Sg_Negate(y);
      else return Sg_RationalSub(x, y);
    }
    else if (SG_FLONUMP(y)) {
      return Sg_MakeFlonum((double)SG_INT_VALUE(x) - SG_FLONUM_VALUE(y));
    }
    else if (SG_COMPLEXP(y)) {
      SgObject img = SG_COMPLEX(y)->imag;
      if (x == SG_MAKE_INT(0)) return Sg_Negate(y);
      /* imag part must be negate. */
      else return Sg_MakeComplex(Sg_Sub(x, SG_COMPLEX(y)->real),
				 Sg_Negate(img));
    }
  }
  else if (SG_FLONUMP(x)) {
    if (SG_INTP(y)) {
      if (y == SG_MAKE_INT(0)) return x;
      else {
	double z = SG_FLONUM_VALUE(x) - (double)SG_INT_VALUE(y);
	return Sg_MakeFlonum(z);
      }
    }
    else if (SG_BIGNUMP(y) || SG_RATIONALP(y)) {
      return Sg_MakeFlonum(SG_FLONUM_VALUE(x) - Sg_GetDouble(y));
    }
    else if (SG_FLONUMP(y)) {
      return Sg_MakeFlonum(SG_FLONUM_VALUE(x) - SG_FLONUM_VALUE(y));
    }
    else if (SG_COMPLEXP(y)) {
      /* imag part must be negate. */
      SgObject img = SG_COMPLEX(y)->imag;
      return Sg_MakeComplex(Sg_Sub(x, SG_COMPLEX(y)->real), Sg_Negate(img));
    }
  }
  else if (SG_BIGNUMP(x)) {
    if (SG_INTP(y)) {
      if (y == SG_MAKE_INT(0)) return x;
      else return Sg_BignumSubSI(SG_BIGNUM(x), SG_INT_VALUE(y));
    }
    else if (SG_BIGNUMP(y)) {
      return Sg_BignumSub(SG_BIGNUM(x), SG_BIGNUM(y));
    }
    else if (SG_RATIONALP(y)) {
      return Sg_RationalSub(x, y);
    }
    else if (SG_FLONUMP(y)) {
      return Sg_MakeFlonum(Sg_BignumToDouble(x) - SG_FLONUM_VALUE(y));
    }
    else if (SG_COMPLEXP(y)) {
      /* imag part must be negate. */
      SgObject img = SG_COMPLEX(y)->imag;
      return Sg_MakeComplex(Sg_Sub(x, SG_COMPLEX(y)->real), Sg_Negate(img));
    }
  }
  else if (SG_RATIONALP(x)) {
    if (SG_INTP(y)) {
      if (y == SG_MAKE_INT(0)) return x;
      else return Sg_RationalSub(x, y);
    }
    else if (SG_BIGNUMP(y) || SG_RATIONALP(y)) {
      return Sg_RationalSub(x, y);
    }
    else if (SG_FLONUMP(y)) {
      return Sg_MakeFlonum(Sg_GetDouble(x) - SG_FLONUM_VALUE(y));
    }
    else if (SG_COMPLEXP(y)) {
      /* imag part must be negate. */
      SgObject img = SG_COMPLEX(y)->imag;
      return Sg_MakeComplex(Sg_Sub(x, SG_COMPLEX(y)->real), Sg_Negate(img));
    }
  }
  else if (SG_COMPLEXP(x)) {
    if (SG_INTP(y)) {
      if (y == SG_MAKE_INT(0)) return x;
      else return Sg_MakeComplex(Sg_Sub(SG_COMPLEX(x)->real, y),
				 SG_COMPLEX(x)->imag);
    }
    else if (SG_BIGNUMP(y) || SG_RATIONALP(y)) {
      return Sg_MakeComplex(Sg_Sub(SG_COMPLEX(x)->real, y),
			    SG_COMPLEX(x)->imag);
      
    }
    else if (SG_FLONUMP(y)) {
      return Sg_MakeComplex(Sg_Sub(SG_COMPLEX(x)->real, y),
			    Sg_Inexact(SG_COMPLEX(x)->imag));
    }
    else if (SG_COMPLEXP(y)) {
      SgObject real = Sg_Sub(SG_COMPLEX(x)->real, SG_COMPLEX(y)->real);
      SgObject imag = Sg_Sub(SG_COMPLEX(x)->imag, SG_COMPLEX(y)->imag);
      return oprtr_norm_complex(real, imag);
    }
  }
  wte(SG_INTERN("-"), "number", SG_LIST2(x, y));
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_Mul(SgObject x, SgObject y)
{
  if (SG_INTP(x)) {
    if (SG_INTP(y)) {
      long v0 = SG_INT_VALUE(x);
      long v1 = SG_INT_VALUE(y);
      udlong k = v0 * v1;
      if ((v1 != 0 && k / v1 != v0) || !(k >= SG_INT_MIN && k <= SG_INT_MAX)) {
	SgObject big = Sg_MakeBignumFromSI(v0);
	return Sg_BignumMulSI(SG_BIGNUM(big), v1);
      } else 
	return Sg_MakeInteger((long)k);
    } else if (SG_BIGNUMP(y)) {
      if (x == SG_MAKE_INT(0)) return x;
      else if (x == SG_MAKE_INT(1)) return y;
      else return Sg_BignumMulSI(SG_BIGNUM(y), SG_INT_VALUE(x));
    } else if (SG_RATIONALP(y)) {
      if (x == SG_MAKE_INT(0)) return x;
      else if (x == SG_MAKE_INT(1)) return y;
      else return Sg_RationalMul(x, y);
    } else if (SG_FLONUMP(y)) {
      if (x == SG_MAKE_INT(0)) return x;
      else if (x == SG_MAKE_INT(1)) return y;
      else return Sg_MakeFlonum((double)SG_INT_VALUE(x) * SG_FLONUM_VALUE(y));
    } else if (SG_COMPLEXP(y)) {
      if (x == SG_MAKE_INT(0)) return x;
      else if (x == SG_MAKE_INT(1)) return y;
      else return Sg_MakeComplex(Sg_Mul(x, SG_COMPLEX(y)->real),
				 Sg_Mul(x, SG_COMPLEX(y)->imag));
    }
  }
  else if (SG_FLONUMP(x)) {
    if (SG_INTP(y)) {
      if (x == SG_MAKE_INT(0)) return x;
      else if (x == SG_MAKE_INT(1)) return y;
      else {
	double z = SG_FLONUM_VALUE(x) * (double)SG_INT_VALUE(y);
	return Sg_MakeFlonum(z);
      }
    } else if (SG_BIGNUMP(y) || SG_RATIONALP(y)) {
      return Sg_MakeFlonum(SG_FLONUM_VALUE(x) * Sg_GetDouble(y));
    } else if (SG_FLONUMP(y)) {
      if (SG_FLONUM_VALUE(y) == 1.0) return x;
      return Sg_MakeFlonum(SG_FLONUM_VALUE(x) * SG_FLONUM_VALUE(y));
    } else if (SG_COMPLEXP(y)) {
      return Sg_MakeComplex(Sg_Mul(x, SG_COMPLEX(y)->real),
			    Sg_Mul(x, SG_COMPLEX(y)->imag));
    }
  }
  else if (SG_BIGNUMP(x)) {
    if (SG_INTP(y)) {
      if (y == SG_MAKE_INT(0)) return y;
      return Sg_BignumMulSI(SG_BIGNUM(x), SG_INT_VALUE(y));
    } else if (SG_BIGNUMP(y)) {
      /* squaring is faster than multiplication so if given arguments
	 are the same object, then do it with square. */
      if (SG_EQ(x, y)) return Sg_BignumSquare(x);
      return Sg_BignumMul(SG_BIGNUM(x), SG_BIGNUM(y));
    } else if (SG_RATIONALP(y)) {
      return Sg_RationalMul(x, y);
    } else if (SG_FLONUMP(y)) {
      return Sg_MakeFlonum(Sg_BignumToDouble(x) * SG_FLONUM_VALUE(y));
    } else if (SG_COMPLEXP(y)) {
      return Sg_MakeComplex(Sg_Mul(x, SG_COMPLEX(y)->real),
			    Sg_Mul(x, SG_COMPLEX(y)->imag));
    }
  }
  else if (SG_RATIONALP(x)) {
    if (SG_INTP(y)) {
      if (x == SG_MAKE_INT(0)) return x;
      else if (x == SG_MAKE_INT(1)) return y;
      else return Sg_RationalMul(x, y);
    } else if (SG_BIGNUMP(y) || SG_RATIONALP(y)) {
      return Sg_RationalMul(x, y);
    } else if (SG_FLONUMP(y)) {
      return Sg_MakeFlonum(Sg_GetDouble(x) * SG_FLONUM_VALUE(y));
    } else if (SG_COMPLEXP(y)) {
      return Sg_MakeComplex(Sg_Mul(x, SG_COMPLEX(y)->real),
			    Sg_Mul(x, SG_COMPLEX(y)->imag));
    }
  }
  else if (SG_COMPLEXP(x)) {
    if (SG_INTP(y)) {
      if (x == SG_MAKE_INT(0)) return x;
      if (x == SG_MAKE_INT(1)) return y;
      return Sg_MakeComplex(Sg_Mul(SG_COMPLEX(x)->real, y),
			    Sg_Mul(SG_COMPLEX(x)->imag, y));
    }
    else if (SG_BIGNUMP(y) || SG_RATIONALP(y)) {
      return Sg_MakeComplex(Sg_Mul(SG_COMPLEX(x)->real, y),
			    Sg_Mul(SG_COMPLEX(x)->imag, y));
    }
    else if (SG_FLONUMP(y)) {
      return Sg_MakeComplex(Sg_Mul(SG_COMPLEX(x)->real, y),
			    Sg_Mul(SG_COMPLEX(x)->imag, y));
    }
    else if (SG_COMPLEXP(y)) {
      SgObject real = Sg_Sub(Sg_Mul(SG_COMPLEX(x)->real, SG_COMPLEX(y)->real),
			     Sg_Mul(SG_COMPLEX(x)->imag, SG_COMPLEX(y)->imag));
      SgObject imag = Sg_Add(Sg_Mul(SG_COMPLEX(x)->imag, SG_COMPLEX(y)->real),
			     Sg_Mul(SG_COMPLEX(x)->real, SG_COMPLEX(y)->imag));
      return oprtr_norm_complex(real, imag);
    }
  }
  wte(SG_INTERN("*"), "number", SG_LIST2(x, y));
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_Div(SgObject x, SgObject y)
{
  SgObject real, imag;
  if (SG_INTP(x)) {
    if (SG_INTP(y)) {
      if (y == SG_MAKE_INT(0)) goto div_by_zero;
      else if (x == SG_MAKE_INT(0)) return x;
      else if (y == SG_MAKE_INT(1)) return x;
      else return Sg_MakeRational(x, y);
    }
    else if (SG_BIGNUMP(y)) {
      if (x == SG_MAKE_INT(0)) return x;
      else return Sg_MakeRational(x, y);
    }
    else if (SG_RATIONALP(y)) {
      return Sg_MakeRational(Sg_Mul(x, SG_RATIONAL(y)->denominator),
			     SG_RATIONAL(y)->numerator);
    }
    else if (SG_FLONUMP(y)) {
      if (SG_FLONUM_VALUE(y) == 0.0) goto a_normal;
      return Sg_MakeFlonum((double)SG_INT_VALUE(x) / SG_FLONUM_VALUE(y));
    }
    else if (SG_COMPLEXP(y)) {
      SgObject r2;
      real = SG_COMPLEX(y)->real;
      imag = SG_COMPLEX(y)->imag;
      r2 = Sg_Add(Sg_Mul(real, real), Sg_Mul(imag, imag));
      return Sg_MakeComplex(Sg_Div(Sg_Mul(real, x), r2),
			    Sg_Negate(Sg_Div(Sg_Mul(x, imag), r2)));
    }
  }
  else if (SG_FLONUMP(x)) {
    if (SG_INTP(y)) {
      if (y == SG_MAKE_INT(0)) goto a_normal;
      else if (y == SG_MAKE_INT(1)) return x;
      else return Sg_MakeFlonum(SG_FLONUM_VALUE(x) / SG_INT_VALUE(y));
    }
    else if (SG_BIGNUMP(y) || SG_RATIONALP(y)) {
      return Sg_MakeFlonum(SG_FLONUM_VALUE(x) / Sg_GetDouble(y));
    }
    else if (SG_FLONUMP(y)) {
      if (SG_FLONUM_VALUE(y) == 0.0) goto a_normal;
      else return Sg_MakeFlonum(SG_FLONUM_VALUE(x) / SG_FLONUM_VALUE(y));
    }
    else if (SG_COMPLEXP(y)) {
      SgObject r2;
      real = SG_COMPLEX(y)->real;
      imag = SG_COMPLEX(y)->imag;
      r2 = Sg_Add(Sg_Mul(real, real), Sg_Mul(imag, imag));
      return Sg_MakeComplex(Sg_Div(Sg_Mul(real, x), r2),
			    Sg_Negate(Sg_Div(Sg_Mul(x, imag), r2)));
    }
  }
  else if (SG_BIGNUMP(x)) {
    if (SG_INTP(y)) {
      if (y == SG_MAKE_INT(0)) goto div_by_zero;
      else if (y == SG_MAKE_INT(1)) return x;
      else return Sg_MakeRational(x, y);
    }
    else if (SG_BIGNUMP(y)) {
      return Sg_MakeRational(x, y);
    }
    else if (SG_RATIONALP(y)) {
      return Sg_MakeRational(Sg_Mul(SG_RATIONAL(y)->denominator, x),
			     SG_RATIONAL(y)->numerator);
    }
    else if (SG_FLONUMP(y)) {
      if (SG_FLONUM_VALUE(y) == 0.0) goto a_normal;
      return Sg_MakeFlonum(Sg_GetDouble(x) / SG_FLONUM_VALUE(y));
    }
    else if (SG_COMPLEXP(y)) {
      SgObject r2;
      real = SG_COMPLEX(y)->real;
      imag = SG_COMPLEX(y)->imag;
      r2 = Sg_Add(Sg_Mul(real, real), Sg_Mul(imag, imag));
      return Sg_MakeComplex(Sg_Div(Sg_Mul(real, x), r2),
			    Sg_Negate(Sg_Div(Sg_Mul(x, imag), r2)));
    }
  }
  else if (SG_RATIONALP(x)) {
    if (SG_INTP(y)) {
      if (y == SG_MAKE_INT(0)) goto div_by_zero;
      else if (y == SG_MAKE_INT(1)) return x;
      else return Sg_MakeRational(SG_RATIONAL(x)->numerator,
				  Sg_Mul(SG_RATIONAL(x)->denominator, y));
    }
    else if (SG_BIGNUMP(y)) {
      return Sg_MakeRational(SG_RATIONAL(x)->numerator,
			     Sg_Mul(SG_RATIONAL(x)->denominator, y));
    }
    else if (SG_RATIONALP(y)) {
      return Sg_RationalDiv(x, y);
    }
    else if (SG_FLONUMP(y)) {
      return Sg_MakeFlonum(Sg_GetDouble(x) / SG_FLONUM_VALUE(y));
    }
    else if (SG_COMPLEXP(y)) {
      SgObject r2;
      real = SG_COMPLEX(y)->real;
      imag = SG_COMPLEX(y)->imag;
      r2 = Sg_Add(Sg_Mul(real, real), Sg_Mul(imag, imag));
      return Sg_MakeComplex(Sg_Div(Sg_Mul(real, x), r2),
			    Sg_Negate(Sg_Div(Sg_Mul(x, imag), r2)));
    }
  }
  else if (SG_COMPLEXP(x)) {
    real = SG_COMPLEX(x)->real;
    imag = SG_COMPLEX(x)->imag;
    if (SG_INTP(y)) {
      return Sg_MakeComplex(Sg_Div(real, y), Sg_Div(imag, y));
    }
    else if (SG_BIGNUMP(y) || SG_RATIONALP(y)) {
      return Sg_MakeComplex(Sg_Div(real, y), Sg_Div(imag, y));
    }
    else if (SG_FLONUMP(y)) {
      return Sg_MakeComplex(Sg_Div(real, y), Sg_Div(imag, y));
    }
    else if (SG_COMPLEXP(y)) {
      SgObject real2 = SG_COMPLEX(y)->real;
      SgObject imag2 = SG_COMPLEX(y)->imag;
      SgObject r2 = Sg_Add(Sg_Mul(real2, real2), Sg_Mul(imag2, imag2));
      SgObject real3 = Sg_Div(Sg_Add(Sg_Mul(real, real2),
				     Sg_Mul(imag, imag2)), r2);
      SgObject imag3 = Sg_Div(Sg_Sub(Sg_Mul(imag, real2),
				     Sg_Mul(real, imag2)), r2);
      return oprtr_norm_complex(real3, imag3);
    }
  }
  wte(SG_INTERN("/"), "number", SG_LIST2(x, y));
  return SG_UNDEF;		/* dummy */

 a_normal:
  {
    int s = Sg_Sign(x);
    if (s == 0) return SG_NAN;
    else {
      if (SG_INTP(y)) {
      only_x:
	if (s < 0) return SG_NEGATIVE_INFINITY;
	else       return SG_POSITIVE_INFINITY;
      } else {
	union { double f64; int64_t i64; } d;
	d.f64 = SG_FLONUM_VALUE(y);
	/* if pisitive 0 is given then i64 is 0. */
	if (d.i64 < 0)
 	  if (s < 0) return SG_POSITIVE_INFINITY;
	  else       return SG_NEGATIVE_INFINITY;
	else goto only_x;
      }
    }
  }
 div_by_zero:
  Sg_AssertionViolation(SG_INTERN("/"), SG_MAKE_STRING("undefined for 0"),
			SG_LIST2(x, y));
  return SG_UNDEF;
}

SgObject Sg_Quotient(SgObject x, SgObject y, SgObject *rem)
{
  SgObject real;
  SgObject imag;
  double rx, ry;

#define do_complex(t, label)			\
  real = SG_COMPLEX(t)->real;			\
  imag = SG_COMPLEX(t)->imag;			\
  if (Sg_ZeroP(imag)) {				\
    (t) = real;					\
    goto label;					\
  }						\
  else goto bad_arg;				\

 start_again:
  if (SG_INTP(x)) {
    if (SG_INT_VALUE(x) == 0)  {
      if (rem) *rem = SG_MAKE_INT(0);
      return SG_MAKE_INT(0);
    }
  fixnum_again:
    if (SG_INTP(y)) {
      long q, r;
      if (SG_INT_VALUE(y) == 0) {
	goto div_by_zero;
      }
      q = SG_INT_VALUE(x) / SG_INT_VALUE(y);
      if (rem) {
	r = SG_INT_VALUE(x) % SG_INT_VALUE(y);
	*rem = SG_MAKE_INT(r);
      }
      return SG_MAKE_INT(q);
    }
    else if (SG_BIGNUMP(y)) {
      SgObject qr = Sg_BignumDivRem(SG_BIGNUM(Sg_MakeBignumFromSI(SG_INT_VALUE(x))),
				    SG_BIGNUM(y));
      if (rem) *rem = SG_CDR(qr);
      return SG_CAR(qr);
    }
    else if (SG_FLONUMP(y)) {
      rx = (double)SG_INT_VALUE(x);
      ry = SG_FLONUM_VALUE(y);
      if (ry != floor(ry)) goto bad_argy;
      goto do_flonum;
    }
    else if (SG_COMPLEXP(y)) {
      do_complex(y, fixnum_again);
    }
    else goto bad_argy;
  } 
  if (SG_BIGNUMP(x)) {
  bignum_again:
    if (SG_INTP(y)) {
      long r;
      SgObject q = Sg_BignumDivSI(SG_BIGNUM(x), SG_INT_VALUE(y), &r);
      if (rem) *rem = SG_MAKE_INT(r);
      return q;
    }
    else if (SG_BIGNUMP(y)) {
      SgObject qr = Sg_BignumDivRem(SG_BIGNUM(x), SG_BIGNUM(y));
      if (rem) *rem = SG_CDR(qr);
      return SG_CAR(qr);
    }
    else if (SG_FLONUMP(y)) {
      rx = Sg_BignumToDouble(SG_BIGNUM(x));
      ry = SG_FLONUM_VALUE(y);
      if (ry != floor(ry)) goto bad_argy;
      else goto do_flonum;
    }
    else if (SG_COMPLEXP(y)) {
      do_complex(y, bignum_again);
    }
    goto bad_argy;
  }
  else if (SG_COMPLEXP(x)) {
    do_complex(x, start_again);
  }
  else if (SG_FLONUMP(x)) {
    rx = SG_FLONUM_VALUE(x);
    if (rx != floor(rx)) goto bad_arg;
  flonum_again:
    if (SG_INTP(y)) {
      ry = (double)SG_INT_VALUE(y);
    } else if (SG_BIGNUMP(y)) {
      ry = Sg_BignumToDouble(SG_BIGNUM(y));
    } else if (SG_FLONUMP(y)) {
      ry = SG_FLONUM_VALUE(y);
      if (ry != floor(ry)) goto bad_argy;
    } else if (SG_COMPLEXP(y)) {
      do_complex(y, flonum_again);
    } else {
      goto bad_argy;
    }
  do_flonum:
    {
      double q;
      if (ry == 0.0) goto div_by_zero;
      q = (rx*ry > 0)? floor(rx/ry) : ceil(rx/ry);
      if (rem) {
	double rr = roundeven(rx - q*ry);
	*rem = Sg_MakeFlonum(rr);
      }
      return Sg_MakeFlonum(q);
    }
  }
  goto bad_arg;

 div_by_zero:
  Sg_AssertionViolation(SG_INTERN("quotient"),
			SG_MAKE_STRING("attempt to calculate a quotient by zero"),
			SG_LIST2(x, y));
 bad_argy:
  x = y;
 bad_arg:
  wte(SG_INTERN("quotient"), "integer", x);
  return SG_UNDEF;		/* dummy */
#undef do_complex
}

SgObject Sg_Modulo(SgObject x, SgObject y, int remp)
{
  SgObject real, imag;
  SgObject bx;
  double rx, ry;

#define do_complex(t, label)			\
  real = SG_COMPLEX(t)->real;			\
  imag = SG_COMPLEX(t)->imag;			\
  if (Sg_ZeroP(imag)) {				\
    (t) = real;					\
    goto label;					\
  }						\
  else goto bad_arg;				\

 start_again:
  if (SG_INTP(x)) {
  fixnum_again:
    if (SG_INTP(y)) {
      long r;
      if (SG_EQ((y), SG_MAKE_INT(0))) goto div_by_zero;
      r = SG_INT_VALUE(x) % SG_INT_VALUE(y);
      if (!remp && r) {
	if ((SG_INT_VALUE(x) > 0 && SG_INT_VALUE(y) < 0)
	    || (SG_INT_VALUE(x) < 0 && SG_INT_VALUE(y) > 0)) {
	  r += SG_INT_VALUE(y);
	}
      }
      return SG_MAKE_INT(r);
    }
    else if (SG_BIGNUMP(y)) {
      bx = Sg_MakeBignumFromSI(SG_INT_VALUE(x));
      goto do_bignumy;
    }
    else if (SG_FLONUMP(y)) {
      rx = (double)SG_INT_VALUE(x);
      ry = SG_FLONUM_VALUE(y);
      if (ry != floor(ry)) goto bad_argy;
      else goto do_flonum;
    }
    else if (SG_COMPLEXP(y)) {
      do_complex(y, fixnum_again);
    }
    else goto bad_arg;
  }
  if (SG_BIGNUMP(x)) {
  bignum_again:
    if (SG_INTP(y)) {
#if 0
      long iy = SG_INT_VALUE(y);
      long rem;
      Sg_BignumDivSI(SG_BIGNUM(x), iy, &rem);
      if (!remp
	  && rem
	  && ((SG_BIGNUM_GET_SIGN(x) < 0 && iy > 0)
	      || (SG_BIGNUM_GET_SIGN(x) > 0 && iy < 0))) {
	return SG_MAKE_INT(iy + rem);
      }
      else return SG_MAKE_INT(rem);
#else
      if (SG_INT_VALUE(y) == 0) goto div_by_zero;
      return Sg_BignumModuloSI(SG_BIGNUM(x), SG_INT_VALUE(y), remp);
#endif
    }
    else if (SG_BIGNUMP(y)) {
      bx = x;
    do_bignumy:
      return Sg_BignumModulo(bx, SG_BIGNUM(y), remp);
    }
    else if (SG_FLONUMP(y)) {
      rx = Sg_BignumToDouble(SG_BIGNUM(x));
      ry = SG_FLONUM_VALUE(y);
      if (ry != floor(ry)) goto bad_argy;
      else goto do_flonum;
    }
    else if (SG_COMPLEXP(y)) {
      do_complex(y, bignum_again);
    }
    else goto bad_argy;
  }
  else if (SG_FLONUMP(x)) {
    double rem;
    rx = SG_FLONUM_VALUE(x);
  flonum_again:
    if (rx != floor(rx)) goto bad_arg;
    if (SG_INTP(y)) {
      ry = (double)SG_INT_VALUE(y);
    } else if (SG_BIGNUMP(y)) {
      ry = Sg_BignumToDouble(y);
    } else if (SG_FLONUMP(y)) {
      ry = SG_FLONUM_VALUE(y);
    } else if (SG_COMPLEXP(y)) {
      do_complex(y, flonum_again);
    } else {
      goto bad_argy;
    }
  do_flonum:
    if (ry == 0.0) goto div_by_zero;
    rem = fmod(rx, ry);
    if (!remp) {
      if (rem == 0.0) return Sg_MakeFlonum(0.0);
      if ((rx > 0 && ry < 0) || (rx < 0 && ry > 0)) {
	rem += ry;
      }
    }
    return Sg_MakeFlonum(rem);
  }
  else if (SG_COMPLEXP(x)) {
    do_complex(x, start_again);
  }
  else goto bad_arg;

 div_by_zero:
  Sg_AssertionViolation((remp) ? SG_INTERN("remainder") : SG_INTERN("modulo"),
	SG_MAKE_STRING("attempt to calculate a remainder/modulo by zero"),
	SG_LIST2(x, y));
 bad_argy:
  x = y;
 bad_arg:
  wte((remp) ? SG_INTERN("remainder") : SG_INTERN("modulo"),
      "integer", x);
  return SG_UNDEF;		/* dummy */

#undef do_complex
}

static double cos_pi(double x)
{
  return cos(M_PI * x);
}

static double sin_pi(double x)
{
  return sin(M_PI * x);
}


static SgObject expt_body(SgObject x, SgObject y)
{
  if (SG_FLONUMP(x) && SG_FLONUM_VALUE(x) == 0.0) {
    if (SG_COMPLEXP(y)) {
      if (Sg_PositiveP(SG_COMPLEX(y)->real)) return SG_FL_POSITIVE_ZERO;
    } else {
      if (Sg_PositiveP(y)) {
	int exp, sign;
	Sg_DecodeFlonum(SG_FLONUM_VALUE(x), &exp, &sign);
	if (sign < 0)
	  return Sg_OddP(y) ? SG_FL_NEGATIVE_ZERO : SG_FL_POSITIVE_ZERO;
	return SG_FL_POSITIVE_ZERO;
      }
    }
  }
  if (Sg_ExactP(y)) {
    if (SG_INTP(y)) {
      if (SG_INT_VALUE(y) == 0) {
	if (Sg_ExactP(x)) return SG_MAKE_INT(1);
	else return Sg_MakeFlonum(1.0);
      } else if (SG_FLONUMP(x))
	return Sg_MakeFlonum(pow(SG_FLONUM_VALUE(x),
				 (double)SG_INT_VALUE(y)));
      else return oprtr_expt(x, SG_INT_VALUE(y));
    }
    else if (SG_BIGNUMP(y)) {
      if (!Sg_ExactP(x)) {
	/* Sg_Error(UC("exact number required, but got %S"), x); */
	return SG_UNDEF;
      }
      else if (SG_REALP(y)) {
	double n = Sg_BignumToDouble(SG_BIGNUM(y));
	return Sg_MakeFlonum(pow(Sg_GetDouble(x), n));
      }
      else return Sg_Exp(Sg_Mul(y, Sg_Log(x)));
    }
    else if (SG_RATIONALP(y)) {
      double n = Sg_GetDouble(y);
      if (SG_REALP(x) && !Sg_NegativeP(x))
	return Sg_MakeFlonum(pow(Sg_GetDouble(x), n));
      else 
	return Sg_Exp(Sg_Mul(y, Sg_Log(x)));
    }
    else if (SG_COMPLEXP(y)) {
      return Sg_Exp(Sg_Mul(y, Sg_Log(x)));
    }
    else goto bad_arg;
  } else {
    if (SG_FLONUMP(y)) {
      if (SG_REALP(x)) {
	double n = SG_FLONUM_VALUE(y);
	double m = Sg_GetDouble(x);
	if (n < 0 && !Sg_IntegerP(x)) {
	  /* x^y = exp(y * log(x)) = exp(y*log(|x|))*exp(y*arg(x)*i) */
	  double mag = exp(n * log(-m));
	  return Sg_MakeComplex(Sg_MakeFlonum(mag * cos_pi(n)),
				Sg_MakeFlonum(mag * sin_pi(n)));
	} else return Sg_MakeFlonum(pow(m, n));
      }
      else return Sg_Exp(Sg_Mul(y, Sg_Log(x)));
    } else {
      return Sg_Exp(Sg_Mul(y, Sg_Log(x)));
    }
  }
 bad_arg:
  /* Sg_Error(UC("real number required, but got %S"), y); */
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_Expt(SgObject x, SgObject y)
{
  if (x == SG_MAKE_INT(1)) {
    if (Sg_ExactP(y)) return SG_MAKE_INT(1);
    else return Sg_MakeFlonum(1.0);
  }
  else if (x == SG_MAKE_INT(-1) && Sg_ExactP(y)) {
    if (Sg_OddP(y)) return SG_MAKE_INT(-1);
    else return SG_MAKE_INT(1);
  }
  else if (x == SG_MAKE_INT(0)) {
    if (Sg_ZeroP(y)) {
      if (Sg_ExactP(y)) return SG_MAKE_INT(1);
      else return Sg_MakeFlonum(1.0);
    }
    else if (Sg_RealValuedP(y)) {
      if (Sg_NegativeP(y)) return Sg_MakeComplex(SG_NAN, SG_NAN);
      else if (Sg_ExactP(y)) return SG_MAKE_INT(0);
      else return Sg_MakeFlonum(0.0);
    } else {
      ASSERT(SG_COMPLEXP(y));
      if (Sg_PositiveP(SG_COMPLEX(y)->real)) {
	if (Sg_ExactP(y)) return SG_MAKE_INT(0);
	else return Sg_MakeFlonum(0.0);
      }
      else return Sg_MakeComplex(SG_NAN, SG_NAN);
    }
  }
  else if (Sg_ExactP(x) && SG_BIGNUMP(y)) {
    SgObject msg = 
      Sg_Sprintf(UC("expt: calculated number is too big to fit into memory %S %S"),
		 x, y);
    Sg_ImplementationRestrictionViolation(SG_INTERN("expt"), msg, 
					  SG_LIST2(x, y));
    return SG_UNDEF;
  }
  else return expt_body(x, y);
}

SgObject Sg_Exp(SgObject obj)
{
  if (SG_INTP(obj)) {
    if (SG_INT_VALUE(obj) == 0) return SG_MAKE_INT(1);
    else return Sg_MakeFlonum(exp((double)SG_INT_VALUE(obj)));
  }
  else if (SG_COMPLEXP(obj)) {
    double real = Sg_GetDouble(SG_COMPLEX(obj)->real);
    double imag = Sg_GetDouble(SG_COMPLEX(obj)->imag);
    double a = exp(real);
    return Sg_MakeComplex(Sg_MakeFlonum(a * cos(imag)),
			  Sg_MakeFlonum(a * sin(imag)));
  }
  else if (SG_REALP(obj)) return Sg_MakeFlonum(exp(Sg_GetDouble(obj)));
  wte(SG_INTERN("exp"), "real number", obj);
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_Sin(SgObject obj)
{
  if (SG_INTP(obj)) {
    if (obj == SG_MAKE_INT(0)) return obj;
    else return Sg_MakeFlonum(sin((double)SG_INT_VALUE(obj)));
  }
  else if (SG_COMPLEXP(obj)) {
    double real = Sg_GetDouble(SG_COMPLEX(obj)->real);
    double imag = Sg_GetDouble(SG_COMPLEX(obj)->imag);
    double e = exp(imag);
    double f = 1.0 / e;
    return Sg_MakeComplex(Sg_MakeFlonum(0.5 * sin(real) * (e + f)),
			  Sg_MakeFlonum(0.5 * cos(real) * (e - f)));
  }
  else if (SG_REALP(obj)) return Sg_MakeFlonum(sin(Sg_GetDouble(obj)));
  wte(SG_INTERN("sin"), "number", obj);
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_Cos(SgObject obj)
{
  if (SG_INTP(obj)) {
    if (obj == SG_MAKE_INT(0)) return SG_MAKE_INT(1);
    else return Sg_MakeFlonum(cos((double)SG_INT_VALUE(obj)));
  }
  else if (SG_COMPLEXP(obj)) {
    double real = Sg_GetDouble(SG_COMPLEX(obj)->real);
    double imag = Sg_GetDouble(SG_COMPLEX(obj)->imag);
    double e = exp(imag);
    double f = 1.0 / e;
    return Sg_MakeComplex(Sg_MakeFlonum(0.5 * cos(real) * (f + e)),
			  Sg_MakeFlonum(0.5 * sin(real) * (f - e)));
  }
  else if (SG_REALP(obj)) return Sg_MakeFlonum(cos(Sg_GetDouble(obj)));
  wte(SG_INTERN("cos"), "number", obj);
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_Tan(SgObject obj)
{
  if (SG_INTP(obj)) {
    if (obj == SG_MAKE_INT(0)) return obj;
    else return Sg_MakeFlonum(tan((double)SG_INT_VALUE(obj)));
  }
  else if (SG_COMPLEXP(obj)) {
    double real = Sg_GetDouble(SG_COMPLEX(obj)->real);
    double imag = Sg_GetDouble(SG_COMPLEX(obj)->imag);
    double e = exp(2.0 * imag);
    double f = 1.0 / e;
    double d = cos(2.0 * real) + 0.5 * (e + f);
    return Sg_MakeComplex(Sg_MakeFlonum(sin(2.0 * real) / d),
			  Sg_MakeFlonum(0.5 * (e - f) / d));
  }
  else if (SG_REALP(obj)) return Sg_MakeFlonum(tan(Sg_GetDouble(obj)));
  wte(SG_INTERN("tan"), "number", obj);
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_Asin(SgObject obj)
{
  SgComplex *cn;
  SgObject ans;
  if (SG_EQ(obj, SG_MAKE_INT(0))) return obj;

  if (SG_REALP(obj) || (SG_COMPLEXP(obj) && Sg_ZeroP(SG_COMPLEX(obj)->imag))) {
    double x = Sg_GetDouble(obj);
    if (x >= -1.0 && x <= 1.0) return Sg_MakeFlonum(asin(Sg_GetDouble(obj)));
    else if (x < 0.0) return Sg_Negate(Sg_Asin(Sg_MakeFlonum(-x)));
    cn = Sg_MakeComplex(Sg_MakeFlonum(0.0), Sg_MakeFlonum(x));
  } else {
    ASSERT(SG_COMPLEXP(obj));
    if (Sg_PositiveP(SG_COMPLEX(obj)->imag)) 
      return Sg_Negate(Sg_Asin(Sg_Negate(obj)));
    cn = Sg_MakeComplex(Sg_Negate(SG_COMPLEX(obj)->imag),
			SG_COMPLEX(obj)->real);
  }
  ans = Sg_Log(Sg_Add(Sg_Sqrt(Sg_Sub(SG_MAKE_INT(1), Sg_Mul(obj, obj))),
		      cn));
  if (SG_COMPLEXP(ans)) {
    return Sg_MakeComplex(Sg_MakeFlonum(Sg_GetDouble(SG_COMPLEX(ans)->imag)),
			  Sg_MakeFlonum(-Sg_GetDouble(SG_COMPLEX(ans)->real)));
  }
  else 
    return Sg_MakeComplex(Sg_MakeFlonum(0.0),
			  Sg_MakeFlonum(-Sg_GetDouble(ans)));
	       
}

SgObject Sg_Acos(SgObject obj)
{
  if (SG_EQ(obj, SG_MAKE_INT(1))) return SG_MAKE_INT(0);

  if (SG_REALP(obj) || (SG_COMPLEXP(obj) && Sg_ZeroP(SG_COMPLEX(obj)->imag))) {
    double x = Sg_GetDouble(obj);
    if (x >= -1.0 && x <= 1.0) return Sg_MakeFlonum(acos(Sg_GetDouble(obj)));
  }
  return Sg_Sub(Sg_MakeFlonum(M_PI / 2.0), Sg_Asin(obj));
}

SgObject Sg_Atan(SgObject obj)
{
  if (SG_INTP(obj)) {
    if (obj == SG_MAKE_INT(0)) return obj;
    return Sg_MakeFlonum(atan((double)SG_INT_VALUE(obj)));
  }
  if (SG_COMPLEXP(obj)) {
    SgComplex *cn = Sg_MakeComplex(Sg_Negate(SG_COMPLEX(obj)->imag),
				   SG_COMPLEX(obj)->real);
    SgObject ans = Sg_Log(Sg_Div(Sg_Add(SG_MAKE_INT(1), cn),
				 Sg_Sub(SG_MAKE_INT(1), cn)));
    if (SG_COMPLEXP(ans)) {
      return Sg_MakeComplex(Sg_MakeFlonum(0.5 * Sg_GetDouble(SG_COMPLEX(ans)->imag)),
			    Sg_MakeFlonum(-0.5 * Sg_GetDouble(SG_COMPLEX(ans)->real)));
    }
    return Sg_MakeComplex(Sg_MakeFlonum(0.0),
			  Sg_MakeFlonum(-0.5 * Sg_GetDouble(ans)));
  }
  if (SG_REALP(obj)) return Sg_MakeFlonum(atan(Sg_GetDouble(obj)));
  wte(SG_INTERN("atan"), "number", obj);
  return SG_UNDEF;
}

SgObject Sg_Atan2(SgObject x, SgObject y)
{
  if (SG_EQ(x, SG_MAKE_INT(0))) return x;
  return Sg_MakeFlonum(atan2(Sg_GetDouble(x), Sg_GetDouble(y)));
}

static inline int either_nan_p(SgObject arg0, SgObject arg1)
{
  if (SG_FLONUMP(arg0) && isnan(SG_FLONUM_VALUE(arg0))) return TRUE;
  if (SG_FLONUMP(arg1) && isnan(SG_FLONUM_VALUE(arg1))) return TRUE;
  return FALSE;
}

int Sg_NumEq(SgObject x, SgObject y)
{
  /* not to break zero?'s compiler builtin inliner, sucks!! */
  if (SG_INTP(y) && SG_EQ(y, SG_MAKE_INT(0)) && Sg_ZeroP(x)) return TRUE;
  if (SG_COMPLEXP(x)) {
    if (SG_COMPLEXP(y)) {
      return ((Sg_NumCmp(SG_COMPLEX(x)->real, SG_COMPLEX(y)->real) == 0)
	      && (Sg_NumCmp(SG_COMPLEX(x)->imag, SG_COMPLEX(y)->imag) == 0));
    }
    if (Sg_ZeroP(SG_COMPLEX(x)->imag)) {
      return Sg_NumEq(SG_COMPLEX(x)->real, y);
    }
    return FALSE;
  } else {
    if (SG_COMPLEXP(y)) {
      if (Sg_ZeroP(SG_COMPLEX(y)->imag)) {
	return Sg_NumEq(x, SG_COMPLEX(y)->real);
      }
      return FALSE;
    }
    if (either_nan_p(x, y)) return FALSE;
    return (Sg_NumCmp(x, y) == 0);
  }
}

int Sg_NumCmp(SgObject x, SgObject y)
{
  SgObject badnum = SG_FALSE;
    /* on WATCOM, somehow NAN is bigger than 0 */
#ifdef __WATCOMC__
#define nan_return(_r) do { if (isnan(_r)) return 0; } while(0)
#else
#define nan_return(_r) 		/* dummy */
#endif
 start_again:
  if (SG_INTP(x)) {
  int_again:
    if (SG_INTP(y)) {
      long r = SG_INT_VALUE(x) - SG_INT_VALUE(y);
      if (r < 0) return -1;
      else if (r > 0) return 1;
      else return 0;
    }
    else if (SG_FLONUMP(y)) {
      double r = SG_INT_VALUE(x) - SG_FLONUM_VALUE(y);
      nan_return(r);
      if (r < 0) return -1;
      else if (r > 0) return 1;
      else return 0;
    }
    else if (SG_BIGNUMP(y)) {
      return Sg_BignumCmp(SG_BIGNUM(Sg_MakeBignumFromSI(SG_INT_VALUE(x))),
			  SG_BIGNUM(y));
    }
    else if (SG_RATIONALP(y)) {
      if (SG_MAKE_INT(0) == x) {
	return -Sg_Sign(y);
      } else {
	/*  roughly estimate the result by coercing the ratnum to double */
	double y2 = Sg_GetDouble(y);
	double r = SG_INT_VALUE(x) - y2;
	double err = y2 * 2.0e-52;
	if (r < -err) return -1;
	else if (r > err) return 1;
	else return Sg_NumCmp(Sg_Mul(x, SG_RATIONAL(y)->denominator),
			      SG_RATIONAL(y)->numerator);
      }
    } else if (SG_COMPLEXP(y)) {
      if (Sg_ZeroP(SG_COMPLEX(y)->imag)) {
	y = SG_COMPLEX(y)->real;
	goto int_again;
      }
    }
    badnum = y;
  }
  else if (SG_FLONUMP(x)) {
  flo_again:
    if (SG_INTP(y)) {
      double r = SG_FLONUM_VALUE(x) - SG_INT_VALUE(y);
      nan_return(r);
      if (r < 0) return -1;
      else if (r > 0) return 1;
      else return 0;
    }
    else if (SG_FLONUMP(y)) {
      double  r = SG_FLONUM_VALUE(x) - SG_FLONUM_VALUE(y);
      nan_return(r);
      if (r < 0) return -1;
      else if (r > 0) return 1;
      else return 0;
    }
    else if (SG_BIGNUMP(y) || SG_RATIONALP(y)) {
      double r = SG_FLONUM_VALUE(x) - Sg_GetDouble(y);
      nan_return(r);
      if (r < 0) return -1;
      else if (r > 0) return 1;
      else {
	return Sg_NumCmp(Sg_Exact(x), y);
      }
    } else if (SG_COMPLEXP(y)) {
      if (Sg_ZeroP(SG_COMPLEX(y)->imag)) {
	y = SG_COMPLEX(y)->real;
	goto flo_again;
      }
    }
    badnum = y;
  }
  else if (SG_BIGNUMP(x)) {
  big_again:
    if (SG_INTP(y)) {
      return Sg_BignumCmp(SG_BIGNUM(x),
			  SG_BIGNUM(Sg_MakeBignumFromSI(SG_INT_VALUE(y))));
    }
    else if (SG_FLONUMP(y)) {
      return -Sg_NumCmp(y, x);
    }
    else if (SG_BIGNUMP(y)) {
      return Sg_BignumCmp(SG_BIGNUM(x), SG_BIGNUM(y));
    }
    else if (SG_RATIONALP(y)) {
      SgObject d1 = SG_RATIONAL(y)->denominator;
      return Sg_NumCmp(Sg_Mul(x, d1),
		       SG_RATIONAL(y)->numerator);
    }
    else if (SG_COMPLEXP(y)) {
      if (Sg_ZeroP(SG_COMPLEX(y)->imag)) {
	y = SG_COMPLEX(y)->real;
	goto big_again;
      }
    }
    badnum = y;
  }
  else if (SG_RATIONALP(x)) {
  rat_again:
    if (SG_INTP(y) || SG_BIGNUMP(y) || SG_FLONUMP(y)) {
      return -Sg_NumCmp(y, x);
    }
    if (SG_RATIONALP(y)) {
      SgObject nx = SG_RATIONAL(x)->numerator, dx = SG_RATIONAL(x)->denominator;
      SgObject ny = SG_RATIONAL(y)->numerator, dy = SG_RATIONAL(y)->denominator;
      int sx = Sg_Sign(nx), sy = Sg_Sign(ny), d, n;
      
      if (sx < sy) return -1;
      if (sx > sy) return 1;
      d = Sg_NumCmp(dx, dy);
      if (d == 0) return Sg_NumCmp(nx, ny);
      else if ((sx > 0 && sy > 0) || (sx < 0 && sy < 0)) {
	n = Sg_NumCmp(nx, ny) * sx;
	if (d > 0 && n <= 0) return -sx;
	if (d < 0 && n >= 0) return sx;
      }
      return Sg_NumCmp(Sg_Mul(nx, dy),
		       Sg_Mul(ny, dx));
    } else if (SG_COMPLEXP(y)) {
      if (Sg_ZeroP(SG_COMPLEX(y)->imag)) {
	y = SG_COMPLEX(y)->real;
	goto rat_again;
      }
    }
    badnum = y;
  }
  else if (SG_COMPLEXP(x)) {
    if (Sg_ZeroP(SG_COMPLEX(x)->imag)) {
      x = SG_COMPLEX(x)->real;
      goto start_again;
    }
    badnum = x;
  }
  if (SG_FALSEP(badnum)) badnum = x;

  wte4(SG_FALSE, "real number", badnum, SG_LIST2(x, y));
  return 0;			/* dummy */
}

int Sg_NumGt(SgObject x, SgObject y)
{
  if (either_nan_p(x, y)) return FALSE;
  return (Sg_NumCmp(x, y) > 0);
}
int Sg_NumGe(SgObject x, SgObject y)
{
  if (either_nan_p(x, y)) return FALSE;
  return (Sg_NumCmp(x, y) >= 0);
}

int Sg_NumLt(SgObject x, SgObject y)
{
  if (either_nan_p(x, y)) return FALSE;
  return (Sg_NumCmp(x, y) < 0);
}
int Sg_NumLe(SgObject x, SgObject y)
{
  if (either_nan_p(x, y)) return FALSE;
  return (Sg_NumCmp(x, y) <= 0);
}

SgObject Sg_Abs(SgObject obj)
{
  if (SG_INTP(obj)) {
    long v = SG_INT_VALUE(obj);
    if (v < 0) {
      obj = Sg_MakeInteger(-v);
    }
  } else if (SG_BIGNUMP(obj)) {
    if (SG_BIGNUM_GET_SIGN(obj) < 0) {
      obj = Sg_BignumCopy(SG_BIGNUM(obj));
      SG_BIGNUM_SET_SIGN(obj, 1);
    }
  } else if (SG_FLONUMP(obj)) {
    double v = SG_FLONUM_VALUE(obj);
    if (signbit(v)) return Sg_MakeFlonum(-v);
  } else if (SG_RATIONALP(obj)) {
    if (Sg_Sign(SG_RATIONAL(obj)->numerator) < 0) {
      obj = Sg_MakeRational(Sg_Negate(SG_RATIONAL(obj)->numerator),
			    SG_RATIONAL(obj)->denominator);
    }
  } else if (SG_COMPLEXP(obj)) {
    SgObject r = SG_COMPLEX(obj)->real;
    SgObject i = SG_COMPLEX(obj)->imag;
    SgObject a = Sg_Sqrt(Sg_Add(Sg_Mul(r, r), Sg_Mul(i, i)));
    return a;
  } else {
    wte(SG_INTERN("abs"), "number", obj);
  }
  return obj;
}

SgObject Sg_Sqrt(SgObject obj)
{
  if (SG_INTP(obj)) {
    long value = SG_INT_VALUE(obj);
    if (value == 0) return SG_MAKE_INT(0);
    if (value > 0) {
      double root = sqrt((double)value);
      long iroot = (long)floor(root);
      if (iroot * iroot == value) return SG_MAKE_INT(iroot);
      return Sg_MakeFlonum(root);
    } else {
      long iroot;
      value = -value;
      iroot = (long)floor(sqrt((double)value));
      if (iroot * iroot == value) 
	return Sg_MakeComplex(SG_MAKE_INT(0), SG_MAKE_INT(iroot));
      return  Sg_MakeComplex(Sg_MakeFlonum(0.0),
			     Sg_MakeFlonum(sqrt((double)value)));
    }
  }
  if (SG_BIGNUMP(obj)) {
    return Sg_BignumSqrt(SG_BIGNUM(obj));
  }
  if (SG_RATIONALP(obj)) {
    SgObject nume, deno;
    int comp = FALSE;
    if (Sg_NegativeP(SG_RATIONAL(obj)->numerator)) {
      nume = Sg_Sqrt(Sg_Negate(SG_RATIONAL(obj)->numerator));
      comp = TRUE;
    } else {
      nume = Sg_Sqrt(SG_RATIONAL(obj)->numerator);
    }
    deno = Sg_Sqrt(SG_RATIONAL(obj)->denominator);
    if ((SG_INTP(nume) || SG_BIGNUMP(nume))
	&& (SG_INTP(deno) || SG_BIGNUMP(deno))) {
      if (comp) return Sg_MakeComplex(SG_MAKE_INT(0), Sg_MakeRational(nume, deno));
      return Sg_MakeRational(nume, deno);
    }
    if (comp) return Sg_MakeComplex(Sg_MakeFlonum(0.0), Sg_Div(nume, deno));
    return Sg_Div(nume, deno);
  }
  if (SG_FLONUMP(obj)) {
    double s = SG_FLONUM_VALUE(obj);
    if (s < 0.0) return Sg_MakeComplex(Sg_MakeFlonum(0.0), Sg_MakeFlonum(sqrt(-s)));
    return Sg_MakeFlonum(sqrt(s));
  }
  if (SG_COMPLEXP(obj)) {
    double real, imag, m, x, y, s;
    SgComplex *cn = SG_COMPLEX(obj);
    if (Sg_ExactP(obj)) {
      SgObject m = Sg_Magnitude(cn);
      SgObject x = Sg_Div(Sg_Add(cn->real, m), SG_MAKE_INT(2));
      SgObject y = Sg_Div(cn->imag, SG_MAKE_INT(2));
      SgObject s = Sg_Sqrt(Sg_Div(m, Sg_Add(Sg_Mul(x, x), Sg_Mul(y, y))));
      return oprtr_norm_complex(Sg_Mul(x,  s), Sg_Mul(y, s));
    }
    real = Sg_GetDouble(cn->real);
    imag = Sg_GetDouble(cn->imag);
    m = sqrt(real * real + imag * imag);
    x = (real + m) / 2;
    y = imag / 2;
    s = sqrt(m / (x * x + y * y));
    Sg_MakeComplex(Sg_MakeFlonum(x * s), Sg_MakeFlonum(y * s));
  }
  wte(SG_INTERN("sqrt"), "number", obj);
  return SG_UNDEF;		/* dummy */
}

static inline SgObject exact_integer_sqrt(SgObject k)
{
  SgObject ik = Sg_Sqrt(k);
  if (Sg_FiniteP(ik)) {
    return Sg_Exact(Sg_Round(ik, SG_ROUND_FLOOR));
  } else {
    long len = Sg_IntegerLength(k);
    SgObject quo = Sg_Quotient(SG_MAKE_INT(len), SG_MAKE_INT(2), NULL);
    ASSERT(SG_INTP(quo));
    return Sg_Ash(SG_MAKE_INT(1), SG_INT_VALUE(quo));
  }
}

SgObject Sg_ExactIntegerSqrt(SgObject k)
{
  double d;

  if (!SG_EXACT_INTP(k)) {
    wte(SG_INTERN("exact-integer-sqrt"), "exact integer", k);
  }

  d = Sg_GetDouble(k);
  if (d < iexpt_2n53) {
    double t = floor(sqrt(d));
    SgObject s = Sg_Exact(Sg_MakeFlonum(t));
    return Sg_Values2(s, Sg_Sub(k, Sg_Mul(s, s)));
  } else {
    SgObject s = exact_integer_sqrt(k);
    while (TRUE) {
      SgObject s2 = Sg_Mul(s, s);
      if (Sg_NumCmp(k, s2) < 0) {
	s = Sg_Quotient(Sg_Add(s2, k), Sg_Mul(SG_MAKE_INT(2), s), NULL);
	continue;
      } else {
	SgObject s2p = Sg_Add(Sg_Add(s2, Sg_Mul(SG_MAKE_INT(2), s)),
			      SG_MAKE_INT(1));
	if (Sg_NumCmp(k, s2p) < 0) {
	  return Sg_Values2(s, Sg_Sub(k, s2));
	} else {
	  s = Sg_Quotient(Sg_Add(s2, k), Sg_Mul(SG_MAKE_INT(2), s), NULL);
	  continue;
	}
      }
    }
  }
}

int Sg_Sign(SgObject obj)
{
  int r = 0;
  if (SG_INTP(obj)) {
    long v = SG_INT_VALUE(obj);
    if (v > 0) r = 1;
    else if (v < 0) r = -1;
  } else if (SG_BIGNUMP(obj)) {
    r = SG_BIGNUM_GET_SIGN(obj);
  } else if (SG_FLONUMP(obj)) {
    double v = SG_FLONUM_VALUE(obj);
    if (v != 0.0) {
      r = v > 0.0 ? 1 : -1;
    }
  } else if (SG_RATIONALP(obj)) {
    return Sg_Sign(SG_RATIONAL(obj)->numerator);
  } else {
    wte(SG_INTERN("sign"), "real number", obj);
  }
  return r;
}

static inline unsigned long gcd_fixfix(unsigned long x, unsigned long y)
{
  while (y > 0) {
    unsigned long r = x % y;
    x = y;
    y = r;
  }
  return x;
}

static inline double gcd_floflo(double x, double y)
{
  if (x < 0) x = -x;
  if (y < 0) y = -y;
  if (x < y) { double t = x; x = y; y = t; }

  while (y > 0.0) {
    double r = fmod(x, y);
    x = y;
    y = r;
  }
  return x;
}

static unsigned long gcd_bigfix(SgBignum *x, unsigned long y)
{
  long rem;
  Sg_BignumDivSI(x, (signed long)y, &rem);
  if (rem < 0) rem = -rem;
  return gcd_fixfix(y, (unsigned long)rem);
}

SgObject Sg_Gcd(SgObject x, SgObject y)
{
  int ox = FALSE, oy = FALSE;
  long ix, iy;
  unsigned long ux, uy, ur;
  if (!Sg_IntegerP(x)) {
    wte(SG_INTERN("gcd"), "integer", x);
  }
  if (!Sg_IntegerP(y)) {
    wte(SG_INTERN("gcd"), "integer", y);
  }

  if (SG_FLONUMP(x) || SG_FLONUMP(y)) {
    return Sg_MakeFlonum(gcd_floflo(Sg_GetDouble(x), Sg_GetDouble(y)));
  }

  if (SG_MAKE_INT(0) == x) return y;
  if (SG_MAKE_INT(0) == y) return x;
  
  ix = Sg_GetIntegerClamp(x, SG_CLAMP_NONE, &ox);
  iy = Sg_GetIntegerClamp(y, SG_CLAMP_NONE, &oy);

  if (!ox && !oy) {
    ux = (ix < 0) ? - ix : ix;
    uy = (iy < 0) ? - iy : iy;
    if (ux >= uy) {
      ur = gcd_fixfix(ux, uy);
    } else {
      ur = gcd_fixfix(uy, ux);
    }
    return Sg_MakeIntegerU(ur);
  }

  if (!oy && iy != LONG_MIN) {
    ASSERT(SG_BIGNUMP(x));
    uy = (iy < 0) ? -iy : iy;
    ur = gcd_bigfix(SG_BIGNUM(x), uy);
    return Sg_MakeIntegerU(ur);
  }

  if (!ox && ix != LONG_MIN) {
    ASSERT(SG_BIGNUMP(y));
    ux = (ix < 0) ? -ix : ix;
    ur = gcd_bigfix(SG_BIGNUM(y), ux);
    return Sg_MakeIntegerU(ur);
  }
  ASSERT(SG_BIGNUMP(x) && SG_BIGNUMP(y));
  return Sg_BignumGcd(SG_BIGNUM(x), SG_BIGNUM(y));
  /*
  x = Sg_Abs(x);
  y = Sg_Abs(y);
  if (Sg_NumCmp(x, y) < 0) {SgObject t = x; x = y; y = t;}
  
  while (!SG_EQ(SG_MAKE_INT(0), y)) {
    SgObject r = Sg_Modulo(x, y, TRUE);
    x = y;
    y = r;
  }
  return x;
  */
}

SgObject Sg_Magnitude(SgObject z)
{
  if (SG_COMPLEXP(z)) {
    SgComplex *cn = SG_COMPLEX(z);
    if (Sg_ExactP(cn)) {
      if (Sg_ZeroP(cn->real)) return Sg_Magnitude(cn->imag);
      if (Sg_ZeroP(cn->imag)) return Sg_Magnitude(cn->real);
      return Sg_Sqrt(Sg_Add(Sg_Mul(cn->real, cn->real),
			    Sg_Mul(cn->imag, cn->imag)));
    } else {
      double real = Sg_GetDouble(cn->real);
      double imag = Sg_GetDouble(cn->imag);
      double m;
      if (isinf(real) || isinf(imag)) return SG_POSITIVE_INFINITY;
      m = sqrt(real * real + imag * imag);
      if (m < DBL_EPSILON || isinf(m)) return Sg_MakeFlonum(imag / sin(atan2(imag, real)));
      return Sg_MakeFlonum(m);
    }
  }
  if (SG_REALP(z)) {
    if (Sg_NegativeP(z)) return Sg_Negate(z);
    return z;
  }
  wte(SG_INTERN("magnitude"), "number", z);
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_Angle(SgObject obj)
{
  if (SG_COMPLEXP(obj)) {
    double real = Sg_GetDouble(SG_COMPLEX(obj)->real);
    double imag = Sg_GetDouble(SG_COMPLEX(obj)->imag);
    return Sg_MakeFlonum(atan2(imag, real));
  }
  if (SG_REALP(obj)) {
    if (Sg_NegativeP(obj)) return Sg_MakeFlonum(atan2(0.0, -1.0)); /* pi */
    if (SG_FLONUMP(obj)) return Sg_MakeFlonum(0.0);
    return SG_MAKE_INT(0);
  }
  wte(SG_INTERN("angle"), "number", obj);
  return SG_UNDEF;		/* dummy */
}

static inline SgObject log_handle_inf(SgObject n)
{
  /* passing argument must be always bignum */
  /* Sg_BignumSqrtApprox returns approximate 
     sqrt and returns always exact number */
  SgObject r = Sg_BignumSqrtApprox(n);
  return Sg_Add(Sg_Log(r), Sg_Log(r));  
}

SgObject Sg_Log(SgObject obj)
{
  double real;
  double imag;

  if (SG_INTP(obj)) {
    long value = SG_INT_VALUE(obj);
    if (value > 0) {
      if (value == 1) return SG_MAKE_INT(0);
      return Sg_MakeFlonum(log((double)value));
    }
    real = value;
    return Sg_MakeComplex(Sg_MakeFlonum(0.5 * log(real * real)),
			  Sg_MakeFlonum(atan2(0.0, real)));
  }
  if (SG_COMPLEXP(obj)) {
    SgObject or = SG_COMPLEX(obj)->real, oi = SG_COMPLEX(obj)->imag;

    if (!SG_BIGNUMP(or) && !SG_BIGNUMP(oi)) {
      real = Sg_GetDouble(or);
      imag = Sg_GetDouble(oi);
      return Sg_MakeComplex(Sg_MakeFlonum(0.5 * log(real * real + imag * imag)),
			    Sg_MakeFlonum(atan2(imag, real)));
    } else {
      /* do the same but with Scheme function */
      SgObject r2 = Sg_Mul(or, or);
      SgObject i2 = Sg_Mul(oi, oi);
      static SgObject HALF = NULL;
      if (!HALF) HALF = Sg_MakeFlonum(0.5);
      return Sg_MakeComplex(Sg_Mul(HALF, Sg_Log(Sg_Add(r2, i2))),
			    Sg_Atan2(oi, or));
    }
  }
  if (SG_REALP(obj)) {
    real = Sg_GetDouble(obj);
    if (isinf(real) && SG_BIGNUMP(obj)) return log_handle_inf(obj);
    if (real > 0) return Sg_MakeFlonum(log(real));
    imag = atan2(0.0, real);
    if (imag == 0.0) return Sg_MakeFlonum(0.5 * log(real * real));
    return Sg_MakeComplex(Sg_MakeFlonum(0.5 * log(real * real)),
			  Sg_MakeFlonum(imag));
  }
  wte(SG_INTERN("log"), "number", obj);
  return SG_UNDEF;		/* dummy */
}

void Sg_MinMax(SgObject arg0, SgObject args, SgObject *min, SgObject *max)
{
#define EXACTP(o) (SG_EXACT_INTP(o) || SG_RATIONALP(o))
  int inexact = !EXACTP(arg0);
  SgObject mi = arg0;
  SgObject ma = arg0;

  /* nan check */
  if (Sg_NanP(arg0)) {
    if (min) *min = arg0;
    if (max) *max = arg0;
    return;
  }

  for (;;) {
    if (!SG_REALP(arg0))
      wte(SG_INTERN("min/max"), "real number", arg0);
    if (SG_NULLP(args)) {
      if (min) {
	if (inexact && EXACTP(mi)) {
	  *min = Sg_Inexact(mi);
	} else {
	  *min = mi;
	}
      }
      if (max) {
	if (inexact && EXACTP(ma)) {
	  *max = Sg_Inexact(ma);
	} else {
	  *max = ma;
	}
      }
      return;
    }
    /* nan check */
    if (Sg_NanP(SG_CAR(args))) {
      if (min) *min = SG_CAR(args);
      if (max) *max = SG_CAR(args);
      return;
    }
    if (!EXACTP(SG_CAR(args))) inexact = TRUE;
    if (min && Sg_NumCmp(mi, SG_CAR(args)) > 0) {
      mi = SG_CAR(args);
    }
    if (max && Sg_NumCmp(ma, SG_CAR(args)) < 0) {
      ma = SG_CAR(args);
    }
    args = SG_CDR(args);
  }
#undef EXACTP
}

SgObject Sg_IntegerDiv(SgObject x, SgObject y)
{
  if (!SG_REALP(x)) wte(SG_INTERN("div"), "real number", x);
  if (!SG_REALP(y)) wte(SG_INTERN("div"), "real number", y);
  if (!Sg_FiniteP(x) || Sg_NanP(x)) {
    Sg_AssertionViolation(SG_INTERN("div"),
	  SG_MAKE_STRING("dividend must be neither infinite nor NaN"), x);
  }
  if (Sg_ZeroP(y)) {
    Sg_AssertionViolation(SG_INTERN("div"),
			  SG_MAKE_STRING("undefined for 0"), y);
  }
  
  if (SG_INTP(x)) {
    if (SG_INTP(y)) {
      long xx = SG_INT_VALUE(x);
      long yy = SG_INT_VALUE(y);
      long div;
      if (xx == 0) {
	div = 0;
      } else if (xx > 0) {
	div = xx / yy;
      } else if (yy > 0) {
	div = (xx - yy + 1) / yy;
      } else {
	div = (xx + yy + 1) / yy;
      }
      return Sg_MakeInteger(div);
    }
  }
  if (SG_FLONUMP(x) || SG_FLONUMP(y)) {
    double xx = Sg_GetDouble(x);
    double yy = Sg_GetDouble(y);
    return Sg_MakeFlonum((yy > 0.0) ? floor(xx / yy) : - floor(xx / -yy));
  }
  if (Sg_PositiveP(y)) return Sg_Round(Sg_Div(x, y), SG_ROUND_FLOOR);
  return Sg_Negate(Sg_Round(Sg_Div(x, Sg_Negate(y)), SG_ROUND_FLOOR));
}

SgObject Sg_IntegerDiv0(SgObject x, SgObject y)
{
  SgObject div, mod;
  if (!SG_REALP(x)) wte(SG_INTERN("div0"), "real number", x);
  if (!SG_REALP(y)) wte(SG_INTERN("div0"), "real number", y);
  if (!Sg_FiniteP(x) || Sg_NanP(x)) {
    Sg_AssertionViolation(SG_INTERN("div0"),
	  SG_MAKE_STRING("dividend must be neither infinite nor NaN"), x);
  }
  if (Sg_ZeroP(y)) {
    Sg_AssertionViolation(SG_INTERN("div0"),
	  SG_MAKE_STRING("undefined for 0"), y);
  }

  div = Sg_IntegerDiv(x, y);
  mod = Sg_Sub(x, Sg_Mul(div, y));
  if (Sg_NumCmp(mod, Sg_Magnitude(Sg_Div(y, SG_MAKE_INT(2)))) < 0) return div;
  if (Sg_PositiveP(y)) return Sg_Add(div, SG_MAKE_INT(1));
  return Sg_Sub(div, SG_MAKE_INT(1));
  
}

SgObject Sg_IntegerMod(SgObject x, SgObject y)
{
  /* only for bignum */
  if (SG_EXACT_INTP(x) && SG_EXACT_INTP(y)) {
    /* for performance we need to use Sg_Modulo */
    int xsign = Sg_Sign(x), ysign = Sg_Sign(y);
    if (ysign == 0) goto err;
    if (xsign > 0) {
      if (ysign > 0) return Sg_Modulo(x, y, TRUE);
      else return Sg_Modulo(x, y, TRUE);
    } else if (xsign < 0) {
      SgObject r = Sg_Modulo(x, y, TRUE);
      if (SG_EQ(SG_MAKE_INT(0), r)) return r;
      if (ysign > 0) return Sg_Add(r, y);
      else return Sg_Sub(r, y);
    } else if (xsign == 0) {
      return SG_MAKE_INT(0);
    }
  } else {
    SgObject d = Sg_IntegerDiv(x, y);
    SgObject m = Sg_Mul(d, y);
    return Sg_Sub(x, m);
  }

 err:
  /* y was 0 */
  wte(SG_INTERN("mod"), "non zero number", y);
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_IntegerMod0(SgObject x, SgObject y)
{
  /* only for bignum */
  if (SG_EXACT_INTP(x) && SG_EXACT_INTP(y)) {
    /* for performance we need to use Sg_Modulo */
    int xsign = Sg_Sign(x), ysign = Sg_Sign(y);
    SgObject r;
    if (ysign == 0) goto err;
    r = Sg_Modulo(x, y, TRUE);
    if (xsign >= 0) {
      if (ysign > 0) {
	if (Sg_NumCmp(Sg_Mul(r, SG_MAKE_INT(2)), y) >= 0)
	  return Sg_Sub(r, y);
	else return r;
      } else {
	if (Sg_NumCmp(Sg_Mul(r, SG_MAKE_INT(2)), Sg_Negate(y)) >= 0)
	  return Sg_Add(r, y);
	else return r;
      }
    } else {
      if (ysign > 0) {
	if (Sg_NumCmp(Sg_Mul(r, SG_MAKE_INT(2)), Sg_Negate(y)) < 0)
	  return Sg_Add(r, y);
	else return r;
      } else {
	if (Sg_NumCmp(Sg_Mul(r, SG_MAKE_INT(2)), y) < 0)
	  return Sg_Sub(r, y);
	else return r;
      }
    }
  } else {
    SgObject d = Sg_IntegerDiv0(x, y);
    SgObject m = Sg_Mul(d, y);
    return Sg_Sub(x, m);
  }
 err:
  /* y was 0 */
  wte(SG_INTERN("mod0"), "non zero number", y);
  return SG_UNDEF;		/* dummy */
}

/* for better performance */
SgObject Sg_ModInverse(SgObject x, SgObject m)
{
  SgObject u1, u3, v1, v3;
  int sign = 1;
  if (!SG_EXACT_INTP(x)) wte(SG_INTERN("mod-inverse"), "exact integer", x);
  if (!SG_EXACT_INTP(m)) wte(SG_INTERN("mod-inverse"), "exact integer", m);
  if (Sg_Sign(m) != 1) {
    wte(SG_INTERN("mod-inverse"), "positive number", m);
  }

  if (SG_BIGNUMP(x) && SG_BIGNUMP(m)) {
    return Sg_BignumModInverse(x, m);
  }
  u1 = SG_MAKE_INT(1);
  u3 = x;
  v1 = SG_MAKE_INT(0);
  v3 = m;

  while (!SG_EQ(v3, SG_MAKE_INT(0))) {
    SgObject t3, w, t1, q;
    t3 = Sg_IntegerMod(u3, v3);
    q = Sg_Quotient(u3, v3, NULL);
    w = Sg_Mul(q, v1);
    t1 = Sg_Add(u1, w);
    u1 = v1; v1 = t1; u3 = v3; v3 = t3;
    sign = -sign;
  }
  if (sign < 0) {
    return Sg_Sub(m, u1);
  } else {
    return u1;
  }
}

SgObject Sg_ModExpt(SgObject x, SgObject e, SgObject m)
{
  SgObject y;
  int invertp = FALSE;
  if (!SG_EXACT_INTP(x)) wte(SG_INTERN("mod-expt"), "exact integer", x);
  if (!SG_EXACT_INTP(e)) wte(SG_INTERN("mod-expt"), "exact integer", e);
  if (!SG_EXACT_INTP(m)) wte(SG_INTERN("mod-expt"), "exact integer", m);
  if (Sg_Sign(m) <= 0) {
    wte(SG_INTERN("mod-expt"), "positive number", m);
  }

  /* some obvious cases */
  if (SG_EQ(x, SG_MAKE_INT(0))) return SG_MAKE_INT(0);
  /* NOTE: (expt 0 0) = 1, so follow it */
  if (SG_EQ(e, SG_MAKE_INT(0))) return SG_MAKE_INT(1);
  
  /* TODO handle more efficiently */
  if (SG_INTP(x)) {
    if (SG_INTP(e)) {
      if (SG_INTP(m)) {
	/* can be done in C */
#if LONG_MAX > 1UL << 32
	goto small_entry;
#else
	int64_t yy = 1, n = SG_INT_VALUE(e), d = SG_INT_VALUE(m);
	int64_t xx = SG_INT_VALUE(x);
	if (n < 0) {
	  n = -n;
	  invertp = TRUE;
	}
	while (n > 0) {
	  if (n % 2) {
	    yy = (yy * xx) % d;
	  }
	  n >>= 1;
	  if (n > 0) {
	    xx = (xx * xx) % d;
	  }
	}
	y = Sg_MakeIntegerFromS64(yy);
	if (invertp) {
	  return Sg_ModInverse(y, m);
	} else {
	  return y;
	}
#endif
      }
      e = Sg_MakeBignumFromSI(SG_INT_VALUE(e));
    }
    x = Sg_MakeBignumFromSI(SG_INT_VALUE(x));
    if (SG_INTP(m)) {
      m = Sg_MakeBignumFromSI(SG_INT_VALUE(m));
    }
  } else if (SG_INTP(e)) {
    if (SG_BIGNUMP(x) && SG_BIGNUMP(m)) {
      /* if both are bignum, then the cost of mod and mul would be
	 more expensive than converting e to bignum */
      e = Sg_MakeBignumFromSI(SG_INT_VALUE(e));
    } else {
      long n;
#if LONG_MAX > 1UL << 32
  small_entry:
#endif
      n = SG_INT_VALUE(e);
      y = SG_MAKE_INT(1);
      if (n < 0) {
	invertp = TRUE;
	n = -n;
      }
      while (n > 0) {
	if (n % 2) {
	  y = Sg_IntegerMod(Sg_Mul(y, x), m);
	}
	n >>= 1;
	if (n > 0) {
	  x = Sg_IntegerMod(Sg_Mul(x, x), m);
	}
      }
      if (invertp) {
	return Sg_ModInverse(y, m);
      } else {
	return y;
      }
    }
  } else if (SG_INTP(m)) {
    /* both x and e are bignum */
    m = Sg_MakeBignumFromSI(SG_INT_VALUE(m));
  }
  if (!(SG_BIGNUMP(x) && SG_BIGNUMP(e) && SG_BIGNUMP(m))) {
    Sg_Error(UC("[internal] bignum required. %S %S %S"), x, e, m);
  }
  return Sg_BignumModExpt(SG_BIGNUM(x), SG_BIGNUM(e), SG_BIGNUM(m));
}

SgObject Sg_Square(SgObject x)
{
  if (SG_BIGNUMP(x)) {
    /* a bit of optimisation */
    return Sg_BignumSquare(SG_BIGNUM(x));
  }
  /* TODO rational and complex but it's rare i think...*/
  return Sg_Mul(x, x);
}

SgObject Sg_Round(SgObject num, int mode)
{
  if (SG_EXACT_INTP(num)) return num;
  if (SG_RATIONALP(num)) {
    int offset = 0;
    SgObject rem;
    SgObject quot = Sg_Quotient(SG_RATIONAL(num)->numerator,
				SG_RATIONAL(num)->denominator, &rem);
    /* this shouldn't happen but just in case */
    if (SG_EQ(rem, SG_MAKE_INT(0))) return quot;
    switch (mode) {
    case SG_ROUND_FLOOR:
      offset = (Sg_Sign(num) < 0) ? -1 : 0;
      break;
    case SG_ROUND_CEIL:
      offset = (Sg_Sign(num) < 0) ? 0 : 1;
      break;
    case SG_ROUND_TRUNC:
      offset = 0;
      break;
    case SG_ROUND_ROUND: {
      SgObject rem2 = Sg_Mul(Sg_Abs(rem), SG_MAKE_INT(2));
      int cmp = Sg_NumCmp(SG_RATIONAL(num)->denominator, rem2);
      if (cmp > 0) {
	/* NUM is closer to zero than halfway */
	offset = 0;
      } else if (cmp < 0) {
	/* NUM is further from zero than halfway */
	offset = (Sg_Sign(num) < 0) ? -1 : 1;
      } else {
	/* NUM is exactly the halfway. We round to even */
	if (Sg_OddP(quot)) {
	  offset = (Sg_Sign(num) < 0) ? -1 : 1;
	} else {
	  offset = 0;
	}
      }
      break;
    }
    default:
      Sg_Panic("something screwed up");
    }
    if (offset == 0) return quot;
    else return Sg_Add(quot, SG_MAKE_INT(offset));
  }
  if (SG_FLONUMP(num)) {
    double r = 0.0, v;
    v = SG_FLONUM_VALUE(num);
    switch (mode) {
    case SG_ROUND_FLOOR: r = floor(v); break;
    case SG_ROUND_CEIL: r = ceil(v); break;
    case SG_ROUND_TRUNC:
      r = (v < 0.0) ? ceil(v) : floor(v); break;
    case SG_ROUND_ROUND: r = roundeven(v); break;
    default:
      Sg_Panic("something screwed up");
    }
    return Sg_MakeFlonum(r);
  }
  wte(SG_INTERN("round"), "real number", num);
  return SG_UNDEF;
}

/* from gauche */
static inline int numcmp3(SgObject x, SgObject d, SgObject y)
{
  if (SG_INTP(x) && SG_INTP(d) && SG_INTP(y)) {
    long xd = SG_INT_VALUE(x) + SG_INT_VALUE(d);
    if (xd < SG_INT_VALUE(y)) return -1;
    if (xd > SG_INT_VALUE(y)) return 1;
    else return 0;
  } else {
    SgObject bx = SG_BIGNUMP(x) ? x : Sg_MakeBignumFromSI(SG_INT_VALUE(x));
    SgObject bd = SG_BIGNUMP(d) ? d : Sg_MakeBignumFromSI(SG_INT_VALUE(d));
    SgObject by = SG_BIGNUMP(y) ? y : Sg_MakeBignumFromSI(SG_INT_VALUE(y));
    return Sg_BignumCmp3U(SG_BIGNUM(bx), SG_BIGNUM(bd), SG_BIGNUM(by));
  }
}

#ifdef _MSC_VER
/* to shut compiler */
# define strcpy_ strcpy_s
#else
/* this works fine anyway */
# define strcpy_(buf, len, s) strcpy(buf, s)
#endif

static void double_print(char *buf, int buflen, double val, int plus_sign)
{
  SgObject f;
  int exp, sign;
  f = Sg_DecodeFlonum(val, &exp, &sign);
  if (val == 0.0) {
    if (plus_sign) strcpy_(buf, buflen, "+0.0");
    else if (sign < 0) strcpy_(buf, buflen, "-0.0");
    else strcpy_(buf, buflen,  "0.0");
    return;
  } else if (isinf(val)) {
    if (sign < 0) strcpy_(buf, buflen, "-inf.0");
    else strcpy_(buf, buflen, "+inf.0");
    return;
  } else if (isnan(val)) {
    strcpy_(buf, buflen, "+nan.0");
    return;
  }
  if (sign < 0) *buf++ = '-', buflen--;
  else if (plus_sign) *buf++ = '+', buflen--;

  {
    SgObject r, s, mp, mm, q;
    int est, tc1, tc2, tc3, digs, point, round;
    int mp2 = FALSE, fixup = FALSE;
    if (sign < 0) val = -val;
    /* initialize r, s, m+ and m- */
    
    round = !Sg_OddP(f);
    if (exp >= 0) {
      SgObject be = Sg_Ash(SG_MAKE_INT(1), exp);
      if (Sg_NumCmp(f, SG_2_52) != 0) {
	r = Sg_Ash(f, exp + 1);
	s = SG_MAKE_INT(2);
	mp2 = FALSE;
	mm = be;
      } else {
	r = Sg_Ash(f, exp + 2);
	s = SG_MAKE_INT(4);
	mp2 = TRUE;
	mm = be;
      }
    } else {
      if (exp == -1023 || Sg_NumCmp(f, SG_2_52) != 0) {
	r = Sg_Ash(f, 1);
	s = Sg_Ash(SG_MAKE_INT(1), -exp + 1);
	mp2 = FALSE;
	mm = SG_MAKE_INT(1);
      } else {
	r = Sg_Ash(f, 2);
	s = Sg_Ash(SG_MAKE_INT(1), -exp + 2);
	mp2 = TRUE;
	mm = SG_MAKE_INT(1);
      }
    }
    /* Sg_Printf(Sg_StandardErrorPort(),  UC("exp=%d, round=%d, r=%S, s=%S, mp=%d, mm=%S\n"), */
    /* 	      exp, round, r, s, mp, mm); */

    /* estimate scale */
    est = (int)ceil(log10(val) - 0.1);
    if (est >= 0) {
      s = Sg_Mul(s, Sg_Expt(SG_MAKE_INT(10), SG_MAKE_INT(est)));
    } else {
      SgObject scale = Sg_Expt(SG_MAKE_INT(10), SG_MAKE_INT(-est));
      r = Sg_Mul(r, scale);
      mm = Sg_Mul(mm, scale);
    }
    /* fixup. avoid calculating m+ for obvious case. */
    if (Sg_NumCmp(r, s) >= 0) {
      fixup = TRUE;
    } else {
      mp = (mp2 ? Sg_Ash(mm, 1) : mm);
      if (round) {
	fixup = (numcmp3(r, mp, s) >= 0);
      } else {
	fixup = (numcmp3(r, mp, s) > 0);
      }
    }
    if (fixup) {
      s = Sg_Mul(s, SG_MAKE_INT(10));
      est++;
    }
    /* Sg_Printf(Sg_StandardErrorPort(),  UC("est=%d, r=%S, s=%S, mp=%S, mm=%S\n"), */
    /* 	      est, r, s, mp, mm); */

    /* determine position of decimal point. */
    if (est < 10 && est > -3) {
      point = est; est = 1;
    } else {
      point = 1;
    }

    /* generate */
    if (point <= 0) {
      *buf++ = '0'; buflen--;
      *buf++ = '.', buflen--;
      for (digs = point; digs < 0 && buflen > 5; digs++) {
	*buf++ = '0'; buflen--;
      }
    }
    for (digs = 1; buflen > 5; digs++) {
      SgObject r10 = Sg_Mul(r, SG_MAKE_INT(10));
      q = Sg_Quotient(r10, s, &r);
      mm = Sg_Mul(mm, SG_MAKE_INT(10));
      mp = (mp2 ? Sg_Ash(mm, 1) : mm);

      /* Sg_Printf(Sg_StandardErrorPort(),  UC("q=%S, r=%S, mp=%S, mm=%S\n"), */
      /* 		q, r, mp, mm); */

      ASSERT(SG_INTP(q));
      if (round) {
	tc1 = (Sg_NumCmp(r, mm) <= 0);
	tc2 = (numcmp3(r, mp, s) >= 0);
      } else {
	tc1 = (Sg_NumCmp(r, mm) < 0);
	tc2 = (numcmp3(r, mp, s) > 0);
      }

      if (!tc1) {
	if (!tc2) {
	  *buf++ = (char)SG_INT_VALUE(q) + '0';
	  buflen--;
	  if (digs == point) *buf++ = '.', buflen--;
	  continue;
	} else {
	  *buf++ = (char)SG_INT_VALUE(q) + '1';
	  buflen--;
	  break;
	}
      } else {
	if (!tc2) {
	  *buf++ = (char)SG_INT_VALUE(q) + '0';
	  buflen--;
	  break;
	} else {
	  tc3 = numcmp3(r, r, s);
	  if ((round && tc3 <= 0) || (!round && tc3 < 0)) {
	    *buf++ = (char)SG_INT_VALUE(q) + '0';
	    buflen--;
	    break;
	  } else {
	    *buf++ = (char)SG_INT_VALUE(q) + '1';
	    buflen--;
	    break;
	  }
	}
      }
    }
    if (digs <= point) {
      for (; digs < point && buflen > 5; digs++) {
	*buf++ = '0', buflen--;
      }
      *buf++ = '.', buflen--;
      *buf++ = '0', buflen--;
    }
    est--;
    if (est != 0) {
      *buf++ = 'e', buflen--;
      snprintf(buf, buflen, "%d", (int)est);
    } else {
      *buf++ = 0;
    }
  }
}

static void number_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  SgObject s = Sg_NumberToString(obj, 10, FALSE);
  Sg_Puts(port, SG_STRING(s));
}

#define FLT_BUF 50

SgObject Sg_NumberToString(SgObject obj, int radix, int use_upper)
{
  SgObject r = SG_NIL;
  char buf[FLT_BUF];

  if (SG_INTP(obj)) {
    char *pbuf = buf;
    long value = SG_INT_VALUE(obj);
    if (value < 0) {
      *pbuf++ = '-';
      value = -value;
    }
    if (radix == 10) {
      snprintf(pbuf, FLT_BUF - 1, "%ld" , value);
    } else if (radix == 16) {
      snprintf(pbuf, FLT_BUF - 1, (use_upper ? "%lX" : "%lx"), value);
    } else if (radix == 8) {
      snprintf(pbuf, FLT_BUF - 1, "%lo", value);
    } else {
      /* should use better way */
      r = Sg_BignumToString(SG_BIGNUM(Sg_MakeBignumFromSI(SG_INT_VALUE(obj))),
			    radix, use_upper);
    }
    if (SG_NULLP(r)) r = Sg_MakeStringC(buf);
  } else if (SG_BIGNUMP(obj)) {
    r = Sg_BignumToString(SG_BIGNUM(obj), radix, use_upper);
  } else if (SG_FLONUMP(obj)) {
    double_print(buf, FLT_BUF, SG_FLONUM_VALUE(obj), FALSE);
    r = Sg_MakeStringC(buf);
  } else if (SG_RATIONALP(obj)) {
    SgObject nume, deno;
    nume = Sg_NumberToString(SG_RATIONAL(obj)->numerator, radix, use_upper);
    deno = Sg_NumberToString(SG_RATIONAL(obj)->denominator, radix, use_upper);
    r = Sg_StringAppend2(nume, SG_MAKE_STRING("/"));
    r = Sg_StringAppend2(r, deno);
  } else if (SG_COMPLEXP(obj)) {
    SgObject real, imag;
    int needPlus = FALSE;
    real = Sg_NumberToString(SG_COMPLEX(obj)->real, radix, use_upper);
    imag = Sg_NumberToString(SG_COMPLEX(obj)->imag, radix, use_upper);
    needPlus = (SG_STRING_VALUE_AT(imag, 0) != '+' &&
		SG_STRING_VALUE_AT(imag, 0) != '-');
    if (needPlus) {
      r = Sg_StringAppend(SG_LIST4(real, SG_MAKE_STRING("+"),
				   imag, SG_MAKE_STRING("i")));
    } else {
      r = Sg_StringAppend(SG_LIST3(real, imag, SG_MAKE_STRING("i")));
    }
  } else {
    wte(SG_INTERN("number->string"), "number", obj);
  }
  return r;
}

SgObject Sg__ConstObjes[SG_NUM_CONST_OBJS] = {SG_FALSE};

extern void Sg__InitBignum();
void Sg__InitNumber()
{
/* VC does not have these macros. SUCKS!! 
   Seems Solaris doesn't have it either...
 */
#if !defined(INFINITY) || !defined(NAN)
#undef INFINITY
#undef NAN
  static double __d0 = 0.0;
  static double __d1 = 1.0;
#define INFINITY (__d1/__d0)
#define NAN      (__d0/__d0)
#endif

  int radix, i;
  unsigned long n;

  for (radix = RADIX_MIN; radix <= RADIX_MAX; radix++) {
    longlimit[radix - RADIX_MIN] = 
      (unsigned long)floor((double)LONG_MAX / radix - radix);
    /* Find max D where R^(D+1)-1 <= LONG_MAX */
    for (i = 0, n = 1; ; i++, n *= radix) {
      if (n >= (unsigned long)(LONG_MAX / radix)) {
	longdigs[radix - RADIX_MIN] = i - 1;
	bigdig[radix - RADIX_MIN] = n;
	break;
      }
    }
  }

  SG_2_52   = Sg_MakeBignumFromS64(iexpt_2n52);
  
#define INIT_CONST_FL(o, d) o=make_flonum(d)
  INIT_CONST_FL(SG_NAN, NAN);
  INIT_CONST_FL(SG_POSITIVE_INFINITY, INFINITY);
  INIT_CONST_FL(SG_NEGATIVE_INFINITY, -INFINITY);
  INIT_CONST_FL(SG_FL_POSITIVE_ZERO, 0.0);
  INIT_CONST_FL(SG_FL_NEGATIVE_ZERO, -0.0);
  INIT_CONST_FL(SG_FL_POSITIVE_ONE, 1.0);
  INIT_CONST_FL(SG_FL_NEGATIVE_ONE, -1.0);

  Sg__InitBignum();
}

#define SG_DEFINE_LOG_OP_STUB(name, noarg, c_op, sname)			\
  static SgObject name(SgObject *args, int argc, void *data)		\
  {									\
    SgObject rest = args[argc - 1];					\
    SgObject r;								\
    int i;								\
    if (argc == 1) return SG_MAKE_INT(noarg); /* no argument */		\
    if (!SG_EXACT_INTP(args[0])) {					\
      wte(SG_INTERN(#sname), "exact integer", args[0]);			\
    }									\
    if (argc == 2) return args[0];	      /* 1 argument */		\
    r = c_op(args[0], args[1]);						\
    for (i = 2; i < argc - 1; i++) {					\
      r = c_op(r, args[i]);						\
    }									\
    if (!SG_NULLP(rest)) {						\
      SgObject cp;							\
      SG_FOR_EACH(cp, rest) {						\
	r = c_op(r, SG_CAR(cp));					\
      }									\
    }									\
    return r;								\
  }									\
  static SG_DEFINE_SUBR(SG_CPP_CAT(name, _stub), 0, 10, name, SG_FALSE, NULL)

/* bitwise-xor */
SG_DEFINE_LOG_OP_STUB(bitwise_ior, 0,  Sg_LogIor, bitwise-ior);
SG_DEFINE_LOG_OP_STUB(bitwise_and, -1, Sg_LogAnd, bitwise-and);
SG_DEFINE_LOG_OP_STUB(bitwise_xor, 0,  Sg_LogXor, bitwise-xor);

#undef SG_DEFINE_LOG_OP_STUB

void Sg__InitNumberProcs()
{
  SgLibrary *lib = Sg_FindLibrary(SG_INTERN("(core)"), FALSE);
#define INSERT_LOG_OP(name, sname)					\
  SG_PROCEDURE_NAME(&SG_CPP_CAT(name, _stub)) = SG_INTERN(sname);	\
  SG_PROCEDURE_TRANSPARENT(&SG_CPP_CAT(name, _stub)) = SG_PROC_TRANSPARENT; \
  Sg_InsertBinding(lib, SG_INTERN(sname), SG_OBJ(&SG_CPP_CAT(name, _stub)));

  INSERT_LOG_OP(bitwise_ior, "bitwise-ior");
  INSERT_LOG_OP(bitwise_and, "bitwise-and");
  INSERT_LOG_OP(bitwise_xor, "bitwise-xor");
}
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
