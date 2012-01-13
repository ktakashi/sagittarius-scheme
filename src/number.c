/* -*- C -*- */
/*
 * number.c
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
#include <string.h>
#include <float.h>
#include <math.h>
#include <ctype.h>
#define LIBSAGITTARIUS_BODY
#include "sagittarius/number.h"
#include "sagittarius/bignum.h"
#include "sagittarius/string.h"
#include "sagittarius/pair.h"
#include "sagittarius/core.h"
#include "sagittarius/unicode.h"
#include "sagittarius/error.h"
#include "sagittarius/numconst.h"
#include "sagittarius/arith.h"
#include "sagittarius/bits.h"
#include "sagittarius/values.h"
#include "sagittarius/vm.h"

#ifndef M_PI
#define M_PI 3.14159265358979323846
#endif

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

static double roundeven(double v);

static inline unsigned long ipow(int r, int n)
{
  unsigned long k;
  for (k = 1; n > 0; n--) k *= r;
  return k;
}

static double pow10n(double x, int n)
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

static SgObject intptr_to_integer(intptr_t value)
{
  if ((value <= SG_INT_MAX) && (value >= SG_INT_MIN)) return SG_MAKE_INT(value);
  return Sg_MakeBignumFromSI(value);
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
  if (SG_INTP(imag) && SG_INT_VALUE(imag) == 0) return real;
  if (SG_BIGNUMP(imag) && SG_BIGNUM_GET_SIGN(SG_BIGNUM(imag)) == 0) return real;
  if (SG_FLONUMP(real) + SG_FLONUMP(imag) == 1) {
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
  
  if (SG_RATIONALP(lhs)) {
    return Sg_MakeRational(oprtr_expt(SG_RATIONAL(lhs)->numerator, n),
			   oprtr_expt(SG_RATIONAL(lhs)->denominator, n));
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

static SgObject bn_demote(SgBignum *b)
{
  ASSERT(SG_BIGNUM_GET_SIGN(b) != 0);
  if (SG_BIGNUM_GET_COUNT(b) == 0) return SG_MAKE_INT(0);
  if (SG_BIGNUM_GET_COUNT(b) == 1) {
    unsigned long n = b->elements[0];
    int sign = SG_BIGNUM_GET_SIGN(b);
    if (!(sign < 0 && n < abs(SG_INT_MIN)) /* not smaller than fixnum_min */
	&& (n <= SG_INT_MAX)) {
      return SG_MAKE_INT(n);
    }
  }
  return b;
}

static SgObject integer_init_n_alloc(int64_t m, int shift_left)
{
  ASSERT(m >= 0);
  if (m == 0) return SG_MAKE_INT(0);
  else {
    SgObject b = Sg_MakeBignumFromS64(m);
    b = Sg_BignumShiftLeft(SG_BIGNUM(b), shift_left);
    SG_BIGNUM_SET_SIGN(b, 1);
    return bn_demote(SG_BIGNUM(b));
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
  if (m == iexpt_2n52) return ldexp((double)(iexpt_2n53 - 1), k - 1);
  return ldexp((double)(m - 1), k);
}

/* 
//  Reference:
//  William D. Clinger.
//  How to read floating point numbers accurately
//  Proceedings of the ACM SIGPLAN 1990 conference on Programming language design and implementation, p.92-101, June 1990
*/
static double algorithmR(SgObject f, const int e, const double z0)
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
      if (negP && m == iexpt_2n52 &&
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

static SgObject number_read_error(const char *msg, struct numread_packet *context)
{
  if (context->strict) {
    Sg_Error(UC("bad number format %s: %A"), msg,
	     Sg_MakeString(context->buffer, SG_HEAP_STRING));
  }
  return SG_FALSE;
}


static long longdigs[RADIX_MAX - RADIX_MIN + 1] = {0};
static unsigned long longlimit[RADIX_MAX - RADIX_MIN + 1] = {0};
static unsigned long bigdig[RADIX_MAX - RADIX_MIN + 1] = {0};

static SgObject read_uint(const SgChar **strp, int *lenp,
			  struct numread_packet *ctx,
			  SgObject initval)
{
  const SgChar *str = *strp;
  int digread = FALSE;
  int len = *lenp;
  int radix = ctx->radix;
  int digits = 0, diglimit = longdigs[radix - RADIX_MIN];
  unsigned long limit = longlimit[radix - RADIX_MIN],
                bdig = bigdig[radix - RADIX_MIN];
  unsigned long value_int = 0;
  SgBignum *value_big = NULL;
  SgChar c;
  static const char tab[] = "0123456789abcdefghijklmnopqrstuvwxyz";
  const char *ptab;

  if (!SG_FALSEP(initval)) {
    if (SG_INTP(initval)) {
      if ((uintptr_t)SG_INT_VALUE(initval) > limit) {
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

static SgObject read_real(const SgChar **strp, int *lenp,
			  struct numread_packet *ctx)
{
  int minusp = FALSE, exp_minusp = FALSE, exp_overflow = FALSE;
  int sign_seen = FALSE, has_fraction = FALSE, has_exponent = FALSE;
  int fracdigs = 0;
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
      int lensave;
      if ((*lenp) <= 1 || mark == *strp) return SG_FALSE;
      (*strp)++; (*lenp)--;
      lensave = *lenp;
      denom = read_uint(strp, lenp, ctx, SG_FALSE);
      if (SG_FALSEP(denom)) return SG_FALSE;
      if (SG_MAKE_INT(0) == denom) {
	if (lensave > *lenp) {
	  if (IS_EXACT(ctx)) {
	    return number_read_error("(exact infinity/nan is not supported.)", ctx);
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
      if (IS_INEXACT(ctx)) {
	return Sg_Inexact(Sg_Div(intpart, denom));
      } else {
	return Sg_MakeRational(intpart, denom);
      }
    }
  } else {
    intpart = SG_FALSE;		/* indicate there was no intpart */
  }

  /* read fractional part */
  if (**strp == '.') {
    int lensave;
    if (ctx->radix != 10) {
      return number_read_error("(only 10-based fraction is supported)", ctx);
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
    SgObject e = Sg_Mul(fraction, Sg_Expt(SG_MAKE_INT(10),
					  Sg_MakeInteger(exponent - fracdigs)));
    if (minusp) return Sg_Negate(e);
    else        return e;
  } else if (ctx->exactness == NOEXACT &&
	     !has_fraction && !has_exponent) {
    return intpart;
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
static SgObject read_number(const SgChar *str, int len, int radix, int strict)
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
      return Sg_MakeComplexPolar(realpart, angle);
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
      if (SG_FALSEP(imagpart) || len != 1 || *str != 'i') {
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
    v = SG_FLONUM(obj)->value;
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
    v = SG_FLONUM(obj)->value;
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
    v = SG_FLONUM(obj)->value;
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
    intptr_t v = SG_INT_VALUE(obj);
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
    double v = SG_FLONUM(obj)->value;
    int64_t maxval;
    if (v < 0) {
      if (!(clamp & SG_CLAMP_LO)) goto err;
      return 0;
    }

    SG_SET_UINT64_MAX(maxval);

    if (v > (double)maxval) {
      if (!(clamp & SG_CLAMP_HI)) goto err;
      return maxval;
    } else {
      return (int64_t)v;
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
  SG_SET_HEADER(z, TC_RATIONAL);
  z->numerator = nume;
  z->denominator = deno;
  return z;
}

SgObject Sg_MakeRational(SgObject numerator, SgObject denominator)
{
  SgRational *z;
  if(!Sg_ExactP(numerator)) {
    Sg_Error(UC("numerator must be an exact integer, but got %S"), numerator);
  }
  if(!Sg_ExactP(denominator)) {
    Sg_Error(UC("denominator must be an exact integer, but got %S"), denominator);
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
    Sg_Error(UC("exect rational number required, but got %S"), rational);
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

SgObject Sg_RationalAddSub(SgObject x, SgObject y, int subtract)
{
  SgObject nx = SG_RATIONALP(x) ? SG_RATIONAL(x)->numerator   : x;
  SgObject dx = SG_RATIONALP(x) ? SG_RATIONAL(x)->denominator : SG_MAKE_INT(1);
  SgObject ny = SG_RATIONALP(y) ? SG_RATIONAL(y)->numerator   : y;
  SgObject dy = SG_RATIONALP(y) ? SG_RATIONAL(y)->denominator : SG_MAKE_INT(1);
  SgObject gcd, fx, fy, nr, dr;

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
  SgObject nx = SG_RATIONALP(x) ? SG_RATIONAL(x)->numerator   : x;
  SgObject dx = SG_RATIONALP(x) ? SG_RATIONAL(x)->denominator : SG_MAKE_INT(1);
  SgObject ny = SG_RATIONALP(y) ? SG_RATIONAL(y)->numerator   : y;
  SgObject dy = SG_RATIONALP(y) ? SG_RATIONAL(y)->denominator : SG_MAKE_INT(1);
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
  SgFlonum *f = SG_NEW_ATOMIC(SgFlonum);
  SG_SET_HEADER(f, TC_FLONUM);
  f->value = d;
  return SG_OBJ(f);
}

/* well this is flonum cache. */
#if USE_CONST_FLONUM
#define DEFAULT_FLONUM_CACHE_COUNT 1000

static SgFlonum CONST_FLONUM[DEFAULT_FLONUM_CACHE_COUNT];

static SgFlonum * search_flonum_from_cache(double d)
{
  if (d > 0.0) {
    double intpart;
    double fract = modf(d, &intpart);
    if (fract == 0.0 && d < (double)DEFAULT_FLONUM_CACHE_COUNT) {
      return &(CONST_FLONUM[(int)d]);
    }
  }
  return make_flonum(d);
}

#endif

SgObject Sg_MakeFlonum(double d)
{
#if USE_CONST_FLONUM
  if (d == 0.0) {
    union { double f64; int64_t i64; } datum;
    datum.f64 = d;
    if (datum.i64 < 0) {
      return SG_FL_NEGATIVE_ZERO;
    } else {
      return SG_FL_POSITIVE_ZERO;
    }
  }
  if (isnan(d)) return SG_NAN;
  return search_flonum_from_cache(d);
#else
  return make_flonum(d);
#endif
}

static inline SgObject make_complex(SgObject real, SgObject imag)
{
  SgComplex *c;
  ASSERT(!SG_COMPLEXP(real));
  ASSERT(!SG_COMPLEXP(imag));
  c = SG_NEW(SgComplex);
  SG_SET_HEADER(c, TC_COMPLEX);
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
  if (SG_FLONUMP(obj)) return SG_FLONUM(obj)->value;
  else if (SG_INTP(obj)) return (double)SG_INT_VALUE(obj);
  else if (SG_BIGNUMP(obj)) return Sg_BignumToDouble(SG_BIGNUM(obj));
  else if (SG_RATIONAL(obj)) return Sg_RationalToDouble(obj);
  else if (SG_COMPLEXP(obj)) {
    SgComplex *c = SG_COMPLEX(obj);
    if (Sg_ZeroP(c->imag)) return Sg_GetDouble(c->real);
  }
  else return 0.0; 		/* should this be error? */
  return 0.0;			/* dummy  */
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
  double nume = Sg_GetDouble(obj->numerator); /* what if numerator is rational? */
  double deno = Sg_GetDouble(obj->denominator); /* what if denominator is rational? */
  if (isinf(nume) || isinf(deno)) {
    if (isinf(nume) && isinf(deno)) {
      int nume_bitsize = Sg_BignumBitSize(obj->numerator);
      int deno_bitsize = Sg_BignumBitSize(obj->denominator);
      int shift = (nume_bitsize > deno_bitsize) ? nume_bitsize - BITSIZE_TH : deno_bitsize - BITSIZE_TH;
      if (shift < 1) shift = 1;
      nume = Sg_GetDouble(Sg_BignumShiftRight(obj->numerator, shift));
      deno = Sg_GetDouble(Sg_BignumShiftRight(obj->denominator, shift));
    } else if (isinf(deno)) {
      int deno_bitsize = Sg_BignumBitSize(obj->denominator);
      int shift = deno_bitsize - BITSIZE_TH;
      if (shift < 1) shift = 1;
      nume = ldexp(nume, -shift);
      deno = Sg_GetDouble(Sg_BignumShiftRight(obj->denominator, shift));
    } else {
      int nume_bitsize = Sg_BignumBitSize(obj->numerator);
      int shift = nume_bitsize - BITSIZE_TH;
      if (shift < 1) shift = 1;
      nume = Sg_GetDouble(Sg_BignumShiftRight(obj->numerator, shift));
      deno = ldexp(deno, -shift);
    }
  }
  return nume / deno;
}

SgObject Sg_Numerator(SgObject x)
{
  int inexact;
  SgObject obj;
  if (!SG_NUMBERP(x)) Sg_Error(UC("number required, but got %S"), x);
  if (SG_FLONUMP(x) && SG_FLONUM(x)->value == 0.0) return x;
  inexact = SG_FLONUMP(x);
  obj = Sg_Exact(x);
  if (SG_RATIONALP(obj)) {
    if (inexact) return Sg_Inexact(SG_RATIONAL(obj)->numerator);
    return SG_RATIONAL(obj)->numerator;
  }
  return inexact ? Sg_Inexact(obj) : obj;
}

SgObject Sg_Denominator(SgObject x)
{
  int inexact;
  SgObject obj;
  if (!SG_NUMBERP(x)) Sg_Error(UC("number required, but got %S"), x);
  inexact = SG_FLONUMP(x);
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
  if (SG_FLONUMP(obj)) return SG_FLONUM(obj)->value == 0.0;
  if (SG_BIGNUMP(obj)) {
    ASSERT(SG_BIGNUM_GET_SIGN(obj) != 0);
    return FALSE;
  }
  if (SG_RATIONALP(obj)) {
    ASSERT(Sg_ZeroP(SG_RATIONAL(obj)->numerator) == FALSE);
    return FALSE;
  }
  if (SG_COMPLEXP(obj)) {
    SgComplex *c = SG_COMPLEX(obj);
    return Sg_ZeroP(c->real) && Sg_ZeroP(c->imag);
  }
  Sg_Error(UC("number required, but got %S"), obj);
  return -1;			/* dummy */
}

int Sg_IntegerP(SgObject obj)
{
  if (SG_INTP(obj) || SG_BIGNUMP(obj)) return TRUE;
  if (SG_RATIONALP(obj)) return FALSE; /* normalized ratnum never be integer */
  if (SG_FLONUMP(obj)) {
    double d = SG_FLONUM(obj)->value;
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
    return (fmod(SG_FLONUM(obj)->value, 2.0) != 0.0);
  }
  Sg_Error(UC("integer required, but got %S"), obj);
  return FALSE;			/* dummy */
}

int Sg_FiniteP(SgObject obj)
{
  return !Sg_InfiniteP(obj) && !Sg_NanP(obj);
}

int Sg_InfiniteP(SgObject obj)
{
  if (SG_FLONUMP(obj)) {
    double v = SG_FLONUM(obj)->value;
    return isinf(v);
  } else if (SG_COMPLEXP(obj)) {
    SgObject r = SG_COMPLEX(obj)->real;
   SgObject i = SG_COMPLEX(obj)->imag;
    return Sg_InfiniteP(r) || Sg_InfiniteP(i);
  } else if (!SG_NUMBERP(obj)) {
    Sg_Error(UC("number required, but got %S"), obj);
  }
  return FALSE;
}

int Sg_NanP(SgObject obj)
{
  if (SG_FLONUMP(obj)) {
    double v = SG_FLONUM(obj)->value;
    return isnan(v);
  } else if (SG_COMPLEXP(obj)) {
    SgObject r = SG_COMPLEX(obj)->real;
   SgObject i = SG_COMPLEX(obj)->imag;
    return Sg_NanP(r) || Sg_NanP(i);
  } else if (!SG_NUMBERP(obj)) {
    Sg_Error(UC("number required, but got %S"), obj);
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
    intptr_t n = SG_INT_VALUE(obj);
    if (n == SG_INT_MIN) return intptr_to_integer(-n);
    return SG_MAKE_INT(-n);
  }
  if (SG_FLONUMP(obj)) {
    return Sg_MakeFlonum(-SG_FLONUM(obj)->value);
  }
  if (SG_BIGNUMP(obj)) {
    SgBignum *b = Sg_BignumCopy(obj);
    SG_BIGNUM_SET_SIGN(b, -SG_BIGNUM_GET_SIGN(obj));
    return oprtr_norm_integer(b);
  }
  if (SG_RATIONALP(obj)) {
    SgRational *r = SG_RATIONAL(obj);
    return Sg_MakeRational(Sg_Negate(r->numerator), r->denominator);
  }
  if (SG_COMPLEXP(obj)) {
    SgComplex *c = SG_COMPLEX(obj);
    return Sg_MakeComplex(Sg_Negate(c->real), Sg_Negate(c->imag));
  }
  Sg_Error(UC("number required, but got %S"), obj);
  return SG_UNDEF;		/* dummy */
}

int Sg_NegativeP(SgObject obj)
{
  if (SG_INTP(obj)) return SG_INT_VALUE(obj) < 0;
  if (SG_BIGNUMP(obj)) return SG_BIGNUM_GET_SIGN(obj) < 0;
  if (SG_FLONUMP(obj)) return SG_FLONUM(obj)->value < 0.0;
  if (SG_RATIONALP(obj)) return Sg_NegativeP(SG_RATIONAL(obj)->numerator);
  if (SG_COMPLEXP(obj)) return Sg_NegativeP(SG_COMPLEX(obj)->real);
  Sg_Error(UC("number required, but got %S"), obj);
  return FALSE;			/* dummy */
}

int Sg_PositiveP(SgObject obj)
{
  if (SG_INTP(obj)) return SG_INT_VALUE(obj) > 0;
  if (SG_BIGNUMP(obj)) return SG_BIGNUM_GET_SIGN(obj) > 0;
  if (SG_FLONUMP(obj)) return SG_FLONUM(obj)->value > 0.0;
  if (SG_RATIONALP(obj)) return Sg_PositiveP(SG_RATIONAL(obj)->numerator);
  if (SG_COMPLEXP(obj)) return Sg_PositiveP(SG_COMPLEX(obj)->real);
  Sg_Error(UC("number required, but got %S"), obj);
  return FALSE;			/* dummy */
}

SgObject Sg_Exact(SgObject obj)
{
  if (SG_FLONUMP(obj)) {
    double d = SG_FLONUM(obj)->value;
    double f, i;
    if ((f = modf(d, &i)) == 0.0) {
      if (d < SG_INT_MIN || d > SG_INT_MAX) {
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
  Sg_Error(UC("number required, but got %S"), obj);
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
    Sg_Error(UC("number required, but got %S"), obj);
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
  Sg_Error(UC("number required, but got %S"), obj);
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
  Sg_Error(UC("number required, but got %S"), obj);
  return FALSE;
}

SgObject Sg_Inverse(SgObject obj)
{
  if (SG_INTP(obj)) {
    if (SG_INT_VALUE(obj) == 0) Sg_Error(UC("inverse required not 0 number."));
    if (SG_INT_VALUE(obj) > 0) {
      if (SG_INT_VALUE(obj) == 1) return obj;
      return Sg_MakeRational(SG_MAKE_INT(1), obj);
    }
    if (obj == SG_MAKE_INT(-1)) return obj;
    return Sg_MakeRational(SG_MAKE_INT(-1), Sg_Negate(obj));
  }
  if (SG_FLONUMP(obj)) return Sg_MakeFlonum(1.0 / SG_FLONUM(obj)->value);
  if (SG_BIGNUMP(obj)) {
    if (SG_BIGNUM_GET_SIGN(obj) == 0) Sg_Error(UC("inverse required not 0 number."));
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
  Sg_Error(UC("number required, bot got %S"), obj);
  return SG_UNDEF;		/* dummy */
}

static inline int integer_length_rec(SgObject n)
{
  int n2;
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

int Sg_IntegerLength(SgObject n)
{
  if (!Sg_IntegerP(n)) Sg_Error(UC("integer required, but got %S"), n);
  return integer_length_rec(n);
}

SgObject Sg_Ash(SgObject x, int count)
{
  if (SG_INTP(x)) {
    intptr_t ix = SG_INT_VALUE(x);
    if (count <= -(SIZEOF_LONG * 8)) {
      ix = (ix < 0) ? -1 : 0;
      return Sg_MakeInteger(ix);
    } else if (count < 0) {
      if (ix < 0) {
	ix = ~((~ix) >> (-count));
      } else {
	ix >>= -count;
      }
      return Sg_MakeInteger(ix);
    } else if (count < SG_INT_MIN) {
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
    {
      SgObject big = Sg_MakeBignumFromSI(ix);
      return Sg_BignumAsh(SG_BIGNUM(big), count);
    }
  } else if (SG_BIGNUMP(x)) {
    return Sg_BignumAsh(SG_BIGNUM(x), count);
  }
  Sg_Error(UC("exact integer required, but got %S"), x);
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_LogNot(SgObject x)
{
  if (!SG_EXACT_INTP(x)) Sg_Error(UC("exact integer required, but got %S"), x);
  if (SG_INTP(x)) {
    /* this won't cause an overflow */
    return SG_MAKE_INT(~SG_INT_VALUE(x));
  } else {
    return Sg_Negate(Sg_BignumAddSI(SG_BIGNUM(x), 1));
  }
}

SgObject Sg_LogAnd(SgObject x, SgObject y)
{
  if (!SG_EXACT_INTP(x)) Sg_Error(UC("exact integer required, but got %S"), x);
  if (!SG_EXACT_INTP(y)) Sg_Error(UC("exact integer required, but got %S"), y);
  if (SG_INTP(x)) {
    if (SG_INTP(y)) {
      return SG_MAKE_INT(SG_INT_VALUE(x) & SG_INT_VALUE(y));
    } else if (SG_INT_VALUE(x) >= 0 && SG_BIGNUM_GET_SIGN(y) >= 0) {
      return Sg_MakeInteger(SG_INT_VALUE(x) & SG_BIGNUM(y)->elements[0]);
    }
    x = Sg_MakeBignumFromSI(SG_INT_VALUE(x));
  } else if (SG_INTP(y)) {
    if (SG_INT_VALUE(y) >= 0 && SG_BIGNUM_GET_SIGN(x) >= 0) {
      return Sg_MakeInteger(SG_INT_VALUE(y) & SG_BIGNUM(x)->elements[0]);
    }
    y = Sg_MakeBignumFromSI(SG_INT_VALUE(y));
  }
  return Sg_BignumLogAnd(SG_BIGNUM(x), SG_BIGNUM(y));
}

SgObject Sg_LogIor(SgObject x, SgObject y)
{
  if (!SG_EXACT_INTP(x)) Sg_Error(UC("exact integer required, but got %S"), x);
  if (!SG_EXACT_INTP(y)) Sg_Error(UC("exact integer required, but got %S"), y);
  if (SG_INTP(x)) {
    if (SG_INTP(y)) {
      return SG_MAKE_INT(SG_INT_VALUE(x) | SG_INT_VALUE(y));
    } else {
      x = Sg_MakeBignumFromSI(SG_INT_VALUE(x));
    }
  } else {
    if (SG_INTP(y)) y = Sg_MakeBignumFromSI(SG_INT_VALUE(y));
  }
  return Sg_BignumLogIor(SG_BIGNUM(x), SG_BIGNUM(y));
}

SgObject Sg_LogXor(SgObject x, SgObject y)
{
  if (!SG_EXACT_INTP(x)) Sg_Error(UC("exact integer required, but got %S"), x);
  if (!SG_EXACT_INTP(y)) Sg_Error(UC("exact integer required, but got %S"), y);
  if (SG_INTP(x)) {
    if (SG_INTP(y)) {
      return SG_MAKE_INT(SG_INT_VALUE(x) ^ SG_INT_VALUE(y));
    } else {
      x = Sg_MakeBignumFromSI(SG_INT_VALUE(x));
    }
  } else {
    if (SG_INTP(y)) y = Sg_MakeBignumFromSI(SG_INT_VALUE(y));
  }
  return Sg_BignumLogXor(SG_BIGNUM(x), SG_BIGNUM(y));
}

int Sg_BitCount(SgObject x)
{
  if (!SG_EXACT_INTP(x)) Sg_Error(UC("exact integer required, but got %S"), x);
  if (SG_INTP(x)) {
    intptr_t n = SG_INT_VALUE(x);
    if (n > 0) {
      return nbits(n);
    } else {
      return ~nbits(~n);
    }
  } else {
    return Sg_BignumBitCount(SG_BIGNUM(x));
  }
}

int Sg_BitSize(SgObject x)
{
  if (!SG_EXACT_INTP(x)) Sg_Error(UC("exact integer required, but got %S"), x);
  if (SG_INTP(x)) {
    intptr_t n = SG_INT_VALUE(x), n2;
    if (n == 0) return 0;
    n2 = (n < 0) ? ~n : n;
    return WORD_BITS - nlz(n2);
  } else {
    if (SG_BIGNUM_GET_SIGN(x) > 0) return Sg_BignumBitSize(SG_BIGNUM(x));
    else return Sg_BitSize(Sg_LogNot(x));
  }  
}

int Sg_FirstBitSet(SgObject x)
{
  if (!SG_EXACT_INTP(x)) Sg_Error(UC("exact integer required, but got %S"), x);
  if (SG_INTP(x)) {
    intptr_t n = SG_INT_VALUE(x);
    int bit;
    if (n == 0) return -1;
    bit = 0;
    bit += ntz(n);
    return bit;
  } else {
    return Sg_BignumFirstBitSet(SG_BIGNUM(x));
  }
}

SgObject Sg_Add(SgObject x, SgObject y)
{
  if (SG_INTP(x)) {
    if (SG_INTP(y)) {
      intptr_t r = SG_INT_VALUE(x) + SG_INT_VALUE(y);
      return Sg_MakeInteger(r);
    }
    if (SG_BIGNUMP(y)) {
      if (x == SG_MAKE_INT(0)) return y;
      return Sg_BignumAddSI(SG_BIGNUM(y), SG_INT_VALUE(x));
    }
    if (SG_RATIONALP(y)) {
      if (x == SG_MAKE_INT(0)) return y;
      return Sg_RationalAdd(SG_RATIONAL(y), x);
    }
    if (SG_FLONUMP(y)) {
      return Sg_MakeFlonum((double)SG_INT_VALUE(x) + SG_FLONUM(y)->value);
    }
    if (SG_COMPLEXP(y)) {
      if (x == SG_MAKE_INT(0)) return y;
      return Sg_MakeComplex(Sg_Add(SG_COMPLEX(y)->real, x), SG_COMPLEX(y)->imag);
    }
  }
  else if (SG_FLONUMP(x)) {
    if (SG_INTP(y)) {
      double z;
      if (y == SG_MAKE_INT(0)) return x;
      z = SG_FLONUM(x)->value + (double)SG_INT_VALUE(y);
      return Sg_MakeFlonum(z);
    }
    if (SG_BIGNUMP(y) || SG_RATIONALP(y)) {
      return Sg_MakeFlonum(SG_FLONUM(x)->value + Sg_GetDouble(y));
    }
    if (SG_FLONUMP(y)) {
      if (SG_FLONUM(x)->value == 0.0) return y;
      if (SG_FLONUM(y)->value == 0.0) return x;
      return Sg_MakeFlonum(SG_FLONUM(x)->value + SG_FLONUM(y)->value);
    }
    if (SG_COMPLEXP(y)) {
      return Sg_MakeComplex(Sg_Add(SG_COMPLEX(y)->real, x), SG_COMPLEX(y)->imag);
    }
  }
  else if (SG_BIGNUMP(x)) {
    if (SG_INTP(y)) {
      if (y == SG_MAKE_INT(0)) return x;
      return Sg_BignumAddSI(SG_BIGNUM(x), SG_INT_VALUE(y));
    }
    if (SG_BIGNUMP(y)) {
      return Sg_BignumAdd(SG_BIGNUM(x), SG_BIGNUM(y));
    }
    if (SG_RATIONALP(y)) {
      return Sg_RationalAdd(x, y);
    }
    if (SG_FLONUMP(y)) {
      return Sg_MakeFlonum(Sg_BignumToDouble(x) + SG_FLONUM(y)->value);
    }
    if (SG_COMPLEXP(y)) {
      return Sg_MakeComplex(Sg_Add(SG_COMPLEX(y)->real, x), SG_COMPLEX(y)->imag);
    }
  }
  else if (SG_RATIONALP(x)) {
    if (SG_INTP(y)) {
      if (y == SG_MAKE_INT(0)) return x;
      return Sg_RationalAdd(x, y);
    }
    if (SG_BIGNUMP(y) || SG_RATIONALP(y)) {
      return Sg_RationalAdd(x, y);
    }
    if (SG_FLONUMP(y)) {
      return Sg_MakeFlonum(Sg_GetDouble(x) + SG_FLONUM(y)->value);
    }
    if (SG_COMPLEXP(y)) {
      return Sg_MakeComplex(Sg_Add(SG_COMPLEX(y)->real, x), SG_COMPLEX(y)->imag);
    }
  }
  else if (SG_COMPLEXP(x)) {
    if (SG_INTP(y)) {
      if (y == SG_MAKE_INT(0)) return x;
      return Sg_MakeComplex(Sg_Add(SG_COMPLEX(x)->real, y),
			    SG_COMPLEX(x)->imag);
    }
    if (SG_BIGNUMP(y) || SG_RATIONALP(y)) {
      return Sg_MakeComplex(Sg_Add(SG_COMPLEX(x)->real, y),
			    SG_COMPLEX(x)->imag);
      
    }
    if (SG_FLONUMP(y)) {
      return Sg_MakeComplex(Sg_Add(SG_COMPLEX(x)->real, y),
			    Sg_Inexact(SG_COMPLEX(x)->imag));
    }
    if (SG_COMPLEXP(y)) {
      SgObject real = Sg_Add(SG_COMPLEX(x)->real, SG_COMPLEX(y)->real);
      SgObject imag = Sg_Add(SG_COMPLEX(x)->imag, SG_COMPLEX(y)->imag);
      return oprtr_norm_complex(real, imag);
    }
  }
  Sg_Error(UC("Sg_Add: wrong type of argument(x %S, y %S)"), x, y);
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_Sub(SgObject x, SgObject y)
{
  if (SG_INTP(x)) {
    if (SG_INTP(y)) {
      intptr_t r = SG_INT_VALUE(x) - SG_INT_VALUE(y);
      return Sg_MakeInteger(r);
    }
    if (SG_BIGNUMP(y)) {
      SgObject big = Sg_MakeBignumFromSI(SG_INT_VALUE(x));
      return Sg_BignumSub(SG_BIGNUM(big), SG_BIGNUM(y));
    }
    if (SG_RATIONALP(y)) {
      if (x == SG_MAKE_INT(0)) return y;
      return Sg_RationalSub(x, y);
    }
    if (SG_FLONUMP(y)) {
      return Sg_MakeFlonum((double)SG_INT_VALUE(x) - SG_FLONUM(y)->value);
    }
    if (SG_COMPLEXP(y)) {
      if (x == SG_MAKE_INT(0)) return y;
      return Sg_MakeComplex(Sg_Sub(x, SG_COMPLEX(y)->real), SG_COMPLEX(y)->imag);
    }
  }
  else if (SG_FLONUMP(x)) {
    if (SG_INTP(y)) {
      double z;
      if (y == SG_MAKE_INT(0)) return x;
      z = SG_FLONUM(x)->value - (double)SG_INT_VALUE(y);
      return Sg_MakeFlonum(z);
    }
    if (SG_BIGNUMP(y) || SG_RATIONALP(y)) {
      return Sg_MakeFlonum(SG_FLONUM(x)->value - Sg_GetDouble(y));
    }
    if (SG_FLONUMP(y)) {
      if (SG_FLONUM(y)->value == 0.0) return x;
      return Sg_MakeFlonum(SG_FLONUM(x)->value - SG_FLONUM(y)->value);
    }
    if (SG_COMPLEXP(y)) {
      return Sg_MakeComplex(Sg_Sub(x, SG_COMPLEX(y)->real), SG_COMPLEX(y)->imag);
    }
  }
  else if (SG_BIGNUMP(x)) {
    if (SG_INTP(y)) {
      if (y == SG_MAKE_INT(0)) return x;
      return Sg_BignumSubSI(SG_BIGNUM(x), SG_INT_VALUE(y));
    }
    if (SG_BIGNUMP(y)) {
      return Sg_BignumSub(SG_BIGNUM(x), SG_BIGNUM(y));
    }
    if (SG_RATIONALP(y)) {
      return Sg_RationalSub(x, y);
    }
    if (SG_FLONUMP(y)) {
      return Sg_MakeFlonum(Sg_BignumToDouble(x) - SG_FLONUM(y)->value);
    }
    if (SG_COMPLEXP(y)) {
      return Sg_MakeComplex(Sg_Sub(x, SG_COMPLEX(y)->real), SG_COMPLEX(y)->imag);
    }
  }
  else if (SG_RATIONALP(x)) {
    if (SG_INTP(y)) {
      if (y == SG_MAKE_INT(0)) return x;
      return Sg_RationalSub(x, y);
    }
    if (SG_BIGNUMP(y) || SG_RATIONALP(y)) {
      return Sg_RationalSub(x, y);
    }
    if (SG_FLONUMP(y)) {
      return Sg_MakeFlonum(Sg_GetDouble(x) - SG_FLONUM(y)->value);
    }
    if (SG_COMPLEXP(y)) {
      return Sg_MakeComplex(Sg_Sub(x, SG_COMPLEX(y)->real), SG_COMPLEX(y)->imag);
    }
  }
  else if (SG_COMPLEXP(x)) {
    if (SG_INTP(y)) {
      if (y == SG_MAKE_INT(0)) return x;
      return Sg_MakeComplex(Sg_Sub(SG_COMPLEX(x)->real, y),
			    SG_COMPLEX(x)->imag);
    }
    if (SG_BIGNUMP(y) || SG_RATIONALP(y)) {
      return Sg_MakeComplex(Sg_Sub(SG_COMPLEX(x)->real, y),
			    SG_COMPLEX(x)->imag);
      
    }
    if (SG_FLONUMP(y)) {
      return Sg_MakeComplex(Sg_Sub(SG_COMPLEX(x)->real, y),
			    Sg_Inexact(SG_COMPLEX(x)->imag));
    }
    if (SG_COMPLEXP(y)) {
      SgObject real = Sg_Sub(SG_COMPLEX(x)->real, SG_COMPLEX(y)->real);
      SgObject imag = Sg_Sub(SG_COMPLEX(x)->imag, SG_COMPLEX(y)->imag);
      return oprtr_norm_complex(real, imag);
    }
  }
  Sg_Error(UC("Sg_Sub: wrong type of argument(x %S, y %S)"), x, y);
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_Mul(SgObject x, SgObject y)
{
  if (SG_INTP(x)) {
    if (SG_INTP(y)) {
      intptr_t v0 = SG_INT_VALUE(x);
      intptr_t v1 = SG_INT_VALUE(y);
      intptr_t k = v0 * v1;
      if ((v1 != 0 && k / v1 != v0) || !(k >= SG_INT_MIN && k <= SG_INT_MAX)) {
	SgObject big = Sg_MakeBignumFromSI(v0);
	return Sg_BignumMulSI(SG_BIGNUM(big), v1);
      } else 
	return Sg_MakeInteger(k);
    }
    if (SG_BIGNUMP(y)) {
      if (x == SG_MAKE_INT(0)) return x;
      if (x == SG_MAKE_INT(1)) return y;
      return Sg_BignumMulSI(SG_BIGNUM(y), SG_INT_VALUE(x));
    }
    if (SG_RATIONALP(y)) {
      if (x == SG_MAKE_INT(0)) return x;
      if (x == SG_MAKE_INT(1)) return y;
      return Sg_RationalMul(x, y);
    }
    if (SG_FLONUMP(y)) {
      if (x == SG_MAKE_INT(0)) return x;
      if (x == SG_MAKE_INT(1)) return y;
      return Sg_MakeFlonum((double)SG_INT_VALUE(x) * SG_FLONUM(y)->value);
    }
    if (SG_COMPLEXP(y)) {
      if (x == SG_MAKE_INT(0)) return x;
      if (x == SG_MAKE_INT(1)) return y;
      return Sg_MakeComplex(Sg_Mul(x, SG_COMPLEX(y)->real),
			    Sg_Mul(x, SG_COMPLEX(y)->imag));
    }
  }
  else if (SG_FLONUMP(x)) {
    if (SG_INTP(y)) {
      double z;
      if (x == SG_MAKE_INT(0)) return x;
      if (x == SG_MAKE_INT(1)) return y;
      z = SG_FLONUM(x)->value * (double)SG_INT_VALUE(y);
      return Sg_MakeFlonum(z);
    }
    if (SG_BIGNUMP(y) || SG_RATIONALP(y)) {
      return Sg_MakeFlonum(SG_FLONUM(x)->value * Sg_GetDouble(y));
    }
    if (SG_FLONUMP(y)) {
      if (SG_FLONUM(y)->value == 1.0) return x;
      return Sg_MakeFlonum(SG_FLONUM(x)->value * SG_FLONUM(y)->value);
    }
    if (SG_COMPLEXP(y)) {
      return Sg_MakeComplex(Sg_Mul(x, SG_COMPLEX(y)->real),
			    Sg_Mul(x, SG_COMPLEX(y)->imag));
    }
  }
  else if (SG_BIGNUMP(x)) {
    if (SG_INTP(y)) {
      if (y == SG_MAKE_INT(0)) return y;
      return Sg_BignumMulSI(SG_BIGNUM(x), SG_INT_VALUE(y));
    }
    if (SG_BIGNUMP(y)) {
      return Sg_BignumMul(SG_BIGNUM(x), SG_BIGNUM(y));
    }
    if (SG_RATIONALP(y)) {
      return Sg_RationalMul(x, y);
    }
    if (SG_FLONUMP(y)) {
      return Sg_MakeFlonum(Sg_BignumToDouble(x) * SG_FLONUM(y)->value);
    }
    if (SG_COMPLEXP(y)) {
      return Sg_MakeComplex(Sg_Mul(x, SG_COMPLEX(y)->real),
			    Sg_Mul(x, SG_COMPLEX(y)->imag));
    }
  }
  else if (SG_RATIONALP(x)) {
    if (SG_INTP(y)) {
      if (x == SG_MAKE_INT(0)) return x;
      if (x == SG_MAKE_INT(1)) return y;
      return Sg_RationalMul(x, y);
    }
    if (SG_BIGNUMP(y) || SG_RATIONALP(y)) {
      return Sg_RationalMul(x, y);
    }
    if (SG_FLONUMP(y)) {
      return Sg_MakeFlonum(Sg_GetDouble(x) * SG_FLONUM(y)->value);
    }
    if (SG_COMPLEXP(y)) {
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
    if (SG_BIGNUMP(y) || SG_RATIONALP(y)) {
      return Sg_MakeComplex(Sg_Mul(SG_COMPLEX(x)->real, y),
			    Sg_Mul(SG_COMPLEX(x)->imag, y));
    }
    if (SG_FLONUMP(y)) {
      return Sg_MakeComplex(Sg_Mul(SG_COMPLEX(x)->real, y),
			    Sg_Mul(SG_COMPLEX(x)->imag, y));
    }
    if (SG_COMPLEXP(y)) {
      SgObject real = Sg_Sub(Sg_Mul(SG_COMPLEX(x)->real, SG_COMPLEX(y)->real), Sg_Mul(SG_COMPLEX(x)->imag, SG_COMPLEX(y)->imag));
      SgObject imag = Sg_Add(Sg_Mul(SG_COMPLEX(x)->imag, SG_COMPLEX(y)->real), Sg_Mul(SG_COMPLEX(x)->real, SG_COMPLEX(y)->imag));
      return oprtr_norm_complex(real, imag);
    }
  }
  Sg_Error(UC("Sg_Mul: wrong type of argument(x %S, y %S)"), x, y);
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_Div(SgObject x, SgObject y)
{
  SgObject real, imag;
  if (SG_INTP(x)) {
    if (SG_INTP(y)) {
      if (y == SG_MAKE_INT(0)) goto a_normal;
      if (x == SG_MAKE_INT(0)) return x;
      if (y == SG_MAKE_INT(1)) return x;
      return Sg_MakeRational(x, y);
    }
    if (SG_BIGNUMP(y)) {
      if (x == SG_MAKE_INT(0)) return x;
      return Sg_MakeRational(x, y);
    }
    if (SG_RATIONALP(y)) {
      return Sg_MakeRational(Sg_Mul(x, SG_RATIONAL(y)->denominator),
			     SG_RATIONAL(y)->numerator);
    }
    if (SG_FLONUMP(y)) {
      if (SG_FLONUM(y)->value == 0.0) goto a_normal;
      return Sg_MakeFlonum((double)SG_INT_VALUE(x) / SG_FLONUM(y)->value);
    }
    if (SG_COMPLEXP(y)) {
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
      if (y == SG_MAKE_INT(1)) return x;
      return Sg_MakeFlonum(SG_FLONUM(x)->value / SG_INT_VALUE(y));
    }
    if (SG_BIGNUMP(y) || SG_RATIONALP(y)) {
      return Sg_MakeFlonum(SG_FLONUM(x)->value / Sg_GetDouble(y));
    }
    if (SG_FLONUMP(y)) {
      if (SG_FLONUM(y)->value == 0.0) goto a_normal;
      return Sg_MakeFlonum(SG_FLONUM(x)->value / SG_FLONUM(y)->value);
    }
    if (SG_COMPLEXP(y)) {
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
      if (y == SG_MAKE_INT(0)) goto a_normal;
      if (y == SG_MAKE_INT(1)) return x;
      return Sg_MakeRational(x, y);
    }
    if (SG_BIGNUMP(y)) {
      return Sg_MakeRational(x, y);
    }
    if (SG_RATIONALP(y)) {
      return Sg_MakeRational(Sg_Mul(SG_RATIONAL(y)->denominator, x),
			     SG_RATIONAL(y)->numerator);
    }
    if (SG_FLONUMP(y)) {
      if (SG_FLONUM(y)->value == 0.0) goto a_normal;
      return Sg_MakeFlonum(Sg_GetDouble(x) / SG_FLONUM(y)->value);
    }
    if (SG_COMPLEXP(y)) {
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
      if (y == SG_MAKE_INT(0)) goto a_normal;
      if (y == SG_MAKE_INT(1)) return x;
      return Sg_MakeRational(SG_RATIONAL(x)->numerator,
			     Sg_Mul(SG_RATIONAL(x)->denominator, y));
    }
    if (SG_BIGNUMP(y)) {
      return Sg_MakeRational(SG_RATIONAL(x)->numerator,
			     Sg_Mul(SG_RATIONAL(x)->denominator, y));
    }
    if (SG_RATIONALP(y)) {
      return Sg_RationalDiv(x, y);
    }
    if (SG_FLONUMP(y)) {
      return Sg_MakeFlonum(Sg_GetDouble(x) / SG_FLONUM(y)->value);
    }
    if (SG_COMPLEXP(y)) {
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
      return Sg_MakeComplex(Sg_Div(real, y),
			    Sg_Div(imag, y));
    }
    if (SG_BIGNUMP(y) || SG_RATIONALP(y)) {
      return Sg_MakeComplex(Sg_Div(real, y),
			    Sg_Div(imag, y));
    }
    if (SG_FLONUMP(y)) {
      return Sg_MakeComplex(Sg_Div(real, y),
			    Sg_Div(imag, y));
    }
    if (SG_COMPLEXP(y)) {
      SgObject real2 = SG_COMPLEX(y)->real;
      SgObject imag2 = SG_COMPLEX(y)->imag;
      SgObject r2 = Sg_Add(Sg_Mul(real2, real2), Sg_Mul(imag2, imag2));
      SgObject real3 = Sg_Div(Sg_Add(Sg_Mul(real, real2), Sg_Mul(imag, imag2)), r2);
      SgObject imag3 = Sg_Div(Sg_Sub(Sg_Mul(imag, real2), Sg_Mul(real, imag2)), r2);
      return oprtr_norm_complex(real3, imag3);
    }
  }
  Sg_Error(UC("Sg_Div: wrong type of argument(x %S, y %S)"), x, y);
  return SG_UNDEF;		/* dummy */

 a_normal:
  {
    int s = Sg_Sign(x);
    if (s == 0) return SG_NAN;
    if (s < 0)  return SG_NEGATIVE_INFINITY;
    else        return SG_POSITIVE_INFINITY;
  }
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
  goto bad_arg;					\

 start_again:
  if (SG_INTP(x)) {
    if (SG_INT_VALUE(x) == 0) return SG_MAKE_INT(0);
  fixnum_again:
    if (SG_INTP(y)) {
      long q, r;
      if (SG_INTP(y) == 0) {
	goto div_by_zero;
      }
      q = SG_INT_VALUE(x) / SG_INT_VALUE(y);
      if (rem) {
	r = SG_INT_VALUE(x) % SG_INT_VALUE(y);
	*rem = SG_MAKE_INT(r);
      }
      return SG_MAKE_INT(q);
    }
    if (SG_BIGNUMP(y)) {
      SgObject qr = Sg_BignumDivRem(SG_BIGNUM(Sg_MakeBignumFromSI(SG_INT_VALUE(x))),
				    SG_BIGNUM(y));
      if (rem) *rem = SG_CDR(qr);
      return SG_CAR(qr);
    }
    if (SG_FLONUMP(y)) {
      rx = (double)SG_INT_VALUE(x);
      ry = SG_FLONUM(y)->value;
      if (ry != floor(ry)) goto bad_argy;
      goto do_flonum;
    }
    if (SG_COMPLEXP(y)) {
      do_complex(y, fixnum_again);
    }
    goto bad_argy;
  } 
  if (SG_BIGNUMP(x)) {
  bignum_again:
    if (SG_INTP(y)) {
      long r;
      SgObject q = Sg_BignumDivSI(SG_BIGNUM(x), SG_INT_VALUE(y), &r);
      if (rem) *rem = SG_MAKE_INT(r);
      return q;
    }
    if (SG_BIGNUMP(y)) {
      SgObject qr = Sg_BignumDivRem(SG_BIGNUM(x), SG_BIGNUM(y));
      if (rem) *rem = SG_CDR(qr);
      return SG_CAR(qr);
    }
    if (SG_FLONUMP(y)) {
      rx = Sg_BignumToDouble(SG_BIGNUM(x));
      ry = SG_FLONUM(y)->value;
      if (ry != floor(ry)) goto bad_argy;
      goto do_flonum;
    }
    if (SG_COMPLEXP(y)) {
      do_complex(y, bignum_again);
    }
    goto bad_argy;
  }
  if (SG_COMPLEXP(x)) {
    do_complex(x, start_again);
  }
  if (SG_FLONUMP(x)) {
    rx = SG_FLONUM(x)->value;
    if (rx != floor(rx)) goto bad_arg;
  flonum_again:
    if (SG_INTP(y)) {
      ry = (double)SG_INT_VALUE(y);
    } else if (SG_BIGNUMP(y)) {
      ry = Sg_BignumToDouble(SG_BIGNUM(y));
    } else if (SG_FLONUMP(y)) {
      ry = SG_FLONUM(y)->value;
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
      q = roundeven(rx / ry);
      if (rem) {
	double v = rx - q*ry;
	double rr = (v < 0.0) ? ceil(v) : floor(v);
	*rem = Sg_MakeFlonum(rr);
      }
      return Sg_MakeFlonum(q);
    }
  }
  goto bad_arg;

 div_by_zero:
  Sg_Error(UC("attempt to calculate a quotient by zero"));
 bad_argy:
  x = y;
 bad_arg:
  Sg_Error(UC("integer required, but got %S"), x);
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
  goto bad_arg;					\

 start_again:
  if (SG_INTP(x)) {
  fixnum_again:
    if (SG_INTP(y)) {
      intptr_t r;
      if (SG_INT_VALUE(y) == 0) goto div_by_zero;
      r = SG_INT_VALUE(x) % SG_INT_VALUE(y);
      if (!remp && r) {
	if ((SG_INT_VALUE(x) > 0 && SG_INT_VALUE(y) < 0)
	    || (SG_INT_VALUE(x) < 0 && SG_INT_VALUE(y) > 0)) {
	  r += SG_INT_VALUE(y);
	}
      }
      return SG_MAKE_INT(r);
    }
    if (SG_BIGNUMP(y)) {
      bx = Sg_MakeBignumFromSI(SG_INT_VALUE(x));
      goto do_bignumy;
    }
    if (SG_FLONUMP(y)) {
      rx = (double)SG_INT_VALUE(x);
      ry = SG_FLONUM(y)->value;
      if (ry != floor(ry)) goto bad_argy;
      goto do_flonum;
    }
    if (SG_COMPLEXP(y)) {
      do_complex(y, fixnum_again);
    }
    goto bad_arg;
  }
  if (SG_BIGNUMP(x)) {
  bignum_again:
    if (SG_INTP(y)) {
      intptr_t iy = SG_INT_VALUE(y);
      long rem;
      Sg_BignumDivSI(SG_BIGNUM(x), iy, &rem);
      if (!remp
	  && rem
	  && ((SG_BIGNUM_GET_SIGN(x) < 0 && iy > 0)
	      || (SG_BIGNUM_GET_SIGN(x) > 0 && iy < 0))) {
	return SG_MAKE_INT(iy + rem);
      }
      return SG_MAKE_INT(rem);
    }
    if (SG_BIGNUMP(y)) {
      SgObject rem;
      bx = x;
    do_bignumy:
      rem = SG_CDR(Sg_BignumDivRem(SG_BIGNUM(bx), SG_BIGNUM(y)));
      if (!remp
	  && (rem != SG_MAKE_INT(0))
	  && (SG_BIGNUM_GET_SIGN(bx) * SG_BIGNUM_GET_SIGN(y) < 0)) {
	if (SG_BIGNUMP(rem)) {
	  return Sg_BignumAdd(SG_BIGNUM(y), SG_BIGNUM(rem));
	} else {
	  return Sg_BignumAddSI(SG_BIGNUM(y), SG_INT_VALUE(rem));
	}
      }
      return rem;
    }
    if (SG_FLONUMP(y)) {
      rx = Sg_BignumToDouble(SG_BIGNUM(x));
      ry = SG_FLONUM(y)->value;
      if (ry != floor(ry)) goto bad_argy;
      goto do_flonum;
    }
    if (SG_COMPLEXP(y)) {
      do_complex(y, bignum_again);
    }
    goto bad_arg;
  }
  if (SG_FLONUMP(x)) {
    double rem;
    rx = SG_FLONUM(y)->value;
  flonum_again:
    if (rx != floor(rx)) goto bad_arg;
    if (SG_INTP(y)) {
      ry = (double)SG_INT_VALUE(y);
    } else if (SG_BIGNUMP(y)) {
      ry = Sg_BignumToDouble(y);
    } else if (SG_FLONUMP(y)) {
      ry = SG_FLONUM(y)->value;
    } else if (SG_COMPLEXP(y)) {
      do_complex(y, flonum_again);
    } else {
      goto bad_argy;
    }
  do_flonum:
    if (ry == 0.0) goto div_by_zero;
    rem = fmod(rx, ry);
    if (!remp && rem != 0.0) {
      if ((rx > 0 && ry < 0) || (rx < 0 && ry > 0)) {
	rem += ry;
      }
    }
    return Sg_MakeFlonum(rem);
  }
  if (SG_COMPLEXP(x)) {
    do_complex(x, start_again);
  }
  goto bad_arg;

 div_by_zero:
  Sg_Error(UC("attempt to calculate a quotient by zero"));
 bad_argy:
  x = y;
 bad_arg:
  Sg_Error(UC("integer required, but got %S"), x);
  return SG_UNDEF;		/* dummy */

#undef do_complex
}

static SgObject expt_body(SgObject x, SgObject y)
{
  if (SG_FLONUMP(x) && SG_FLONUM(x)->value == 0.0) {
    if (SG_COMPLEXP(y)) {
      if (Sg_PositiveP(SG_COMPLEX(y)->real)) return Sg_MakeFlonum(0.0);
    } else {
      if (Sg_PositiveP(y)) return Sg_MakeFlonum(0.0);
    }
  }
  if (Sg_ExactP(y)) {
    if (SG_INTP(y)) {
      if (SG_INT_VALUE(y) == 0) return SG_MAKE_INT(1);
      if (SG_FLONUMP(x)) return Sg_MakeFlonum(pow(SG_FLONUM(x)->value, (double)SG_INT_VALUE(y)));
      return oprtr_expt(x, SG_INT_VALUE(y));
    }
    if (SG_BIGNUMP(y)) {
      if (!Sg_ExactP(x)) {
	/* Sg_Error(UC("exact number required, but got %S"), x); */
	return SG_UNDEF;
      }
      if (SG_REALP(y)) {
	double n = Sg_BignumToDouble(SG_BIGNUM(y));
	return Sg_MakeFlonum(pow(Sg_GetDouble(x), n));
      }
      return Sg_Exp(Sg_Mul(y, Sg_Log(x)));
    }
    if (SG_RATIONALP(y)) {
      double n = Sg_GetDouble(y);
      if (SG_REALP(x) && !Sg_NegativeP(x)) return Sg_MakeFlonum(pow(Sg_GetDouble(x), n));
      return Sg_Exp(Sg_Mul(y, Sg_Log(x)));
    }
    if (SG_COMPLEXP(y)) {
      return Sg_Exp(Sg_Mul(y, Sg_Log(x)));
    }
    goto bad_arg;
  } else {
    if (SG_FLONUMP(y)) {
      if (SG_REALP(x) && !Sg_NegativeP(x)) {
	double n = SG_FLONUM(y)->value;
	return Sg_MakeFlonum(pow(Sg_GetDouble(x), n));
      }
      return Sg_Exp(Sg_Mul(y, Sg_Log(x)));
    }
    return Sg_Exp(Sg_Mul(y, Sg_Log(x)));
  }
 bad_arg:
  /* Sg_Error(UC("real number required, but got %S"), y); */
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_Expt(SgObject x, SgObject y)
{
  if (x == SG_MAKE_INT(1)) {
    if (Sg_ExactP(y)) return SG_MAKE_INT(1);
    return Sg_MakeFlonum(1.0);
  }
  if (x == SG_MAKE_INT(-1) && Sg_ExactP(y)) {
    if (Sg_OddP(y)) return SG_MAKE_INT(-1);
    return SG_MAKE_INT(1);
  }
  if (x == SG_MAKE_INT(0)) {
    if (Sg_ZeroP(y)) {
      if (Sg_ExactP(y)) return SG_MAKE_INT(1);
      return Sg_MakeFlonum(1.0);
    }
    if (Sg_RealValuedP(y)) {
      if (Sg_NegativeP(y)) return Sg_MakeComplex(SG_NAN, SG_NAN);
      if (Sg_ExactP(y)) return SG_MAKE_INT(0);
      return Sg_MakeFlonum(0.0);
    } else {
      ASSERT(SG_COMPLEXP(y));
      if (Sg_PositiveP(SG_COMPLEX(y)->real)) {
	if (Sg_ExactP(y)) return SG_MAKE_INT(0);
	return Sg_MakeFlonum(0.0);
      }
      return Sg_MakeComplex(SG_NAN, SG_NAN);
    }
  }
  if (Sg_ExactP(x) && SG_BIGNUMP(y)) {
    Sg_Error(UC("expt: calculated number is too big to fit into memory %S %S"), x, y);
    return SG_UNDEF;
  }
  return expt_body(x, y);
}

SgObject Sg_Exp(SgObject obj)
{
  if (SG_INTP(obj)) {
    if (SG_INT_VALUE(obj) == 0) return SG_MAKE_INT(1);
    return Sg_MakeFlonum(exp((double)SG_INT_VALUE(obj)));
  }
  if (SG_COMPLEXP(obj)) {
    double real = Sg_GetDouble(SG_COMPLEX(obj)->real);
    double imag = Sg_GetDouble(SG_COMPLEX(obj)->imag);
    double a = exp(real);
    return Sg_MakeComplex(Sg_MakeFlonum(a * cos(imag)),
			  Sg_MakeFlonum(a * sin(imag)));
  }
  if (SG_REALP(obj)) return Sg_MakeFlonum(exp(Sg_GetDouble(obj)));
  Sg_Error(UC("real number required, but got %S"), obj);
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_Sin(SgObject obj)
{
  if (SG_INTP(obj)) {
    if (obj == SG_MAKE_INT(0)) return obj;
    return Sg_MakeFlonum(sin((double)SG_INT_VALUE(obj)));
  }
  if (SG_COMPLEXP(obj)) {
    double real = Sg_GetDouble(SG_COMPLEX(obj)->real);
    double imag = Sg_GetDouble(SG_COMPLEX(obj)->imag);
    double e = exp(imag);
    double f = 1.0 / e;
    return Sg_MakeComplex(Sg_MakeFlonum(0.5 * sin(real) * (e + f)),
			  Sg_MakeFlonum(0.5 * cos(real) * (e - f)));
  }
  if (SG_REALP(obj)) return Sg_MakeFlonum(sin(Sg_GetDouble(obj)));
  Sg_Error(UC("number required, but got %S"), obj);
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_Cos(SgObject obj)
{
  if (SG_INTP(obj)) {
    if (obj == SG_MAKE_INT(0)) return SG_MAKE_INT(1);
    return Sg_MakeFlonum(cos((double)SG_INT_VALUE(obj)));
  }
  if (SG_COMPLEXP(obj)) {
    double real = Sg_GetDouble(SG_COMPLEX(obj)->real);
    double imag = Sg_GetDouble(SG_COMPLEX(obj)->imag);
    double e = exp(imag);
    double f = 1.0 / e;
    return Sg_MakeComplex(Sg_MakeFlonum(0.5 * cos(real) * (e + f)),
			  Sg_MakeFlonum(0.5 * sin(real) * (e - f)));
  }
  if (SG_REALP(obj)) return Sg_MakeFlonum(cos(Sg_GetDouble(obj)));
  Sg_Error(UC("number required, but got %S"), obj);
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_Tan(SgObject obj)
{
  if (SG_INTP(obj)) {
    if (obj == SG_MAKE_INT(0)) return obj;
    return Sg_MakeFlonum(tan((double)SG_INT_VALUE(obj)));
  }
  if (SG_COMPLEXP(obj)) {
    double real = Sg_GetDouble(SG_COMPLEX(obj)->real);
    double imag = Sg_GetDouble(SG_COMPLEX(obj)->imag);
    double e = exp(imag);
    double f = 1.0 / e;
    double d = cos(2.0 * real) + 0.5 * (e + f);
    return Sg_MakeComplex(Sg_MakeFlonum(sin(2.0 * real) / d),
			  Sg_MakeFlonum(0.5 * (e - f) / d));
  }
  if (SG_REALP(obj)) return Sg_MakeFlonum(tan(Sg_GetDouble(obj)));
  Sg_Error(UC("number required, but got %S"), obj);
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_Asin(SgObject obj)
{
  SgComplex *cn;
  SgObject ans;
  if (SG_REALP(obj) || (SG_COMPLEXP(obj) && Sg_ZeroP(SG_COMPLEX(obj)->imag))) {
    double x = Sg_GetDouble(obj);
    if (x >= -1.0 && x <= 1.0) return Sg_MakeFlonum(asin(Sg_GetDouble(obj)));
    if (x < 0.0) return Sg_Negate(Sg_Asin(Sg_MakeFlonum(-x)));
    cn = Sg_MakeComplex(Sg_MakeFlonum(0.0), Sg_MakeFlonum(x));
  } else {
    ASSERT(SG_COMPLEXP(obj));
    if (Sg_PositiveP(SG_COMPLEX(obj)->imag)) return Sg_Negate(Sg_Asin(Sg_Negate(obj)));
    cn = Sg_MakeComplex(Sg_Negate(SG_COMPLEX(obj)->imag), SG_COMPLEX(obj)->real);
  }
  ans = Sg_Log(Sg_Add(Sg_Sqrt(Sg_Sub(SG_MAKE_INT(1), Sg_Mul(obj, obj))),
		      cn));
  if (SG_COMPLEXP(ans)) {
    return Sg_MakeComplex(Sg_MakeFlonum(Sg_GetDouble(SG_COMPLEX(ans)->imag)),
			  Sg_MakeFlonum(-Sg_GetDouble(SG_COMPLEX(ans)->real)));
  }
  return Sg_MakeComplex(Sg_MakeFlonum(0.0), Sg_MakeFlonum(-Sg_GetDouble(ans)));
	       
}

SgObject Sg_Acos(SgObject obj)
{
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
  Sg_Error(UC("number required, but got %S"), obj);
  return SG_UNDEF;
}

SgObject Sg_Atan2(SgObject x, SgObject y)
{
  if (SG_EQ(x, SG_MAKE_INT(0))) return x;
  return Sg_MakeFlonum(atan2(Sg_GetDouble(x), Sg_GetDouble(y)));
}

static inline int either_nan_p(SgObject arg0, SgObject arg1)
{
  if (SG_FLONUMP(arg0) && isnan(SG_FLONUM(arg0)->value)) return TRUE;
  if (SG_FLONUMP(arg1) && isnan(SG_FLONUM(arg1)->value)) return TRUE;
  return FALSE;
}

int Sg_NumEq(SgObject x, SgObject y)
{
  if (SG_COMPLEXP(x)) {
    if (SG_COMPLEXP(y)) {
      return ((Sg_NumCmp(SG_COMPLEX(x)->real, SG_COMPLEX(y)->real) == 0)
	      && (Sg_NumCmp(SG_COMPLEX(x)->imag, SG_COMPLEX(y)->imag) == 0));
    }
    return FALSE;
  } else {
    if (SG_COMPLEXP(y)) return FALSE;
    if (either_nan_p(x, y)) return FALSE;
    return (Sg_NumCmp(x, y) == 0);
  }
}

int Sg_NumCmp(SgObject x, SgObject y)
{
  SgObject badnum;
  if (SG_INTP(x)) {
    if (SG_INTP(y)) {
      intptr_t r = SG_INT_VALUE(x) - SG_INT_VALUE(y);
      if (r < 0) return -1;
      if (r > 0) return 1;
      return 0;
    }
    if (SG_FLONUMP(y)) {
      double r = SG_INT_VALUE(x) - SG_FLONUM(y)->value;
      if (r < 0) return -1;
      if (r > 0) return 1;
      return 0;
    }
    if (SG_BIGNUMP(y)) {
      return Sg_BignumCmp(SG_BIGNUM(Sg_MakeBignumFromSI(SG_INT_VALUE(x))),
			  SG_BIGNUM(y));
    }
    if (SG_RATIONALP(y)) {
      if (SG_MAKE_INT(0) == x) {
	return -Sg_Sign(y);
      } else {
	/*  roughly estimate the result by coercing the ratnum to double */
	double y2 = Sg_GetDouble(y);
	double r = SG_INT_VALUE(x) - y2;
	double err = y2 * 2.0e-52;
	if (r < -err) return -1;
	if (r > err) return 1;
	return Sg_NumCmp(Sg_Mul(x, SG_RATIONAL(y)->denominator),
			 SG_RATIONAL(y)->numerator);
      }
    }
    badnum = y;
  }
  else if (SG_FLONUMP(x)) {
    if (SG_INTP(y)) {
      double r = SG_FLONUM(x)->value - SG_INT_VALUE(y);
      if (r < 0) return -1;
      if (r > 0) return 1;
      return 0;
    }
    if (SG_FLONUMP(y)) {
      double r = SG_FLONUM(x)->value - SG_FLONUM(y)->value;
      if (r < 0) return -1;
      if (r > 0) return 1;
      return 0;
    }
    if (SG_BIGNUMP(y) || SG_RATIONALP(y)) {
      double r = SG_FLONUM(x)->value - Sg_GetDouble(y);
      if (r < 0) return -1;
      if (r > 0) return 1;
      return 0;
    }
    badnum = y;
  }
  else if (SG_BIGNUMP(x)) {
    if (SG_INTP(y)) {
      return Sg_BignumCmp(SG_BIGNUM(x),
			  SG_BIGNUM(Sg_MakeBignumFromSI(SG_INT_VALUE(y))));
    }
    if (SG_FLONUMP(y)) {
      return -Sg_NumCmp(y, x);
    }
    if (SG_BIGNUMP(y)) {
      return Sg_BignumCmp(SG_BIGNUM(x), SG_BIGNUM(y));
    }
    if (SG_RATIONALP(y)) {
      SgObject d1 = SG_RATIONAL(y)->denominator;
      return Sg_NumCmp(Sg_Mul(x, d1),
		       SG_RATIONAL(y)->numerator);
    }
    badnum = y;
  }
  else if (SG_RATIONALP(x)) {
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
      if ((sx > 0 && sy > 0) || (sx < 0 && sy < 0)) {
	n = Sg_NumCmp(nx, ny) * sx;
	if (d > 0 && n <= 0) return -sx;
	if (d < 0 && n >= 0) return sx;
      }
      return Sg_NumCmp(Sg_Mul(nx, dy),
		       Sg_Mul(ny, dx));
    }
    badnum = y;
  }
  else badnum = x;

  Sg_Error(UC("real number required, but got %S"), badnum);
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
    intptr_t v = SG_INT_VALUE(obj);
    if (v < 0) {
      obj = Sg_MakeInteger(-v);
    }
  } else if (SG_BIGNUMP(obj)) {
    if (SG_BIGNUM_GET_SIGN(obj) < 0) {
      obj = Sg_BignumCopy(SG_BIGNUM(obj));
      SG_BIGNUM_SET_SIGN(obj, 1);
    }
  } else if (SG_FLONUMP(obj)) {
    double v = SG_FLONUM(obj)->value;
    if (v < 0) return Sg_MakeFlonum(-v);
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
    Sg_Error(UC("number required, but got %S"), obj);
  }
  return obj;
}

SgObject Sg_Sqrt(SgObject obj)
{
  if (SG_INTP(obj)) {
    intptr_t value = SG_INT_VALUE(obj);
    if (value == 0) return SG_MAKE_INT(0);
    if (value > 0) {
      double root = sqrt((double)value);
      long iroot = (long)floor(root);
      if (iroot * iroot == value) return SG_MAKE_INT(iroot);
      return Sg_MakeFlonum(root);
    } else {
      double root = sqrt((double)-value);
      long iroot = (long)floor(root);
      if (iroot * iroot == value) return Sg_MakeComplex(SG_MAKE_INT(0), SG_MAKE_INT(iroot));
      return  Sg_MakeComplex(Sg_MakeFlonum(0.0), Sg_MakeFlonum(root));
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
    double s = SG_FLONUM(obj)->value;
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
  Sg_Error(UC("number requried, but got %S"), obj);
  return SG_UNDEF;		/* dummy */
}

static inline SgObject exact_integer_sqrt(SgObject k)
{
  SgObject ik = Sg_Sqrt(k);
  if (Sg_FiniteP(ik)) {
    return Sg_Exact(Sg_Round(ik, SG_ROUND_FLOOR));
  } else {
    int len = Sg_IntegerLength(k);
    SgObject quo = Sg_Quotient(SG_MAKE_INT(len), SG_MAKE_INT(2), NULL);
    ASSERT(SG_INTP(quo));
    return Sg_Ash(SG_MAKE_INT(1), SG_INT_VALUE(quo));
  }
}

SgObject Sg_ExactIntegerSqrt(SgObject k)
{
  double d;
  SgObject ans;

  if (!SG_EXACT_INTP(k)) Sg_Error(UC("exact integer required, but got %S"), k);

  d = Sg_GetDouble(k);
  ans = Sg_MakeValues(2);
  if (d < iexpt_2n53) {
    double t = floor(sqrt(d));
    SgObject s = Sg_Exact(Sg_MakeFlonum(t));
    SG_VALUES_ELEMENT(ans, 0) = s;
    SG_VALUES_ELEMENT(ans, 1) = Sg_Sub(k, Sg_Mul(s, s));
    return ans;
  } else {
    SgObject s = exact_integer_sqrt(k);
    SgObject s2 = Sg_Mul(s, s);
    while (TRUE) {
      if (Sg_NumCmp(k, s2) < 0) {
	s = Sg_Quotient(Sg_Add(s2, k), Sg_Mul(SG_MAKE_INT(2), s), NULL);
	continue;
      } else {
	SgObject s2p = Sg_Add(Sg_Add(s2, Sg_Mul(SG_MAKE_INT(2), s)),
			      SG_MAKE_INT(1));
	if (Sg_NumCmp(k, s2p) < 0) {
	  SG_VALUES_ELEMENT(ans, 0) = s;
	  SG_VALUES_ELEMENT(ans, 1) = Sg_Sub(k, s2);
	  return ans;
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
  intptr_t r = 0;
  if (SG_INTP(obj)) {
    r = SG_INT_VALUE(obj);
    if (r > 0) r = 1;
    else if (r < 0) r = -1;
  } else if (SG_BIGNUMP(obj)) {
    r = SG_BIGNUM_GET_SIGN(obj);
  } else if (SG_FLONUMP(obj)) {
    double v = SG_FLONUM(obj)->value;
    if (v != 0.0) {
      r = v > 0.0 ? 1 : -1;
    }
  } else if (SG_RATIONALP(obj)) {
    return Sg_Sign(SG_RATIONAL(obj)->numerator);
  } else {
    Sg_Error(UC("real number required, but got %S"), obj);
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
    Sg_Error(UC("integer required, but got %S"), x);
  }
  if (!Sg_IntegerP(y)) {
    Sg_Error(UC("integer required, but got %S"), y);
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
  Sg_Error(UC("number required, but got %S"), z);
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
  Sg_Error(UC("number required, bot got %S"), obj);
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_Log(SgObject obj)
{
  double real;
  double imag;
  if (SG_INTP(obj)) {
    intptr_t value = SG_INT_VALUE(obj);
    if (value > 0) {
      if (value == 1) return SG_MAKE_INT(0);
      return Sg_MakeFlonum(log((double)value));
    }
    real = value;
    return Sg_MakeComplex(Sg_MakeFlonum(0.5 * log(real * real)),
			  Sg_MakeFlonum(atan2(0.0, real)));
  }
  if (SG_COMPLEXP(obj)) {
    real = Sg_GetDouble(SG_COMPLEX(obj)->real);
    imag = Sg_GetDouble(SG_COMPLEX(obj)->imag);
    return Sg_MakeComplex(Sg_MakeFlonum(0.5 * log(real * real + imag * imag)),
			  Sg_MakeFlonum(atan2(imag, real)));
  }
  if (SG_REALP(obj)) {
    real = Sg_GetDouble(obj);
    if (real > 0) return Sg_MakeFlonum(log(real));
    imag = atan2(0.0, real);
    if (imag == 0.0) return Sg_MakeFlonum(0.5 * log(real * real));
    return Sg_MakeComplex(Sg_MakeFlonum(0.5 * log(real * real)),
			  Sg_MakeFlonum(imag));
  }
  Sg_Error(UC("number required, but got %S"), obj);
  return SG_UNDEF;		/* dummy */
}

void Sg_MinMax(SgObject arg0, SgObject args, SgObject *min, SgObject *max)
{
#define EXACTP(o) (SG_EXACT_INTP(o) || SG_RATIONALP(o))
  int inexact = !EXACTP(arg0);
  SgObject mi = arg0;
  SgObject ma = arg0;

  for (;;) {
    if (!SG_REALP(arg0))
      Sg_Error(UC("real number required, but got %S"), arg0);
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
  if (!SG_REALP(x) || !SG_REALP(y)) Sg_Error(UC("real number required, but got %S and %S"), x, y);
  if (!Sg_FiniteP(x) || Sg_NanP(x)) Sg_Error(UC("dividend must be neither infinite nor NaN: %S"), x);
  if (Sg_ZeroP(y)) Sg_Error(UC("undefined for 0"));
  
  if (SG_INTP(x)) {
    if (SG_INTP(y)) {
      intptr_t xx = SG_INT_VALUE(x);
      intptr_t yy = SG_INT_VALUE(y);
      intptr_t div;
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
  if (!SG_REALP(x) || !SG_REALP(y)) Sg_Error(UC("real number required, but got %S and %S"), x, y);
  if (!Sg_FiniteP(x) || Sg_NanP(x)) Sg_Error(UC("dividend must be neither infinite nor NaN: %S"), x);
  if (Sg_ZeroP(y)) Sg_Error(UC("undefined for 0"));

  div = Sg_IntegerDiv(x, y);
  mod = Sg_Sub(x, Sg_Mul(div, y));
  if (Sg_NumCmp(mod, Sg_Magnitude(Sg_Div(y, SG_MAKE_INT(2)))) < 0) return div;
  if (Sg_PositiveP(y)) return Sg_Add(div, SG_MAKE_INT(1));
  return Sg_Sub(div, SG_MAKE_INT(1));
  
}

SgObject Sg_IntegerMod(SgObject x, SgObject y)
{
  SgObject d = Sg_IntegerDiv(x, y);
  SgObject m = Sg_Mul(d, y);
  return Sg_Sub(x, m);
}

SgObject Sg_IntegerMod0(SgObject x, SgObject y)
{
  SgObject d = Sg_IntegerDiv0(x, y);
  SgObject m = Sg_Mul(d, y);
  return Sg_Sub(x, m);
}


static inline double roundeven(double v)
{
  double r;
  double frac = modf(v, &r);
  if (v > 0.0) {
    if (frac > 0.5) r += 1.0;
    else if (frac == 0.5) {
      if (fmod(r, 2.0) != 0.0) r += 1.0;
    }
  } else {
    if (frac < -0.5) r -= 1.0;
    else if (frac == -0.5) {
      if (fmod(r, 2.0) != 0.0) r -= 1.0;
    }
  }
  return r;
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
    v = SG_FLONUM(num)->value;
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
  Sg_Error(UC("real number required, but got %S"), num);
  return SG_UNDEF;
}

/* from gauche */
static inline int numcmp3(SgObject x, SgObject d, SgObject y)
{
  if (SG_INTP(x) && SG_INTP(d) && SG_INTP(y)) {
    intptr_t xd = SG_INT_VALUE(x) + SG_INT_VALUE(d);
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

static void double_print(char *buf, int buflen, double val, int plus_sign)
{
  if (val == 0.0) {
    if (plus_sign) strcpy(buf, "+0.0");
    else strcpy(buf, "0.0");
    return;
  } else if (isinf(val)) {
    if (val < 0.0) strcpy(buf, "-inf.0");
    else strcpy(buf, "+inf.0");
    return;
  } else if (isnan(val)) {
    strcpy(buf, "+nan.0");
    return;
  }
  if (val < 0.0) *buf++ = '-', buflen--;
  else if (plus_sign) *buf++ = '+', buflen--;
  {
    SgObject f, r, s, mp, mm, q;
    int exp, sign, est, tc1, tc2, tc3, digs, point, round;
    int mp2 = FALSE, fixup = FALSE;
    
    if (val < 0) val = -val;
    /* initialize r, s, m+ and m- */
    f = Sg_DecodeFlonum(val, &exp, &sign);
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
	  if (digs == point) *buf++ = '.', buflen--;
	  continue;
	} else {
	  *buf++ = (char)SG_INT_VALUE(q) + '1';
	  break;
	}
      } else {
	if (!tc2) {
	  *buf++ = (char)SG_INT_VALUE(q) + '0';
	  break;
	} else {
	  tc3 = numcmp3(r, r, s);
	  if ((round && tc3 <= 0) || (!round && tc3 < 0)) {
	    *buf++ = (char)SG_INT_VALUE(q) + '0';
	    break;
	  } else {
	    *buf++ = (char)SG_INT_VALUE(q) + '1';
	    break;
	  }
	}
      }
    }
    if (digs <= point) {
      for (; digs < point && buflen > 5; digs++) {
	*buf++ = '0', buflen--;
      }
      *buf++ = '.';
      *buf++ = '0';
    }
    est--;
    if (est != 0) {
      *buf++ = 'e';
      sprintf(buf, "%d", (int)est);
    } else {
      *buf++ = 0;
    }
  }
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
    double_print(buf, FLT_BUF, SG_FLONUM(obj)->value, FALSE);
    r = Sg_MakeStringC(buf);
  } else if (SG_RATIONALP(obj)) {
    SgObject nume, deno;
    nume = Sg_NumberToString(SG_RATIONAL(obj)->numerator, radix, use_upper);
    deno = Sg_NumberToString(SG_RATIONAL(obj)->denominator, radix, use_upper);
    r = Sg_StringAppend(SG_LIST3(nume, Sg_MakeString(UC("/"), SG_LITERAL_STRING), deno));
  } else if (SG_COMPLEXP(obj)) {
    SgObject real, imag;
    int needPlus = FALSE;
    real = Sg_NumberToString(SG_COMPLEX(obj)->real, radix, use_upper);
    imag = Sg_NumberToString(SG_COMPLEX(obj)->imag, radix, use_upper);
    needPlus = (SG_STRING_VALUE_AT(imag, 0) != '+' && SG_STRING_VALUE_AT(imag, 0) != '-');
    if (needPlus) {
      r = Sg_StringAppend(SG_LIST4(real, Sg_MakeString(UC("+"), SG_LITERAL_STRING),
				   imag, Sg_MakeString(UC("i"), SG_LITERAL_STRING)));
    } else {
      r = Sg_StringAppend(SG_LIST3(real, 
				   imag, Sg_MakeString(UC("i"), SG_LITERAL_STRING)));
    }
  } else {
    Sg_Error(UC("number required: %S"), obj);
  }
  return r;
}


SgObject Sg__ConstObjes[SG_NUM_CONST_OBJS] = {SG_FALSE};

void Sg__InitNumber()
{
/* VC does not have these macros. SUCKS!! */
#ifdef _MSC_VER
  static double __d0 = 0.0;
  static double __d1 = 1.0;
#define INFINITY (__d1/__d0)
#define NAN      (__d0/__d0)
#endif

  int radix, i;
  unsigned long n;

  for (radix = RADIX_MIN; radix <= RADIX_MAX; radix++) {
    longlimit[radix - RADIX_MIN] = (unsigned long)floor((double)LONG_MAX / radix - radix);
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

#if USE_CONST_FLONUM
  /* initialize const flonums */
#define INIT_CONST_FL(n, v)					\
  { (n) = SG_NEW_ATOMIC(SgFlonum); SG_SET_HEADER(n, TC_FLONUM);	\
    SG_FLONUM(n)->value = (v);}

  INIT_CONST_FL(SG_NAN, NAN);
  INIT_CONST_FL(SG_POSITIVE_INFINITY, INFINITY);
  INIT_CONST_FL(SG_NEGATIVE_INFINITY, -INFINITY);
  INIT_CONST_FL(SG_FL_POSITIVE_ZERO, 0.0);
  INIT_CONST_FL(SG_FL_NEGATIVE_ZERO, -0.0);
  {
    int i;
    for (i = 0; i < DEFAULT_FLONUM_CACHE_COUNT; i++) {
      SG_SET_HEADER(&CONST_FLONUM[i], TC_FLONUM);
      CONST_FLONUM[i].value = (double)i/1.0;
    }
  }
#else
  SG_POSITIVE_INFINITY = Sg_MakeFlonum(INFINITY);
  SG_NEGATIVE_INFINITY = Sg_MakeFlonum(-INFINITY);
  SG_NAN = Sg_MakeFlonum(NAN);
#endif
}
  
/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
