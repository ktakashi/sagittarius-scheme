/* c99_flonum.                                     -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2017  Takashi Kato <ktakashi@ymail.com>
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
/* private file 
   This file provides either C99 flonum funcions from C99 math.h
   or equivalent function (more or less for MSVC)
 */
#ifndef C99_FLONUM_H
#define C99_FLONUM_H

#include <math.h>
#include <float.h>
#include <sagittarius/config.h>

#include "roundeven.inc"

#if !(__STDC_VERSION__ >= 199901L) && !defined(__GNUC__)
# if defined(_MSC_VER)
#   define logb      	_logb
#   if  _MSC_VER < 1800
/* under VS 2012. add required ones */
#     define nextafter 	_nextafter
#     define copysign  	_copysign

#     undef FP_FAST_FMA

static double d0__ = 0.0;
static double d1__ = 1.0;
#define INFINITY (d1__/d0__)
#define NAN      (d0__/d0__)

static __inline double log2(double d)
{
  return log(d) / log(2.0);
}

static __inline double exp2(double d)
{
  return pow(2.0, d);
}

static __inline int ilogb(double d)
{
  return (int)logb(d);
}
  
static __inline int signbit(double d)
{
  return (*(__int64 *)(&d) & (1LL << 63)) != 0LL;
}

static __inline double acosh(double x)
{
  return log(x + sqrt((x * x) - 1.0));
}

static __inline double asinh(double x)
{
  return log(x + sqrt((x * x) + 1.0));
}

static __inline double atanh(double x)
{
  return (log(1.0 + x) - log(1.0 - x)) / 2;
}

static __inline double cbrt(double x)
{
  return (x >= 0.0) ? pow(x, 1.0 / 3.0) : -pow(-x, 1.0 / 3.0);
}

static const double rel_error= 1E-12;
static double erf(double x);
static double erfc(double x);

static double erf(double x)
{
  static const double two_sqrtpi = 1.128379167095512574;
  double sum, term, xsqr;
  int j = 1;
  
  /* edge cases */
  if (x == 0.0) return x;
  if (isnan(x)) return x;
  if (isinf(x)) {
    if (x > 0.0) return 1.0;
    return -1.0;
  }
  if (x < 0.0) return -erf(-x);
  sum = term = x; 
  if (sum > 2.2) {
    return 1.0 - erfc(x);
  }
  xsqr = x*x;
  do {
    term *= xsqr/j;
    sum -= term/(2*j+1);
    j++;
    term *= xsqr/j;
    sum += term/(2*j+1);
    j++;
  } while (term/sum > rel_error);
  return two_sqrtpi*sum;
}

static double erfc(double x)
{
  static const double one_sqrtpi=  0.564189583547756287;
  double a=1, b=x;
  double c=x, d=x*x+0.5;
  double q1, q2= b/d;
  double n= 1.0, t;

  if (isnan(x)) return x;
  if (x == 0)   return 1.0;
  if (x < 0.0)  return 2.0 - erfc(-x);
  if (isinf(x)) return 0.0;

  if (fabs(x) < 2.2) {
    return 1.0 - erf(x);
  }
  do {
    t  = a*n+b*x;
    a  = b;
    b  = t;
    t  = c*n+d*x;
    c  = d;
    d  = t;
    n += 0.5;
    q1 = q2;
    q2 = b/d;
  } while (fabs(q1-q2)/q2 > rel_error);
  return one_sqrtpi*exp(-x*x)*q2;
}

static __inline double expm1(double x)
{
  return (fabs(x) < 1e-5) ? x + 0.5*x*x : exp(x) - 1.0;
}

static __inline double fdim(double x, double y)
{
    return (x > y) ? x - y : 0.0;
}

static __inline double fma(double x, double y, double z)
{
    return x*y + z;
}

#define FP_INFINITE  1
#define FP_NAN       2
#define FP_NORMAL    (-1)
#define FP_SUBNORMAL (-2)
#define FP_ZERO      0
static __inline int fpclassify(double x)
{
  switch (_fpclass(x)) {
  case _FPCLASS_SNAN: return FP_NAN;
  case _FPCLASS_QNAN: return FP_NAN;
  case _FPCLASS_NINF: return FP_INFINITE;
  case _FPCLASS_PINF: return FP_INFINITE;
  case _FPCLASS_NN  : return FP_NORMAL;
  case _FPCLASS_PN  : return FP_NORMAL;
  case _FPCLASS_ND  : return FP_SUBNORMAL;
  case _FPCLASS_PD  : return FP_SUBNORMAL;
  }
  return FP_ZERO;
}
static __inline double log1p(double x)
{
    if(fabs(x) > 1e-4){
        return log(1.0 + x);
    }
    return (-0.5 * x + 1.0) * x;
}
static __inline double trunc(double x)
{
    return (x > 0.0) ? floor(x) : ceil(x);
}

static __inline double rint(double x)
{
    const double two_to_52 = 4.5035996273704960e+15;
    double fa = fabs(x);
    if(fa >= two_to_52){
        return x;
    } else{
        return copysign(two_to_52 + fa - two_to_52, x);
    }
}
static __inline double remquo(double x, double y, int* q)
{
  double d = rint(x / y);
  *q = (int)d;
  return (x - (d * y));
}

static double GAMMA_COEFS[] = {
  0.0,
  1.0,
  +0.5772156649015329,
  -0.6558780715202538,
  -0.0420026350340952,
  +0.1665386113822915, /* ; x^5 */
  -0.0421977345555443,
  -0.0096219715278770,
  +0.0072189432466630,
  -0.0011651675918591,
  -0.0002152416741149, /* ; x^10 */
  +0.0001280502823882,
  -0.0000201348547807,
  -0.0000012504934821,
  +0.0000011330272320,
  -0.0000002056338417, /* ; x^15 */
  +0.0000000061160950,
  +0.0000000050020075,
  -0.0000000011812746,
  +0.0000000001043427,
  +0.0000000000077823, /* ; x^20 */
  -0.0000000000036968,
  +0.0000000000005100,
  -0.0000000000000206,
  -0.0000000000000054,
  +0.0000000000000014, /* ; x^25 */
  +0.0000000000000001
};

static double polynomial_at(double x, double *coefs, int size)
{
  double  n1 = 0.0;
  int i;
  for (i = size-1; i >= 0; i--) {
    n1 = fma(x, n1, coefs[i]);
  }
  return n1;
}

#define asize(a) ((int)(sizeof(a)/sizeof(a[0])))

static double tgamma(double x);
static double lgamma(double x);

static __inline int flodd(double x)
{
  return (x * 0.5) != floor(x * 0.5);
}

#define UPPER_THRESHOLD 12.0
static double tgamma(double x)
{
  static const double gamma = 0.577215664901532860606512090;
  /* numerator coefficients */
  static const double p[] = {
    -1.71618513886549492533811E+0,
    2.47656508055759199108314E+1,
    -3.79804256470945635097577E+2,
    6.29331155312818442661052E+2,
    8.66966202790413211295064E+2,
    -3.14512729688483675254357E+4,
    -3.61444134186911729807069E+4,
    6.64561438202405440627855E+4
  };
  /* denominator coefficients*/
  static const double q[] = {
    -3.08402300119738975254353E+1,
    3.15350626979604161529144E+2,
    -1.01515636749021914166146E+3,
    -3.10777167157231109440444E+3,
    2.25381184209801510330112E+4,
    4.75584627752788110767815E+3,
    -1.34659959864969306392456E+5,
    -1.15132259675553483497211E+5
  };

  /* it's undefined so follow C99 tgamma (returns +inf.0) */
  if (x == 0.0) return INFINITY;
  if (x < 0.0) {
    if (x == floor(x)) return NAN;
    return tgamma(x + 2.0) / x / (x + 1.0);
  }

  if (x < 0.001) return 1.0/(x*(1.0 + gamma*x));
  if (x < UPPER_THRESHOLD) {
    double y = x, num = 0.0, den = 1.0, z, result;
    int n, less = (y < 1.0), i;
    if (less) y += 1.0;
    else {
      n = (int)floor(y) - 1;
      y -= n;
    }
    z = y - 1;
    for (i = 0; i < asize(p); i++) {
      num = (num + p[i])*z;
      den = den*z + q[i];
    }
    result = num/den + 1.0;
    if (less) {
      result /= (y - 1.0);
    } else {
      for (i = 0; i < n; i++) {
	result *= y++;
      }
    }
    return result;
  } 
  if (x > 171.624) {
    return DBL_MAX * 2.0;
  }
  return exp(lgamma(x));
}

static __inline double eqn6_1_41(double x)
{
  static const double c[8] = {
    1.0/12.0,
    -1.0/360.0,
    1.0/1260.0,
    -1.0/1680.0,
    1.0/1188.0,
    -691.0/360360.0,
    1.0/156.0,
    -3617.0/122400.0
  };
  static const double halfLogTwoPi = 0.91893853320467274178032973640562;
  double z = 1.0/(x*x);
  double sum = c[7], series;
  int i;
  for (i = 6; i >= 0; i--) {
    sum *= z;
    sum += c[i];
  }
  series = sum/x;
  return (x - 0.5) * log(x) - x + halfLogTwoPi + series;
}

static double lgamma(double x)
{
  if (isnan(x)) return x;
  if (isinf(x)) {
    if (x > 0.0) return x;
    else return INFINITY;
  }
  if (x < UPPER_THRESHOLD) return log(fabs(tgamma(x)));
  return eqn6_1_41(x);
}

#     define jn _jn
#   endif
/* seems MSVC _yn returns NaN whilst C99 yn return -inf.0
   when the second value is 0.0.
   (though, it's pole error case, so can be NaN I think...)
 */
static __inline double yn_wrap(int n, double x)
{
  if (x == 0.0) return -INFINITY;
  return _yn(n, x);
}
#     define yn yn_wrap
# else
#  error "not supported"
# endif
#endif

/* libc under 2.22 returns wrong value with remquo so fix it
   FIXME: is this correct?
 */
#if defined(__GNUC__) && defined(HAVE_FEATURES_H)
#  include <features.h>
#  if defined(__GNU_LIBRARY__) && (__GLIBC_MINOR__ < 22)

/* FIXME copy&paste */
static inline double remquo__(double x, double y, int* q)
{
  double d = rint(x / y);
  *q = (int)d;
  return (x - (d * y));
}

#   define remquo remquo__
#  endif  /* __GNU_LIBRARY__ */
#endif

#endif	/* C99_FLONUM_H */
