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
   or equivalent function (more or less MSVC)
 */
#ifndef C99_FLONUM_H
#define C99_FLONUM_H

#include <math.h>
#include <float.h>

#if !(__STDC_VERSION__ >= 199901L) || !(__GNUC__ >= 4)
# if defined(_MSC_VER)
#   if  _MSC_VER < 1800
/* under VS 2012. add required ones them */
#     define nextafter 	_nextafter
#     define copysign  	_copysign
#     define logb      	_logb

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
  return (*(__int64 *)(&d) & (1LL << 63)) != 0;
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
  return (x > 0.0) ? pow(x, 1.0 / 3.0) : -pow(-x, 1.0 / 3.0);
}

static __inline double erf(double x)
{
  double a1 = 0.254829592, a2 = -0.284496736, a3 = 1.421413741;
  double a4 = -1.453152027, a5 = 1.061405429, p = 0.3275911;
  double t, y;
  int sign = (x >= 0) ? 1 : -1;
  x = fabs(x);
  t = 1.0 / (1.0 + p*x);
  y = 1.0 - (((((a5 * t + a4 ) * t) + a3) * t + a2) * t + a1) * t * exp(-x * x);
  return sign * y;
}

static __inline double erfc(double x)
{
  return 1 - erf(x);
}

static __inline double expm1(double x){
    if(fabs(x) < 1e-5)
        return x + 0.5 * x * x;
    else
        return exp(x) - 1.0;
}

static __inline double fdim(double x, double y)
{
    return (x > y) ? x - y : 0.0;
}

static __inline double fma(double x, double y, double z)
{
    return ((x * y) + z);
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

static double gamma(double x)
{
  if (x > 2.0) {
    x -= 2.0;
    return x * (x + 1.0) * gamma(x);
  } else if (x == 2.0) {
    return 1.0;
  } else if (x > 1.0) {
    x -= 1.0;
    return x * gamma(x);
  } else if (x == 1.0) {
    return 1.0;
  } else if (x == 0.0) {
    return INFINITY;
  } else if (x < 0.0) {
    if (!isnan(x) && !isinf(x) && x == floor(x)) {
      return NAN;
    } else {
      return gamma(x + 2.0) / x / (x + 1.0);
    }
  } else {
    return 1.0 / polynomial_at(x, GAMMA_COEFS, asize(GAMMA_COEFS));
  }
}

static __inline int flodd(double x)
{
  return (x * 0.5) != floor(x * 0.5);
}
static __inline double tgamma(double x)
{
  static double upper = 2.0;
  static double lower = -2.0;
  if (upper == 2.0) {
    for (; isinf(gamma(upper)); upper += 1.0);
  }
  if (lower == -2.0) {
    for (; gamma(nextafter(lower, 0.0)); upper -= 1.0);
  }
  if (x >= upper) {
    return INFINITY;
  } else if (x <= lower) {
    if (isinf(x) && signbit(x)) return NAN;
    else if (x == floor(x)) return NAN;
    else if (flodd(trunc(x))) return 0.0;
    else return -0.0;
  } else {
    return gamma(x);
  }
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

static __inline double lgamma(double x)
{
#define UPPER_THRESHOLD 20.0
  if (isinf(x)) {
    if (x > 0.0) {
      return x;
    } else {
      return INFINITY;
    }
  } else if (x >= UPPER_THRESHOLD) {
    return eqn6_1_41(x);
  } else if (x > 0.0) {
    double g = tgamma(x);
    return log(g);
  } else {
    double g = tgamma(x);
    return log(fabs(g));
  }
}

#   endif
#   define jn _jn
#   define yn _yn
# else
#  error "not supported"
# endif
#endif

#endif
