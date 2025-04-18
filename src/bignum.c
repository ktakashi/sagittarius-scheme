/* bignum.c                                        -*- mode:c; coding:utf-8; -*-
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

#include <math.h>
#include <string.h>

#define NO_NBITS
#define LIBSAGITTARIUS_BODY
#include "sagittarius/private/bignum.h"
#include "sagittarius/private/core.h"
#include "sagittarius/private/number.h"
#include "sagittarius/private/error.h"
#include "sagittarius/private/arith.h"
#include "sagittarius/private/bits.h"
#include "sagittarius/private/pair.h"
#include "sagittarius/private/port.h"
#include "sagittarius/private/string.h"
#include "sagittarius/private/vm.h"

#define USE_MUTABLE_BIGNUM

#undef min
#define min(x, y)   (((x) < (y))? (x) : (y))
#undef max
#define max(x, y)   (((x) > (y))? (x) : (y))

/* will be used in bignum.inc
   TODO move to bignum.inc
 */
#define clear_buffer(v, size)				\
  do {							\
    long __i, __size = (size);				\
    for (__i = 0; __i < __size; __i++) (v)[__i] = 0L;	\
  } while (0)

#ifdef HAVE_ALLOCA
#define ALLOC_TEMP_BUFFER_REC(v, type, size)		\
    (v) = (type *)alloca(sizeof(type) * (size));
#else
#define ALLOC_TEMP_BUFFER_REC(v, type, size)			\
    (v) = SG_NEW_ATOMIC2(type *, sizeof(type) * (size));
#endif

#define ALLOC_TEMP_BUFFER(v, type, size)	\
  do {						\
    ALLOC_TEMP_BUFFER_REC(v, type, size)	\
    clear_buffer(v, size);			\
  } while (0)


/* debug utility macro */
#define dump_array_rec(flag, array, size)		\
  do {							\
    int __i, __size = (size);				\
    fprintf(stderr, #array "(%d) = [ ", __size);	\
    for (__i = 0; __i < __size; __i++) {		\
      fprintf(stderr, flag, (array)[__i]);		\
    }							\
    fprintf(stderr, "]\n");				\
  } while (0)

#define dump_sarray(array, size) dump_array_rec("%ld ", array, size)
#define dump_uarray(array, size) dump_array_rec("%lu ", array, size)
#if SIZEOF_LONG == 8
#define dump_xarray(array, size) dump_array_rec("%016lx ", array, size)
#else
#define dump_xarray(array, size) dump_array_rec("%08lx ", array, size)
#endif

#define dump_rec(flag, v) fprintf(stderr, #v" "flag, v)
#define dump_s(v) dump_rec("%ld\n", v)
#define dump_u(v) dump_rec("%lu\n", v)
#if SIZEOF_LONG == 8
#define dump_x(v) dump_rec("%016lx\n", v)
#else
#define dump_x(v) dump_rec("%08lx\n", v)
#endif

#define dump_bignum_s(b) dump_sarray((b)->elements, (b)->size)
#define dump_bignum_u(b) dump_uarray((b)->elements, (b)->size)
#define dump_bignum_x(b) dump_xarray((b)->elements, (b)->size)

/* TODO maybe we want to put this in header file? */
#define SG_LEFT_SHIFT_SPACE(size, amount)		\
  (int)((size) + ((amount) + WORD_BITS -1)/WORD_BITS) 
#include "bignum.inc"

#ifdef USE_MUTABLE_BIGNUM
# include "mbignum.c"
#endif

static long bignum_safe_size_for_add(SgBignum *x, SgBignum *y);
static SgBignum *bignum_add_int(SgBignum *br, SgBignum *bx, SgBignum *by);

static SgBignum* bignum_clear(SgBignum *b, long size)
{
  long i;
  for (i = 0; i < size; i++) b->elements[i] = 0UL;
  return b;
}

static SgBignum* make_bignum_rec(long size, int need_clear)
{
  SgBignum *b;
  long real_size = size;
  if (size < 0) Sg_Error(UC("[internal error] invalid bignum size: %d"), size);
  if (size > SG_BIGNUM_MAX_DIGITS) Sg_Error(UC("too large bignum"));
  if (real_size == 0) real_size++; /* to avoid minus allocation */
  b = SG_NEW_ATOMIC2(SgBignum*, BIGNUM_SIZE(real_size));
  SG_SET_CLASS(b, SG_CLASS_INTEGER);
  if (size == 0) {
    SG_BIGNUM_SET_ZERO(b);
  } else {
    SG_BIGNUM_SET_COUNT(b, size);
    SG_BIGNUM_SET_SIGN(b, 1);
  }
  if (need_clear)
    return bignum_clear(b, real_size);
  else
    return b;
}

SgBignum* Sg_AllocateBignum(int size)
{
  /* should we initialise to zero? */
  return make_bignum_rec(size, TRUE);
}

#define make_bignum(size) make_bignum_rec(size, TRUE)

/* Should we expose this? */
#ifdef HAVE_ALLOCA
#define ALLOC_TEMP_BIGNUM_REC(var, size)		\
  do {							\
    (var) = SG_BIGNUM(alloca(BIGNUM_SIZE(size)));	\
    SG_SET_CLASS(var, SG_CLASS_INTEGER);		\
    SG_BIGNUM_SET_COUNT(var, size);			\
    SG_BIGNUM_SET_SIGN(var, 1);				\
  } while(0)

#define ALLOC_TEMP_BIGNUM(var, size)			\
  do {							\
    ALLOC_TEMP_BIGNUM_REC(var, size);			\
    bignum_clear(var, size);				\
  } while (0)
#else
#define ALLOC_TEMP_BIGNUM_REC(var, size)	\
  do {						\
    (var) = make_bignum_rec(size, FALSE);	\
  } while (0)

#define ALLOC_TEMP_BIGNUM(var, size)		\
  do {						\
    ALLOC_TEMP_BIGNUM_REC(var, size);		\
    bignum_clear(var, size);			\
  } while (0)
#endif


SgObject Sg_MakeBignumFromSI(long value)
{
  SgBignum *b;
  if (value == 0) return make_bignum(0);
  else if (value == LONG_MIN) {
    b = make_bignum(1);
    SG_BIGNUM_SET_SIGN(b, -1);
    b->elements[0] = (unsigned long)LONG_MAX + 1;
  } else if (value < 0) {
    b = make_bignum(1);
    SG_BIGNUM_SET_SIGN(b, -1);
    b->elements[0] = -value;
  } else {
    b = make_bignum(1);
    SG_BIGNUM_SET_SIGN(b, 1);
    b->elements[0] = value;
  }
  return SG_OBJ(b);
}

SgObject Sg_MakeBignumFromUI(unsigned long value)
{
  SgBignum *b;
  if (value == 0) return make_bignum(0);
  b = make_bignum(1);
  SG_BIGNUM_SET_SIGN(b, 1);
  b->elements[0] = value;
  return SG_OBJ(b);
}

SgObject Sg_MakeBignumFromU64(uint64_t value)
{
  if (value) {
#if SIZEOF_LONG >= 8
    SgBignum *ans = make_bignum(1);
    SG_BIGNUM_SET_SIGN(ans, 1);
    ans->elements[0] = value;
    return ans;
#else
    SgBignum *ans;
    if ((value >> WORD_BITS) != 0) {
      ans = make_bignum(2);
      ans->elements[0] = value & SG_ULONG_MAX;
      ans->elements[1] = value >> WORD_BITS;
    } else {
      ans = make_bignum(1);
      ans->elements[0] = (unsigned long)value;
    }
    SG_BIGNUM_SET_SIGN(ans, 1);
    return ans;
#endif
  }
  return make_bignum(0);
}

SgObject Sg_MakeBignumFromS64(int64_t value)
{
  if (value) {
#if SIZEOF_LONG >= 8
    SgBignum *ans = make_bignum(1);
    if (value > 0) {
      SG_BIGNUM_SET_SIGN(ans, 1);
      ans->elements[0] = value;
    } else {
      SG_BIGNUM_SET_SIGN(ans, -1);
      ans->elements[0] = -value;
    }
    return ans;
#else
    int sign;
    SgBignum *ans;
    if (value > 0) {
      sign = 1;
    } else {
      sign = -1;
      value = -value;
    }
    if ((value >> WORD_BITS) != 0) {
      ans = make_bignum(2);
      ans->elements[0] = value & SG_ULONG_MAX;
      ans->elements[1] = value >> WORD_BITS;
    } else {
      ans = make_bignum(1);
      ans->elements[0] = (long)value;
    }
    SG_BIGNUM_SET_SIGN(ans, sign);
    return ans;
#endif
  }
  return make_bignum(0);
}

SgObject Sg_MakeBignumFromDouble(double value)
{
  int expornent, sign;
  SgObject mantissa, b;
  if (value >= LONG_MIN && value <= LONG_MAX) {
    return Sg_MakeBignumFromSI((long)value);
  }
  mantissa = Sg_DecodeFlonum(value, &expornent, &sign);
  if (!SG_NUMBERP(mantissa)) {
    Sg_Error(UC("can't convert %lf to an integer"), value);
  }
  b = Sg_Ash(mantissa, expornent);
  if (sign < 0) b = Sg_Negate(b);
  if (SG_INTP(b)) {
    return Sg_MakeBignumFromSI(SG_INT_VALUE(b));
  } else {
    return b;
  }
}

static inline void bignum_copy(SgBignum *dst, SgBignum *src)
{
  long i;
  long size = SG_BIGNUM_GET_COUNT(src);
  ASSERT(SG_BIGNUM_GET_COUNT(dst) >= size);
  SG_BIGNUM_SET_SIGN(dst, SG_BIGNUM_GET_SIGN(src));
  SG_BIGNUM_SET_COUNT(dst, size);
  for (i = 0; i < size; i++) dst->elements[i] = src->elements[i];
}

SgObject Sg_BignumCopy(SgBignum *b)
{
  SgBignum *c = make_bignum_rec(SG_BIGNUM_GET_COUNT(b), FALSE);
  bignum_copy(c, b);
  return SG_OBJ(c);  
}

static SgObject bignum_normalize_rec(SgBignum *bn, int convertp)
{
  long size = SG_BIGNUM_GET_COUNT(bn);
  long i;
  for (i = size - 1; i > 0; i--) {
    if (bn->elements[i] == 0) size--;
    else break;
  }
  /* if given bignum was 0, i can be -1 */
  if (SG_BIGNUM_GET_SIGN(bn) == 0 && convertp) return SG_MAKE_INT(0);
  if (i <= 0) {
    if (convertp) {
      if (SG_BIGNUM_GET_SIGN(bn) == 0 ||
	  (size == 1 && bn->elements[0] == 0)) {
	return SG_MAKE_INT(0);
      }
      if (SG_BIGNUM_GET_SIGN(bn) > 0
	  && bn->elements[0] <= (unsigned long)SG_INT_MAX) {
	return SG_MAKE_INT(bn->elements[0]);
      }
      if (SG_BIGNUM_GET_SIGN(bn) < 0
	  && bn->elements[0] <= (unsigned long)-SG_INT_MIN) {
	return SG_MAKE_INT(-((signed long)bn->elements[0]));
      }
    } else {
      /* make 0 bignum */
      if (size == 1 && bn->elements[0] == 0) {
	SG_BIGNUM_SET_ZERO(bn);
	return bn;
      }
    }
  }
  SG_BIGNUM_SET_COUNT(bn, size);
  return SG_OBJ(bn);
}

static SgBignum* bignum_normalize(SgBignum *bn)
{
  return SG_BIGNUM(bignum_normalize_rec(bn, FALSE));
}

SgObject Sg_NormalizeBignum(SgBignum *bn)
{
  return bignum_normalize_rec(bn, TRUE);
}


/* assume bignums are normalized */
int Sg_BignumCmp(SgBignum *lhs, SgBignum *rhs)
{
  long lhs_count = SG_BIGNUM_GET_COUNT(lhs);
  long rhs_count = SG_BIGNUM_GET_COUNT(rhs);
  int lhs_sign = SG_BIGNUM_GET_SIGN(lhs);
  int rhs_sign = SG_BIGNUM_GET_SIGN(rhs);
  long i;

  if (lhs_sign < rhs_sign) return -1;
  if (lhs_sign > rhs_sign) return 1;
  if (lhs_count < rhs_count) return (lhs_sign > 0) ? -1 : 1;
  if (lhs_count > rhs_count) return (lhs_sign > 0) ? 1 : -1;
  for (i = lhs_count - 1; i >= 0; i--) {
    if (lhs->elements[i] < rhs->elements[i]) return (lhs_sign > 0) ? -1 : 1;
    if (lhs->elements[i] > rhs->elements[i]) return (lhs_sign > 0) ? 1 : -1;
  }
  return 0;
}

int Sg_BignumCmp3U(SgBignum *bx, SgBignum *off, SgBignum *by)
{
  long xsize = SG_BIGNUM_GET_COUNT(bx);
  long ysize = SG_BIGNUM_GET_COUNT(by);
  long osize = SG_BIGNUM_GET_COUNT(off);
  long tsize;
  long i;
  SgBignum *br;
  
  if (xsize > ysize) return 1;
  if (xsize < ysize) {
    if (osize < ysize && by->elements[ysize - 1] > 1) {
      return -1;
    }
    if (osize == ysize) {
      if (off->elements[osize - 1] > by->elements[ysize - 1]) return 1;
      if (off->elements[osize - 1] < by->elements[ysize - 1] - 1) return -1;
    }
  } else {
    /* xsize == ysize */
    unsigned long w;
    int c = 0;
    if (osize > ysize) return 1;
    if (bx->elements[xsize - 1] > by->elements[ysize - 1]) return 1;
    if (osize < xsize) {
      if (bx->elements[xsize - 1] < by->elements[ysize - 1] - 1) return -1;
    } else {
      /* osize == ysize */
      unsigned long xx = bx->elements[xsize - 1], oo = off->elements[osize-1];
      UADD(w, c, xx, oo);
      if (c > 0 || w > by->elements[ysize - 1]) return 1;
      if (w < by->elements[ysize - 1] - 1) return -1;
    }
  }
  tsize = bignum_safe_size_for_add(bx, off);
  ALLOC_TEMP_BIGNUM(br, tsize);
  bignum_add_int(br, bx, off);
  
  if (SG_BIGNUM_GET_COUNT(br) < SG_BIGNUM_GET_COUNT(by)) return -1;
  for (i = SG_BIGNUM_GET_COUNT(br) - 1; i >= 0; i--) {
    if (i >= SG_BIGNUM_GET_COUNT(by)) {
      if (br->elements[i]) return 1;
      continue;
    }
    if (br->elements[i] < by->elements[i]) return -1;
    if (br->elements[i] > by->elements[i]) return 1;
  }
  return 0;
}

static double u64_to_double(uint64_t u)
{
  union { double f64; uint64_t u64; } d;
  d.u64 = u;
  return d.f64;
}

static long lsb(SgBignum *b)
{
  long count = SG_BIGNUM_GET_COUNT(b), i;
  long l;
  if (count == 0) return -1;
  
  for (i = 0; i < count; i++) {
    l = b->elements[i];
    if (l != 0) break;
  }
  return (i<<SHIFT_MAGIC) + ntz(l);
}

double Sg_BignumToDouble(SgBignum *b)
{
  long count = SG_BIGNUM_GET_COUNT(b);
  long exponent, shift, increment;
  uint64_t twiceSigFloor, sigFloor, sigRounded, bits;
  SgObject o, ab;

  if (count == 0) return 0.0;

  exponent = Sg_BignumBitSize(b) - 1;
  if (exponent < 64 - 1) {
    return (double)Sg_BignumToS64(b, SG_CLAMP_NONE, NULL);
  } else if (exponent > 1023) {
    return SG_BIGNUM_GET_SIGN(b) > 0
      ? u64_to_double(0x7ff0000000000000ULL)
      : u64_to_double(0xfff0000000000000ULL);
  }
  shift = exponent - 53; 	/* significand width */

  /* TODO improve performance... */
  ab = Sg_Abs(b);
  o = Sg_BignumShiftRight(ab, shift);
  if (SG_INTP(o)) {
    twiceSigFloor = SG_INT_VALUE(o);
  } else {
    twiceSigFloor = Sg_BignumToU64(o, SG_CLAMP_NONE, NULL);
  }
  sigFloor = twiceSigFloor >> 1;
  sigFloor &= 0x000FFFFFFFFFFFFFL;
  increment = (twiceSigFloor & 1) != 0
    && ((sigFloor & 1) != 0 || lsb(ab) < shift);
  sigRounded = increment ? sigFloor + 1: sigFloor;
  bits = (uint64_t)(exponent + 1023) << (53 - 1);
  bits += sigRounded;
  bits |= SG_BIGNUM_GET_SIGN(b) & 0x8000000000000000L;
  return u64_to_double(bits);
}

#ifndef NDEBUG
static inline int bn_norm_pred(SgBignum *bn)
{
  long bn_count = SG_BIGNUM_GET_COUNT(bn);
  return (bn_count == 0) || (bn->elements[bn_count - 1] != 0);
}
#endif

SgObject Sg_BignumToInteger(SgBignum *bn)
{
  ASSERT(bn_norm_pred(bn));
  ASSERT(SG_BIGNUM_GET_SIGN(bn) != 0);
  if (SG_BIGNUM_GET_COUNT(bn) == 0) return SG_MAKE_INT(0);
  if (SG_BIGNUM_GET_COUNT(bn) == 1) {
    unsigned long n = bn->elements[0];
    if (SG_BIGNUM_GET_SIGN(bn) < 0) {
      if (n < SG_ULONG_MAX) {
	n = (unsigned long)-(long)n;
      } else {
	/* more than long */
	return Sg_BignumCopy(bn);
      }
    }
    if ((n >= SG_INT_MIN) && (n <= SG_INT_MAX)) return SG_MAKE_INT(n);
  }
  return Sg_BignumCopy(bn);
}

long Sg_BignumToSI(SgBignum *b, int clamp, int *oor)
{
  if (clamp == SG_CLAMP_NONE && oor != NULL) *oor = FALSE;
  if (SG_BIGNUM_GET_SIGN(b) >= 0) {
    if (b->elements[0] > LONG_MAX || SG_BIGNUM_GET_COUNT(b) >= 2) {
      if (clamp & SG_CLAMP_HI) return LONG_MAX;
      else goto err;
    } else {
      return (long)b->elements[0];
    }
  } else {
    if (b->elements[0] > (unsigned long)LONG_MAX + 1 ||
	SG_BIGNUM_GET_COUNT(b) >= 2) {
      if (clamp & SG_CLAMP_LO) return LONG_MIN;
      else goto err;
    } else {
      return -(long)b->elements[0];
    }
  }
 err:
  if (clamp == SG_CLAMP_NONE && oor != NULL) *oor = TRUE;
  else {
    Sg_Error(UC("argument out of range: %S"), SG_OBJ(b));
  }
  return 0;
}

unsigned long Sg_BignumToUI(SgBignum *b, int clamp, int *oor)
{
  if (clamp == SG_CLAMP_NONE && oor != NULL) *oor = FALSE;
  if (SG_BIGNUM_GET_SIGN(b) >= 0) {
    if (SG_BIGNUM_GET_COUNT(b) >= 2) {
      if (clamp & SG_CLAMP_HI) return SG_ULONG_MAX;
      else goto err;
    } else {
      return b->elements[0];
    }
  } else {
    if (clamp & SG_CLAMP_LO) return 0;
    else goto err;
  }
 err:
  if (clamp == SG_CLAMP_NONE && oor != NULL) *oor = TRUE;
  else {
    Sg_Error(UC("argument out of range: %S"), SG_OBJ(b));
  }
  return 0;
}

#if SIZEOF_LONG >= 8
int32_t  Sg_BignumToS32(SgBignum *b, int clamp, int *oor)
{
  if (clamp == SG_CLAMP_NONE && oor != NULL) *oor = FALSE;
  if (SG_BIGNUM_GET_SIGN(b) >= 0) {
    if (b->elements[0] > INT32_MAX || SG_BIGNUM_GET_COUNT(b) >= 2) {
      if (clamp & SG_CLAMP_HI) return INT32_MAX;
      else goto err;
    } else {
      return (int32_t)b->elements[0];
    }
  } else {
    if (b->elements[0] > (uint32_t)INT32_MAX + 1 ||
	SG_BIGNUM_GET_COUNT(b) >= 2) {
      if (clamp & SG_CLAMP_LO) return INT32_MIN;
      else goto err;
    } else {
      return -(int32_t)b->elements[0];
    }
  }
 err:
  if (clamp == SG_CLAMP_NONE && oor != NULL) *oor = TRUE;
  else {
    Sg_Error(UC("argument out of range: %S"), SG_OBJ(b));
  }
  return 0;  
}

uint32_t Sg_BignumToU32(SgBignum *b, int clamp, int *oor)
{
  if (clamp == SG_CLAMP_NONE && oor != NULL) *oor = FALSE;
  if (SG_BIGNUM_GET_SIGN(b) > 0) {
    if (b->elements[0] <= UINT32_MAX) {
      return (uint32_t)b->elements[0];
    } else {
      if (!(clamp & SG_CLAMP_HI)) goto err;
      return UINT32_MAX;
    }
  } else {
    if (clamp & SG_CLAMP_LO) return 0;
    else goto err;
  }
 err:
  if (clamp == SG_CLAMP_NONE && oor != NULL) *oor = TRUE;
  else {
    Sg_Error(UC("argument out of range: %S"), SG_OBJ(b));
  }
  return 0;
}

#else
int64_t Sg_BignumToS64(SgBignum *b, int clamp, int *oor)
{
  int64_t r = 0;
  if (clamp == SG_CLAMP_NONE && oor != NULL) *oor = FALSE;
  if (SG_BIGNUM_GET_SIGN(b) > 0) {
    if (SG_BIGNUM_GET_COUNT(b) == 1) {
      r = b->elements[0];
    } else if (SG_BIGNUM_GET_COUNT(b) > 2 || b->elements[1] > LONG_MAX) {
      if (!(clamp & SG_CLAMP_HI)) goto err;
      r = (((int64_t)LONG_MAX) << 32) + (int64_t)ULONG_MAX;
    } else {
      r = ((int64_t)b->elements[1] << 32) + (uint64_t)b->elements[0];
    }
  } else {
    if (SG_BIGNUM_GET_COUNT(b) == 1) {
      r = -(int64_t)b->elements[0];
    } else if (SG_BIGNUM_GET_COUNT(b) > 2 ||
	       b->elements[1] > (unsigned long)LONG_MAX+1) {
      if (!(clamp & SG_CLAMP_HI)) goto err;
      r = (((int64_t)LONG_MAX + 1) << 32);
    } else {
      r = -(int64_t)(((int64_t)b->elements[1] << 32)+(uint64_t)b->elements[0]);
    }
  }
  return r;
 err:
  if (clamp == SG_CLAMP_NONE && oor != NULL) {
    *oor = TRUE;
  } else {
    Sg_Error(UC("argument out of range: %S"), SG_OBJ(b));
  }
  return r;
}

uint64_t Sg_BignumToU64(SgBignum *b, int clamp, int *oor)
{
  uint64_t r = 0;
  if (clamp == SG_CLAMP_NONE && oor != NULL) *oor = FALSE;
  if (SG_BIGNUM_GET_SIGN(b) > 0) {
    if (SG_BIGNUM_GET_COUNT(b) > 2) {
      if (!(clamp & SG_CLAMP_HI)) goto err;
      r = (((uint64_t)ULONG_MAX) << 32) + (uint64_t)ULONG_MAX;

    } else if (SG_BIGNUM_GET_COUNT(b) == 2) {
      r = (((uint64_t)b->elements[1]) << 32) + (uint64_t)b->elements[0];
    } else {
      r = (uint64_t)b->elements[0];
    }
  } else {
    if (!(clamp & SG_CLAMP_LO)) goto err;
  }
  return r;
 err:
  if (clamp == SG_CLAMP_NONE && oor != NULL) {
    *oor = TRUE;
  } else {
    Sg_Error(UC("argument out of range: %S"), SG_OBJ(b));
  }
  return r;
}

#endif


static inline SgBignum* bignum_2scmpl(SgBignum *br)
{
  mp_2scmpl(br->elements, br->size);
  return br;
}

SgObject Sg_BignumComplement(SgBignum *bx)
{
  SgBignum *r = SG_BIGNUM(Sg_BignumCopy(bx));
  return SG_OBJ(bignum_2scmpl(r));
}


long Sg_BignumBitCount(SgBignum *b)
{
  unsigned long *bits;
  SgBignum *z;
  long size;
  if (SG_BIGNUM_GET_SIGN(b) == 0) {
    return 0;
  } else if (SG_BIGNUM_GET_SIGN(b) > 0) {
    z = b;
    goto calc_bit_count;
  }
  ALLOC_TEMP_BIGNUM(z, SG_BIGNUM_GET_COUNT(b));
  bignum_copy(z, b);
  bignum_2scmpl(z);

 calc_bit_count:
  size = SG_BIGNUM_GET_COUNT(z) * WORD_BITS;
  bits = z->elements;
  if (SG_BIGNUM_GET_SIGN(b) > 0) {
    return Sg_BitsCount1(bits, 0, size);
  } else {
    return ~Sg_BitsCount0(bits, 0, size);
  }
}

long Sg_BignumBitSize(SgBignum *b)
{
  if (SG_BIGNUM_GET_SIGN(b) == 0) return 0;
  return mp_bit_size(b->elements, b->size);
}

long Sg_BignumFirstBitSet(SgBignum *b)
{
  long bit = 0, i, size;
  SgBignum *z;
  if (SG_BIGNUM_GET_SIGN(b) == 0) return 0;
  else if (SG_BIGNUM_GET_SIGN(b) > 0) {
    z = b;
    goto calc_bit_set;
  }
  ALLOC_TEMP_BIGNUM_REC(z, SG_BIGNUM_GET_COUNT(b));
  bignum_copy(z, b);
  bignum_2scmpl(z);

 calc_bit_set:
  size = SG_BIGNUM_GET_COUNT(z);
  for (i = 0; i < size; i++) {
    unsigned long n = z->elements[i];
    if (n == 0) { bit += WORD_BITS; continue; }
    bit += ntz(n);
    return bit;
  }
  /* Sg_Write(b, Sg_StandardErrorPort(), 0); */
  ASSERT(FALSE);
  return -1;			/* dummy */
}

static inline int bignum_test_bit(SgBignum *b, long p)
{
  long pos = p >> SHIFT_MAGIC;
  ulong v;
  if (pos >= (long)SG_BIGNUM_GET_COUNT(b)) {
    return FALSE;
  }
  v = b->elements[pos];
  if (pos) {
    p %= WORD_BITS;
  }
  return (v & (1L << p)) != 0;
}

int Sg_BignumBitSetP(SgBignum *b, long n)
{
  return bignum_test_bit(b, n);
}

int Sg_BignumAbsCmp(SgBignum *bx, SgBignum *by)
{
  long i;
  long xsize = SG_BIGNUM_GET_COUNT(bx);
  long ysize = SG_BIGNUM_GET_COUNT(by);
    
  if (xsize < ysize) return -1;
  if (xsize > ysize) return 1;
  for (i = (int)xsize-1; i >= 0; i--) {
    if (bx->elements[i] < by->elements[i]) return -1;
    if (bx->elements[i] > by->elements[i]) return 1;
  }
  return 0;
}

/* returns bignum, so weird name... */
SgObject Sg_BignumAshSI(long lx, long count)
{
  SgBignum *x;
  if (count > 0 && lx == 1) {
    long size = SG_LEFT_SHIFT_SPACE(1, count);
    long words = count / WORD_BITS;
    long nbits = count % WORD_BITS;
    ulong t = (ulong)lx << nbits;
    x = make_bignum(size);
    x->elements[words] = t;
    return Sg_NormalizeBignum(x);
  } else {
    ALLOC_TEMP_BIGNUM(x, 1);
    if (lx < 0) {
      x->elements[0] = -lx;
      x->sign = -1;
    } else {
      x->elements[0] = lx;
    }
    return Sg_BignumAsh(x, count);
  }
}

SgObject Sg_BignumAsh(SgBignum *b, long count)
{
  if (count == 0) return Sg_NormalizeBignum(b);
  else if (count > 0) return Sg_BignumShiftLeft(b, count);
  else return Sg_BignumShiftRight(b, -count);
}

static SgBignum* bignum_lshift(SgBignum *br, SgBignum *bx, long amount)
{
  mp_lshift(br->elements, br->size, bx->elements, bx->size, amount);
  if (br != bx) {
    SG_BIGNUM_SET_SIGN(br, SG_BIGNUM_GET_SIGN(bx));
  }
  return br;
}

SgObject Sg_BignumShiftLeft(SgBignum *b, long shift)
{
  int rsize = SG_LEFT_SHIFT_SPACE(b->size, shift);
  SgBignum *r = make_bignum(rsize);
  return Sg_NormalizeBignum(bignum_lshift(r, b, shift));
}

static SgBignum* bignum_rshift(SgBignum *br, SgBignum *bx, long amount)
{
  ulong size = mp_rshift(br->elements, br->size,
			 bx->elements, bx->size, amount);
  
  SG_BIGNUM_SET_COUNT(br, size);
  SG_BIGNUM_SET_SIGN(br, SG_BIGNUM_GET_SIGN(bx));
  return br;
}

SgObject Sg_BignumShiftRight(SgBignum *b, long shift)
{
  long rsize = SG_BIGNUM_GET_COUNT(b) + (-shift)/WORD_BITS;
  if (rsize < 1) {
    if (SG_BIGNUM_GET_SIGN(b) < 0) {
      return SG_MAKE_INT(-1);
    } else {
      return SG_MAKE_INT(0);
    }
  } else {
    if (SG_BIGNUM_GET_SIGN(b) < 0) {
      SgObject r = Sg_Quotient(Sg_Add(SG_OBJ(b), SG_MAKE_INT(1)),
			       Sg_Ash(SG_MAKE_INT(1), shift),
			       NULL);
      return Sg_Add(r, SG_MAKE_INT(-1));
    } else {
      SgBignum *r = make_bignum(rsize);
      return Sg_NormalizeBignum(bignum_rshift(r, b, shift));
    }
  }
}

#define DEF_BIGNUM_LOG_OP(name, op)					\
  static SgBignum* name(SgBignum *z, SgBignum *x, SgBignum *y,		\
			int x2sc, int y2sc)				\
  {									\
    int i;								\
    long xs = SG_BIGNUM_GET_COUNT(x);					\
    long ys = SG_BIGNUM_GET_COUNT(y);					\
    long zs = SG_BIGNUM_GET_COUNT(z);					\
    long m = min(xs, ys);						\
    for (i = m-1; i >= 0; i--) {					\
      z->elements[i] = x->elements[i] op y->elements[i];		\
    }									\
    if (xs > m && xs <= zs) {						\
      for (i = xs-1; i >= m; i--)					\
	z->elements[i] = x->elements[i] op (y2sc ? SG_ULONG_MAX : 0);	\
    }									\
    if (ys > m && ys <= zs) {						\
      for (i = ys-1; i >= m; i--)					\
	z->elements[i] = y->elements[i] op (x2sc ? SG_ULONG_MAX : 0);	\
    }									\
    return z;								\
  }

DEF_BIGNUM_LOG_OP(bignum_and, &)

SgObject Sg_BignumLogAnd(SgBignum *x, SgBignum *y)
{
  long xsize = SG_BIGNUM_GET_COUNT(x);
  int  xsign = SG_BIGNUM_GET_SIGN(x);
  long ysize = SG_BIGNUM_GET_COUNT(y);
  int  ysign = SG_BIGNUM_GET_SIGN(y);
  long zsize, minsize = min(xsize, ysize);
  SgBignum *xx, *yy, *z;

  /* handle obvious case first */
  if (xsign == 0 || ysign == 0) return SG_MAKE_INT(0);

  if (xsign > 0) {
    if (ysign > 0) { 		
      /* (+, +) */
      z = bignum_and(make_bignum(minsize), x, y, FALSE, FALSE);
      return Sg_NormalizeBignum(z);
    } else {
      /* (+, -) */
      ALLOC_TEMP_BIGNUM_REC(yy, ysize);
      bignum_copy(yy, y);
      bignum_2scmpl(yy);
      z = bignum_and(make_bignum(xsize), x, yy, FALSE, TRUE);
      return Sg_NormalizeBignum(z);
    }
  } else {
    if (ysign > 0) {
      /* (-, +) */
      ALLOC_TEMP_BIGNUM_REC(xx, xsize);
      bignum_copy(xx, x);
      bignum_2scmpl(xx);
      z = bignum_and(make_bignum(ysize), xx, y, TRUE, FALSE);
      return Sg_NormalizeBignum(z);
    } else {
      /* (-, -) */
      ALLOC_TEMP_BIGNUM_REC(xx, xsize);
      bignum_copy(xx, x);
      bignum_2scmpl(xx);
      ALLOC_TEMP_BIGNUM_REC(yy, ysize);
      bignum_copy(yy, y);
      bignum_2scmpl(yy);

      zsize = max(xsize, ysize);
      z = bignum_and(make_bignum(zsize), xx, yy, TRUE, TRUE);
      SG_BIGNUM_SET_SIGN(z, -1);
      bignum_2scmpl(z);
      return Sg_NormalizeBignum(z);
    }
  }
}

DEF_BIGNUM_LOG_OP(bignum_ior, |)

SgObject Sg_BignumLogIor(SgBignum *x, SgBignum *y)
{
  long xsize = SG_BIGNUM_GET_COUNT(x);
  int xsign = SG_BIGNUM_GET_SIGN(x);
  long ysize = SG_BIGNUM_GET_COUNT(y);
  int ysign = SG_BIGNUM_GET_SIGN(y);
  long zsize, minsize = min(xsize, ysize);
  SgBignum *xx, *yy, *z;

  /* handle 0 case first */
  if (xsign == 0 || ysign == 0) {
    if (xsign) return Sg_NormalizeBignum(x);
    if (ysign) return Sg_NormalizeBignum(y);
    return SG_MAKE_INT(0);
  }

  if (xsign > 0) {
    if (ysign > 0) {
      /* (+, +) */
      zsize = max(xsize, ysize);
      z = bignum_ior(make_bignum(zsize), x, y, FALSE, FALSE);
      return Sg_NormalizeBignum(z);
    } else {
      /* (+, -) */
      ALLOC_TEMP_BIGNUM_REC(yy, ysize);
      bignum_copy(yy, y);
      bignum_2scmpl(yy);
      z = bignum_ior(make_bignum(ysize), x, yy, FALSE, TRUE);
      SG_BIGNUM_SET_SIGN(z, -1);
      bignum_2scmpl(z);
      return Sg_NormalizeBignum(z);
    }
  } else {
    if (ysign > 0) {
      /* (-, +) */
      ALLOC_TEMP_BIGNUM_REC(xx, xsize);
      bignum_copy(xx, x);
      bignum_2scmpl(xx);
      z = bignum_ior(make_bignum(xsize), xx, y, TRUE, FALSE);
      SG_BIGNUM_SET_SIGN(z, -1);
      bignum_2scmpl(z);
      return Sg_NormalizeBignum(z);
    } else {
      /* (-, -) */
      ALLOC_TEMP_BIGNUM_REC(xx, xsize);
      bignum_copy(xx, x);
      bignum_2scmpl(xx);
      ALLOC_TEMP_BIGNUM_REC(yy, ysize);
      bignum_copy(yy, y);
      bignum_2scmpl(yy);

      z = bignum_ior(make_bignum(minsize), xx, yy, TRUE, TRUE);
      SG_BIGNUM_SET_SIGN(z, -1);
      bignum_2scmpl(z);
      return Sg_NormalizeBignum(z);
    }
  }
}

DEF_BIGNUM_LOG_OP(bignum_xor, ^)

SgObject Sg_BignumLogXor(SgBignum *x, SgBignum *y)
{
  long xsize = SG_BIGNUM_GET_COUNT(x);
  int  xsign = SG_BIGNUM_GET_SIGN(x);
  long ysize = SG_BIGNUM_GET_COUNT(y);
  int  ysign = SG_BIGNUM_GET_SIGN(y);
  long zsize = max(xsize, ysize);
  SgBignum *xx, *yy, *z;

  /* handle 0 case first */
  if (xsign == 0 || ysign == 0) {
    if (xsign) return Sg_NormalizeBignum(x);
    if (ysign) return Sg_NormalizeBignum(y);
    return SG_MAKE_INT(0);
  }

  if (xsign > 0) {
    if (ysign > 0) {
      /* (+,+) */
      z = bignum_xor(make_bignum(zsize), x, y, FALSE, FALSE);
      return Sg_NormalizeBignum(z);
    } else {
      /* (+,-) */
      ALLOC_TEMP_BIGNUM_REC(yy, ysize);
      bignum_copy(yy, y);
      bignum_2scmpl(yy);
      z = bignum_xor(make_bignum(zsize), x, yy, FALSE, TRUE);
      SG_BIGNUM_SET_SIGN(z, -1);
      bignum_2scmpl(z);
      return Sg_NormalizeBignum(z);
    }
  } else {
    if (ysign > 0) {
      /* (-,+) */
      ALLOC_TEMP_BIGNUM_REC(xx, xsize);
      bignum_copy(xx, x);
      bignum_2scmpl(xx);
      z = bignum_xor(make_bignum(zsize), xx, y, TRUE, FALSE);
      SG_BIGNUM_SET_SIGN(z, -1);
      bignum_2scmpl(z);
      return Sg_NormalizeBignum(z);
    } else {
      /* (-,-) */
      ALLOC_TEMP_BIGNUM_REC(xx, xsize);
      bignum_copy(xx, x);
      bignum_2scmpl(xx);
      ALLOC_TEMP_BIGNUM_REC(yy, ysize);
      bignum_copy(yy, y);
      bignum_2scmpl(yy);

      z = bignum_xor(make_bignum(zsize), xx, yy, TRUE, TRUE);
      return Sg_NormalizeBignum(z);
    }
  }
}

#define DEF_SI_LOGOP(op)				\
  SgObject SG_CPP_CAT(op, SI)(SgBignum *x, long y)	\
  {							\
    SgBignum *by;					\
    ALLOC_TEMP_BIGNUM(by, 1);				\
    if (y == 0) {					\
      SG_BIGNUM_SET_SIGN(by, 0);			\
    } else if (y == LONG_MIN) {				\
      by->elements[0] = (unsigned long)LONG_MAX + 1;	\
      SG_BIGNUM_SET_SIGN(by, -1);			\
    } else if (y < 0) {					\
      by->elements[0] = -y;				\
      SG_BIGNUM_SET_SIGN(by, -1);			\
    } else {						\
      by->elements[0] = y;				\
      SG_BIGNUM_SET_SIGN(by, 1);			\
    }							\
    return op(x, by);					\
}

DEF_SI_LOGOP(Sg_BignumLogAnd)
DEF_SI_LOGOP(Sg_BignumLogIor)
DEF_SI_LOGOP(Sg_BignumLogXor)

static long bignum_safe_size_for_add(SgBignum *x, SgBignum *y)
{
  long xsize = SG_BIGNUM_GET_COUNT(x);
  long ysize = SG_BIGNUM_GET_COUNT(y);
  return mp_safe_size_for_add(x->elements, xsize, y->elements, ysize);
}

static SgBignum* bignum_add_int(SgBignum *br, SgBignum *bx, SgBignum *by)
{
  long rsize = SG_BIGNUM_GET_COUNT(br);
  long xsize = SG_BIGNUM_GET_COUNT(bx);
  long ysize = SG_BIGNUM_GET_COUNT(by);
  long i;

  /* handle 0 first */
  if (xsize == 0) {
    if (ysize == 0) {
      SG_BIGNUM_SET_ZERO(br);
      return br;
    }
    for (i = 0; i < ysize; i++) {
      br->elements[i] = by->elements[i];
    }
    SG_BIGNUM_SET_SIGN(br, SG_BIGNUM_GET_SIGN(by));
  } else if (ysize == 0) {
    for (i = 0; i < xsize; i++) {
      br->elements[i] = bx->elements[i];
    }
    SG_BIGNUM_SET_SIGN(br, SG_BIGNUM_GET_SIGN(bx));
  } else {
      /* if x is shorter, swap it */
    if (xsize < ysize) {
      SgBignum *t = bx;
      ulong tsize = xsize;
      bx = by; by = t;
      xsize = ysize; ysize = tsize;
    }
    mp_add(br->elements, rsize, bx->elements, xsize, by->elements, ysize);
  }
  return br;
}

static SgBignum* bignum_sub_int(SgBignum *br, SgBignum *bx, SgBignum *by)
{
  long rsize = SG_BIGNUM_GET_COUNT(br);
  long xsize = SG_BIGNUM_GET_COUNT(bx);
  long ysize = SG_BIGNUM_GET_COUNT(by);
  long i;

  /* handle 0 first */
  if (xsize == 0) {
    if (ysize == 0) {
      SG_BIGNUM_SET_ZERO(br);
      return br;
    }
    for (i = 0; i < ysize; i++) {
      br->elements[i] = by->elements[i];
    }
    /* 0 - n must be - */
    SG_BIGNUM_SET_SIGN(br, SG_BIGNUM_GET_SIGN(by));
  } else if (ysize == 0) {
    for (i = 0; i < xsize; i++) {
      br->elements[i] = bx->elements[i];
    }
    SG_BIGNUM_SET_SIGN(br, SG_BIGNUM_GET_SIGN(bx));
  } else {
    int flip = FALSE, c;
    if (xsize < ysize) {
      SgBignum *t = bx;
      bx = by; by = t;
      xsize = SG_BIGNUM_GET_COUNT(bx);
      ysize = SG_BIGNUM_GET_COUNT(by);
      flip = TRUE;
    }
    c = mp_sub(br->elements, rsize, bx->elements, xsize, by->elements, ysize);
    if (flip || c) {
      if (c) bignum_2scmpl(br);
      SG_BIGNUM_SET_SIGN(br, -SG_BIGNUM_GET_SIGN(br));
    }
  }
  return br;
}

static SgBignum* bignum_add(SgBignum *bx, SgBignum *by)
{
  long rsize = bignum_safe_size_for_add(bx, by);
  SgBignum *br = make_bignum(rsize);
  SG_BIGNUM_SET_SIGN(br, SG_BIGNUM_GET_SIGN(bx));
  if (SG_BIGNUM_GET_SIGN(bx) == SG_BIGNUM_GET_SIGN(by)) {
    bignum_add_int(br, bx, by);
  } else {
    bignum_sub_int(br, bx, by);
  }
  return br;
}
static SgBignum* bignum_sub(SgBignum *bx, SgBignum *by)
{
  long rsize = bignum_safe_size_for_add(bx, by);
  SgBignum *br = make_bignum(rsize);
  SG_BIGNUM_SET_SIGN(br, SG_BIGNUM_GET_SIGN(bx));
  if (SG_BIGNUM_GET_SIGN(bx) == SG_BIGNUM_GET_SIGN(by)) {
    bignum_sub_int(br, bx, by);
  } else {
    bignum_add_int(br, bx, by);
    if (SG_BIGNUM_GET_SIGN(bx) == 0) {
      /*flip sign*/ 
      SG_BIGNUM_SET_SIGN(br, -SG_BIGNUM_GET_SIGN(by));
    }
  }
  return br;
}

static SgBignum* bignum_add_si(SgBignum *bx, long y)
{
  ulong yabs;
  int ysign;
  SgBignum *br;

  if (y == 0) return bx;	/* short cut */

  yabs =  ((y < 0) ? -y : y);
  ysign = ((y < 0) ? -1: 1);
  br = make_bignum(bx->size + 1);
  SG_BIGNUM_SET_SIGN(br, SG_BIGNUM_GET_SIGN(bx));
  /* dispatch here */
  if (SG_BIGNUM_GET_SIGN(bx) == ysign) {
    mp_add_ul(br->elements, br->size, bx->elements, bx->size, yabs);
  } else {
    mp_sub_ul(br->elements, br->size, bx->elements, bx->size, yabs);
  }
  return br;
}

SgObject Sg_BignumAdd(SgBignum *a, SgBignum *b)
{
  return Sg_NormalizeBignum(bignum_add(a, b));
}


SgObject Sg_BignumAddSI(SgBignum *a, long b)
{
  return Sg_NormalizeBignum(bignum_add_si(a, b));
}

SgObject Sg_BignumSub(SgBignum *a, SgBignum *b)
{
  return Sg_NormalizeBignum(bignum_sub(a, b));
}

SgObject Sg_BignumSubSI(SgBignum *a, long b)
{
  return Sg_NormalizeBignum(bignum_add_si(a, -b));
}

/* whatever i did, it's slow. */
#if 0
#include <emmintrin.h>

typedef struct
{
  ulong r0lo;			/* r0 low */
  ulong r0hi;			/* r0 high */
  ulong r1lo;			/* r1 low */
  ulong r1hi;			/* r1 high */
} r4;

static inline SgBignum* bignum_mul_word(SgBignum *br, SgBignum *bx,
					unsigned long y)
{
  int size = SG_BIGNUM_GET_COUNT(bx);
  /* do trivial case first */
  if (size == 1) {
    /* only one element in bx */
    udlong p;
    p = (udlong)bx->elements[0] * y;
    br->elements[0] = (ulong)p;
    br->elements[1] = (ulong)(p >> WORD_BITS);
    return br;
  } else if (size == 2) {
    /* only 2 elements in bx */
    udlong p;
    p = (udlong)bx->elements[0] * y;
    br->elements[0] = (ulong)p;
    p = (udlong)bx->elements[1] * y + (ulong)(p >> WORD_BITS);
    br->elements[1] = (ulong)p;
    br->elements[2] = (ulong)(p >> WORD_BITS);
    return br;
  } else {
    /* more than 3 elements means at least one loop */
#if 1
    /* try use SSE as mere (64 bit) register. 
       why is there no 64x64 multiplication? */
    int i;
    __m128i p, x, yy, t;
    yy = _mm_cvtsi32_si128(y);
    for (i = 0; i < size; i++) {
      x = _mm_cvtsi32_si128(bx->elements[i]);
      t = _mm_cvtsi32_si128(br->elements[i]); /* carry is here */
      p = _mm_add_epi64(_mm_mul_epu32(x, yy), t);
      _mm_storel_epi64(&br->elements[i], p);
    }
    /* the last carry is already stored */
    return br;
#else
    /* following was actually slow... */
    /* debug */
/* #define DEBUG_SSE2 */
#ifdef DEBUG_SSE2
#define dump_m128(m)					\
  do {							\
    r4 r_ __attribute__((aligned(16)));			\
    _mm_store_si128((__m128i *)&r_, (m));		\
    fprintf(stderr, "%08lx:%08lx:%08lx:%08lx:%s\n",	\
	    r_.r0hi, r_.r0lo, r_.r1hi, r_.r1lo, #m);	\
  } while (0)
#else
#define dump_m128(m)		/* dummy */
#endif
    /* 
       p0(h,l) = x0 * y
       p1(h,l) = x1 * y + p0(h)
       p2(h,l) = x2 * y + p1(h)

       i.e.)
         p0    = 0x0000002e257a5000
         p0(h) =         0x0000002e
	 x1y   = 0x0000002478ce03be
	 p1    = 0x0000002478ce03ec
	 p1(h) =         0x00000024 = high(low(x1y) + p0(h)) + high(x1y)

       pn(h) = high(low(xn * y) + pn-1(h)) + high(xn * y)

       c0 register holds pn-1(h)
       c1 register holds pn(h)
     */
#define compute_sse(x0, x1)						\
  do {									\
    /* compute carry from previous (r1 register) */			\
    /* carry pi-1 and pi-2 */						\
    c1 = _mm_shuffle_epi32(p, _MM_SHUFFLE(0,1,2,3));			\
    c1 = _mm_and_si128(c1, ml); /* mask */				\
    dump_m128(c1);							\
    xx = _mm_set_epi64x((x1), (x0));					\
    p  = _mm_mul_epu32(xx, yy); /* (x0 * y0) & (x1 * y1)  */		\
    dump_m128(p);							\
    c0 = _mm_and_si128(p, ml);  /* low(x1y) */				\
    c0 = _mm_add_epi64(c0, c1); /* low(x1y) + p0(h) */			\
    dump_m128(c0);							\
    xx = _mm_srli_epi64(c0, WORD_BITS);	/* high(low(x1y) + p0(h)) */	\
    c0 = _mm_srli_epi64(p, WORD_BITS);  /* high(x1y) */			\
    dump_m128(xx);							\
    xx = _mm_add_epi64(xx, c0);	/* high(low(x1y) +p0(h)) + high(x1y) */ \
    dump_m128(xx);							\
    /* now xx contains p1(h) in r1 so shuffle it */			\
    /* p1 = x*y + (ulong)(p0>>32) */					\
    xx = _mm_and_si128(xx, mx); /* clear r0,r1,r2 */			\
    xx = _mm_shuffle_epi32(xx, _MM_SHUFFLE(1,0,3,2));			\
    c1 = _mm_and_si128(c1, mx);						\
    dump_m128(xx);							\
    c0 = _mm_or_si128(xx, c1);						\
    dump_m128(c0);							\
    p  = _mm_add_epi64(p, c0);						\
    dump_m128(p);							\
  } while (0)

    int i;
    __m128i p, xx, yy, c0, c1, ml, mx;
    __attribute__((aligned(16))) r4 pr = {0, 0, 0, 0};

    yy = _mm_set_epi64x(y, y);
    ml = _mm_set_epi32(0, 0xffffffffUL, 0, 0xffffffffUL);
    mx = _mm_set_epi32(0, 0, 0, 0xffffffffUL);
    /* set 0 */
    p = _mm_setzero_si128();
    for (i = 0; i < size - 1; i += 2) {
      compute_sse(bx->elements[i], bx->elements[i+1]);
      _mm_store_si128((__m128i *)&pr, p);
      br->elements[i]   = pr.r0lo;
      br->elements[i+1] = pr.r1lo;
    }
    /* do the rest */
    if (size & 1) {
      compute_sse(bx->elements[i], 0);
      _mm_store_si128((__m128i *)&pr, p);
      br->elements[i]   = pr.r0lo;
      br->elements[i+1] = pr.r0hi;
    } else {
      br->elements[i] = pr.r1hi;
    }
    return br;
#undef compute_sse
#endif
  }
}
#else
static inline SgBignum* bignum_mul_word(SgBignum *br, SgBignum *bx,
					unsigned long y)
{
  mp_mul_ul(br->elements, br->size, bx->elements, bx->size, y);
  return br;
}
#endif

static SgBignum* bignum_mul_int(SgBignum *br, SgBignum *bx, SgBignum *by)
{
  long xlen = SG_BIGNUM_GET_COUNT(bx);
  long ylen = SG_BIGNUM_GET_COUNT(by);
  /* early check */
  SG_BIGNUM_SET_SIGN(br, SG_BIGNUM_GET_SIGN(bx) * SG_BIGNUM_GET_SIGN(by));
  mp_mul(br->elements, br->size, bx->elements, xlen, by->elements, ylen);
  return br;
}

static SgBignum* bignum_mul(SgBignum *bx, SgBignum *by)
{
  long xlen = SG_BIGNUM_GET_COUNT(bx);
  long ylen = SG_BIGNUM_GET_COUNT(by);
  SgBignum *br;

  if (xlen == 0) return bx;
  if (ylen == 0) return by;

  br = make_bignum(xlen + ylen);
  return bignum_mul_int(br, bx, by);
}

static SgBignum* bignum_mul_si(SgBignum *bx, long y)
{
  SgBignum *br;
  unsigned long yabs;

  if (y == 1L) return bx;
  else if (y == 0) {
    br = make_bignum(1);
    SG_BIGNUM_SET_SIGN(br, 0);
    br->elements[0] = 0;
    return br;
  } else if (y == -1L) {
    br = SG_BIGNUM(Sg_BignumCopy(bx));
    SG_BIGNUM_SET_SIGN(br, -SG_BIGNUM_GET_SIGN(bx));
    return br;
  } else {
    br = make_bignum(SG_BIGNUM_GET_COUNT(bx) + 1);
    yabs = (y < 0) ? -y : y;
    SG_BIGNUM_SET_SIGN(br, SG_BIGNUM_GET_SIGN(bx));
    bignum_mul_word(br, bx, yabs);
    if (y < 0) SG_BIGNUM_SET_SIGN(br, -SG_BIGNUM_GET_SIGN(br));
    return br;
  }
}


SgObject Sg_BignumMul(SgBignum *a, SgBignum *b)
{
  return Sg_NormalizeBignum(bignum_mul(a, b));
}

SgObject Sg_BignumMulSI(SgBignum *a, long b)
{
  return Sg_NormalizeBignum(bignum_mul_si(a, b));
}

SgObject Sg_BignumSquare(SgBignum *bx)
{
  long len = SG_BIGNUM_GET_COUNT(bx);  
  SgBignum *br = make_bignum(len<<1);
  /* square_to_len(bx->elements, len, br->elements); */
  mp_square(br->elements, br->size, bx->elements, len);
  SG_BIGNUM_SET_SIGN(br, 1);
  return Sg_NormalizeBignum(br);
}

static void bignum_gdiv_rem(SgBignum *dividend, SgBignum *divisor,
			    SgBignum *quotient, SgBignum *remainder)
{
  ulong rsize;
  ulong *q = NULL, *r = NULL;
  ulong qs = 0, rs = 0;
  if (remainder) {
    rs = remainder->size;
    r =  remainder->elements;
  }
  if (quotient) {
    q = quotient->elements;
    qs = quotient->size;
  }
  rsize = mp_div_rem(q, qs,
		     r, rs,
		     dividend->elements, dividend->size,
		     divisor->elements, divisor->size);
  if (remainder) {
    remainder->size = rsize;
    remainder->sign = dividend->sign;
  }
}

static SgBignum* bignum_gdiv_rec(SgBignum *dividend, SgBignum *divisor,
				 SgBignum *quotient, int remainderp)
{
  SgBignum *u = NULL;
  if (remainderp) {
    u = make_bignum(dividend->size + 1); 
  }
  bignum_gdiv_rem(dividend, divisor, quotient, u);
  return u;
}

#define bignum_gdiv(dend, dvis, quo) bignum_gdiv_rec(dend, dvis, quo, TRUE)


static ulong bignum_sdiv_rec(SgBignum *quotient, 
			     SgBignum *dividend, ulong divisor)
{
#ifdef USE_DLONG
  if (dividend->size == 1) {
    udlong de = dividend->elements[0];
    ulong q = (ulong) (de / divisor);
    ulong r = (ulong) (de - q * divisor);
    quotient->elements[0] = q;
    quotient->size = 1; 	/* shouldn't be needed but in case */
    return r;
  } else {
    long n = dividend->size - 1;
    udlong rem = (udlong)0L;
    ulong *pu = dividend->elements;
    ulong *qu = quotient->elements;
    for (; n >= 0; n--) {
      rem = (rem << WORD_BITS) | pu[n];
      qu[n] = (ulong)(rem / divisor);
      rem = rem % divisor;
    }
    return (ulong)rem;
  }
#else
  /* only HALF_WORD */
  if (divisor < HALF_WORD) {
    int n = dividend->size - 1;
    unsigned long *pu = dividend->elements;
    ulong q0 = 0, r0 = 0, q1, r1;
    
    for (; n > 0; n--) {
      q1 = pu[n] / divisor + q0;
      r1 = ((pu[n] % divisor) << HALF_BITS) + HI(pu[n - 1]);
      q0 = ((r1 / divisor) << HALF_BITS);
      r0 = r1 % divisor;
      pu[n] = q1;
      pu[n - 1] = (r0 << HALF_BITS) + LO(pu[n - 1]);
    }
    q1 = pu[0] / divisor + q0;
    r1 = pu[0] % divisor;
    pu[0] = q1;
    return r1;
  } else {
    /* FIXME this doesn't work */
    SgBignum *bv = SG_BIGNUM(Sg_MakeBignumFromSI(divisor));
    SgBignum *br, *q;
    int i;
    q = make_bignum(dividend->size + 1);
    br = bignum_gdiv(dividend, bv, q);
    for (i = 0; i < q->size; i++) {
      dividend->elements[i] = q->elements[i];
    }
    return br->elements[0];
  }
#endif
}

#define bignum_sdiv(a, si) bignum_sdiv_rec(a, a, si)

static SgBignum *ZERO = NULL;
static SgBignum *ONE = NULL;
static SgBignum *MIN_ONE = NULL;

static SgBignum ** bignum_div_rem(SgBignum *a, SgBignum *b, SgBignum **rr)
{
  SgBignum *q, *r;
  long qsize = a->size - b->size + 1;
  if (Sg_BignumAbsCmp(a, b) < 0) {
    rr[0] = ZERO;
    rr[1] = a;
    return rr;
  }
  if (rr[0] && rr[0]->size >= qsize) {
    q = rr[0];
  } else {
    q = make_bignum(qsize);
  }
  if (rr[1] && rr[1]->size >= a->size+1) {
    r = rr[1];
  } else {
    r = make_bignum(a->size+1);
  }
  /* this would help alot on sizeof(long) == 8 environment */
  if (b->size == 1) {
    ulong ur;
    ur = bignum_sdiv_rec(q, a, b->elements[0]);
    r->elements[0] = ur;
    r->size = 1;
    r->sign = a->sign;
  } else {
    bignum_gdiv_rem(a, b, q, r);
  }
  SG_BIGNUM_SET_SIGN(q, SG_BIGNUM_GET_SIGN(a) * SG_BIGNUM_GET_SIGN(b));
  SG_BIGNUM_SET_SIGN(r, SG_BIGNUM_GET_SIGN(a));
  rr[0] = bignum_normalize_rec(q, FALSE);
  rr[1] = bignum_normalize_rec(r, FALSE);
  return rr;
}

SgObject Sg_BignumDivRem(SgBignum *a, SgBignum *b)
{
  SgBignum *rr[2] = {NULL, };
  bignum_div_rem(a, b, rr);
  return Sg_Cons(Sg_NormalizeBignum(rr[0]), Sg_NormalizeBignum(rr[1]));
}

SgObject Sg_BignumModulo(SgBignum *a, SgBignum *b, int remp)
{
  SgBignum *r;
  r = bignum_gdiv(a, b, NULL);
  r = Sg_NormalizeBignum(r);
  if (!remp
      && (r != SG_MAKE_INT(0))
      && (SG_BIGNUM_GET_SIGN(a) * SG_BIGNUM_GET_SIGN(b) < 0)) {
    if (SG_BIGNUMP(r)) {
      return Sg_BignumAdd(SG_BIGNUM(b), SG_BIGNUM(r));
    } else {
      return Sg_BignumAddSI(SG_BIGNUM(b), SG_INT_VALUE(r));
    }
  }
  return r;
}

SgObject Sg_BignumModuloSI(SgBignum *a, long b, int remp)
{
  long r = 0;
  long d = (b < 0) ? -b : b;
  long i;
  int sign = SG_BIGNUM_GET_SIGN(a);

  /* ASSERT(b != 0); */
  for (i = SG_BIGNUM_GET_COUNT(a)-1 ; i >= 0; i--) {
    r = (((udlong)r<<WORD_BITS) | a->elements[i]) % d;
  }
  r = r * sign;			/* got remainder */

  if (!remp && r && sign * b < 0){
    return Sg_MakeIntegerFromS64((int64_t)(b + r));
  }
  return Sg_MakeInteger(r);

}

SgObject Sg_BignumDivSI(SgBignum *a, long b, long *rem)
{
  unsigned long dd= (b < 0) ? -b : b;
  unsigned long rr;
  int d_sign = (b < 0) ? -1 : 1;
  SgBignum *q;

  q = SG_BIGNUM(Sg_BignumCopy(a));
  rr = bignum_sdiv(q, dd);

  if (rem) {
    *rem = ((SG_BIGNUM_GET_SIGN(a) < 0) ? -(signed long)rr : (signed long)rr);
  }
  SG_BIGNUM_SET_SIGN(q, SG_BIGNUM_GET_SIGN(a) * d_sign);
  return Sg_NormalizeBignum(q);
}

static SgBignum * bn_sqrt(SgBignum *bn)
{
  long count, workpad_count, bitsize;
  SgBignum *s, *workpad;

  if (SG_BIGNUM_GET_SIGN(bn) == 0) return bn;
  count = SG_BIGNUM_GET_COUNT(bn);
  ALLOC_TEMP_BIGNUM_REC(s, count);
  bignum_copy(s, bn);
  SG_BIGNUM_SET_SIGN(s, 1);

  workpad_count = count + 1;
  ALLOC_TEMP_BIGNUM(workpad, workpad_count);

  bitsize = Sg_BignumBitSize(bn);
  bignum_rshift(s, s, (bitsize - 1) / 2);
  bignum_normalize(s);

  while (TRUE) {
    memset(workpad->elements, 0, sizeof(long) * workpad_count);
    SG_BIGNUM_SET_COUNT(workpad, workpad_count);
    /* only need quotient */
    bignum_gdiv_rec(bn, s, workpad, FALSE);
    if (SG_BIGNUM_GET_SIGN(workpad) == SG_BIGNUM_GET_SIGN(s)) {
      bignum_add_int(workpad, workpad, s);
    } else {
      bignum_sub_int(workpad, workpad, s);
    }
    bignum_rshift(workpad, workpad, 1);
    bignum_normalize(workpad);

    if (Sg_BignumCmp(workpad, s) >= 0) {
      bignum_copy(bn, s);
      SG_BIGNUM_SET_SIGN(bn, 1);
      return bignum_normalize(bn);;
    }
    bignum_copy(s, workpad);
    /* count = SG_BIGNUM_GET_COUNT(workpad); */
    /* SG_BIGNUM_SET_COUNT(s, count); */
    /* memcpy(s->elements, workpad->elements, sizeof(long) * count); */
  }
  /* never reach */
  return bn;
}

SgObject Sg_BignumSqrt(SgBignum *bn)
{
  long count;
  SgBignum *pad;
  double s;

  count = SG_BIGNUM_GET_COUNT(bn);
  ALLOC_TEMP_BIGNUM_REC(pad, count);
  bignum_copy(pad, bn);
  bn_sqrt(pad);
  if (bn->elements[0] == pad->elements[0] * pad->elements[0]) {
    SgBignum *s2;
    ALLOC_TEMP_BIGNUM_REC(s2, SG_BIGNUM_GET_COUNT(pad)<<1);
    /* square_to_len(pad->elements, pad->size, s2->elements); */
    mp_square(s2->elements, pad->size<<1, pad->elements, pad->size);
    bignum_normalize(s2);
    /* set the same sign for temp otherwise cmp returns non 0 even the
       value is the same. */
    SG_BIGNUM_SET_SIGN(s2, SG_BIGNUM_GET_SIGN(bn));
    if (Sg_BignumCmp(bn, s2) == 0) {
      if (SG_BIGNUM_GET_SIGN(bn) > 0) return Sg_BignumToInteger(pad);
      return Sg_MakeComplex(SG_MAKE_INT(0), Sg_BignumToInteger(pad));
    }
  }
  s = Sg_BignumToDouble(bn);
  s = sqrt(s < 0.0 ? -s : s);
  if (SG_BIGNUM_GET_SIGN(bn) > 0) return Sg_MakeFlonum(s);
  return Sg_MakeComplex(Sg_MakeFlonum(0.0), Sg_MakeFlonum(s));
}

SgObject Sg_BignumSqrtApprox(SgBignum *bn)
{
  long count;
  SgBignum *workpad;

  count = SG_BIGNUM_GET_COUNT(bn);
  ALLOC_TEMP_BIGNUM_REC(workpad, count);
  bignum_copy(workpad, bn);
  bn_sqrt(workpad);
  if (SG_BIGNUM_GET_SIGN(bn) > 0) return Sg_BignumToInteger(workpad);
  else return Sg_MakeComplex(SG_MAKE_INT(0), Sg_BignumToInteger(workpad));
}

SgObject Sg_MakeBignumWithSize(long size, unsigned long init)
{
  SgBignum *b = make_bignum(size);
  b->elements[0] = init;
  return b;
}

SgObject Sg_BignumAccMultAddUI(SgBignum *acc, unsigned long coef,
			       unsigned long c)
{
  SgBignum *r;
  long rsize = SG_BIGNUM_GET_COUNT(acc) + 1, i;
  unsigned long carry;
  ALLOC_TEMP_BIGNUM(r, rsize);
  r->elements[0] = c;
  carry = mp_mul_add(r->elements, acc->elements, 
		     SG_BIGNUM_GET_COUNT(acc), coef);
  r->elements[SG_BIGNUM_GET_COUNT(acc)] = carry;

  if (r->elements[rsize - 1] == 0) {
    for (i = 0; i < SG_BIGNUM_GET_COUNT(acc); i++) {
      acc->elements[i] = r->elements[i];
    }
    return acc;
  } else {
    SgBignum *rr;
    rr = make_bignum(rsize + 3);
    SG_BIGNUM_SET_SIGN(rr, SG_BIGNUM_GET_SIGN(acc));
    for (i = 0; i < rsize; i++) {
      rr->elements[i] = r->elements[i];
    }
    return rr;
  }
}

static long calc_string_size(SgBignum *q, int radix)
{
  long count = 0;
  long size = SG_BIGNUM_GET_COUNT(q);
  for (; size > 0;) {
    bignum_sdiv(q, radix);
    count++;
    for (; q->elements[size - 1] == 0 && size > 0; size--);
  }
  return count;
}

static long radix2_string_helper(SgObject r, ulong e, long size, long off)
{
  long i;
  for (i = size-1; i >= 0; i--) {
    int b = (e>>i) & 1;
    SG_STRING_VALUE_AT(r, off++) = (b) ? '1' : '0';
  }
  return off;
}
static SgObject radix2_string(SgBignum *b)
{
  /* if the radix is 2, we can convert bit by bit */
  SgObject r;
  long count, n, off = 0, i;
  /* we only need to care the first element's bit size */
  count = n = WORD_BITS - nlz(b->elements[b->size-1]);

  if (b->sign < 0) count++;
  /* compute the rest of elements bit */
  count += (b->size-1)*WORD_BITS;

  /* allocate string */
  r = Sg_ReserveString(count, 0);
  if (b->sign < 0) SG_STRING_VALUE_AT(r, off++) = '-';

  off = radix2_string_helper(r, b->elements[b->size-1], n, off);
  for (i = b->size-2; i >= 0; i--) {
    off = radix2_string_helper(r, b->elements[i], WORD_BITS, off);
  }
  return r;
}

/* radix 8 is a bit tricky
   we need to split each elements 3 bit each however the word
   length is either 32 or 64, none of them are multiple of 3.
   so first we need to calculate offset of the first word then
   proceed.
 */
static SgObject radix8_string(SgBignum *b)
{
  static const char tab[] = "012345678";
  long bits = Sg_BignumBitSize(b);
  int ob = bits % 3;		/* offset bit */
  long off = 0, i, fb, times, c;
  ulong e;
  SgObject r = Sg_ReserveString((bits/3)+((ob)?1:0)+((b->sign<0)?1:0), 0);

  if (b->sign < 0) SG_STRING_VALUE_AT(r, off++) = '-';
  /*
    e.g
    word = 4, bits 
    first word (fb = 7)
    1000100 0..0 0..0 (2 words + fb = 71)
    ob = 2
    we need to use offset -1 from first word to make total bit
    multiple of 3.
    
    so not the first word is
    01000100 (fb 8)
  */
  /* handle first element */
  ob = (ob)? (3-ob) : 0;
  fb = WORD_BITS - nlz(b->elements[b->size-1]) + ob;
  times = fb / 3;
  c = fb % 3;			/* for next iteration */
  e = b->elements[b->size-1];

  for (i = 0; i < times; i++) {
    int t = (e >> ((fb-3)-(i*3))) & 7;
    SG_STRING_VALUE_AT(r, off++) = tab[t];
  }
  e = (e <<(3-c))&7;		/* gets carry bits */

  /* do the rest */
  for (i = b->size-2; i >= 0; i--) {
    ulong re = b->elements[i];
    int j;

    times = (WORD_BITS+c)/3;
    for (j = 0; j < times; j++) {
      int t = ((re >> ((WORD_BITS-(3-c))-(j*3)))|e)&7;
      SG_STRING_VALUE_AT(r, off++) = tab[t];
      if (e) e = 0;		/* clear carry */
    }
    c = (WORD_BITS+c)%3;	/* next carry */
    /* only if the c is not 0, otherwise we'll get garbage */
    if (c) e = (re <<(3-c))&7;		/* gets carry bits */
  }
  return r;
}

static SgObject radix16_string(SgBignum *b, int use_upper)
{
  /* if the radix is 16 then we can simply dump the elements */
  char buf[(SIZEOF_LONG<<1)+1];
  SgObject r;
#if SIZEOF_LONG == 8
  char *fmt = (use_upper)? "%016lX": "%016lx";
#else
  char *fmt = (use_upper)? "%08lX": "%08lx";
#endif
  char *first_fmt = (use_upper)? "%lX": "%lx";
  long count, n, off = 0, i, j;
  /* this is the very first time I'm using the return value of
     printf related procedure...
  */
  count = n = snprintf(buf, sizeof(buf), first_fmt, b->elements[b->size-1]);
  if (b->sign < 0) count++;
  /* calculate the rest of words */
  count += (b->size-1) * (SIZEOF_LONG<<1);

  r = Sg_ReserveString(count, 0);
  /* set the first word */
  if (b->sign < 0) SG_STRING_VALUE_AT(r, off++) = '-';
  for (i = 0; i < n; i++) {
    SG_STRING_VALUE_AT(r, off++) = buf[i];
  }

  for (i = b->size-2; i >= 0; i--) {
    snprintf(buf, sizeof(buf), fmt, b->elements[i]);
    for (j = 0; j < sizeof(buf)-1; j++) {
      SG_STRING_VALUE_AT(r, off++) = buf[j];
    }
  }
  return r;
}

#define SCHONEHAGE_BASE_CONVERSION_THRESHOLD 20

static SgObject small_bignum_to_string(SgBignum *b, int radix, int use_upper)
{
  static const char ltab[] = "0123456789abcdefghijklmnopqrstuvwxyz";
  static const char utab[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  const char *tab = use_upper ? utab : ltab;
  /* SgObject h = SG_NIL, t = SG_NIL; */
  SgObject rs;
  SgBignum *q;
  long rem, size, count;
  long i;
  q = SG_BIGNUM(Sg_BignumCopy(b));
  size = SG_BIGNUM_GET_COUNT(q);
    
  count = calc_string_size(q, radix);
  if (SG_BIGNUM_GET_SIGN(q) < 0) count++;
  for (i = 0; i < size; i++) q->elements[i] = b->elements[i];
  rs = Sg_ReserveString(count, 0);
    
  for (i = count-1; size > 0; i--) {
    rem = bignum_sdiv(q, radix);
    /* SG_APPEND1(h, t, SG_MAKE_CHAR(tab[rem])); */
    SG_STRING_VALUE_AT(rs, i) = tab[rem];
    for (; q->elements[size - 1] == 0 && size > 0; size--);
  }
  if (SG_BIGNUM_GET_SIGN(q) < 0) {
    SG_STRING_VALUE_AT(rs, 0) = '-';
    /* SG_APPEND1(h, t, SG_MAKE_CHAR('-')); */
  }
  /* return Sg_ListToString(Sg_ReverseX(h), 0, -1); */
  return rs;
}

/* FIXME this is also in number.c */
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
/* this is used here */
static SgBignum * bignum_expt(SgBignum *b, long exponent);

#define MAX_RADIX 36
static SgBignum *RADIXES[MAX_RADIX+1] = {NULL,};
/* returns radix^2^exponent 
   the exponent is usually not so big (unless the number is extremely huge,
   but such numbers can't be create on Sagittarius due to the memory 
   limitation). So caluculating it each time is more expensive than
   allocating small space for cache.
 */
static SgBignum* radix_conversion(int radix, int exponent, SgBignum ***two_exps)
{
  SgObject c;
  SgBignum *r;

  if (!*two_exps) {
    /* the first time, then create the array with exponent. don't worry,
       it's not so big */
    *two_exps = SG_NEW_ARRAY(SgBignum *, exponent+1);
  }
  r = (*two_exps)[exponent];
  if (r) return r;

  c  = Sg_Expt(SG_MAKE_INT(2), SG_MAKE_INT(exponent));
  if (!SG_INTP(c)) Sg_Error(UC("big num is too big to show"));
  /* keep it as bignum */
  r = bignum_normalize_rec(bignum_expt(RADIXES[radix], SG_INT_VALUE(c)),
			   FALSE);
  (*two_exps)[exponent] = r;
  return r;
}

/*
  it does improve the performance as long as the number is small
  however once it consumed all stack then the GC would get affected
  by the large grown stack area. I believe, this impacted performance.
  c.f)
  with stack
  ;;  (string->number (number->string (expt 2 (vector-ref v 0))))
  ;;  -6.915366 real    2.154000 user    0.218000 sys
  11.53s user 0.31s system 103% cpu 11.410 total

  without stack
  ;;  (string->number (number->string (expt 2 (vector-ref v 0))))
  ;;  0.337534 real    9.266000 user    0.437000 sys
  9.67s user 0.50s system 105% cpu 9.657 total

  2 seconds are not negligible difference.
 */
#undef USE_STACK_FOR_SCHONEHAGE

/* cache constant log values */
/* static double DBL_LOG2 = 0.0; */
#define DBL_LOG2 0.6931471805599453
static double DBL_RADIXES[MAX_RADIX+1];
/*
  Using Schonehage base conversion

  This is much faster than before (it's now named small_bignum_to_string),
  however the implementation uses considerable memory if the bignum is
  huge. In that case it's more fight against GC than small performance
  tuning such as caching.
  
  For future note.
  There's couple of things I can try to squeeze performance.
   - rope: string port isn't that bad but calling overhead would be redueced
   - non recursive conversion: then we can use stack instead of heap
  The second one would be a challenge.
 */
static void schonehage_to_string(SgBignum *b, SgObject out, int radix,
				 int count, int use_upper, 
				 /* r^2^e cache */
				 SgBignum ***two_exps
				 /*, int use_stack */)
{
  long bits;
  int n, e;
  SgBignum *v;
  SgBignum *result[2] = {NULL, NULL};
  double lr, l2;
#ifdef USE_STACK_FOR_SCHONEHAGE
  intptr_t stack_size;
  ulong qsize, rsize;
#endif
  if (b->size < SCHONEHAGE_BASE_CONVERSION_THRESHOLD) {
    SgObject r = small_bignum_to_string(b, radix, use_upper);
    long i;
    if (SG_STRING_SIZE(r) < count && Sg_PortPosition(out) > 0) {
      for (i = SG_STRING_SIZE(r); i < count; i++) Sg_PutcUnsafe(out, '0');
    }
    Sg_PutsUnsafe(out, r);
    return;
  }
  bits = Sg_BitSize(b);
  l2 = DBL_LOG2;
  lr = DBL_RADIXES[radix];

  n = (int)roundeven(log(bits * l2 / lr) / lr - 1.0);
  v = radix_conversion(radix, n, two_exps);

#ifdef USE_STACK_FOR_SCHONEHAGE
  if (use_stack) {
    stack_size = Sg_AvailableStackSize((volatile uintptr_t)&bits);
    qsize = b->size - v->size + 1;
    rsize = b->size+1;
    if (stack_size > 0 && stack_size > BIGNUM_SIZE(qsize)) {
      stack_size -= BIGNUM_SIZE(qsize);
      ALLOC_TEMP_BIGNUM(result[0], qsize);
    } else {
      use_stack = FALSE;
    }
    if (stack_size > 0 && stack_size > BIGNUM_SIZE(rsize)) {
      ALLOC_TEMP_BIGNUM(result[1], rsize);
    } else {
      use_stack = FALSE;
    }
  }
#endif

  bignum_div_rem(b, v, result);
  e = 1<<n;  
  schonehage_to_string(result[0], out, radix, count-e, use_upper, two_exps);
  schonehage_to_string(result[1], out, radix, e, use_upper, two_exps);
}

SgObject Sg_BignumToString(SgBignum *b, int radix, int use_upper)
{
  if (radix < 2 || radix > 36) {
    Sg_Error(UC("radix out of range: %d"), radix);
  }
  /* special case 0 */
  if (b->sign == 0 || b->size == 0) return SG_MAKE_STRING("0");

  /* handle easily converted case */
  if (radix == 2)  return radix2_string(b);
  if (radix == 8)  return radix8_string(b);
  if (radix == 16) return radix16_string(b, use_upper);

  /* The Art of Computer Programming Vol2 4.4 (Q 14) answer*/
  if (b->size < SCHONEHAGE_BASE_CONVERSION_THRESHOLD) {
    return small_bignum_to_string(b, radix, use_upper);
  } else {
    SgPort *out;
    SgStringPort sp;
    SgBignum **two_exponents = NULL;
    out = Sg_InitStringOutputPort(&sp, 1024);
    if (b->sign < 0) {
      b = SG_BIGNUM(Sg_Negate(SG_OBJ(b)));
      Sg_PutcUnsafe(out, '-');
    }
    schonehage_to_string(b, out, radix, 0, use_upper, &two_exponents);
    return Sg_GetStringFromStringPort(&sp);
  }
}

/* we do this destructively */
static int bignum_difference(SgBignum *a, SgBignum *b)
{
  int sign = Sg_BignumCmp(a, b);

  if (sign == 0) return 0;

  if (sign < 0) {
    SgBignum *tmp = a;
    a = b;
    b = tmp;
  }
  ASSERT(SG_BIGNUM_GET_COUNT(a) >= SG_BIGNUM_GET_COUNT(b));
  bignum_sub_int(a, a, b);
  bignum_normalize(a);
  return sign;
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

/* avoid memory allocation as much as possible */
static SgObject binary_gcd(SgBignum *bx, SgBignum *by)
{
  /* Algorithm B from Knuth section 4.5.2 */
  SgBignum *u = SG_BIGNUM(Sg_Abs(Sg_BignumCopy(bx)));
  SgBignum *v = SG_BIGNUM(Sg_Abs(Sg_BignumCopy(by)));
  SgObject ret;
  /* step B1 */
  long s1 = Sg_BignumFirstBitSet(u);
  long s2 = Sg_BignumFirstBitSet(v);
  long k = min(s1, s2);
  /* these are for step B2 */
  int uOdd, tsign;
  long lb;
  SgBignum *t;
  /* Sg_Printf(Sg_StandardErrorPort(), UC(";; x=%A, y=%A, k=%d\n"), u, v, k); */
  if (k != 0) {
    bignum_rshift(u, u, k);
    bignum_rshift(v, v, k);
    bignum_normalize(u);
    bignum_normalize(v);
  }
  /* Step B2 */
  uOdd = (k==s1);
  t = uOdd ? v: u;
  tsign = uOdd ? -1 : 1;

  while ((lb = Sg_BignumFirstBitSet(t)) >= 0) {
    /* Step B3 and B4 */
    /* Sg_Printf(Sg_StandardErrorPort(), UC("(print (= (>> %A %d) "), t, lb); */
    bignum_rshift(t, t, lb);
    bignum_normalize(t);
    /* Sg_Printf(Sg_StandardErrorPort(), UC("%A))\n"), t); */
    /* Step B5 */
    if (tsign > 0) u = t;
    else v = t;
    /* special case one word numbers */
    if (SG_BIGNUM_GET_COUNT(u) < 2 && SG_BIGNUM_GET_COUNT(v) < 2) {
      unsigned long x = u->elements[0], y = v->elements[0];
      x = (x >= y) ? gcd_fixfix(x, y) : gcd_fixfix(y, x);
      ret = Sg_MakeIntegerU(x);
      if (k > 0) {
	/* Sg_Printf(Sg_StandardErrorPort(), UC(";; r=%A, k=%d\n"), ret, k); */
	ret = Sg_Ash(ret, k);
      }
      goto end;
    }
    /* Step B6 */
    /*
      Sg_Printf(Sg_StandardErrorPort(), UC("(print (= (abs (- %A %A))"), u, v);
    */
    if ((tsign = bignum_difference(u, v)) == 0) break;
    t = (tsign >= 0) ? u : v;
    /* Sg_Printf(Sg_StandardErrorPort(), UC("%A))\n"), t); */
  }
  if (k > 0) {
    /* Sg_Printf(Sg_StandardErrorPort(), UC(";; u=%A, k=%d\n"), u, k); */
    ret = Sg_BignumShiftLeft(u, k);
  } else {
    ret = Sg_NormalizeBignum(u);
  }
 end:
  /* Sg_Printf(Sg_StandardErrorPort(), UC(";; return=%A\n"), ret); */
  return ret;
}

SgObject Sg_BignumGcd(SgBignum *bx, SgBignum *by)
{
  /* return binary_gcd(bx, by); */
  /* hybrid was faster. */
  while (SG_BIGNUM_GET_COUNT(by) != 0) {
    SgBignum *r;
    SgObject t;
    if (labs((long)SG_BIGNUM_GET_COUNT(bx)-(long)SG_BIGNUM_GET_COUNT(by)) < 2)
      return binary_gcd(bx, by);
    r = bignum_gdiv(bx, by, NULL);
    t = Sg_NormalizeBignum(r);
    if (SG_INTP(t)) return Sg_Gcd(by, t);
    bx = by;
    by = r;
  }
  return bx;
}

static long inverse_mod_long(ulong val)
{
  long t = val;
  t *= 2 - val*t;
  t *= 2 - val*t;
  t *= 2 - val*t;
  t *= 2 - val*t;
#if SIZEOF_LONG == 8
  t *= 2 - val*t;
  t *= 2 - val*t;
  t *= 2 - val*t;
  t *= 2 - val*t;
#endif
  return t;
}

#define BIGNUM_ZEROP(bn) (SG_BIGNUM_GET_SIGN(bn) == 0)
#define BIGNUM_ONEP(bn) (SG_BIGNUM_GET_COUNT(bn) == 1 && (bn)->elements[0] == 1)

static SgBignum * bignum_mod(SgBignum *a, SgBignum *b, SgBignum *q)
{
  SgBignum *r = bignum_gdiv(a, b, q);
  bignum_normalize(r);
  if (!BIGNUM_ZEROP(r)
      && (SG_BIGNUM_GET_SIGN(a) * SG_BIGNUM_GET_SIGN(b) < 0)) {
    return bignum_normalize(bignum_add(b, r));
  }
  return r;
}

/* 
   Using extended Euclidean algorithm.

   The complexity of the compuation is O(log(m)^2).

   For now this is actually good enough since both x and m are not
   really huge. If we got a huge number (e.g. 2048 bits) we can think
   about other algorithm.
   Alternatives:
   - The Montgomery inverse and its applications, Burton S. Kaliski:
   
   - Constant Time Modular Inversion, Joppe W Bos: 
     http://www.joppebos.com/files/CTInversion.pdf
*/
#ifndef USE_MUTABLE_BIGNUM

static SgBignum * bignum_mod_inverse(SgBignum *x, SgBignum *m)
{
  SgBignum *u1, *u3, *v1, *v3, *q, *w;
  int sign = 1;
  long qs, ws;

  if (SG_BIGNUM_GET_SIGN(m) < 0) {
    Sg_Error(UC("modulus not positive %S"), m);
  }
  /* setup buffers */
  if (SG_BIGNUM_GET_COUNT(x) > SG_BIGNUM_GET_COUNT(m)) {
    qs = SG_BIGNUM_GET_COUNT(x) + 1;
  } else {
    qs = SG_BIGNUM_GET_COUNT(m) + 1;
  }
  ALLOC_TEMP_BIGNUM(q, qs);

  ws = max(x->size, m->size) + 1;
  ALLOC_TEMP_BIGNUM(w, ws);

  u1 = ONE;
  u3 = x;
  v1 = ZERO;
  v3 = m;

#define reset_buffer(bn, size)						\
  do {									\
    memset((bn)->elements, 0, SG_BIGNUM_GET_COUNT(bn) * sizeof(long));	\
    SG_BIGNUM_SET_COUNT(bn, size);					\
    SG_BIGNUM_SET_SIGN(bn, 1);						\
  } while (0)
  
  while (!BIGNUM_ZEROP(v3)) {
    SgBignum *t3, *t1;		/* We can't pre allocate them... *sigh* */
    t3 = bignum_mod(u3, v3, q);
    bignum_normalize(q);
    /* w = bignum_normalize(bignum_mul(q, v1)); */
    w = bignum_normalize(bignum_mul_int(w, q, v1));
    t1 = bignum_normalize(bignum_add(u1, w));

    /* dump_xarray(u1->elements, u1->size); */
    /* dump_xarray(u3->elements, u3->size); */
    /* dump_xarray(v1->elements, v1->size); */
    /* dump_xarray(v3->elements, v3->size); */
    /* dump_xarray(q->elements, q->size); */
    /* dump_xarray(t3->elements, t3->size); */
    /* dump_xarray(w->elements, w->size); */
    /* dump_xarray(t1->elements, t1->size); */
    /* fputs("\n", stderr); */
    
    u1 = v1; v1 = t1; u3 = v3; v3 = t3;
    sign = -sign;
    /* reset buffer */
    /* clear the content as well since bignum_gdiv may not clear the
       array. (got the bug due to the non cleared stack area...)

       NB: element size number is sufficient since it's initialised to 0.
    */
    reset_buffer(q, qs);
    reset_buffer(w, ws);
  }
#undef reset_buffer

  /* Sg_Printf(Sg_StandardErrorPort(), UC("x : %A\n"), x); */
  /* Sg_Printf(Sg_StandardErrorPort(), UC("m : %A\n"), m); */
  /* Sg_Printf(Sg_StandardErrorPort(), UC("u1: %A\n"), u1); */
  /* Sg_Printf(Sg_StandardErrorPort(), UC("v1: %A\n"), v1); */
  if (sign < 0) {
    return bignum_normalize(bignum_sub(m, u1));
  } else {
    return bignum_normalize(u1);
  }
}

#else

static SgBignum * bignum_mod_inverse(SgBignum *x, SgBignum *m)
{
  mbignum_t *u1, *u3, *v1, *v3, *q, *w, *t1, *t3;
  ulong size;
  int sign = 1;
  if (SG_BIGNUM_GET_SIGN(m) < 0) {
    Sg_Error(UC("modulus not positive %S"), m);
  }
  size = max(x->size, m->size) + 1;
  alloc_temp_mbignum( q, size);
  alloc_temp_mbignum( w, size);
  alloc_temp_mbignum(u1, size);
  alloc_temp_mbignum(u3, size);
  alloc_temp_mbignum(v1, size);
  alloc_temp_mbignum(v3, size);
  alloc_temp_mbignum(t1, size);
  alloc_temp_mbignum(t3, size);

  mbignum_one(u1);
  copy_from_bignum(u3, x);
  mbignum_zero(v1);
  copy_from_bignum(v3, m);

#define reset_buffer(mbn, n)						\
  do {									\
    memset((mbn)->elements, 0, (mbn)->buffer_size * sizeof(long));	\
    mbignum_reset(mbn, n);						\
  } while (0)
  
  while (!mbignum_zerop(v3)) {
    mbignum_t *t;
    mbignum_mod(u3, v3, q, t3);
    w = mbignum_normalize(mbignum_mul(w, q, v1));
    t1 = mbignum_normalize(mbignum_add(t1, u1, w));

    /* fprintf(stderr, "sign - q: %d, v1: %d, u1: %d, w: %d, t1: %d\n", */
    /* 	    q->sign, v1->sign, u1->sign, w->sign, t1->sign); */
    /* dump_xarray(u1->elements, u1->size); */
    /* dump_xarray(u3->elements, u3->size); */
    /* dump_xarray(v1->elements, v1->size); */
    /* dump_xarray(v3->elements, v3->size); */
    /* dump_xarray(q->elements, q->size); */
    /* dump_xarray(t3->elements, t3->size); */
    /* dump_xarray(w->elements, w->size); */
    /* dump_xarray(t1->elements, t1->size); */
    /* fputs("\n", stderr); */

    /* rotate */
    t = u1; u1 = v1; v1 = t1; t1 = t;
    t = u3; u3 = v3; v3 = t3; t3 = t;
    sign = -sign;

    reset_buffer(q, size);
    reset_buffer(w, size);
    reset_buffer(t1, size);
    reset_buffer(t3, size);
  }
#undef reset_buffer

  if (sign < 0) {
    SgBignum *r = mbignum_to_bignum(u1);
    return bignum_normalize(bignum_sub(m, r));
  } else {
    return mbignum_to_bignum(u1);
  }
}

#endif /* USE_MUTABLE_BIGNUM */

#define EXPMOD_MAX_WINDOWS 7
static ulong exp_mod_threadh_table[EXPMOD_MAX_WINDOWS] = {
  7, 25, 81, 241, 673, 1793, (ulong)-1L
};

static ulong * odd_mod_expt_rec(SgBignum *x, SgBignum *exp, SgBignum *mod,
				ulong inv, ulong *a, ulong *b)
{
  SgBignum *tr, *tb;
  ulong *table[1 << EXPMOD_MAX_WINDOWS], *prod, *t;
  long i, j, tblmask, wbits, ebits;
  int isone = TRUE;
  long modlen = SG_BIGNUM_GET_COUNT(mod);
  long elen = SG_BIGNUM_GET_COUNT(exp);
  unsigned int buf = 0;
  long multpos;
  ulong *mult, *e = exp->elements + elen -1;
  ulong bitpos;
  long modlen2 = modlen<<1;

  wbits = 0;
  ebits = Sg_BignumBitSize(exp);
  /* fprintf(stderr, "modlen = %ld\n", modlen); */
  if ((ebits != 17) ||
      (SG_BIGNUM_GET_COUNT(exp) != 1 && exp->elements[0] != 65537)) {
    while ((unsigned long)ebits > exp_mod_threadh_table[wbits]) {
      wbits++;
    }
  }

  tblmask = 1u << wbits;
  /* convert x to montgomery form */
  ALLOC_TEMP_BIGNUM(tb, SG_BIGNUM_GET_COUNT(x) + modlen);
  /* BIGNUM_CLEAR_LEFT(tb, modlen); */
  /* mp_lshift(tb->elements, SG_BIGNUM_GET_COUNT(tb), */
  /* 	    x->elements, SG_BIGNUM_GET_COUNT(x), */
  /* 	    modlen << SHIFT_MAGIC); */
  /* Above is the proper one, but below is also fine (and faster...) */
  for (i = 0; i < SG_BIGNUM_GET_COUNT(x); i++) {
    tb->elements[i+modlen] = x->elements[i];
  }
  tr = bignum_normalize(bignum_gdiv(tb, mod, NULL));
  if (SG_BIGNUM_GET_COUNT(tr) < modlen) {
    /* table[0] won't be initialised, we need to do it here */
    ALLOC_TEMP_BUFFER(table[0], ulong, modlen);
    for (i = 0; i < (int)SG_BIGNUM_GET_COUNT(tr); i++) {
      table[0][i] = tr->elements[i];
    }
  } else {
    /* when tr has the same length as modlen, we can reuse it. */
    table[0] = tr->elements;
  }
  /* set b to the square of the base */
  /* blen = modlen<<1 */
  /* square_to_len(table[0], modlen, b); */
  mp_square(b, modlen2, table[0], modlen);
  mp_mont_reduce(b, modlen2, mod->elements, modlen, inv);
  /* Use high hald of b to initialise the table */
  t = b+modlen;

  /* Fill int the table with odd powers of the base */
  for (i = 1; i < tblmask; i++) {
    ALLOC_TEMP_BUFFER(prod, ulong, modlen);
    /* alen = modlen<<1 */
    mp_mul(a, modlen2, t, modlen, table[i-1], modlen);
    mp_mont_reduce(a, modlen2, mod->elements, modlen, inv);

    for (j = 0; j < modlen; j++) {
      prod[j] = a[j+modlen];
    }
    table[i] = prod;
  }
  /* hmm, let me make a new scope... */
  bitpos = 1UL << ((ebits-1) & (WORD_BITS-1));
  
  for (i = 0; i <= wbits; i++) {
    buf = (buf << 1) | ((*e & bitpos) != 0);
    bitpos >>= 1;
    if (!bitpos) {
      e--;
      bitpos = 1UL << (WORD_BITS-1);
      elen--;
    }
  }
  /* The first iteration, which is hoisted out of the main loop */
  ebits--;
  multpos = ebits - wbits;
  while ((buf & 1) == 0) {
    buf >>= 1;
    multpos++;
  }
  mult = table[buf >> 1];
  buf = 0;
  if (multpos == ebits) {
    isone = FALSE;
  }
  /* the main loop */
  for (;;) {
    ebits--;
    buf <<= 1;
    if (elen) {
      buf |= ((*e & bitpos) != 0);
      bitpos >>= 1;
      if (!bitpos) {
	e--;
	bitpos = 1UL << (WORD_BITS-1);
	elen--;
      }
    }
    /* examine the window for pending multiplies */
    if (buf & tblmask) {
      multpos = ebits - wbits;
      while ((buf & 1) == 0) {
	buf >>= 1;
	multpos++;
      }
      mult = table[buf >> 1];
      buf = 0;
    }
    /* perform multiply */
    if (ebits == multpos) {
      /* t will be reused anyway, so make it shared */
      t = b+modlen;
      if (isone) {
	for (i = 0; i < modlen; i++) {
	  t[i] = mult[i];
	}
	isone = FALSE;
      } else {
	/* alen = modlen<<1 */
	mp_mul(a, modlen2, t, modlen, mult, modlen);
	mp_mont_reduce(a, modlen2, mod->elements, modlen, inv);
	t = a; a = b; b = t;
      }
    }
    /* check if done */
    if (!ebits) return b;
    /* square the input */
    if (!isone) {
      t = b+modlen;
      /* square_to_len(t, modlen, a); */
      mp_square(a, modlen2, t, modlen);
      mp_mont_reduce(a, modlen2, mod->elements, modlen, inv);
      t = a; a = b; b = t;
    }
  }
}

static SgObject odd_mod_expt(SgBignum *x, SgBignum *exp, SgBignum *mod)
{
  long modlen, i, buflen;
  SgBignum *r;
  ulong *a, *b, *t;
  ulong inv;
  if (BIGNUM_ONEP(exp)) return x;
  if (BIGNUM_ZEROP(x)) return x;

  modlen = SG_BIGNUM_GET_COUNT(mod);
  inv = -inverse_mod_long(mod->elements[0]);

  /* pre allocate */
  r = make_bignum(modlen);

  buflen = modlen * 2;
  /* allocate buffers */
  ALLOC_TEMP_BUFFER(a, ulong, buflen);
  ALLOC_TEMP_BUFFER(b, ulong, buflen);

  /* do main loop */
  b = odd_mod_expt_rec(x, exp, mod, inv, a, b);
  /* convert result out of montgomery form and return */
  t = b+modlen;
  for (i = 0; i < modlen; i++) {
    b[i] = t[i];
    t[i] = 0;			/* free up higher half of t */
  }
  mp_mont_reduce(b, buflen, mod->elements, modlen, inv);
  
  /* get higher half of the result */
  for (i = 0; i < modlen; i++) {
    r->elements[i] = t[i];
  }
  return bignum_normalize(r);
}

/* mod(2^p */
static SgBignum * bignum_mod2(SgBignum *x, long p)
{
  long numInts, excessBits, i;
  SgBignum *r;
  if (Sg_BignumBitSize(x) <= p) return x;
  numInts = (p + (WORD_BITS-1)) >> SHIFT_MAGIC;
  r = make_bignum(numInts);
  for (i = 0; i < numInts; i++) {
    r->elements[i] = x->elements[i];
  }
  excessBits = (numInts << SHIFT_MAGIC) - p;
  r->elements[numInts - 1] &= (1UL << (WORD_BITS - excessBits)) - 1;
  return bignum_normalize(r);
}

static SgBignum * bignum_mod_expt2(SgBignum *x, SgBignum *e, long p)
{
  SgBignum *base = bignum_mod2(x, p), *result = ONE;
  long exp_offset = 0, limit = Sg_BignumBitSize(e);
  if (x->elements[0] & 1) {
    limit = ((p-1) < limit) ? (p-1) : limit;
  }
  while (exp_offset < limit) {
    if (bignum_test_bit(e, exp_offset)) {
      result = bignum_mul(result, base);
      bignum_normalize(result);
      result = bignum_mod2(result, p);
    }
    exp_offset++;
    if (exp_offset < limit) {
      base = bignum_mul(base, base);
      bignum_normalize(base);
      base = bignum_mod2(base, p);
    }
  }
  return result;
}

static SgObject bignum_mod_expt(SgBignum *bx, SgBignum *be, SgBignum *bm)
{
  int invertp = SG_BIGNUM_GET_SIGN(be) < 0;
  SgBignum *base, *result;
  if (invertp) {
    /* keep it bignum */
    SgBignum *b = Sg_BignumCopy(be);
    SG_BIGNUM_SET_SIGN(b, -SG_BIGNUM_GET_SIGN(be));
    be = b;
  }
  base = (SG_BIGNUM_GET_SIGN(bx) < 0 || Sg_BignumCmp(bx, bm) >= 0)
    ? bignum_mod(bx, bm, NULL) : bx;

  if (bm->elements[0] & 1) {	/* odd modulus */
    result = odd_mod_expt(base, be, bm);
  } else {
    /* even modulus. Tear it into an "odd part" (m1) and power of two (m2), 
       exponentiate mod m1, manually exponentiate mod m2, and use Chinese
       Remainder Theorem to combine results.
     */
    long p = Sg_BignumFirstBitSet(bm);
    long lsize = 1 + (p + WORD_BITS - 1) / WORD_BITS;
    long rsize = SG_BIGNUM_GET_COUNT(bm) + (-p)/WORD_BITS;
    SgBignum *m1,  *m2, *base2, *a1, *a2, *y1, *y2;
    SgBignum *ONE;
    ALLOC_TEMP_BIGNUM(ONE, 1);
    ONE->elements[0] = 1;

    if (rsize < 1) {
      if (SG_BIGNUM_GET_SIGN(bm) < 0) {
	m1 = MIN_ONE;
      } else {
	m1 = ZERO;
      }
    } else {
      /* m must be positive */
      /* m1 = make_bignum(rsize); */
      ALLOC_TEMP_BIGNUM(m1, rsize);
      bignum_rshift(m1, bm, p);
      bignum_normalize(m1);
    }
    /* m2 = make_bignum(lsize); */
    ALLOC_TEMP_BIGNUM(m2, lsize);
    bignum_lshift(m2, ONE, p);
    bignum_normalize(m2);

    base2 = (SG_BIGNUM_GET_SIGN(bx) || Sg_BignumCmp(bx, m1) >= 0)
      ? bignum_mod(bx, m1, NULL) : bx;
    a1 = (BIGNUM_ONEP(m1))
      ? ZERO
      : odd_mod_expt(base2, be, m1);
    a2 = bignum_mod_expt2(base, be, p);
    y1 = bignum_mod_inverse(m2, m1);
    y2 = bignum_mod_inverse(m1, m2);
    {
      SgBignum *t1, *t2;
      t1 = bignum_normalize(bignum_mul(a1, m2));
      t1 = bignum_normalize(bignum_mul(t1, y1));
      t2 = bignum_normalize(bignum_mul(a2, m1));
      t2 = bignum_normalize(bignum_mul(t2, y2));
      result = bignum_mod(bignum_normalize(bignum_add(t1, t2)),
			  bm, NULL);
    }
  }
  return (invertp) ? bignum_mod_inverse(result, bm) : result;
}

SgObject Sg_BignumModExpt(SgBignum *bx, SgBignum *be, SgBignum *bm)
{
  if (bm->sign <= 0) {
    Sg_Error(UC("modulus must be a positive number: %S"), bm);
  }
  if (BIGNUM_ZEROP(be)) {
    return SG_MAKE_INT(1);
  }
  if (BIGNUM_ONEP(bx)) {
    return BIGNUM_ONEP(bm) ? SG_MAKE_INT(0) : SG_MAKE_INT(1);
  }
  if (BIGNUM_ZEROP(bx)) {
    return SG_MAKE_INT(0);
  }
  return Sg_NormalizeBignum(bignum_mod_expt(bx, be, bm));
}

SgObject Sg_BignumModInverse(SgBignum *bx, SgBignum *bm)
{
  return Sg_NormalizeBignum(bignum_mod_inverse(bx, bm));
}


/* using slide window algorithm. */
static int bfffo(ulong x)
{
  static int tabshi[16]={ 4, 3, 2, 2, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0 };
  int value = WORD_BITS - 4;
  ulong arg1 = x;
#if SIZEOF_LONG == 8
  if (arg1 & ~0xffffffffUL) {value -= 32; arg1 >>= 32;}
#endif
  if (arg1 & ~0xffffUL) {value -= 16; arg1 >>= 16;}
  if (arg1 & ~0x00ffUL) {value -= 8; arg1 >>= 8;}
  if (arg1 & ~0x000fUL) {value -= 4; arg1 >>= 4;}
  return value + tabshi[arg1];
}

/* helper to avoid memory allocation. */
static long compute_lr_binary_size(long j, long m, long len)
{
  long r = len;
  for (; j; m <<= 1, j--) {
    r <<= 1; 
    if (m < 0) {
      r += len;
    }
  }
  return r;
}

#define copy_element(src, dst, size)			\
  do {							\
    long i;						\
    for (i = 0; i < size; i++) (src)[i] = (dst)[i];	\
  } while (0)

static SgBignum * leftright_binray_expt(SgBignum *b, long e)
{
  SgBignum *y, *ye, *prod, *t;
  long m = e, j = 1 + bfffo(m);
  long size;
  long ylen = SG_BIGNUM_GET_COUNT(b);
  m <<= j;
  j = WORD_BITS-j;

  /* setup buffer */
  size = compute_lr_binary_size(j, m, SG_BIGNUM_GET_COUNT(b));
  y = make_bignum(size);
  bignum_copy(y, b);
  ye = y;
  ALLOC_TEMP_BIGNUM_REC(prod, size);
  for (; j; m <<= 1, j--) {
    /* y = bignum_mul(y, y); */
    /* TODO make this for bignum */
    /* square_to_len(ye->elements, ylen, prod->elements); */
    mp_square(prod->elements, ylen<<1, ye->elements, ylen);
    ylen <<= 1;
    t = ye;
    ye = prod;
    prod = t;
    /* set ylen to ye */
    SG_BIGNUM_SET_COUNT(ye, ylen);
    if (m < 0) {
      bignum_mul_int(prod, ye, b);
      ylen += SG_BIGNUM_GET_COUNT(b);
      t = ye;
      ye = prod;
      prod = t;
    }
  }
  if (y != ye) {
    copy_element(y->elements, ye->elements, ylen);
  }
  SG_BIGNUM_SET_COUNT(y, ylen);
  return y;
}

static long vals(ulong z)
{
  static char tab[64] = 
    {-1, 0,  1, 12,  2,  6, -1, 13,  3, -1,  7, -1, -1, -1, -1, 14,
     10, 4, -1, -1,  8, -1, -1, 25, -1, -1, -1, -1, -1, 21, 27, 15,
     31, 11, 5, -1, -1, -1, -1, -1,  9, -1, -1, 24, -1, -1, 20, 26,
     30, -1, -1, -1, -1, 23, -1, 19, 29, -1, 22, 18, 28, 17, 16, -1};

#if SIZEOF_INT == 8
  long s;
#endif

  if (!z) return -1;
#if SIZEOF_INT == 8
  if (! (z&0xffffffff)) { s = 32; z >>=32; } else s = 0;
#endif
  z |= ~z + 1;
  z += z << 4;
  z += z << 6;
  z ^= z << 16; /* or  z -= z<<16 */
#if SIZEOF_INT == 8
  return s + tab[(z&0xffffffff)>>26];
#else
  return tab[z>>26];
#endif
}

#define BIGNUM_CLEAR_LEFT(bn, size)				\
  do {								\
    long i, len = (size);					\
    long off = SG_BIGNUM_GET_COUNT(bn)-1;                       \
    for (i = 0; i < len; i++) (bn)->elements[off-i] = 0;	\
  } while (0)
/* I'm not sure if I can write this as a static function since
   it's using alloca to setup table. */
#define setup_table(table, u, b)					\
  do {									\
    SgBignum *x2;							\
    long i_, x2len =SG_BIGNUM_GET_COUNT(b)<<1;				\
    table[0] = b;							\
    ALLOC_TEMP_BIGNUM_REC(x2, x2len);					\
    BIGNUM_CLEAR_LEFT(x2, 2);						\
    mp_square(x2->elements, b->size<<1, b->elements, b->size);		\
    for (i_ = 1; i_ < (u); i_++) {					\
      long size = SG_BIGNUM_GET_COUNT(table[i_-1]) + x2len;		\
      ALLOC_TEMP_BIGNUM_REC(table[i_], size);				\
      BIGNUM_CLEAR_LEFT(table[i_], 2);					\
      bignum_mul_int(table[i_], table[i_-1], x2);			\
    }									\
  } while (0)

/* compute result size of sliding window.
   this is actually the same algorithm without computing */
static long compute_sw_size(long l, long e, long n, SgBignum **table)
{
  long z = 0;
  long tw;
  long w, v, i;
  while (l >= 0) {
    if (e > l+1) e = l + 1;
    w = (n >>(l+1-e)) & ((1UL<<2)-1);
    v = vals(w);
    l -= e;
    tw = SG_BIGNUM_GET_COUNT(table[w>>(v+1)]);
    if (z) {
      for (i = 1; i <= e-v; i++) z <<= 1; /* square z */
      z += tw;				  /* mul */
    } else {
      z = tw;
    }
    for (i = 1; i <= v; i++) z <<= 1; /* square z */
    while (l >= 0) {
      if (n & (1UL<<l)) break;
      z <<= 1;			/* square z */
      l--;
    }
  }
  return z;
}

/* unfortunately, this wasn't rgith from the begining
   but didn't come up so that never reached here... */
static SgBignum * sliding_window_expt(SgBignum *b, long n, long e)
{
  /* max must be 4 */
  long volatile zsize, zlen = 0;
  long i, l = (WORD_BITS-1) - (long)bfffo(n), u = 1UL<<(e-1);
  long w, v;
  SgBignum *r, *table[1UL<<(3-1)], *tw, *z = NULL, *prod1, *prod2;

  setup_table(table, u, b);
  zsize = compute_sw_size(l, e, n, table);
  /* we don't need to clear the buffer here. */
  r = make_bignum_rec(zsize, FALSE);
  SG_BIGNUM_SET_SIGN(r, 1);

  prod1 = r;
  if (Sg_AvailableStackSize((uintptr_t)&zsize) >= zsize) {
    ALLOC_TEMP_BIGNUM_REC(prod2, zsize);
  } else {
    prod2 = make_bignum_rec(zsize, FALSE);
  }

  while (l >= 0) {
    int index;
    if (e > l+1) e = l + 1;
    w = (n >>(l+1-e)) & ((1UL<<2)-1);
    v = vals(w);
    l -= e;
    index = (int)(w>>(v+1));
    tw = table[index];
    if (z) {
      SgBignum *t;
      long len = e-v;
      for (i = 1; i <= len; i++) {
	/* z = bignum_mul(z, z); */
	/* square_to_len(z->elements, zlen, prod1->elements); */
	mp_square(prod1->elements, zlen<<1, z->elements, zlen);
	zlen <<= 1;
	/* swap buffer */
	t = z;
	z = prod1;
	prod1 = t;
	SG_BIGNUM_SET_COUNT(z, zlen);
      }
      /* z = bignum_mul(z, tw); */
      bignum_mul_int(prod1, z, tw);
      zlen += SG_BIGNUM_GET_COUNT(tw);
      /* swap buffer */
      t = z;
      z = prod1;
      prod1 = t;
      SG_BIGNUM_SET_COUNT(z, zlen);
    } else {
      /* z = tw; */
      /* setup buffer */
      copy_element(prod1->elements, tw->elements, SG_BIGNUM_GET_COUNT(tw));
      z = prod1;
      prod1 = prod2;
      zlen = SG_BIGNUM_GET_COUNT(tw);
      SG_BIGNUM_SET_COUNT(z, zlen);
    }
    for (i = 1; i <= v; i++) {
      SgBignum *t;
      /* square_to_len(z->elements, zlen, prod1->elements); */
      mp_square(prod1->elements, zlen<<1, z->elements, zlen);
      zlen <<= 1;
      t = z;
      z = prod1;
      prod1 = t;
      SG_BIGNUM_SET_COUNT(z, zlen);
    }
    while (l >= 0) {
      SgBignum *t;
      if (n & (1UL<<l)) break;
      /* z = bignum_mul(z, z); */
      /* square_to_len(z->elements, zlen, prod1->elements); */
      mp_square(prod1->elements, zlen<<1, z->elements, zlen);
      zlen <<= 1;
      /* swap buffer */
      t = z;
      z = prod1;
      prod1 = t;
      SG_BIGNUM_SET_COUNT(z, zlen);
      l--;
    }
  }
  /* i'm not sure which would the proper one so check */
  if (r != z)
    copy_element(r->elements, z->elements, zsize);
  SG_BIGNUM_SET_COUNT(r, zsize);
  return r;
}

static SgBignum * bignum_expt(SgBignum *b, long exponent)
{
  /* exponent > 0 */
  long l = (WORD_BITS-1) - (long)bfffo((ulong)exponent);
  if (l <= 8) return leftright_binray_expt(b, exponent);
  else if (l <= 24) return sliding_window_expt(b, exponent, 2);
  else return sliding_window_expt(b, exponent, 3);
}


SgObject Sg_BignumExpt(SgBignum *b, long exponent)
{
  /* let's try the easiest one */
  ASSERT(exponent >= 0);
  /* let's handle the rare case, this must be handled by Sg_Expt */
  if (SG_BIGNUM_GET_SIGN(b) == 0) {
    return SG_MAKE_INT(0);
  } else if (!exponent) {
    return SG_MAKE_INT(1);
  } else if (exponent == 1) {
    return SG_OBJ(b);
  } else {
    return Sg_NormalizeBignum(bignum_expt(b, exponent));
  }
}

void Sg__InitBignum()
{
  int i;
  ZERO = Sg_MakeBignumFromSI(0);
  ONE = Sg_MakeBignumFromSI(1);
  MIN_ONE = Sg_MakeBignumFromSI(-1);
  RADIXES[0] = ZERO;
  DBL_RADIXES[0] = log(0);
  for (i = 1; i <= MAX_RADIX; i++) {
    RADIXES[i] = Sg_MakeBignumFromSI(i);
    DBL_RADIXES[i] = log(i);
  }
  /* DBL_LOG2 = DBL_RADIXES[2]; */
}


/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
