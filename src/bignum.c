/* -*- C -*- */
/*
 * bignum.c
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

#include "sagittarius/config.h"

#ifndef __GNUC__
# if HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
#pragma alloca
#  else
#   ifndef alloca /* predefined by HP cc +Olibcalls */
char *alloca ();
#   endif
#  endif
# endif
#else
# ifdef HAVE_ALLOCA_H
#  include <alloca.h>
# endif
# ifdef HAVE_MALLOC_H
/* MinGW helds alloca() in "malloc.h" instead of "alloca.h" */
#  include <malloc.h>
# endif
#endif

#include <math.h>
#include <string.h>

#define LIBSAGITTARIUS_BODY
#include "sagittarius/bignum.h"
#include "sagittarius/number.h"
#include "sagittarius/error.h"
#include "sagittarius/arith.h"
#include "sagittarius/bits.h"
#include "sagittarius/pair.h"
#include "sagittarius/string.h"

#undef min
#define min(x, y)   (((x) < (y))? (x) : (y))
#undef max
#define max(x, y)   (((x) > (y))? (x) : (y))

static int bignum_safe_size_for_add(SgBignum *x, SgBignum *y);
static SgBignum *bignum_add_int(SgBignum *br, SgBignum *bx, SgBignum *by);

static SgBignum* bignum_clear(SgBignum *b)
{
  int size = SG_BIGNUM_GET_COUNT(b);
  int i;
  for (i = 0; i < size; i++) b->elements[i] = 0;
  return b;
}

static SgBignum* make_bignum(int size)
{
  SgBignum *b;
  if (size < 0) Sg_Error(UC("[internal error] invalid bignum size: %d"), size);
  if (size > (int)BIGNUM_MAX_DIGITS) Sg_Error(UC("too large bignum"));
  b = SG_NEW_ATOMIC2(SgBignum*, BIGNUM_SIZE(size));
  SG_SET_HEADER(b, TC_BIGNUM);
  SG_BIGNUM_SET_COUNT(b, size);
  SG_BIGNUM_SET_SIGN(b, 1);
  return bignum_clear(b);
}

#ifdef HAVE_ALLOCA
#define ALLOC_TEMP_BIGNUM(var, size)		\
  (var) = SG_BIGNUM(alloca(BIGNUM_SIZE(size)));	\
	SG_SET_HEADER(var, TC_BIGNUM);		\
	SG_BIGNUM_SET_COUNT(var, size);		\
	SG_BIGNUM_SET_SIGN(var, 1);		\
	bignum_clear(var);			
#else
#define ALLOC_TEMP_BIGNUM(var, size) (var) = make_bignum(size);
#endif

SgObject Sg_MakeBignumFromSI(long value)
{
  SgBignum *b;
  if (value == LONG_MIN) {
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
  SgBignum *b = make_bignum(1);
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
      ans->elements[0] = value;
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
    if (value) {
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
      ans->elements[0] = value;
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
  int i;
  int size = SG_BIGNUM_GET_COUNT(src);
  ASSERT(SG_BIGNUM_GET_COUNT(dst) >= size);
  SG_BIGNUM_SET_SIGN(dst, SG_BIGNUM_GET_SIGN(src));
  for (i = 0; i < size; i++) dst->elements[i] = src->elements[i];
}

SgObject Sg_BignumCopy(SgBignum *b)
{
  SgBignum *c = make_bignum(SG_BIGNUM_GET_COUNT(b));
  bignum_copy(c, b);
  return SG_OBJ(c);  
}

SgObject Sg_NormalizeBignum(SgBignum *bn)
{
  int size = SG_BIGNUM_GET_COUNT(bn);
  int i;
  for (i = size - 1; i > 0; i--) {
    if (bn->elements[i] == 0) size--;
    else break;
  }
  if (i == 0) {
    int sign = SG_BIGNUM_GET_SIGN(bn);
    SG_BIGNUM_SET_ZERO(bn);
    SG_BIGNUM_SET_SIGN(bn, sign);
    if (SG_BIGNUM_GET_SIGN(bn) == 0) {
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
  }
  SG_BIGNUM_SET_COUNT(bn, size);
  return SG_OBJ(bn);
}


/* assume bignums are normalized */
int Sg_BignumCmp(SgBignum *lhs, SgBignum *rhs)
{
  int lhs_count = SG_BIGNUM_GET_COUNT(lhs);
  int rhs_count = SG_BIGNUM_GET_COUNT(rhs);
  int lhs_sign = SG_BIGNUM_GET_SIGN(lhs);
  int rhs_sign = SG_BIGNUM_GET_SIGN(rhs);
  int i;

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
  unsigned int xsize = SG_BIGNUM_GET_COUNT(bx), ysize = SG_BIGNUM_GET_COUNT(by);
  unsigned int osize = SG_BIGNUM_GET_COUNT(off);
  unsigned int tsize;
  int i;
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
      unsigned long xx = bx->elements[xsize - 1], oo = off->elements[osize - 1];
      UADD(w, c, xx, oo);
      if (c > 0 || w > by->elements[ysize - 1]) return 1;
      if (w < by->elements[ysize - 1] - 1) return -1;
    }
  }
  tsize = bignum_safe_size_for_add(bx, off);
  ALLOC_TEMP_BIGNUM(br, tsize);
  bignum_add_int(br, bx, off);
  
  if (SG_BIGNUM_GET_COUNT(br) < SG_BIGNUM_GET_COUNT(by)) return -1;
  for (i = (int)SG_BIGNUM_GET_COUNT(br) - 1; i >= 0; i--) {
    if (i >= (int)SG_BIGNUM_GET_COUNT(by)) {
      if (br->elements[i]) return 1;
      continue;
    }
    if (br->elements[i] < by->elements[i]) return -1;
    if (br->elements[i] > by->elements[i]) return 1;
  }
  return 0;
}

double Sg_BignumToDouble(SgBignum *b)
{
  int count = SG_BIGNUM_GET_COUNT(b);
  double ans = 0.0;
  int i;
#if SIZEOF_LONG >= 8
  if (count == 0) return 0.0;
  else if (count == 1) ans = b->elements[0];
  else { 
    for (i = count - 1; i >= count - 2; i--)
      ans += ldexp((double)b->elements[i], WORD_BITS * i);
  }
  return SG_BIGNUM_GET_SIGN(b) > 0 ? ans : -ans;
#else
  if (count == 0) return 0.0;
  else if (count == 1)
    ans = b->elements[0];
  else if (count == 2)
    ans = b->elements[1] * (double)4294967296.0 + b->elements[0];
  else {
    for (i = count - 1; i >= count - 3; i--)
      ans += ldexp((double)b->elements[i], WORD_BITS * i);
  }
  return SG_BIGNUM_GET_SIGN(b) > 0 ? ans : -ans;
#endif 
}

static inline int bn_norm_pred(SgBignum *bn)
{
  int bn_count = SG_BIGNUM_GET_COUNT(bn);
  return (bn_count == 0) || (bn->elements[bn_count - 1] != 0);
}

SgObject Sg_BignumToInteger(SgBignum *bn)
{
  ASSERT(bn_norm_pred(bn));
  ASSERT(SG_BIGNUM_GET_SIGN(bn) != 0);
  if (SG_BIGNUM_GET_COUNT(bn) == 0) return SG_MAKE_INT(0);
  if (SG_BIGNUM_GET_COUNT(bn) == 1) {
    unsigned long n = bn->elements[0];
    if (SG_BIGNUM_GET_SIGN(bn) < 0) {
      if (n < SG_ULONG_MAX) {
	n = -n;
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
int64_t  Sg_BignumToS64(SgBignum *b, int clamp, int *oor)
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
      r = b->elements[0];
    } else if (SG_BIGNUM_GET_COUNT(b) > 2 || b->elements[1] > LONG_MAX) {
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
  int rsize = SG_BIGNUM_GET_COUNT(br);
  int i, c;
  for (i = 0, c = 1; i < rsize; i++) {
    unsigned long x = ~br->elements[i];
    UADD(br->elements[i], c, x, 0);
  }
  return br;
}

SgObject Sg_BignumComplement(SgBignum *bx)
{
  SgBignum *r = SG_BIGNUM(Sg_BignumCopy(bx));
  return SG_OBJ(bignum_2scmpl(r));
}


int Sg_BignumBitCount(SgBignum *b)
{
  unsigned long *bits;
  SgBignum *z;
  int size;
  if (SG_BIGNUM_GET_SIGN(b) > 0) {
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

int Sg_BignumBitSize(SgBignum *b)
{
  int last = SG_BIGNUM_GET_COUNT(b) - 1;
  int bitsize;
  ASSERT(last >= 0);
  ASSERT(b->elements[last]);
  bitsize = WORD_BITS * last;
  return bitsize + WORD_BITS - nlz((intptr_t)b->elements[last]);
}

int Sg_BignumFirstBitSet(SgBignum *b)
{
  int bit = 0, i, size;
  SgBignum *z;
  if (SG_BIGNUM_GET_SIGN(b) > 0) {
    z = b;
    goto calc_bit_set;
  }
  ALLOC_TEMP_BIGNUM(z, SG_BIGNUM_GET_COUNT(b));
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

int Sg_BignumAbsCmp(SgBignum *bx, SgBignum *by)
{
  int i;
  unsigned int xsize = SG_BIGNUM_GET_COUNT(bx);
  unsigned int ysize = SG_BIGNUM_GET_COUNT(by);
    
  if (xsize < ysize) return -1;
  if (xsize > ysize) return 1;
  for (i = (int)xsize-1; i >= 0; i--) {
    if (bx->elements[i] < by->elements[i]) return -1;
    if (bx->elements[i] > by->elements[i]) return 1;
  }
  return 0;
}

SgObject Sg_BignumAsh(SgBignum *b, int count)
{
  if (count == 0) return Sg_NormalizeBignum(b);
  else if (count > 0) return Sg_BignumShiftLeft(b, count);
  else return Sg_BignumShiftRight(b, -count);
}

static SgBignum* bignum_lshift(SgBignum *br, SgBignum *bx, int amount)
{
  int nwords, nbits, i;
  unsigned long x;

  nwords = amount / WORD_BITS;
  nbits = amount % WORD_BITS;
  if (nbits == 0) {
    for (i = (int)SG_BIGNUM_GET_COUNT(bx) - 1; i >= 0; i--) {
      if ((int)SG_BIGNUM_GET_COUNT(br) > i + nwords)
	br->elements[i + nwords] = bx->elements[i];
    }
    ASSERT(SG_BIGNUM_GET_COUNT(br) >= nwords);
    for (i = nwords - 1; i >= 0; i--) br->elements[i] = 0;
  } else {
    int bxsize = SG_BIGNUM_GET_COUNT(bx);
    int brsize = SG_BIGNUM_GET_COUNT(br);
    if (brsize > bxsize + nwords) {
      br->elements[bxsize+nwords] = bx->elements[bxsize-1] >> (WORD_BITS-nbits);
    }
    for (i = bxsize - 1; i > 0; i--) {
      x = (bx->elements[i]<<nbits)|(bx->elements[i-1]>>(WORD_BITS-nbits));
      if (brsize > i+nwords) br->elements[i+nwords] = x;
    }
    ASSERT(brsize > nwords);
    br->elements[nwords] = bx->elements[0] << nbits;
    for (i = nwords - 1; i >= 0; i--) br->elements[i] = 0;
  }
  if (br != bx) {
    SG_BIGNUM_SET_SIGN(br, SG_BIGNUM_GET_SIGN(bx));
  }
  return br;
}

SgObject Sg_BignumShiftLeft(SgBignum *b, int shift)
{
  int rsize = SG_BIGNUM_GET_COUNT(b) + (shift + WORD_BITS - 1) / WORD_BITS;
  SgBignum *r = make_bignum(rsize);
  return Sg_NormalizeBignum(bignum_lshift(r, b, shift));
}

static SgBignum* bignum_rshift(SgBignum *br, SgBignum *bx, int amount)
{
  unsigned int nwords = amount / WORD_BITS;
  unsigned int nbits = amount % WORD_BITS;
  int i;

  if (SG_BIGNUM_GET_COUNT(bx) <= nwords) {
    SG_BIGNUM_SET_COUNT(br, 0);
    br->elements[0] = 0;
  } else if (nbits == 0) {
    for (i = (int)nwords; i < (int)SG_BIGNUM_GET_COUNT(bx); i++) {
      br->elements[i - nwords] = bx->elements[i];
    }
    SG_BIGNUM_SET_COUNT(br, SG_BIGNUM_GET_COUNT(bx) - nwords);
    SG_BIGNUM_SET_SIGN(br, SG_BIGNUM_GET_SIGN(bx));
  } else {
    unsigned long x;
    int bxsize = SG_BIGNUM_GET_COUNT(bx);
    for (i = (int)nwords; i < bxsize - 1; i++) {
      x = (bx->elements[i+1] << (WORD_BITS - nbits))|(bx->elements[i] >> nbits);
      br->elements[i - nwords] = x;
    }
    ASSERT(SG_BIGNUM_GET_COUNT(br) > i-nwords);
    br->elements[i - nwords] = bx->elements[i] >> nbits;
    SG_BIGNUM_SET_COUNT(br, SG_BIGNUM_GET_COUNT(bx) - nwords);
    SG_BIGNUM_SET_SIGN(br, SG_BIGNUM_GET_SIGN(bx));
  }
  return br;
}

SgObject Sg_BignumShiftRight(SgBignum *b, int shift)
{
  int rsize = SG_BIGNUM_GET_COUNT(b) + (-shift)/WORD_BITS;
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
  static inline SgBignum* name(SgBignum *z, SgBignum *x, SgBignum *y,	\
			       int compsize, int xsize, int ysize)	\
  {									\
    int i;								\
    for (i = 0; i < compsize; i++) {					\
      z->elements[i] = x->elements[i] op y->elements[i];		\
    }									\
    if (i < xsize) {							\
      for (; i < xsize; i++) z->elements[i] = x->elements[i];		\
    } else if (i < ysize) {						\
      for (; i < ysize; i++) z->elements[i] = y->elements[i];		\
    }									\
    return z;								\
  }

DEF_BIGNUM_LOG_OP(bignum_and, &)

SgObject Sg_BignumLogAnd(SgBignum *x, SgBignum *y)
{
  int xsize = SG_BIGNUM_GET_COUNT(x), xsign = SG_BIGNUM_GET_SIGN(x);
  int ysize = SG_BIGNUM_GET_COUNT(y), ysign = SG_BIGNUM_GET_SIGN(y);
  int zsize, minsize = min(xsize, ysize);
  SgBignum *xx, *yy, *z;

  if (xsign > 0) {
    if (ysign > 0) {
      z = bignum_and(make_bignum(minsize), x, y, minsize, 0, 0);
      return Sg_NormalizeBignum(z);
    } else {
      yy = SG_BIGNUM(Sg_BignumComplement(y));
      z = bignum_and(make_bignum(xsize), x, yy, minsize, xsize, 0);
      return Sg_NormalizeBignum(z);
    }
  } else {
    if (ysign > 0) {
      xx = SG_BIGNUM(Sg_BignumComplement(x));
      z = bignum_and(make_bignum(ysize), xx, y, minsize, 0, ysize);
      return Sg_NormalizeBignum(z);
    } else {
      xx = SG_BIGNUM(Sg_BignumComplement(x));
      yy = SG_BIGNUM(Sg_BignumComplement(y));
      zsize = max(xsize, ysize);
      z = bignum_and(make_bignum(zsize), xx, yy, minsize, xsize, ysize);
      SG_BIGNUM_SET_SIGN(z, -1);
      bignum_2scmpl(z);
      return Sg_NormalizeBignum(z);
    }
  }
}

DEF_BIGNUM_LOG_OP(bignum_ior, |)

SgObject Sg_BignumLogIor(SgBignum *x, SgBignum *y)
{
  int xsize = SG_BIGNUM_GET_COUNT(x), xsign = SG_BIGNUM_GET_SIGN(x);
  int ysize = SG_BIGNUM_GET_COUNT(y), ysign = SG_BIGNUM_GET_SIGN(y);
  int zsize, minsize = min(xsize, ysize);
  SgBignum *xx, *yy, *z;

  if (xsign >= 0) {
    if (ysign >= 0) {
      zsize = max(xsize, ysize);
      z = bignum_ior(make_bignum(zsize), x, y, minsize, xsize, ysize);
      return Sg_NormalizeBignum(z);
    } else {
      yy = SG_BIGNUM(Sg_BignumComplement(y));
      z = bignum_ior(make_bignum(ysize), x, yy, minsize, 0, ysize);
      SG_BIGNUM_SET_SIGN(z, -1);
      bignum_2scmpl(z);
      return Sg_NormalizeBignum(z);
    }
  } else {
    if (ysign >= 0) {
      xx = SG_BIGNUM(Sg_BignumComplement(x));
      z = bignum_ior(make_bignum(xsize), xx, y, minsize, xsize, 0);
      SG_BIGNUM_SET_SIGN(z, -1);
      bignum_2scmpl(z);
      return Sg_NormalizeBignum(z);
    } else {
      xx = SG_BIGNUM(Sg_BignumComplement(x));
      yy = SG_BIGNUM(Sg_BignumComplement(y));
      z = bignum_ior(make_bignum(minsize), xx, yy, minsize, 0, 0);
      SG_BIGNUM_SET_SIGN(z, -1);
      bignum_2scmpl(z);
      return Sg_NormalizeBignum(z);
    }
  }
}

SgObject Sg_BignumLogXor(SgBignum *x, SgBignum *y)
{
  SgObject xandy = Sg_BignumLogAnd(x, y);
  SgObject xory = Sg_BignumLogIor(x, y);
  return Sg_LogAnd(xory, Sg_LogNot(xandy));
}

static int bignum_safe_size_for_add(SgBignum *x, SgBignum *y)
{
  int xsize = SG_BIGNUM_GET_COUNT(x);
  int ysize = SG_BIGNUM_GET_COUNT(y);
  if (xsize > ysize) {
    if (x->elements[xsize - 1] == SG_ULONG_MAX) return xsize + 1;
    else return xsize;
  } else if (xsize < ysize) {
    if (y->elements[ysize - 1] == SG_ULONG_MAX) return ysize + 1;
    else return ysize;
  } else {
    return xsize + 1;
  }
}

static SgBignum* bignum_add_int(SgBignum *br, SgBignum *bx, SgBignum *by)
{
  int rsize = SG_BIGNUM_GET_COUNT(br);
  int xsize = SG_BIGNUM_GET_COUNT(bx);
  int ysize = SG_BIGNUM_GET_COUNT(by);
  int i, c;
  unsigned long x, y;

  for (i = 0, c = 0; i < rsize; i++, xsize--, ysize--) {
    if (xsize <= 0) {
      if (ysize <= 0) {
	UADD(br->elements[i], c, 0, 0);
	continue;
      }
      y = by->elements[i];
      UADD(br->elements[i], c, 0, y);
      continue;
    }
    if (ysize <= 0) {
      x = bx->elements[i];
      UADD(br->elements[i], c, x, 0);
      continue;
    }
    x = bx->elements[i];
    y = by->elements[i];
    UADD(br->elements[i], c, x, y);
  }
  return br;
}

static SgBignum* bignum_sub_int(SgBignum *br, SgBignum *bx, SgBignum *by)
{
  int rsize = SG_BIGNUM_GET_COUNT(br);
  int xsize = SG_BIGNUM_GET_COUNT(bx);
  int ysize = SG_BIGNUM_GET_COUNT(by);
  int i, c;
  unsigned long x, y;

  for (i = 0, c = 0; i < rsize; i++, xsize--, ysize--) {
    if (xsize <= 0) {
      if (ysize <= 0) {
	USUB(br->elements[i], c, 0, 0);
	continue;
      }
      y = by->elements[i];
      USUB(br->elements[i], c, 0, y);
      continue;
    }
    if (ysize <= 0) {
      x = bx->elements[i];
      USUB(br->elements[i], c, x, 0);
      continue;
    }
    x = bx->elements[i];
    y = by->elements[i];
    USUB(br->elements[i], c, x, y);
  }
  if (c != 0) {
    bignum_2scmpl(br);
    SG_BIGNUM_SET_SIGN(br, -SG_BIGNUM_GET_SIGN(br)); /* flip sign */
  }
  return br;
}

static SgBignum* bignum_add(SgBignum *bx, SgBignum *by)
{
  int rsize = bignum_safe_size_for_add(bx, by);
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
  int rsize = bignum_safe_size_for_add(bx, by);
  SgBignum *br = make_bignum(rsize);
  SG_BIGNUM_SET_SIGN(br, SG_BIGNUM_GET_SIGN(bx));
  if (SG_BIGNUM_GET_SIGN(bx) == SG_BIGNUM_GET_SIGN(by)) {
    bignum_sub_int(br, bx, by);
  } else {
    bignum_add_int(br, bx, by);
  }
  return br;
}

static SgBignum* bignum_add_si(SgBignum *bx, long y)
{
  long c;
  unsigned int i, rsize = SG_BIGNUM_GET_COUNT(bx) + 1;
  unsigned long yabs = ((y < 0) ? -y : y);
  int ysign = ((y < 0) ? -1: 1);
  SgBignum *br;
  if (y == 0) return bx;

  br = make_bignum(rsize);
  SG_BIGNUM_SET_SIGN(br, SG_BIGNUM_GET_SIGN(bx));
  if (SG_BIGNUM_GET_SIGN(bx) == ysign) {
    for (c = 0, i = 0; i < SG_BIGNUM_GET_COUNT(bx); i++) {
      UADD(br->elements[i], c, bx->elements[i], yabs);
      yabs = 0;
    }
  } else {
    for (c = 0, i = 0; i < SG_BIGNUM_GET_COUNT(bx); i++) {
      USUB(br->elements[i], c, bx->elements[i], yabs);
      yabs = 0;
    }
  }
  br->elements[rsize - 1] = c;
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

static SgBignum* bignum_mul_word(SgBignum *br, SgBignum *bx,
				 unsigned long y, int off)
{
  unsigned long hi, lo, x, r0, r1, c;
  unsigned int i, j;
  unsigned int size = SG_BIGNUM_GET_COUNT(br);
  for (i = 0; i < SG_BIGNUM_GET_COUNT(bx); i++) {
    x = bx->elements[i];
    UMUL(hi, lo, x, y);
    c = 0;
    
    r0 = br->elements[i + off];
    UADD(r1, c, r0, lo);
    ASSERT(size > i+off);
    br->elements[i + off] = r1;

    ASSERT(size > i+off+1);
    r0 = br->elements[i + off + 1];
    UADD(r1, c, r0, hi);
    br->elements[i + off + 1] = r1;

    for (j = i + off + 2; c && j < size; j++) {
      ASSERT(size > j);
      r0 = br->elements[j];
      UADD(r1, c, r0, 0);
      br->elements[j] = r1;
    }
  }
  return br;
}

static SgBignum* bignum_mul(SgBignum *bx, SgBignum *by)
{
  unsigned int i;
  SgBignum *br = make_bignum(SG_BIGNUM_GET_COUNT(bx) + SG_BIGNUM_GET_COUNT(by));
  for (i = 0; i < SG_BIGNUM_GET_COUNT(by); i++) {
    bignum_mul_word(br, bx, by->elements[i], i);
  }
  SG_BIGNUM_SET_SIGN(br, SG_BIGNUM_GET_SIGN(bx) * SG_BIGNUM_GET_SIGN(by));
  return br;
}

static SgBignum* bignum_mul_si(SgBignum *bx, long y)
{
  SgBignum *br;
  unsigned long yabs;

  if (y == 1) return bx;
  if (y == 0) {
    br = make_bignum(1);
    SG_BIGNUM_SET_SIGN(br, 1);
    br->elements[0] = 0;
    return br;
  }
  if (y == -1) {
    br = SG_BIGNUM(Sg_BignumCopy(bx));
    SG_BIGNUM_SET_SIGN(br, -SG_BIGNUM_GET_SIGN(bx));
  }
  br = make_bignum(SG_BIGNUM_GET_COUNT(bx) + 1);
  yabs = (y < 0) ? -y : y;
  SG_BIGNUM_SET_SIGN(br, SG_BIGNUM_GET_SIGN(bx));
  bignum_mul_word(br, bx, yabs, 0);
  if (y < 0) SG_BIGNUM_SET_SIGN(br, -SG_BIGNUM_GET_SIGN(br));
  return br;
}

SgObject Sg_BignumMul(SgBignum *a, SgBignum *b)
{
  return Sg_NormalizeBignum(bignum_mul(a, b));
}

SgObject Sg_BignumMulSI(SgBignum *a, long b)
{
  return Sg_NormalizeBignum(bignum_mul_si(a, b));
}

static inline int div_normalization_factor(unsigned long w)
{
  unsigned long b = (1L << (WORD_BITS - 1)), c = 0;
  for (; b > 0; b >>= 1, c++) {
    if (w & b) return c;
  }
  FATAL("bignum.c: div_normalization_factor: can't be here");
  return 0;
}

static SgBignum* bignum_gdiv(SgBignum *dividend, SgBignum *divisor,
			     SgBignum *quotient)
{
  SgBignum *u, *v;
  unsigned int ds_size = SG_BIGNUM_GET_COUNT(divisor);
  unsigned int de_size = SG_BIGNUM_GET_COUNT(dividend);
  int d = div_normalization_factor(divisor->elements[ds_size - 1]);
  int j, k, n, m;
  unsigned long vn_1, vn_2, vv, uj, uj2, cy;

#define DIGIT(num, n)							\
  (((n)%2)? HI((num)->elements[(n)/2]) : LO((num)->elements[(n)/2]))
#define DIGIT2(num, n)							\
  (((n)%2)?								\
   ((LO((num)->elements[(n)/2+1])<<HALF_BITS)|HI((num)->elements[(n)/2])): \
   (num)->elements[(n)/2])
#define SETDIGIT(num, n, v)						\
  (((n)%2)?								\
   (num->elements[(n)/2]=(num->elements[(n)/2] & LOMASK)|((v) << HALF_BITS)): \
   (num->elements[(n)/2]=(num->elements[(n)/2] & HIMASK)|((v) & LOMASK)))
#define SETDIGIT2(num, n, v)						\
  (((n)%2)?								\
   ((num->elements[(n)/2] = LO(num->elements[(n)/2])|((v)<<HALF_BITS)),	\
    (num->elements[(n)/2+1] = (num->elements[(n)/2+1] & HIMASK)|HI(v))) : \
   (num->elements[(n)/2] = (v)))
  
  u = make_bignum(de_size + 1);
  ALLOC_TEMP_BIGNUM(v, ds_size);
  if (d >= HALF_BITS) {
    d -= HALF_BITS;
    n = ds_size * 2 - 1;
    m = de_size * 2 - n;
  } else {
    n = ds_size * 2;
    m = de_size * 2 - n;
  }
  bignum_lshift(u, dividend, d);
  bignum_lshift(v, divisor, d);
  vn_1 = DIGIT(v, n - 1);
  vn_2 = DIGIT(v, n - 2);

  for (j = m; j >= 0; j--) {
    unsigned long uu = (DIGIT(u, j+n) << HALF_BITS) + DIGIT(u, j+n-1);
    unsigned long qq = uu / vn_1;
    unsigned long rr = uu % vn_1;

    while (qq >= HALF_WORD) { qq--; rr += vn_1; }
    while ((qq * vn_2 > (rr << HALF_BITS) + DIGIT(u, j + n - 2))
	   && (rr < HALF_WORD)) {
      qq--;
      rr += vn_1;
    }
    cy = 0;
    for (k = 0; k < n; k++) {
      vv = qq * DIGIT(v, k);
      uj = DIGIT2(u, j + k);
      uj2 = uj - vv - cy;
      cy =  (uj2 > uj) ? HALF_WORD : 0;
      SETDIGIT2(u, j + k, uj2);
    }
    if (cy) {
      qq--;
      cy = 0;
      for (k = 0; k < n; k++) {
	vv = DIGIT(v, k);
	uj = DIGIT(u, j + k) + vv + cy;
	cy = (uj >= HALF_WORD)? 1 : 0;
	SETDIGIT(u, j +k, uj);
      }
      uj = DIGIT(u, j + n) + cy;
      SETDIGIT(u, j + n, uj);
    }
    if (quotient) 
      SETDIGIT(quotient, j, qq);
  }
  bignum_rshift(u, u, d);
  return u;
}

static unsigned long bignum_sdiv(SgBignum *dividend, unsigned long divisor)
{
  int n = SG_BIGNUM_GET_COUNT(dividend) - 1;
  unsigned long *pu = dividend->elements;
  unsigned long q0 = 0, r0 = 0, q1, r1;

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
}

SgObject Sg_BignumDivRem(SgBignum *a, SgBignum *b)
{
  SgBignum *q, *r;
  if (Sg_BignumAbsCmp(a, b) < 0) {
    return Sg_Cons(SG_MAKE_INT(0), SG_OBJ(a));
  }
  q = make_bignum(SG_BIGNUM_GET_COUNT(a) - SG_BIGNUM_GET_COUNT(b) + 1);
  r = bignum_gdiv(a, b, q);
  SG_BIGNUM_SET_SIGN(q, SG_BIGNUM_GET_SIGN(a) * SG_BIGNUM_GET_SIGN(b));
  SG_BIGNUM_SET_SIGN(r, SG_BIGNUM_GET_SIGN(a));
  return Sg_Cons(Sg_NormalizeBignum(q), Sg_NormalizeBignum(r));
}

SgObject Sg_BignumModulo(SgBignum *a, SgBignum *b, int remp)
{
  SgBignum *r;
  if (Sg_BignumAbsCmp(a, b) < 0) return SG_OBJ(a);
  
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

SgObject Sg_BignumDivSI(SgBignum *a, long b, long *rem)
{
  unsigned long dd= (b < 0) ? -b : b;
  unsigned long rr;
  int d_sign = (b < 0) ? -1 : 1;
  SgBignum *q;

  if (dd < HALF_WORD) {
    q = SG_BIGNUM(Sg_BignumCopy(a));
    rr = bignum_sdiv(q, dd);
  } else {
    SgBignum *bv = SG_BIGNUM(Sg_MakeBignumFromSI(dd));
    SgBignum *br;
    q = make_bignum(SG_BIGNUM_GET_COUNT(a) + 1);
    br = bignum_gdiv(a, bv, q);
    rr = br->elements[0];
  }
  if (rem) {
    *rem = ((SG_BIGNUM_GET_SIGN(a) < 0) ? -(signed long)rr : (signed long)rr);
  }
  SG_BIGNUM_SET_SIGN(q, SG_BIGNUM_GET_SIGN(a) * d_sign);
  return Sg_NormalizeBignum(q);
}

static void bn_sqrt(SgBignum *bn)
{
  int count, workpad_count, bitsize;
  SgBignum *s, *workpad, *tmp;

  if (SG_BIGNUM_GET_SIGN(bn) == 0) return;
  count = SG_BIGNUM_GET_COUNT(bn);
  ALLOC_TEMP_BIGNUM(tmp, count);
  bignum_copy(tmp, bn);
  ALLOC_TEMP_BIGNUM(s, count);

  workpad_count = count + 1;
  ALLOC_TEMP_BIGNUM(workpad, workpad_count);

  bitsize = Sg_BignumBitSize(bn);
  bignum_rshift(s, tmp, (bitsize - 1) / 2);
  Sg_NormalizeBignum(s);

  while (TRUE) {
    tmp = Sg_Quotient(bn, s, NULL);
    /* tmp = Sg_BignumAdd(workpad, s); */
    tmp = bignum_add(workpad, s);
    bignum_rshift(workpad, tmp, 1);
    Sg_NormalizeBignum(workpad);
    if (Sg_BignumCmp(workpad, s) >= 0) {
      bignum_copy(bn, s);
      SG_BIGNUM_SET_SIGN(bn, 1);
      Sg_NormalizeBignum(bn);
      return ;
    }
    count = SG_BIGNUM_GET_COUNT(workpad);
    SG_BIGNUM_SET_COUNT(s, count);
    memcpy(s->elements, workpad->elements, sizeof(long) * count);
  }
}

SgObject Sg_BignumSqrt(SgBignum *bn)
{
  int count;
  SgBignum *workpad;
  double s;

  count = SG_BIGNUM_GET_COUNT(bn);
  ALLOC_TEMP_BIGNUM(workpad, count);
  memcpy(workpad->elements, bn->elements, sizeof(long) * count);
  bn_sqrt(workpad);
  if (bn->elements[0] == workpad->elements[0] * workpad->elements[0]) {
    SgBignum *s2 = Sg_BignumMul(workpad, workpad);
    if (Sg_BignumCmp(bn, s2) == 0) {
      if (SG_BIGNUM_GET_SIGN(bn) == 1) return Sg_BignumToInteger(workpad);
      return Sg_MakeComplex(SG_MAKE_INT(0), Sg_BignumToInteger(workpad));
    }
  }
  s = Sg_BignumToDouble(bn);
  s = sqrt(s < 0.0 ? -s : s);
  if (SG_BIGNUM_GET_SIGN(bn) == 1) return Sg_MakeFlonum(s);
  return Sg_MakeComplex(Sg_MakeFlonum(0.0), Sg_MakeFlonum(s));
}


SgObject Sg_MakeBignumWithSize(int size, unsigned long init)
{
  SgBignum *b = make_bignum(size);
  b->elements[0] = init;
  return b;
}

SgObject Sg_BignumAccMultAddUI(SgBignum *acc, unsigned long coef,
			       unsigned long c)
{
  SgBignum *r;
  unsigned int rsize = SG_BIGNUM_GET_COUNT(acc) + 1, i;
  ALLOC_TEMP_BIGNUM(r, rsize);
  r->elements[0] = c;
  bignum_mul_word(r, acc, coef, 0);
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

SgObject Sg_BignumToString(SgBignum *b, int radix, int use_upper)
{
  static const char ltab[] = "0123456789abcdefghijklmnopqrstuvwxyz";
  static const char utab[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  const char *tab = use_upper ? utab : ltab;
  SgObject h = SG_NIL, t = SG_NIL;
  SgBignum *q;
  long rem, size;
  if (radix < 2 || radix > 36) {
    Sg_Error(UC("radix out of range: %d"), radix);
  }
  q = SG_BIGNUM(Sg_BignumCopy(b));
  size = SG_BIGNUM_GET_COUNT(q);
  for (; size > 0;) {
    rem = bignum_sdiv(q, radix);
    ASSERT(rem >= 0 && rem < radix);
    SG_APPEND1(h, t, SG_MAKE_CHAR(tab[rem]));
    for (; q->elements[size - 1] == 0 && size > 0; size--);
  }
  if (SG_BIGNUM_GET_SIGN(q) < 0) SG_APPEND1(h, t, SG_MAKE_CHAR('-'));
  return Sg_ListToString(Sg_ReverseX(h));
}

/* we need this... */
static SgBignum* normalize_bignum(SgBignum *bn)
{
  int size = SG_BIGNUM_GET_COUNT(bn);
  int i;
  for (i = size - 1; i > 0; i--) {
    if (bn->elements[i] == 0) size--;
    else break;
  }
  SG_BIGNUM_SET_COUNT(bn, size);
  return bn;
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
  normalize_bignum(a);
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
  int s1 = Sg_BignumFirstBitSet(u);
  int s2 = Sg_BignumFirstBitSet(v);
  int k = min(s1, s2);
  /* these are for step B2 */
  int uOdd, tsign, lb;
  SgBignum *t;
  /* Sg_Printf(Sg_StandardErrorPort(), UC(";; x=%A, y=%A, k=%d\n"), u, v, k); */
  if (k != 0) {
    bignum_rshift(u, u, k);
    bignum_rshift(v, v, k);
    normalize_bignum(u);
    normalize_bignum(v);
  }
  /* Step B2 */
  uOdd = (k==s1);
  t = uOdd ? v: u;
  tsign = uOdd ? -1 : 1;

  while ((lb = Sg_BignumFirstBitSet(t)) >= 0) {
    /* Step B3 and B4 */
    /* Sg_Printf(Sg_StandardErrorPort(), UC("(print (= (>> %A %d) "), t, lb); */
    bignum_rshift(t, t, lb);
    normalize_bignum(t);
    /* Sg_Printf(Sg_StandardErrorPort(), UC("%A))\n"), t); */
    /* Step B5 */
    if (tsign > 0) u = t;
    else v = t;
    /* special case one word numbers */
    if (SG_BIGNUM_GET_COUNT(u) < 2 && SG_BIGNUM_GET_COUNT(v) < 2) {
      unsigned long x = u->elements[0], y = v->elements[0];
      x = (x >= y) ? gcd_fixfix(x, y) : gcd_fixfix(y, x);
      ret = Sg_MakeInteger(x);
      if (k > 0) {
	/* Sg_Printf(Sg_StandardErrorPort(), UC(";; r=%A, k=%d\n"), r, k); */
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
    if (abs(SG_BIGNUM_GET_COUNT(bx)-SG_BIGNUM_GET_COUNT(by)) < 2)
      return binary_gcd(bx, by);
    r = bignum_gdiv(bx, by, NULL);
    t = Sg_NormalizeBignum(r);
    if (SG_INTP(t)) return Sg_Gcd(by, t);
    bx = by;
    by = r;
  }
  return bx;
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
