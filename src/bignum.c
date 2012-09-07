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

#include <sagittarius/config.h>

#ifndef __GNUC__
# ifdef HAVE_ALLOCA_H
#  include <alloca.h>
# else
#  ifdef _AIX
#    pragma alloca
#  elif defined(_MSC_VER)
/* _alloca is in <malloc.h> */
#    include <malloc.h>
#    define alloca _alloca
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
  SG_SET_CLASS(b, SG_CLASS_INTEGER);
  if (size == 0) {
    SG_BIGNUM_SET_ZERO(b);
  } else {
    SG_BIGNUM_SET_COUNT(b, size);
    SG_BIGNUM_SET_SIGN(b, 1);
  }
  return bignum_clear(b);
}

#ifdef HAVE_ALLOCA
#define ALLOC_TEMP_BIGNUM(var, size)		\
  (var) = SG_BIGNUM(alloca(BIGNUM_SIZE(size)));	\
	SG_SET_CLASS(var, SG_CLASS_INTEGER);	\
	SG_BIGNUM_SET_COUNT(var, size);		\
	SG_BIGNUM_SET_SIGN(var, 1);		\
	bignum_clear(var);			
#else
#define ALLOC_TEMP_BIGNUM(var, size) (var) = make_bignum(size);
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

static SgObject bignum_normalize_rec(SgBignum *bn, int convertp)
{
  int size = SG_BIGNUM_GET_COUNT(bn);
  int i;
  for (i = size - 1; i > 0; i--) {
    if (bn->elements[i] == 0) size--;
    else break;
  }
  /* if given bignum was 0, i can be -1 */
  if (i <= 0) {
    if (convertp) {
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

int Sg_BignumBitSize(SgBignum *b)
{
  int last = SG_BIGNUM_GET_COUNT(b) - 1;
  int bitsize;
  if (SG_BIGNUM_GET_SIGN(b) == 0) return 0;
  ASSERT(last >= 0);
  ASSERT(b->elements[last]);
  bitsize = WORD_BITS * last;
  return bitsize + WORD_BITS - nlz((long)b->elements[last]);
}

int Sg_BignumFirstBitSet(SgBignum *b)
{
  int bit = 0, i, size;
  SgBignum *z;
  if (SG_BIGNUM_GET_SIGN(b) == 0) return 0;
  else if (SG_BIGNUM_GET_SIGN(b) > 0) {
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
  return Sg_ListToString(Sg_ReverseX(h), 0, -1);
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

static SgBignum *ONE = NULL, *ZERO = NULL;

static void init_const_bignums()
{
  /* These doesn't have to be thread safe. I guess*/
  if (!ONE) {
    ONE = Sg_MakeBignumFromSI(1);
  }
  if (!ZERO) {
    ZERO = Sg_MakeBignumFromSI(0);
  }
}

/* TODO this is slow */
static SgBignum * bignum_mod_inverse(SgBignum *x, SgBignum *m)
{
  SgBignum *u1, *u3, *v1, *v3, *q;
  int sign = 1;
  if (SG_BIGNUM_GET_SIGN(m) != 1) {
    Sg_Error(UC("modulus not positive %S"), m);
  }
  init_const_bignums();
  u1 = ONE;
  u3 = x;
  v1 = ZERO;
  v3 = m;
  ALLOC_TEMP_BIGNUM(q, SG_BIGNUM_GET_COUNT(m));
  while (Sg_BignumCmp(v3, ZERO) != 0) {
    SgBignum *t3, *w, *t1;
    t3 = bignum_normalize(bignum_gdiv(u3, v3, q));
    bignum_normalize(q);
    w = bignum_normalize(bignum_mul(q, v1));
    t1 = bignum_normalize(bignum_add(u1, w));
    u1 = v1; v1 = t1; u3 = v3; v3 = t3;
    sign = -sign;
  }
  if (sign < 0) {
    return bignum_normalize(bignum_sub(m, u1));
  } else {
    return bignum_normalize(u1);
  }
}


static ulong exp_mod_threadh_table[] = {7, 25, 81, 241, 673, 1793, LONG_MAX};

typedef unsigned long ulong;
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

#if SIZEOF_LONG == 8
typedef int128_t dlong;
#else
typedef int64_t dlong;
#endif

#define LONG_MASK SG_ULONG_MAX

static int ulong_array_cmp_to_len(ulong *arg1, ulong *arg2, int len)
{
  int i;
  for (i = 0; i < len; i++) {
    ulong b1 = arg1[i];
    ulong b2 = arg2[i];
    if (b1 < b2) return -1;
    if (b1 > b2) return 1;
  }
  return 0;
}

static ulong sub_n(ulong *a, int alen, ulong *b, int len)
{
  dlong sum = 0;
  while (--len >= 0) {
    sum = a[len] - b[len] + (sum>>WORD_BITS);
    a[len] = (ulong)sum;
  }
  return (ulong)(sum >> WORD_BITS);
}

static ulong mul_add(ulong *out, int outlen, ulong *in,
		     int offset, int len, long k)
{
  ulong klong = k & LONG_MASK;
  ulong carry = 0;
  int i;
  offset = outlen - offset -1;
  for (i = len; i >= 0; i--) {
    dlong product = in[i]*klong + out[offset] + carry;
    out[offset--] = (ulong)product;
    carry = product >> WORD_BITS;
  }
  return carry;
}

static int add_one(ulong *a, int alen, int offset, int mlen, ulong carry)
{
  dlong t;
  offset = alen - 1 - mlen - offset;
  t = a[offset] + carry;
  a[offset] = (ulong)t;
  if ((t >> WORD_BITS) == 0) return 0;
  while (--mlen >= 0) {
    if (--offset < 0) {		/* carry out of number */
      return 1;
    } else {
      a[offset]++;
      if (a[offset] != 0) return 0;
    }
  }
  return 1;
}

static void primitive_left_shift(ulong *a, int len, ulong n)
{
  int i, m;
  ulong c, n2;
  if (len == 0 || n == 0) return;
  n2 = WORD_BITS - n;
  for (i = 0, c = a[i], m=i+len-1; i < m; i++) {
    int b = c;
    c = a[i+1];
    a[i] = (b << n) | (c >> n2);
  }
  a[len-1] <<= n;
}

static ulong* square_to_len(ulong *x, int len, ulong *z)
{
  int zlen = len<<1;
  ulong last_low_word = 0, i, j, offset;
  for (i = 0, j = 0; j < len; j++) {
    dlong piece = (x[j] & LONG_MASK);
    dlong product = piece * piece;
    z[i++] = (last_low_word << (WORD_BITS-1)) | (ulong)(product>>(WORD_BITS+1));
    z[i++] = (ulong)(product >> 1);
    last_low_word = (ulong)product;
  }
  /* ass in off diagonal sums */
  for (i = len, offset = 1; i > 0; i--, offset += 2) {
    ulong t = x[i-1];
    t = mul_add(z, zlen, x, offset, i-1, t);
    add_one(z, zlen, offset-1, i, t);
  }
  primitive_left_shift(z, zlen, 1);
  z[zlen-1] |= x[len-1] & 1;
  return z;
}

static ulong* mont_reduce(ulong *n, int nlen, SgBignum *mod, int mlen, long inv)
{
  int c = 0;
  int len = mlen;
  int offset = 0;
  do {
    ulong nEnd = n[nlen-1-offset];
    ulong carry = mul_add(n, nlen, mod->elements, offset, mlen, inv * nEnd);
    c += add_one(n, nlen, offset, mlen, carry);
    offset++;
  } while (--len > 0);
  while (c > 0) {
    c += sub_n(n, nlen, mod->elements, mlen);
  }
  while (ulong_array_cmp_to_len(n, mod->elements, mlen) >= 0) {
    sub_n(n, nlen, mod->elements, mlen);
  }
  return n;
}

static ulong* multiply_to_len(ulong *x, int xlen, ulong *y, int ylen, ulong *z)
{
  int xstart = xlen -1;
  int ystart = ylen -1;
  ulong carry = 0;
  int i, j, k;
  for (j = ystart, k = ystart + 1 + xstart; j >= 0; j--, k--) {
    dlong product = y[j] * x[xstart] + carry;
    z[k] = (ulong)product;
    carry = product >> WORD_BITS;
  }
  z[xstart] = carry;
  for (i = xstart - 1; i >= 0; i--) {
    carry = 0;
    for (j = ystart, k = ystart + 1 + i; j >= 0; j--, k--) {
      dlong product = y[j] * x[i] + z[k] + carry;
      z[k] = (ulong)product;
      carry = product >> WORD_BITS;
    }
    z[i] = (ulong)carry;
  }
  return z;
}

static SgBignum * strip_leading_zeros(SgBignum *b)
{
  int size = SG_BIGNUM_GET_COUNT(b);
  if (b->elements[0] == 0) {
    /* remove leading 0s */
    int keep;
    for (keep = 0; keep < size && b->elements[keep] == 0; keep++);
    memmove(b->elements, b->elements + keep, sizeof(ulong) * (size - keep));
    SG_BIGNUM_SET_COUNT(b, size - keep);
  }
  return b;
} 

static SgObject odd_mod_expt(SgBignum *x, SgBignum *exp, SgBignum *mod)
{
  int modlen, wbits, ebits, tblmask;
  if (Sg_BignumCmp(exp, ONE)) return x;
  if (SG_BIGNUM_GET_SIGN(x) == 0) return ZERO;

#ifdef HAVE_ALLOCA
#define ALLOC_TEMP_BUFFER(v, type, size)		\
  (v) = (type *)alloca(sizeof(type) * size)
#else
#define ALLOC_TEMP_BUFFER(v, type, size)		\
  (v) = SG_NEW_ATOMIC2(type *, sizeof(type) * size);
#endif
  
  modlen = SG_BIGNUM_GET_COUNT(mod);
  wbits = 0;
  ebits = Sg_BignumBitSize(exp);
  if ((ebits != 17) || exp->elements[0] != 65537) {
    while (ebits > exp_mod_threadh_table[wbits]) {
      wbits++;
    }
  }
  tblmask = 1 << wbits;
  {
    SgBignum *base, *b2, *tr;
    ulong **table, *b, *t, *a;
    long inv;
    int i, bsize, shift;
    ALLOC_TEMP_BUFFER(table, ulong*, tblmask);
    for (i = 0; i < tblmask; i++) {
      /* is this valid? */
      ALLOC_TEMP_BUFFER(table[i], ulong, tblmask);
    }
    /* compute the modular inverse */
    inv = -inverse_mod_long(mod->elements[modlen -1]);
    shift = (modlen<<5);
    bsize = SG_BIGNUM_GET_COUNT(x) + (shift + WORD_BITS - 1)/WORD_BITS;
    ALLOC_TEMP_BIGNUM(base, bsize);
    bignum_lshift(base, x, shift);
    bignum_normalize(base);

    /* a2 = base; */
    a = base->elements;
    b2 = Sg_BignumCopy(mod);
    tr = bignum_gdiv(base, b2, NULL);
    bignum_normalize(tr);
    /* table elements always have at least modlen size */
    if (SG_BIGNUM_GET_COUNT(tr) < modlen) {
      int offset = modlen - SG_BIGNUM_GET_COUNT(tr);
      ulong *t2;
      ALLOC_TEMP_BUFFER(t2, ulong, modlen);
      for (i = 0; i < SG_BIGNUM_GET_COUNT(tr); i++) {
	t2[i+offset] = table[0][i];
      }
      table[0] = t2;
    } else {
      table[0] = tr->elements;
    }
    /* set b to the square of the base */
    ALLOC_TEMP_BUFFER(b, ulong, (modlen<<1));
    square_to_len(table[0], modlen, b);
    mont_reduce(b, modlen<<1, mod, modlen, inv);
    /* set t to high half of b */
    ALLOC_TEMP_BUFFER(t, ulong, modlen);
    for (i = 0; i < modlen; i++) {
      t[i] = b[i];
    }
    /* Fill int the table with odd powers of the base */
    for (i = 1; i < tblmask; i++) {
      ulong *prod;
      ALLOC_TEMP_BUFFER(prod, ulong, (modlen+modlen));
      multiply_to_len(t, modlen, table[i-1], modlen, prod);
      table[i] = mont_reduce(prod, modlen+modlen, mod, modlen, inv);
    }
    /* hmm, let me make a new scope... */
    {
      int bitpos = 1 << ((ebits-1) & (WORD_BITS-1));
      int buf = 0;
      int elen = SG_BIGNUM_GET_COUNT(exp);
      int eindex = 0;
      int multpos, isone = TRUE; 
      ulong *mult, *t2;
      SgBignum *r;
      for (i = 0; i <= wbits; i++) {
	buf = (buf << 1) | (((exp->elements[eindex] & bitpos) != 0)? 1 : 0);
	bitpos >>=1;
	if (bitpos == 0) {
	  eindex++;
	  bitpos = 1 << (WORD_BITS-1);
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
      if (multpos == ebits) isone = FALSE;
      /* the main loop */
      for (;;) {
	ebits--;
	buf <<= 1;
	if (elen != 0) {
	  buf |= ((exp->elements[eindex] & bitpos) != 0) ? 1 : 0;
	  bitpos >>= 1;
	  if (bitpos == 0) {
	    eindex++;
	    bitpos = 1 << (WORD_BITS-1);
	    elen--;
	  }
	}
	/* examine the window for pending multiplies */
	if ((buf & tblmask) != 0) {
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
	  if (isone) {
	    memcpy(b, mult, tblmask * sizeof(ulong));
	    isone = FALSE;
	  } else {
	    t = b;
	    multiply_to_len(t, modlen, mult, modlen, a);
	    mont_reduce(a, modlen, mod, modlen, inv);
	    t = a; a = b; b = t;
	  }
	}
	/* check if done */
	if (ebits == 0) break;
	/* square the input */
	if (!isone) {
	  t = b;
	  square_to_len(t, modlen, a);
	  mont_reduce(a, modlen, mod, modlen, inv);
	  t = a; a = b; b = t;
	}
      }
      /* convert result out of montgomery form and return */
      ALLOC_TEMP_BUFFER(t2, ulong, (2*modlen));
      for (i = 0; i < modlen; i++) {
	t2[i+modlen] = b[i];
      }
      mont_reduce(t2, 2*modlen, mod, modlen, inv);
      r = make_bignum(modlen);
      for (i = 0; i < modlen; i++) {
	r->elements[i] = t2[i];
      }
      return strip_leading_zeros(r);
    }
  }
}
/* mod(2^p */
static SgBignum * bignum_mod2(SgBignum *x, int p)
{
  int numInts, excessBits, i, xsize;
  SgBignum *r;
  if (Sg_BignumBitSize(x) <= p) return x;
  numInts = (p + (WORD_BITS-1)) >> 5;
  r = make_bignum(numInts);
  xsize = SG_BIGNUM_GET_COUNT(x);
  for (i = 0; i < numInts; i++) {
    r->elements[i] = x->elements[i + (xsize - numInts)];
  }
  excessBits = (numInts << 5) - p;
  r->elements[0] &= (1UL << (WORD_BITS - excessBits)) - 1;

  return strip_leading_zeros(r);
}


static int bignum_test_bit(SgBignum *b, int p)
{
  int i, max = (p / WORD_BITS) + 1;
  for (i = 0; i < max && i < SG_BIGNUM_GET_COUNT(b) ; i++, p -= WORD_BITS) {
    if (p <= WORD_BITS) {
      return (b->elements[i] & (1L << p)) != 0;
    } 
  }
  return FALSE;
}

static SgBignum * bignum_mod_expt2(SgBignum *x, SgBignum *e, int p)
{
  SgBignum *base = bignum_mod2(x, p), *result = Sg_MakeBignumFromSI(1);
  SgBignum *bp = Sg_MakeBignumFromSI(p);
  int exp_offset = 0, limit = Sg_BignumBitSize(e);
  if (x->elements[0] & 1) {
    limit = ((p-1) < limit) ? (p-1) : limit;
  }
  while (exp_offset < limit) {
    if (bignum_test_bit(e, exp_offset)) {
      result = bignum_mul(result, base);
      result = bignum_gdiv(result, bp, NULL);
      bignum_normalize(result);
    }
    exp_offset++;
    if (exp_offset < limit) {
      base = bignum_mul(base, base);
      base = bignum_gdiv(base, bp, NULL);
      bignum_normalize(base);
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
    ? bignum_gdiv(bx, bm, NULL) : bx;
  bignum_normalize(base);
  if (bm->elements[0] & 1) {	/* odd modulus */
    result = odd_mod_expt(base, be, bm);
  } else {
    /* even modulus. Tear it into an "odd part" (m1) and power of two (m2), 
       exponentiate mod m1, manually exponentiate mod m2, and use Chinese
       Remainder Theorem to combine results.
     */
    int p = Sg_BignumFirstBitSet(bm);
    int lsize = 1 + (p + WORD_BITS - 1) / WORD_BITS;
    int rsize = SG_BIGNUM_GET_COUNT(bm) + (-p)/WORD_BITS;
    SgBignum *m1,  *m2 = make_bignum(lsize), *base2, *a1, *a2, *y1, *y2;
    if (rsize < 1) {
      if (SG_BIGNUM_GET_SIGN(bm) < 0) {
	m1 = Sg_MakeBignumFromSI(-1);
      } else {
	m1 = ZERO;
      }
    } else {
      /* m must be positive */
      m1 = make_bignum(rsize);
      bignum_rshift(m1, bm, p);
      bignum_normalize(m1);
    }
    bignum_lshift(m2, ONE, p);
    bignum_normalize(m2);
    base2 = (SG_BIGNUM_GET_SIGN(bx) || Sg_BignumCmp(bx, m1) >= 0)
      ? bignum_gdiv(bx, m1, NULL) : bx;
    bignum_normalize(base2);
    a1 = (Sg_BignumCmp(m1, ONE)) ? ZERO : odd_mod_expt(base2, be, m1);
    a2 = bignum_mod_expt2(base, be, p);
    y1 = bignum_mod_inverse(m2, m1);
    y2 = bignum_mod_inverse(m1, m2);
    /* TODO this is not so memory efficient */
    {
      SgBignum *t1, *t2;
      t1 = bignum_normalize(bignum_mul(a1, m2));
      t1 = bignum_normalize(bignum_mul(t1, y1));
      t2 = bignum_normalize(bignum_mul(a2, m1));
      t2 = bignum_normalize(bignum_mul(t2, y2));
      result = bignum_gdiv(bignum_normalize(bignum_add(t1, t2)),
			   bm, NULL);
    }
    bignum_normalize(result);
  }
  return (invertp) ? bignum_mod_inverse(result, bm) : result;
}

SgObject Sg_BignumModExpt(SgBignum *bx, SgBignum *be, SgBignum *bm)
{
  init_const_bignums();

  if (SG_BIGNUM_GET_SIGN(be) == 0) {
    return (Sg_BignumCmp(bm, ONE) == 0) ? SG_MAKE_INT(0) : SG_MAKE_INT(1);
  }
  if (Sg_NumEq(bx, SG_MAKE_INT(1))) {
    return (Sg_BignumCmp(bm, ONE) == 0) ? SG_MAKE_INT(0) : SG_MAKE_INT(1);
  }
  if (Sg_NumEq(bx, SG_MAKE_INT(0))) {
    return (Sg_BignumCmp(bm, ONE) == 0) ? SG_MAKE_INT(0) : SG_MAKE_INT(1);
  }
  return Sg_NormalizeBignum(bignum_mod_expt(bx, be, bm));
}


/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
