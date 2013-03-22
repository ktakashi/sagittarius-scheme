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
  int real_size = size;
  if (size < 0) Sg_Error(UC("[internal error] invalid bignum size: %d"), size);
  if (size > (int)BIGNUM_MAX_DIGITS) Sg_Error(UC("too large bignum"));
  if (real_size == 0) real_size++; /* to avoid minus allocation */
  b = SG_NEW_ATOMIC2(SgBignum*, BIGNUM_SIZE(real_size));
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
  SG_BIGNUM_SET_COUNT(dst, size);
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
  unsigned int xsize = SG_BIGNUM_GET_COUNT(bx);
  unsigned int ysize = SG_BIGNUM_GET_COUNT(by);
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

static SgBignum* bignum_lshift(SgBignum *br, SgBignum const *bx, int amount)
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
      br->elements[bxsize+nwords] = bx->elements[bxsize-1]>>(WORD_BITS-nbits);
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

static SgBignum* bignum_rshift(SgBignum *br, SgBignum const *bx, int amount)
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

  /* handle obvious case first */
  if (xsign == 0 || ysign == 0) return SG_MAKE_INT(0);

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
  }
  return br;
}

static SgBignum* bignum_sub_int(SgBignum *br, SgBignum *bx, SgBignum *by)
{
  int rsize = SG_BIGNUM_GET_COUNT(br);
  int xsize = SG_BIGNUM_GET_COUNT(bx);
  int ysize = SG_BIGNUM_GET_COUNT(by);
  int i, c = 0;
  unsigned long x, y;

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
#define USE_FAST_MULTIPLY
#ifdef USE_FAST_MULTIPLY
/* forward declaration */
static ulong* multiply_to_len(ulong *x, int xlen, ulong *y, int ylen, ulong *z);
static ulong mul_add(ulong *out, ulong *in, int len, ulong k);

static SgBignum* bignum_mul_word(SgBignum *br, SgBignum *bx,
				 unsigned long y, int off)
{
  multiply_to_len(bx->elements, SG_BIGNUM_GET_COUNT(bx), &y, 1, 
		  br->elements + off);
  return br;
}

static SgBignum* bignum_mul(SgBignum *bx, SgBignum *by)
{
  int xlen = SG_BIGNUM_GET_COUNT(bx);
  int ylen = SG_BIGNUM_GET_COUNT(by);
  SgBignum *br = make_bignum(xlen + ylen);
  multiply_to_len(bx->elements, xlen, by->elements, ylen, br->elements);
  SG_BIGNUM_SET_SIGN(br, SG_BIGNUM_GET_SIGN(bx) * SG_BIGNUM_GET_SIGN(by));
  /* Sg_Printf(Sg_StandardErrorPort(), UC("%S x %S = %S\n"), bx, by, br); */
  return br;
}

#else
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
    /* ASSERT(size > i+off); */
    br->elements[i + off] = r1;

    /* ASSERT(size > i+off+1); */
    r0 = br->elements[i + off + 1];
    UADD(r1, c, r0, hi);
    br->elements[i + off + 1] = r1;

    for (j = i + off + 2; c && j < size; j++) {
      /* ASSERT(size > j); */
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
#endif

static SgBignum* bignum_mul_si(SgBignum *bx, long y)
{
  SgBignum *br;
  unsigned long yabs;

  if (y == 1) return bx;
  if (y == 0) {
    br = make_bignum(1);
    SG_BIGNUM_SET_SIGN(br, 0);
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

static SgBignum* bignum_gdiv_rec(SgBignum *dividend, SgBignum *divisor,
				 SgBignum *quotient, int remainderp)
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
  
  if (remainderp) {
    u = make_bignum(de_size + 1);
  } else {
    ALLOC_TEMP_BIGNUM(u, de_size+1);
  }
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
  if (remainderp) {
    return u;
  } else {
    return NULL;
  }
}

#define bignum_gdiv(dend, dvis, quo) bignum_gdiv_rec(dend, dvis, quo, TRUE)

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

SgObject Sg_BignumModuloSI(SgBignum *a, long b, int remp)
{
  SgBignum *bb;
  ASSERT(b != 0);
  ALLOC_TEMP_BIGNUM(bb, 1);
  SG_BIGNUM_SET_SIGN(bb, (b < 0) ? -1 : 1);
  bb->elements[0] = (b < 0) ? -b : b;
  return Sg_BignumModulo(a, bb, remp);
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

static SgBignum * bn_sqrt(SgBignum *bn)
{
  int count, workpad_count, bitsize;
  SgBignum *s, *workpad;

  if (SG_BIGNUM_GET_SIGN(bn) == 0) return bn;
  count = SG_BIGNUM_GET_COUNT(bn);
  ALLOC_TEMP_BIGNUM(s, count);
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
      if (SG_BIGNUM_GET_SIGN(bn) > 0) return Sg_BignumToInteger(workpad);
      return Sg_MakeComplex(SG_MAKE_INT(0), Sg_BignumToInteger(workpad));
    }
  }
  s = Sg_BignumToDouble(bn);
  s = sqrt(s < 0.0 ? -s : s);
  if (SG_BIGNUM_GET_SIGN(bn) > 0) return Sg_MakeFlonum(s);
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
#ifdef USE_FAST_MULTIPLY
  unsigned long carry;
#endif
  ALLOC_TEMP_BIGNUM(r, rsize);
  r->elements[0] = c;
#ifdef USE_FAST_MULTIPLY
  carry = mul_add(r->elements, acc->elements, SG_BIGNUM_GET_COUNT(acc), coef);
  r->elements[SG_BIGNUM_GET_COUNT(acc)] = carry;
#else
  bignum_mul_word(r, acc, coef, 0);
#endif
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
  long count = 0, rem;
  long size = SG_BIGNUM_GET_COUNT(q);
  for (; size > 0;) {
    rem = bignum_sdiv(q, radix);
    count++;
    for (; q->elements[size - 1] == 0 && size > 0; size--);
  }
  return count;
}

SgObject Sg_BignumToString(SgBignum *b, int radix, int use_upper)
{
  static const char ltab[] = "0123456789abcdefghijklmnopqrstuvwxyz";
  static const char utab[] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  const char *tab = use_upper ? utab : ltab;
  /* SgObject h = SG_NIL, t = SG_NIL; */
  SgObject rs;
  SgBignum *q;
  long rem, size, count;
  int i;
  if (radix < 2 || radix > 36) {
    Sg_Error(UC("radix out of range: %d"), radix);
  }
  /* special case 0 */
  if (SG_BIGNUM_GET_SIGN(b) == 0) return SG_MAKE_STRING("0");

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

/* from here, the code base on Java's BigInteger */
typedef unsigned long ulong;
#if SIZEOF_LONG == 8
#ifdef __GNUC__
typedef unsigned int dlong __attribute__((__mode__(TI)));
#else
# error "sizeof(long) == 8 but not GCC (not supported yet)"
#endif
#define SHIFT_MAGIC 6
#else
typedef uint64_t dlong;
#define SHIFT_MAGIC 5
#endif

/* debug utility macro */
#define dump_array_rec(flag, array, size)	\
  do {						\
    int __i, __size = (size);			\
    fprintf(stderr, #array " = [ ");		\
    for (__i = 0; __i < __size; __i++) {	\
      fprintf(stderr, flag, (array)[__i]);	\
    }						\
    fprintf(stderr, "]\n");			\
  } while (0)

#define dump_sarray(array, size) dump_array_rec("%ld ", array, size)
#define dump_uarray(array, size) dump_array_rec("%lu ", array, size)
#define dump_xarray(array, size) dump_array_rec("%lx ", array, size)

#define dump_rec(flag, v) fprintf(stderr, #v" "flag, v)
#define dump_s(v) dump_rec("%ld\n", v)
#define dump_u(v) dump_rec("%lu\n", v)

#define dump_bignum_s(b) dump_sarray((b)->elements, (b)->size)
#define dump_bignum_u(b) dump_uarray((b)->elements, (b)->size)
#define dump_bignum_x(b) dump_xarray((b)->elements, (b)->size)

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
  if (BIGNUM_ZEROP(r)
      && (SG_BIGNUM_GET_SIGN(a) * SG_BIGNUM_GET_SIGN(b) < 0)) {
    return bignum_normalize(bignum_add(b, r));
  }
  return r;
}

/* These something from Java was actually really slow. */
#if 0
static int bignum_compare_elements(SgBignum *x, SgBignum *m)
{
  int i;
  if (SG_BIGNUM_GET_COUNT(x) < SG_BIGNUM_GET_COUNT(m)) return -1;
  if (SG_BIGNUM_GET_COUNT(x) > SG_BIGNUM_GET_COUNT(m)) return 1;
  for (i = 0; i < SG_BIGNUM_GET_COUNT(x); i++) {
    if (x->elements[i] != m->elements[i]) {
      return (x->elements[i] < m->elements[i]) ? -1 : 1;
    }
  }
  return 0;
}

static SgBignum * bignum_fixup(SgBignum *c, SgBignum *p, int k)
{
  int r, i, numWords = k >> SHIFT_MAGIC, numBits;
  SgBignum *t;
  r = -inverse_mod_long(p->elements[0]);
  c = Sg_BignumCopy(c);
  for (i = 0; i <numWords; i++) {
    /* V = R * c (mod 2^j) */
    int v = r * c->elements[0];
    /* c = c + (v * p) */
    t = bignum_mul_si(p, v);
    c = bignum_add(c, t);
    /* c = c / 2 */
    bignum_rshift(c, c, 1);
    bignum_normalize(c);
    /* SG_BIGNUM_SET_COUNT(c, SG_BIGNUM_GET_COUNT(c)-1); */
  }
  numBits = k & 0x1F;
  if (numBits != 0) {
    /* V = R * c (mod 2^j) */
    int v = r * c->elements[0];
    v &= ((1<<numBits) -1);
    /* c = c + (v * p) */
    t = bignum_mul_si(p, v);
    c = bignum_add(c, t);
    /* c = c / 2^j */
    bignum_rshift(c, c, numBits);
    bignum_normalize(c);
  }
  /* in theory, c maybe greater than p at this point (very rare!) */
  while (Sg_BignumCmp(c, p) >= 0) {
    c = bignum_sub(c, p);
  }
  return c;
}

static SgBignum * odd_mod_inverse(SgBignum *x, SgBignum *m)
{
  SgBignum *f = Sg_BignumCopy(x), *p = m, *g = p,
    *c = Sg_MakeBignumFromSI(1), *d = Sg_MakeBignumFromSI(0), *t;
  int k = 0, trailingZeros, dsize;
  if ((f->elements[0] & 1) == 0) {
    trailingZeros = Sg_BignumFirstBitSet(f);
    bignum_rshift(f, f, trailingZeros);
    bignum_normalize(f);
    k = trailingZeros;
  }
  /* the almost inverse algorithm */
  while (Sg_BignumCmp(f, ONE) != 0) {
    /* if gcd(f, g) != 1, number is not invertible modulo mod */
    if (SG_BIGNUM_GET_SIGN(f) == 0) {
      Sg_Error(UC("not invertible(%S, %S)"), x, m);
    }
    if (Sg_BignumCmp(f, g) < 0) {
      SgBignum *temp = f;
      f = g; g = temp;
      temp = d; d = c; c = temp;
    }
    /* if f != g (mod 4) */
    if (((f->elements[0] ^ g->elements[0]) & 3)) {
      f = bignum_sub(f, g);
      c = bignum_sub(c, d);
    } else {			/* if f == g (mod 4) */
      f = bignum_add(f, g);
      c = bignum_add(c, d);
    }
    trailingZeros = Sg_BignumFirstBitSet(f);
    bignum_rshift(f, f, trailingZeros);
    bignum_normalize(f);
    dsize = SG_BIGNUM_GET_COUNT(d)+(trailingZeros+WORD_BITS-1)/WORD_BITS;
    /* can we use alloca here? */
    t = make_bignum(dsize);
    bignum_lshift(t, d, trailingZeros);
    d = t;
    k += trailingZeros;
  }
  while (SG_BIGNUM_GET_SIGN(c) < 0) {
    c = bignum_add(c, p);
  }
  return bignum_fixup(c, p, k);
}

static SgBignum * mod_inverse_bp2(SgBignum *x, int k)
{
  return bignum_fixup(ONE, x, k);
}

static SgBignum * mod_inverse_mp2(SgBignum *x, int k)
{
  SgBignum *r;
  dlong t, p = (dlong)x->elements[0];
  if ((p & 1) == 0) {
    Sg_Error(UC("%S not invertible. (CD != 1)"), x);
  }
  t = (dlong)inverse_mod_long(x->elements[0]);
  if (k < 33) {
    t = (k == 32) ? t : (t & ((1 << k) -1));
  }
  t = t * (2 - p * 5);
  t = (k == 64) ? t : (t & ((1ULL << k) -1));

  r = make_bignum(2);
  r->elements[0] = (ulong)t;
  r->elements[1] = (ulong)(t >> WORD_BITS);
  return bignum_normalize(r);
}

static SgBignum * even_mod_inverse(SgBignum *x, SgBignum *m)
{
  int powersOf2 = Sg_BignumFirstBitSet(m);
  int rsize = SG_BIGNUM_GET_COUNT(m) + (-powersOf2)/WORD_BITS, lsize;
  SgBignum *oddMod, *oddPart, *evenPart, *y1, *y2, *temp, *r;
  ALLOC_TEMP_BIGNUM(oddMod, rsize);
  bignum_rshift(oddMod, m, powersOf2);
  bignum_normalize(oddMod);
  if (Sg_BignumCmp(oddMod, ONE) == 0) {
    return mod_inverse_mp2(x, powersOf2);
  }
  /* 1/a mod oddMod */
  oddPart = odd_mod_inverse(x, oddMod);
  /* 1/a mod evenMod */
  evenPart = mod_inverse_mp2(x, powersOf2);
  y1 = mod_inverse_bp2(oddMod, powersOf2);
  y2 = mod_inverse_mp2(oddMod, powersOf2);

  lsize = SG_BIGNUM_GET_COUNT(oddPart) + (powersOf2 + WORD_BITS - 1)/WORD_BITS;
  ALLOC_TEMP_BIGNUM(temp, lsize);
  bignum_lshift(temp, oddPart, powersOf2);
  r = bignum_mul(temp, y1);
  temp = bignum_mul(evenPart, oddMod);
  temp = bignum_mul(temp, y2);
  r = bignum_add(r, temp);
  return bignum_gdiv(r, m, NULL);
}

static SgBignum * bignum_mod_inverse(SgBignum *x, SgBignum *m)
{
  SgBignum *modVal = x;
  if (SG_BIGNUM_GET_SIGN(m) < 0) {
    Sg_Error(UC("modulus not positive %S"), m);
  }
  if (BIGNUM_ONEP(m)) return Sg_MakeBignumFromSI(0);
  if (SG_BIGNUM_GET_SIGN(x) < 0 || bignum_compare_elements(x, m) >= 0) {
    modVal = bignum_mod(x, m, NULL);
  }
  if (BIGNUM_ONEP(modVal)) return Sg_MakeBignumFromSI(1);
  if (m->elements[0] & 1) return bignum_normalize(odd_mod_inverse(modVal, m));
  if ((x->elements[0] & 1) == 0) {
    /* base and modulus are even */
    Sg_Error(UC("given number %S and %S is not invertible"), x, m);
  }
  return bignum_normalize(even_mod_inverse(modVal, m));
}

#else

static SgBignum * bignum_mod_inverse(SgBignum *x, SgBignum *m)
{
  SgBignum *u1, *u3, *v1, *v3, *q;
  int sign = 1, size;
  if (SG_BIGNUM_GET_COUNT(x) > SG_BIGNUM_GET_COUNT(m)) {
    size = SG_BIGNUM_GET_COUNT(x) + 1;
  } else {
    size = SG_BIGNUM_GET_COUNT(m) + 1;
  }

  if (SG_BIGNUM_GET_SIGN(m) < 0) {
    Sg_Error(UC("modulus not positive %S"), m);
  }
  u1 = Sg_MakeBignumFromSI(1);
  u3 = x;
  v1 = Sg_MakeBignumFromSI(0);
  v3 = m;
  ALLOC_TEMP_BIGNUM(q, size);
  while (!BIGNUM_ZEROP(v3)) {
    SgBignum *t3, *w, *t1;
    t3 = bignum_normalize(bignum_mod(u3, v3, q));
    bignum_normalize(q);
    w = bignum_normalize(bignum_mul(q, v1));
    t1 = bignum_normalize(bignum_add(u1, w));
    u1 = v1; v1 = t1; u3 = v3; v3 = t3;
    sign = -sign;
    /* reset buffer */
    SG_BIGNUM_SET_COUNT(q, size);
    SG_BIGNUM_SET_SIGN(q, 1);
  }
  if (sign < 0) {
    return bignum_normalize(bignum_sub(m, u1));
  } else {
    return bignum_normalize(u1);
  }
}
#endif

#define EXPMOD_MAX_WINDOWS 7
static ulong exp_mod_threadh_table[EXPMOD_MAX_WINDOWS] = {
  7, 25, 81, 241, 673, 1793, (ulong)-1L
};

static int ulong_array_cmp_to_len(ulong *arg1, ulong *arg2, int len)
{
  int i;
  for (i = len-1; i >= 0; i--) {
    if (arg1[i] != arg2[i]) {
      if (arg1[i] < arg2[i]) return -1;
      else return 1;
    }
  }
  return 0;
}

static ulong sub_n(ulong *num1, ulong *num2, int len)
{
  dlong t = (dlong)*num1 - *num2;
  int i;
  *num1 = (ulong)t;
  for (i = 1; i < len; i++) {
    t = (dlong)num1[i] - (dlong)num2[i] - (ulong)-(t >> WORD_BITS);
    num1[i] = (ulong)t;
  }
  return -(ulong)(t >> WORD_BITS);
}

static ulong mul_add(ulong *out, ulong *in, int len, ulong k)
{
  int i;
  dlong p = (dlong)*in * k + *out;
  *out = (ulong)p;

  for (i = 1; i < len; i++) {
    p = (dlong)in[i] * k + (ulong)(p >> WORD_BITS) + out[i];
    out[i] = (ulong)p;
  }
  return (ulong)(p >> WORD_BITS);
}

static int add_one(ulong *num, int len, ulong carry)
{
  dlong t = (dlong)*num + carry;
  int i;
  *num = (ulong)t;
  if ((t >> WORD_BITS) == 0) return 0;
  for (i = 1; i < len; i++) {
    if (++num[i] != 0) return 0;
  }
  return 1;
}

static ulong primitive_left_shift(ulong *a, int len, ulong n)
{
  int i;
  ulong x, carry = 0;
  for (i = 0; i < len; i++) {
    x = a[i];
    a[i] = (x<<n) | carry;
    carry = x >> (WORD_BITS-n);
  }
  return carry;
}

#if 0
static ulong primitive_right_shift(ulong *a, int len, ulong n)
{
  ulong x, carry = 0;
  a += len;
  while (len--) {
    a--;
    x = *a;
    *a = (x>>n) | carry;
    carry = x << (WORD_BITS-n);
  }
  return carry >> (WORD_BITS-n);
}
#endif

static ulong* square_to_len(ulong *num, int len, ulong *prod)
{
  if (!len) {
    /* special case of zero */
    return prod;
  } else {
    int lenx;
    ulong last_low = 0;
    /* Store the squares, right shifted one bit */
    int i, j;

    for (i = len, j = len << 1; i > 0;) {
      dlong t = num[--i];
      dlong p = t * t;
      prod[--j] = (last_low << (WORD_BITS-1))|(ulong)(p >> (WORD_BITS+1));
      prod[--j] = (ulong)(p >> 1);
      last_low = p & 1;
    }

    /* then add in the off diagonal sums */
    for (i = 0, j = 1, lenx = len - 1; lenx; i++, j += 2, lenx--) {
      ulong t = num[i];
      t = mul_add(prod + j, num + i + 1, lenx, t);
      add_one(prod + lenx + j, lenx + 1, t);
    }
    primitive_left_shift(prod, 2*len, 1);
    prod[0] |= num[0] & 1;
    return prod;
  }
}

static ulong* mont_reduce(ulong *n, SgBignum *mod, int mlen, ulong inv)
{
  ulong c = 0;
  int len = mlen;

  do {
    ulong carry = mul_add(n, mod->elements, mlen, inv * n[0]);
    c += add_one(n+mlen, len, carry);
    ++n;
  } while (--len);
  while (c) {
    c -= sub_n(n, mod->elements, mlen);
  }
  while (ulong_array_cmp_to_len(n, mod->elements, mlen) >= 0) {
    sub_n(n, mod->elements, mlen);
  }
  return n;
}

#if 0
static void mul_n1(ulong *out, ulong *in, int nlen, ulong k)
{
  dlong p = (dlong)*in++ * k;
  *out++ = (ulong)p;
  while (--nlen) {
    p = (dlong)*in++ * k + (ulong)(p >> WORD_BITS);
    *out++ = (ulong)p;
  }
  *out = (ulong)(p >> WORD_BITS);
}
#endif

static ulong* multiply_to_len(ulong *x, int xlen, ulong *y, int ylen, ulong *z)
{
  /* multiply first word */
  /* mul_n1(z, x, xlen, *y++); */
  int i;
  ulong k = *y;
  dlong p = (dlong)*x * k;
  *z = (ulong)p;
  for (i = 1; i < xlen; i++) {
    p = (dlong)x[i] * k + (ulong)(p >> WORD_BITS);
    z[i] = (ulong)p;
  }
  z[i] = (ulong)(p >> WORD_BITS);

  /* add in subsequent wors, storing the most significant word which is new
     each time */
  for (i = 1; i < ylen; i++) {
    z[xlen + i] = mul_add((z+i), x, xlen, y[i]);
  }
  return z;
}


#define clear_buffer(v, size)				\
  do {							\
    int __i, __size = (size);				\
    for (__i = 0; __i < __size; __i++) (v)[__i] = 0;	\
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

static ulong * odd_mod_expt_rec(SgBignum *x, SgBignum *exp, SgBignum *mod,
				ulong inv, ulong *a, ulong *b)
{
  SgBignum *tr, *tb;
  ulong *table[1 << EXPMOD_MAX_WINDOWS], *prod, *t;
  int i, j, tblmask, wbits, ebits, modlen = SG_BIGNUM_GET_COUNT(mod);
  int elen = SG_BIGNUM_GET_COUNT(exp), isone = TRUE;
  unsigned int buf = 0, multpos;
  ulong *mult, *e = exp->elements + elen -1;
  ulong bitpos;

  wbits = 0;
  ebits = Sg_BignumBitSize(exp);
  if ((ebits != 17) ||
      (SG_BIGNUM_GET_COUNT(exp) != 1 && exp->elements[0] != 65537)) {
    while ((unsigned int)ebits > exp_mod_threadh_table[wbits]) {
      wbits++;
    }
  }

  tblmask = 1u << wbits;
  /* convert x to montgomery form */
  ALLOC_TEMP_BIGNUM(tb, SG_BIGNUM_GET_COUNT(x) + modlen);
  for (i = 0; i < (int)SG_BIGNUM_GET_COUNT(x); i++) {
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
  square_to_len(table[0], modlen, b);
  mont_reduce(b, mod, modlen, inv);
  /* Use high hald of b to initialise the table */
  t = b+modlen;

  /* Fill int the table with odd powers of the base */
  for (i = 1; i < tblmask; i++) {
    ALLOC_TEMP_BUFFER(prod, ulong, modlen);
    multiply_to_len(t, modlen, table[i-1], modlen, a);
    mont_reduce(a, mod, modlen, inv);

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
	multiply_to_len(t, modlen, mult, modlen, a);
	mont_reduce(a, mod, modlen, inv);
	t = a; a = b; b = t;
      }
    }
    /* check if done */
    if (!ebits) return b;
    /* square the input */
    if (!isone) {
      t = b+modlen;
      square_to_len(t, modlen, a);
      mont_reduce(a, mod, modlen, inv);
      t = a; a = b; b = t;
    }
  }
}

static SgObject odd_mod_expt(SgBignum *x, SgBignum *exp, SgBignum *mod)
{
  int modlen, i, buflen;
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
    t[i] = 0;
  }
  mont_reduce(b, mod, modlen, inv);

  for (i = 0; i < modlen; i++) {
    r->elements[i] = t[i];
  }
  return bignum_normalize(r);
}

/* mod(2^p */
static SgBignum * bignum_mod2(SgBignum *x, int p)
{
  int numInts, excessBits, i;
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

/* bit-set? */
static int bignum_test_bit(SgBignum *b, int p)
{
  int pos = p >> SHIFT_MAGIC;
  ulong v;
  if (pos >= (int)SG_BIGNUM_GET_COUNT(b)) {
    return FALSE;
  }
  v = b->elements[pos];
  if (pos) {
    p %= WORD_BITS;
  }
  return (v & (1L << p)) != 0;
}

static SgBignum * bignum_mod_expt2(SgBignum *x, SgBignum *e, int p)
{
  SgBignum *base = bignum_mod2(x, p), *result = Sg_MakeBignumFromSI(1);
  int exp_offset = 0, limit = Sg_BignumBitSize(e);
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
    int p = Sg_BignumFirstBitSet(bm);
    int lsize = 1 + (p + WORD_BITS - 1) / WORD_BITS;
    int rsize = SG_BIGNUM_GET_COUNT(bm) + (-p)/WORD_BITS;
    SgBignum *m1,  *m2, *base2, *a1, *a2, *y1, *y2;
    SgBignum *ONE;
    ALLOC_TEMP_BIGNUM(ONE, 1);
    ONE->elements[0] = 1;

    if (rsize < 1) {
      if (SG_BIGNUM_GET_SIGN(bm) < 0) {
	m1 = Sg_MakeBignumFromSI(-1);
      } else {
	m1 = Sg_MakeBignumFromSI(0);
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
      ? Sg_MakeBignumFromSI(0)
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
  if (BIGNUM_ZEROP(be)) {
    return (BIGNUM_ONEP(bm)) ? SG_MAKE_INT(0) : SG_MAKE_INT(1);
  }
  if (BIGNUM_ONEP(bx)) {
    return (BIGNUM_ONEP(bm)) ? SG_MAKE_INT(0) : SG_MAKE_INT(1);
  }
  if (BIGNUM_ZEROP(bx)) {
    return (BIGNUM_ONEP(bm)) ? SG_MAKE_INT(0) : SG_MAKE_INT(1);
  }
  return Sg_NormalizeBignum(bignum_mod_expt(bx, be, bm));
}

SgObject Sg_BignumModInverse(SgBignum *bx, SgBignum *bm)
{
  return Sg_NormalizeBignum(bignum_mod_inverse(bx, bm));
}

#if 0
static void compute_buffer_size(int e, int *rr, int base_size, int *br)
{
  int result_size = 1;		/* it's always start with 1 */
  while (e != 0) {
    if (e & 1) {
      result_size += base_size;
    }
    e >>= 1;
    if (e) {
      base_size <<= 1;
    }
  }
  *rr = result_size;
  *br = base_size;
}

static SgBignum * bignum_expt(SgBignum *b, int exponent)
{
  SgBignum *br;
  ulong *base, *result, *base_prod, *result_prod;
  int base_size, result_size, i, sign, b_size = SG_BIGNUM_GET_COUNT(b);

  sign = (SG_BIGNUM_GET_SIGN(b) < 0 && (exponent & 1)) ? -1 : 1;
  compute_buffer_size(exponent, &result_size, b_size, &base_size);
  /* set up result */
  br = make_bignum(result_size);
  result_prod = br->elements;
  ALLOC_TEMP_BUFFER_REC(result, ulong, result_size);
  result[0] = 1;
  /* set up base */
  ALLOC_TEMP_BUFFER_REC(base, ulong, base_size);
  ALLOC_TEMP_BUFFER_REC(base_prod, ulong, base_size);

  for (i = 0; i < b_size; i++) {
    base[i] = b->elements[i];
  }
  /* reset computed size */
  result_size = 1;
  base_size = b_size;
  while (exponent != 0) {
    if ((exponent & 1)) {
      multiply_to_len(result, result_size, base, base_size, result_prod);
      result_size += base_size;
      for (i = 0; i < result_size; i++) {
	result[i] = result_prod[i];
      }
    }
    exponent >>= 1;
    if (exponent) {
      ulong *tmp;
      square_to_len(base, base_size, base_prod);
      /* swich */
      tmp = base;
      base = base_prod;
      base_prod = tmp;
      base_size <<= 1;
    }
  }
  SG_BIGNUM_SET_SIGN(br, sign);
  return br;
}
#else
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
static int compute_lr_binary_size(long j, long m, int len)
{
  int r = len;
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
    int i;						\
    for (i = 0; i < size; i++) (src)[i] = (dst)[i];	\
  } while (0)

static SgBignum * leftright_binray_expt(SgBignum *b, long e)
{
  SgBignum *y;
  long m = e, j = 1 + bfffo(m);
  int size;
  int ylen = SG_BIGNUM_GET_COUNT(b);
  ulong *ye, *prod, *t;
  m <<= j;
  j = WORD_BITS-j;

  /* setup buffer */
  size = compute_lr_binary_size(j, m, SG_BIGNUM_GET_COUNT(b));
  y = make_bignum(size);
  bignum_copy(y, b);
  ye = y->elements;
  ALLOC_TEMP_BUFFER_REC(prod, ulong, size);
  for (; j; m <<= 1, j--) {
    /* y = bignum_mul(y, y); */
    square_to_len(ye, ylen, prod);
    ylen <<= 1;
    t = ye;
    ye = prod;
    prod = t;
    if (m < 0) {
      /* y = bignum_normalize(bignum_mul(y, b)); */
      multiply_to_len(ye, ylen, b->elements, SG_BIGNUM_GET_COUNT(b), prod);
      ylen += SG_BIGNUM_GET_COUNT(b);
      t = ye;
      ye = prod;
      prod = t;
    }
  }
  if (y->elements != ye) {
    copy_element(y->elements, ye, ylen);
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

static void setup_square(ulong *dst, ulong *src, int slen)
{
  int size = slen<<1;
  ulong *prod;
  ALLOC_TEMP_BUFFER_REC(prod, ulong, size);
  square_to_len(src, slen, prod);
  copy_element(dst, prod, size);
}

/* I'm not sure if I can write this as a static function since
   it's using alloca to setup table. */
#define setup_table(table, ltable, u, b)				\
  do {									\
    ulong *x2;								\
    int i, x2len =SG_BIGNUM_GET_COUNT(b)<<1;				\
    table[0] = b->elements;						\
    ltable[0] = SG_BIGNUM_GET_COUNT(b);					\
    ALLOC_TEMP_BUFFER_REC(x2, ulong, x2len);				\
    /* x2 = bignum_mul(b, b) */						\
    setup_square(x2, b->elements, SG_BIGNUM_GET_COUNT(b));		\
    for (i = 1; i < u; i++) {						\
      /* table[i] = bignum_mul(table[i-1], x2); */			\
      int size = ltable[i-1] + x2len;					\
      ALLOC_TEMP_BUFFER_REC(table[i], ulong, size);			\
      multiply_to_len(table[i-1], ltable[i-1], x2, x2len, table[i]);	\
      ltable[i] = size;							\
    }									\
  } while (0)

/* compute result size of sliding window */
static int compute_sw_size(long l, long e, long n, int *ltable)
{
  int z = 0;
  int tw;
  long w, v, i;
  while (l >= 0) {
    if (e > l+1) e = l + 1;
    w = (n >>(l+1-e)) & ((1UL<<2)-1);
    v = vals(w);
    l -= e;
    tw = ltable[w>>(v+1)];
    if (z) {
      for (i = 1; i <= e-v; i++) z <<= 1;
      z += tw; 
    } else {
      z = tw;
    }
    while (l >= 0) {
      if (n & (1UL<<l)) break;
      z <<= 1;
      l--;
    }
  }
  return z;
}

static SgBignum * sliding_window_expt(SgBignum *b, long n, long e)
{
  /* max must be 4 */
  ulong *table[1UL<<(3-1)], *tw, *z = NULL, *prod1, *prod2;
  int    ltable[1UL<<(3-1)], zsize, zlen = 0, twlen;
  long i, l = (WORD_BITS-1) - (long)bfffo(n), u = 1UL<<(e-1);
  long w, v;
  SgBignum *r;

  setup_table(table, ltable, u, b);
  zsize = compute_sw_size(l, e, n, ltable);
  r = make_bignum(zsize);
  SG_BIGNUM_SET_SIGN(r, 1);

  /* ALLOC_TEMP_BUFFER_REC(prod1, ulong, zsize); */
  prod1 = r->elements;
  ALLOC_TEMP_BUFFER_REC(prod2, ulong, zsize);

  while (l >= 0) {
    int index;
    if (e > l+1) e = l + 1;
    w = (n >>(l+1-e)) & ((1UL<<2)-1);
    v = vals(w);
    l -= e;
    index = (int)(w>>(v+1));
    tw = table[index];
    twlen = ltable[index];
    if (z) {
      ulong *t;
      ulong len = e-v;
      for (i = 1; i <= len; i++) {
	/* z = bignum_mul(z, z); */
	square_to_len(z, zlen, prod1);
	zlen <<= 1;
	/* swap buffer */
	t = z;
	z = prod1;
	prod1 = t;
      }
      /* z = bignum_mul(z, tw); */
      multiply_to_len(z, zlen, tw, twlen, prod1);
      zlen += twlen;
      /* swap buffer */
      t = z;
      z = prod1;
      prod1 = t;
    } else {
      /* z = tw; */
      /* setup buffer */
      copy_element(prod1, tw, twlen);
      z = prod1;
      prod1 = prod2;
      zlen = twlen;
    }
    while (l >= 0) {
      ulong *t;
      if (n & (1UL<<l)) break;
      /* z = bignum_mul(z, z); */
      square_to_len(z, zlen, prod1);
      zlen <<= 1;
      /* swap buffer */
      t = z;
      z = prod1;
      prod1 = t;
      l--;
    }
  }
  /* i'm not sure which would the proper one so check */
  if (r->elements != z)
    copy_element(r->elements, z, zsize);
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
#endif


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
}


/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
