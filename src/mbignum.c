/* mbignum.c                                        -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2024  Takashi Kato <ktakashi@ymail.com>
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

#include "sagittarius/private/mbignum.h"

#define copy_mbignum(a, b)						\
  do {									\
    (a)->size = (b)->size;						\
    (a)->sign = (b)->sign;						\
    memcpy((a)->elements, (b)->elements, sizeof(long) * (b)->size);	\
  } while (0)

/* These functions don't raise error, but return NULL in case of an error */

mbignum_t * make_mbignum(long size)
{
  mbignum_t *r = SG_NEW_ATOMIC2(mbignum_t *, mbignum_alloc_size(size));
  if (size == 0) {
    mbignum_zero(r);
  } else {
    mbignum_init(r, size);
  }
  return r;
}

mbignum_t * number_to_mbignum(SgObject n, long size)
{
  mbignum_t *r;
  long real_size;
  if (!SG_EXACT_INTP(n)) return NULL;
  if (SG_INTP(n)) {
    long v = SG_INT_VALUE(n);
    real_size = max(size, 1);
    r = make_mbignum(real_size);
    if (v == 0) {
      mbignum_zero(r);
    } else {
      r->size = 1;
      if (v == LONG_MIN) {
	r->elements[0] = (unsigned long)LONG_MAX + 1;
	r->sign = -1;
      } else if (v < 0) {
	r->elements[0] = -v;
	r->sign = -1;
      } else {
	r->elements[0] = v;
	r->sign = 1;
      }
    }
  } else {
    real_size = max(SG_BIGNUM(n)->size, size);
    r = make_mbignum(real_size);
    r->sign = SG_BIGNUM(n)->sign;
    r->size = SG_BIGNUM(n)->size;
    memcpy(r->elements, SG_BIGNUM(n)->elements, sizeof(long) * r->size);
  }
  return r;
}

SgBignum * mbignum_to_bignum(mbignum_t *mb)
{
  SgBignum *bn = Sg_AllocateBignum(mb->size);
  SG_BIGNUM_SET_SIGN(bn, mb->sign);
  SG_BIGNUM_SET_COUNT(bn, mb->size);
  memcpy(bn->elements, mb->elements, sizeof(long) * mb->size);
  return bn;
}

SgObject mbignum_to_number(mbignum_t *mb)
{
  return Sg_NormalizeBignum(mbignum_to_bignum(mb));
}

mbignum_t * mbignum_normalize(mbignum_t *mbn)
{
  long i, size = mbn->size;
  for (i = size - 1; i > 0; i--) {
    if (mbn->elements[i] == 0) size--;
    else break;
  }
  if (i <= 0) {
    if (size == 1 && mbn->elements[0] == 0) {
      mbignum_zero(mbn);
      return mbn;
    }
  }
  mbn->size = size;
  return mbn;
}

long mbignum_left_shift_space(SgObject n, long amount)
{
#define left_shift_space(size, amount)			\
  (int)((size) + ((amount) + WORD_BITS -1)/WORD_BITS) 

  if (SG_INTP(n)) {
    return left_shift_space(1, amount);
  } else if (SG_BIGNUMP(n)) {
    return left_shift_space(SG_BIGNUM(n)->size, amount);
  }
#undef left_shift_space
  return -1;
}

long mbignum_safe_size(mbignum_op_t op, SgObject x, SgObject y)
{
  if (!(SG_EXACT_INTP(x) && SG_EXACT_INTP(y))) return -1;
  
  if (SG_INTP(x)) {
    if (op == MBIGNUM_QUOTIENT) return 1; /* can't be bigger than x */
    
    if (SG_INTP(y)) {
      if (op == MBIGNUM_REMAINDER) return 1; /* can't be bigger than y */
      return 2;
    }
    return SG_BIGNUM(y)->size + 1;
  } else {
    /* can't be bigger than x */
    if (op == MBIGNUM_QUOTIENT) return SG_BIGNUM(x)->size;
    
    if (SG_INTP(y)) {
      if (op == MBIGNUM_REMAINDER) return 1; /* can't be bigger than y */
      return SG_BIGNUM(x)->size + 1;
    }
    if (op == MBIGNUM_PRODUCT) return SG_BIGNUM(x)->size + SG_BIGNUM(y)->size;
    /* sum */
    return max(SG_BIGNUM(x)->size, SG_BIGNUM(y)->size) + 1;
  }
}

mbignum_t * mbignum_mul_si(mbignum_t *r, mbignum_t *x, long y)
{
  if (y == 1L) {
    copy_mbignum(r, x);
  } else if (y == 0) {
    mbignum_zero(r);
  } else if (y == -1L) {
    copy_mbignum(r, x);
    r->sign = -1;
  } else {
    ulong yabs = (y < 0) ? -y: y;
    r->sign = x->sign;
    r->size = x->size + 1;
    mp_mul_ul(r->elements, r->buffer_size, x->elements, x->size, yabs);
    if (y < 0) r->sign = -r->sign;
  }
  return mbignum_normalize(r);
}

mbignum_t * mbignum_mul(mbignum_t *mbr, mbignum_t *mba, mbignum_t *mbb)
{
  if (mbignum_zerop(mba) || mbignum_zerop(mbb)) {
    mbignum_zero(mbr);
  } else {
    mbr->size = mba->size + mbb->size;
    mbr->sign = mba->sign * mbb->sign;
    /* for performance, we don't check the size */
    mp_mul(mbr->elements, mbr->buffer_size,
	   mba->elements, mba->size,
	   mbb->elements, mbb->size);
  }
  return mbignum_normalize(mbr);
}

static mbignum_t * mbignum_2scmpl(mbignum_t *mbn)
{
  mp_2scmpl(mbn->elements, mbn->size);
  return mbn;
}

static inline long mbignum_safe_size_for_add(mbignum_t *x, mbignum_t *y)
{
  return mp_safe_size_for_add(x->elements, x->size, y->elements, y->size);
}

static mbignum_t * mbignum_add_int(mbignum_t *r, mbignum_t *a, mbignum_t *b)
{
  if (a->size == 0) {
    if (b->size == 0) {
      mbignum_zero(r);
      return r;
    }
    memcpy(r->elements, b->elements, sizeof(long) * b->size);
    r->sign = b->sign;
  } else if (b->size == 0) {
    memcpy(r->elements, a->elements, sizeof(long) * a->size);
    r->sign = a->sign;
  } else {
    if (a->size < b->size) {
      mbignum_t *t = a;
      a = b; b = t;
    }
    mp_add(r->elements, r->buffer_size,
	   a->elements, a->size,
	   b->elements, b->size);
  }
  return r;
}

static mbignum_t * mbignum_sub_int(mbignum_t *r, mbignum_t *a, mbignum_t *b)
{
  if (a->size == 0) {
    if (b->size == 0) {
      mbignum_zero(r);
      return r;
    }
    memcpy(r->elements, b->elements, sizeof(long) * b->size);
    r->sign = b->sign;
  } else if (b->size == 0) {
    memcpy(r->elements, a->elements, sizeof(long) * a->size);
    r->sign = a->sign;
  } else {
    int flip = FALSE, c;
    if (a->size < b->size) {
      mbignum_t *t = a;
      a = b; b = t;
      flip = TRUE;
    }
    c = mp_sub(r->elements, r->buffer_size,
	       a->elements, a->size,
	       b->elements, b->size);
    if (flip || c) {
      if (c) mbignum_2scmpl(r);
      r->sign = -r->sign;
    }
  }
  return r;
}

mbignum_t * mbignum_add(mbignum_t *mbr, mbignum_t *mba, mbignum_t *mbb)
{
  mbr->sign = mba->sign;
  mbr->size = mbignum_safe_size_for_add(mba, mbb);
  if (mba->sign == mbb->sign) {
    mbignum_add_int(mbr, mba, mbb);
  } else {
    mbignum_sub_int(mbr, mba, mbb);
  }
  return mbignum_normalize(mbr);
}

/* This assume abs(x) > abs(y) */
mbignum_t * mbignum_add_si(mbignum_t *r, mbignum_t *x, long y)
{
  ulong yabs;
  int ysign = ((y < 0)? -1: 1);
  if (y == 0) {
    if (r != x) copy_mbignum(r, x);
    return r;
  }
  yabs = ((y < 0) ? -y : y);
  r->sign = x->sign;
  r->size = x->size + 1;
  if (x->sign == ysign) {
    mp_add_ul(r->elements, r->buffer_size, x->elements, x->size, yabs);
  } else {
    mp_sub_ul(r->elements, r->buffer_size, x->elements, x->size, yabs);
  }
  return mbignum_normalize(r);
}

mbignum_t * mbignum_sub(mbignum_t *r, mbignum_t *x, mbignum_t *y)
{
  r->sign = x->sign;
  r->size = mbignum_safe_size_for_add(x, y);
  if (x->sign == y->sign) {
    mbignum_sub_int(r, x, y);
  } else {
    mbignum_add_int(r, x, y);
    if (mbignum_zerop(x)) {
      r->sign = -r->sign;
    }
  }
  return mbignum_normalize(r);
}

void mbignum_gdiv(mbignum_t *dividend, mbignum_t *divisor,
		  mbignum_t *quotient, mbignum_t *remainder)
{
  ulong rsize;
  ulong  *q = NULL, *r = NULL;
  ulong qs = 0, rs = 0;
  if (remainder) {
    rs = remainder->buffer_size;
    r = remainder->elements;
  }
  if (quotient) {
    qs = quotient->buffer_size;
    q = quotient->elements;
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

mbignum_t * mbignum_mod(mbignum_t *mba, mbignum_t *mbb,
			mbignum_t *mbq, mbignum_t *mbr)
{
  mbignum_gdiv(mba, mbb, mbq, mbr);
  mbignum_normalize(mbr);
  mbignum_normalize(mbq);
  if (!mbignum_zerop(mbr) && (mbr->sign * mbb->sign) < 0) {
    return mbignum_normalize(mbignum_add(mbr, mbr, mbb));
  }
  return mbr;
}

int mbignum_bit_setp(mbignum_t *x, long i)
{
  long p = i >> SHIFT_MAGIC;
  ulong v;
  if (p >= (long)x->size) {
    return FALSE;
  }
  v = x->elements[p];
  if (p) {
    i %= WORD_BITS;
  }
  return (v & (1L << i)) != 0;
}

int mbignum_bit_size(mbignum_t *x)
{
  if (mbignum_zerop(x)) return 0;
  return mp_bit_size(x->elements, x->size);
}

mbignum_t * mbignum_ash(mbignum_t *r, mbignum_t *x, long amount)
{
  if (amount == 0) {
    if (r != x) {
      copy_mbignum(r, x);
    }
    return mbignum_normalize(r);
  } else if (amount > 0) return mbignum_lshift(r, x, amount);
  else return mbignum_rshift(r, x, amount);
}

mbignum_t * mbignum_lshift(mbignum_t *r, mbignum_t *x, long amount)
{
  ulong size = mp_lshift(r->elements, r->buffer_size,
			 x->elements, x->size,
			 amount);
  r->sign = x->sign;
  r->size = size;
  return mbignum_normalize(r);
}

mbignum_t * mbignum_rshift(mbignum_t *r, mbignum_t *x, long amount)
{
  ulong size = mp_rshift(r->elements, r->buffer_size,
			 x->elements, x->size,
			 amount);
  r->size = size;
  r->sign = x->sign;
  return mbignum_normalize(r);
}

#define DEF_MBIGNUM_LOG_OP(name, op)					\
  static mbignum_t* name(mbignum_t *z, mbignum_t *x, mbignum_t *y,	\
			 int x2sc, int y2sc)				\
  {									\
    int i;								\
    long xs = (x)->size;						\
    long ys = (y)->size;						\
    long zs = (z)->size;						\
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

DEF_MBIGNUM_LOG_OP(mbignum_and, &)

mbignum_t * mbignum_logand(mbignum_t *r, mbignum_t *x, mbignum_t *y)
{
  if (mbignum_zerop(x) || mbignum_zerop(y)) {
    mbignum_zero(r);
    return r;
  }
  
  if (x->sign > 0) {
    if (y->sign > 0) {
      r->size = min(x->size, y->size);
      mbignum_and(r, x, y, FALSE, FALSE);
    } else {
      mbignum_t *yy; alloc_temp_mbignum(yy, y->size);
      copy_mbignum(yy, y);
      mbignum_2scmpl(yy);
      mbignum_and(r, x, yy, FALSE, TRUE);
      r->size = x->size;
    }
  } else {
    if (y->sign > 0) {
      mbignum_t *xx; alloc_temp_mbignum(xx, x->size);
      copy_mbignum(xx, x);
      mbignum_2scmpl(xx);
      mbignum_and(r, xx, y, TRUE, FALSE);
      r->size = y->size;
    } else {
      mbignum_t *xx, *yy;
      alloc_temp_mbignum(xx, x->size);
      copy_mbignum(xx, x);
      mbignum_2scmpl(xx);
      alloc_temp_mbignum(yy, y->size);
      copy_mbignum(yy, y);
      mbignum_2scmpl(yy);
      mbignum_and(r, xx, yy, TRUE, TRUE);
      mbignum_2scmpl(r);
      r->size = max(x->size, y->size);
    }
  }
  return mbignum_normalize(r);
}

DEF_MBIGNUM_LOG_OP(mbignum_xor, ^)

mbignum_t * mbignum_logxor(mbignum_t *r, mbignum_t *x, mbignum_t *y)
{
  if (mbignum_zerop(x) || mbignum_zerop(y)) {
    if (x->sign) {
      copy_mbignum(r, x);
      return r;
    }
    if (y->sign) {
      copy_mbignum(r, y);
      return r;
    }
    mbignum_zero(r);
    return r;
  }
  r->size = max(x->size, y->size);
  if (x->sign > 0) {
    if (y->sign > 0) {
      mbignum_xor(r, x, y, FALSE, FALSE);
    } else {
      mbignum_t *yy; alloc_temp_mbignum(yy, y->size);
      copy_mbignum(yy, y);
      mbignum_2scmpl(yy);
      mbignum_xor(r, x, yy, FALSE, TRUE);
      r->sign = -1;
      mbignum_2scmpl(r);
    }
  } else {
    if (y->sign > 0) {
      mbignum_t *xx; alloc_temp_mbignum(xx, x->size);
      copy_mbignum(xx, x);
      mbignum_2scmpl(xx);
      mbignum_xor(r, xx, y, TRUE, FALSE);
      r->sign = -1;
      mbignum_2scmpl(r);
    } else {
      mbignum_t *xx, *yy;
      alloc_temp_mbignum(xx, x->size);
      copy_mbignum(xx, x);
      mbignum_2scmpl(xx);
      alloc_temp_mbignum(yy, y->size);
      copy_mbignum(yy, y);
      mbignum_2scmpl(yy);
      mbignum_xor(r, xx, yy, TRUE, TRUE);
    }
  }
  return mbignum_normalize(r);
}
