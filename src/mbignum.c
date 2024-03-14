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

m_bignum_t * make_mbignum(long size)
{
  m_bignum_t *r = SG_NEW_ATOMIC2(m_bignum_t *, mbignum_alloc_size(size));
  if (size == 0) {
    mbignum_zero(r);
  } else {
    mbignum_init(r, size);
  }
  return r;
}

SgBignum * mbignum_to_bignum(m_bignum_t *mb)
{
  SgBignum *bn = Sg_AllocateBignum(mb->size);
  SG_BIGNUM_SET_SIGN(bn, mb->sign);
  SG_BIGNUM_SET_COUNT(bn, mb->size);
  memcpy(bn->elements, mb->elements, sizeof(long) * mb->size);
  return bn;
}

m_bignum_t * mbignum_normalize(m_bignum_t *mbn)
{
  long i, size = mbn->buffer_size;
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

m_bignum_t * mbignum_mul(m_bignum_t *mbr, m_bignum_t *mba, m_bignum_t *mbb)
{
  mbr->sign = mba->sign * mbb->sign;
  /* for performance, we don't check the size */
  mp_mul(mbr->elements, mbr->buffer_size,
	 mba->elements, mba->size,
	 mbb->elements, mbb->size);
  return mbr;
}

static m_bignum_t * mbignum_2scmpl(m_bignum_t *mbn)
{
  mp_2scmpl(mbn->elements, mbn->size);
  return mbn;
}

static inline long mbignum_safe_size_for_add(m_bignum_t *x, m_bignum_t *y)
{
  return mp_safe_size_for_add(x->elements, x->size, y->elements, y->size);
}

static m_bignum_t * mbignum_add_int(m_bignum_t *mbr,
				    m_bignum_t *mba,
				    m_bignum_t *mbb)
{
  if (mba->size == 0) {
    if (mbb->size == 0) {
      mbignum_zero(mbr);
      return mbr;
    }
    memcpy(mbr->elements, mbb->elements, sizeof(long) * mbb->size);
    mbr->sign = mbb->sign;
  } else if (mbb->size == 0) {
    memcpy(mbr->elements, mba->elements, sizeof(long) * mba->size);
    mbr->sign = mba->sign;
  } else {
    if (mba->size < mbb->size) {
      m_bignum_t *t = mba;
      mba = mbb; mbb = t;
    }
    mp_add(mbr->elements, mbr->buffer_size,
	   mba->elements, mba->size,
	   mbb->elements, mbb->size);
  }
  return mbr;
}

static m_bignum_t * mbignum_sub_int(m_bignum_t *mbr,
				    m_bignum_t *mba,
				    m_bignum_t *mbb)
{
  if (mba->size == 0) {
    if (mbb->size == 0) {
      mbignum_zero(mbr);
      return mbr;
    }
    memcpy(mbr->elements, mbb->elements, sizeof(long) * mbb->size);
    mbr->sign = mbb->sign;
  } else if (mbb->size == 0) {
    memcpy(mbr->elements, mba->elements, sizeof(long) * mba->size);
    mbr->sign = mba->sign;
  } else {
    int flip = FALSE, c;
    if (mba->size < mbb->size) {
      m_bignum_t *t = mba;
      mba = mbb; mbb = t;
      flip = TRUE;
    }
    c = mp_sub(mbr->elements, mbr->buffer_size,
	       mba->elements, mba->size,
	       mbb->elements, mbb->size);
    if (flip || c) {
      if (c) mbignum_2scmpl(mbr);
    }
  }
  return mbr;
}

m_bignum_t * mbignum_add(m_bignum_t *mbr, m_bignum_t *mba, m_bignum_t *mbb)
{
  mbr->sign = mba->sign;
  mbr->size = mbignum_safe_size_for_add(mba, mbb);
  if (mba->sign == mbb->sign) {
    mbignum_add_int(mbr, mba, mbb);
  } else {
    mbignum_sub_int(mbr, mba, mbb);
  }
  return mbr;
}


void mbignum_gdiv(m_bignum_t *dividend, m_bignum_t *divisor,
		  m_bignum_t *quotient, m_bignum_t *remainder)
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

m_bignum_t * mbignum_mod(m_bignum_t *mba, m_bignum_t *mbb,
			 m_bignum_t *mbq, m_bignum_t *mbr)
{
  mbignum_gdiv(mba, mbb, mbq, mbr);
  mbignum_normalize(mbr);
  mbignum_normalize(mbq);
  if (!mbignum_zerop(mbr) && (mbr->sign * mbb->sign) < 0) {
    return mbignum_normalize(mbignum_add(mbr, mbr, mbb));
  }
  return mbr;
}
