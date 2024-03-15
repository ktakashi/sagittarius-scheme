/* mbignum.h                                        -*- mode:c; coding:utf-8; -*-
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

#ifndef SAGITTARIUS_PRIVATE_MBIGNUM_H_
#define SAGITTARIUS_PRIVATE_MBIGNUM_H_

#include "sagittariusdefs.h"

typedef struct mbignum_rec
{
  int sign;
  long buffer_size;
  long size;
  unsigned long elements[1];
} mbignum_t;

typedef enum {
  MBIGNUM_SUM,
  MBIGNUM_PRODUCT,
  MBIGNUM_QUOTIENT,
  MBIGNUM_REMAINDER
} mbignum_op_t;

#define mbignum_zerop(mbn) ((mbn)->sign == 0 && (mbn)->size == 0)
#define mbignum_zero(mbn)  \
  do {			   \
    (mbn)->sign = 0;	   \
    (mbn)->size = 0;	   \
  } while (0)
#define mbignum_one(mbn)	\
  do {				\
    (mbn)->sign = 1;		\
    (mbn)->size = 1;		\
    (mbn)->elements[0] = 1;	\
  } while (0)

#define mbignum_alloc_size(size) sizeof(mbignum_t) + ((size)-1)*sizeof(long)
#define mbignum_init(var, size_)	\
  do {					\
    (var)->buffer_size = (size_);	\
    (var)->size = (size_);		\
    (var)->sign = 1;			\
  } while (0)

#define mbignum_reset(var, size_)	\
  do {					\
    (var)->size = (size_);		\
    (var)->sign = 1;			\
  } while (0)

#ifdef HAVE_ALLOCA
#define alloc_temp_mbignum(var, size_)				\
  do {								\
    (var) = (mbignum_t *)alloca(mbignum_alloc_size(size_));	\
    mbignum_init(var, size_);					\
    memset((var)->elements, 0, (size_)*(sizeof(long)));		\
  } while (0)
#else
#define alloc_temp_mbignum(var, size)				\
  do {								\
    (var) = make_mbignum(size);					\
  } while (0)
#endif

#define copy_from_bignum(mb, bn)					\
  do {									\
    (mb)->size = (bn)->size;						\
    (mb)->sign = (bn)->sign;						\
    memcpy((mb)->elements, (bn)->elements, sizeof(long)*(bn)->size);	\
  } while (0)

SG_CDECL_BEGIN

SG_EXTERN mbignum_t * make_mbignum(long size);
SG_EXTERN mbignum_t * number_to_mbignum(SgObject n, long size);
SG_EXTERN SgBignum  * mbignum_to_bignum(mbignum_t *mb);
SG_EXTERN SgObject    mbignum_to_number(mbignum_t *mb);
SG_EXTERN mbignum_t * mbignum_normalize(mbignum_t *mbn);

SG_EXTERN long        mbignum_left_shift_space(SgObject n, long amount);
SG_EXTERN long        mbignum_safe_size(mbignum_op_t op, SgObject x, SgObject y);

/* arithmetic operations */
SG_EXTERN mbignum_t * mbignum_mul_si(mbignum_t *r, mbignum_t *x, long y);
SG_EXTERN mbignum_t * mbignum_mul(mbignum_t *r, mbignum_t *x, mbignum_t *y);
SG_EXTERN mbignum_t * mbignum_add_si(mbignum_t *r, mbignum_t *x, long y);
SG_EXTERN mbignum_t * mbignum_add(mbignum_t *r, mbignum_t *x, mbignum_t *y);
SG_EXTERN mbignum_t * mbignum_sub(mbignum_t *r, mbignum_t *x, mbignum_t *y);
SG_EXTERN void        mbignum_gdiv(mbignum_t *de, mbignum_t *ds,
				   mbignum_t *qs, mbignum_t *r);
SG_EXTERN mbignum_t * mbignum_mod(mbignum_t *x, mbignum_t *y,
				  mbignum_t *q, mbignum_t *r);

SG_EXTERN int         mbignum_bit_setp(mbignum_t *x, long i);
SG_EXTERN int         mbignum_bit_size(mbignum_t *x);

SG_EXTERN mbignum_t * mbignum_ash(mbignum_t *r, mbignum_t *x, long amount);
SG_EXTERN mbignum_t * mbignum_lshift(mbignum_t *r, mbignum_t *x, long amount);
SG_EXTERN mbignum_t * mbignum_rshift(mbignum_t *r, mbignum_t *x, long amount);

SG_EXTERN mbignum_t * mbignum_logand(mbignum_t *r, mbignum_t *x, mbignum_t *y);
SG_EXTERN mbignum_t * mbignum_logior(mbignum_t *r, mbignum_t *x, mbignum_t *y);
SG_EXTERN mbignum_t * mbignum_logxor(mbignum_t *r, mbignum_t *x, mbignum_t *y);

SG_CDECL_END

#endif	/* SAGITTARIUS_PRIVATE_MBIGNUM_H_ */
