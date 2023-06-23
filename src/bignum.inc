/* bignum.inc                                      -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2017  Takashi Kato <ktakashi@ymail.com>
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

/* this file must be included by bignum.c

   Bignum uses flexible array member feature to make memory allocation
   atomic (not pointer contained). This works pretty good with GC
   performance however it makes bignum operation a bit inconvenient.
   For example, we can't split bignum elements simply adding offset
   but need to allocate whole bignum. 
   
   Now we want to do those array offset things for memory space
   efficiency. So separating all bignum operations not to use bignum
   directly but use long array.
 */

/* things must be here */

/* in case of platform whose long size if 8 but no 128 bit storage.
   we put this. however, it's not tested at all. so most likely doesn't
    work at all.
 */
#define USE_DLONG
#include "number.inc"

static inline ulong* mp_2scmpl(ulong *p, ulong s)
{
  ulong i, c;
  for (i = 0, c = 1; i < s; i++) {
    ulong x = ~p[i];
    UADD(p[i], c, x, 0);
  }
  return p;
}

static long mp_bit_size(ulong *x, ulong s)
{
  long last = s - 1;
  long bitsize = WORD_BITS * last;
  if (s == 0) return 0;
  return bitsize + WORD_BITS - nlz(x[last]);
}


/* Addition */

/* helper */
static ulong mp_safe_size_for_add(ulong *xp, ulong xsize, 
				  ulong *yp, ulong ysize)
{
  if (xsize > ysize) {
    if (xp[xsize - 1] == SG_ULONG_MAX) return xsize + 1;
    else return xsize;
  } else if (xsize < ysize) {
    if (yp[ysize - 1] == SG_ULONG_MAX) return ysize + 1;
    else return ysize;
  } else {
    return xsize + 1;
  }
}

/* 
   xp + yp -> rp
   assume both num1 and num2 have the same length and rp has 
   at least 'len' length
   returns carry

   NB: to make better performance, this part should be written in
       assembly. but for now it's ok
 */
static long mp_add_n(ulong *rp, ulong *xp, ulong *yp, long len)
{
  long i;
#ifdef USE_DLONG
  udlong t = (udlong)xp[0] + yp[0];
  rp[0] = (ulong)t;
  for (i = 1; i < len; i++) {
    t = (udlong)xp[i] + (udlong)yp[i] + (ulong)(t >> WORD_BITS);
    rp[i] = (ulong)t;
  }
  return (long)(t >> WORD_BITS);
#else
  long c = 0;
  for (i = 0; i < len; i++) {
    UADD(rp[i], c, xp[i], yp[i]);
  }
  return c;
#endif
}

/* 
   xp + yp -> rp (xsize >= ysize)
   returns carry (0 or 1)
*/
static int mp_add(ulong *rp, ulong rsize, 
		  ulong *xp, ulong xsize,
		  ulong *yp, ulong ysize)
{
  long i;
  long c;
  if (xsize < ysize) return mp_add(rp, rsize, yp, ysize, xp, xsize);
  
  c = mp_add_n(rp, xp, yp, ysize);
  
  /* do the rest */
  for (i = ysize; i < xsize && c; i++) {
    rp[i] = xp[i] + 1;
    c = (rp[i] == 0);
  }
  /* copy remainder */
  for (; i < xsize; i++) {
    rp[i] = xp[i];
  }
  if (c && i < rsize) rp[i] = 1L;
  return (int)c;
}

/*
  xp + y -> rp (xzise > 1)
  returns carry (0 or 1)
*/
/* mp_add_one doesn't add carry to the end */
static int mp_add_one(ulong *rp, ulong rsize, 
		      ulong *xp, ulong xsize,
		      ulong y)
{
#ifdef USE_DLONG
  udlong t = (udlong)xp[0] + y;
  ulong i;
  rp[0] = (ulong)t;
  /* copy if this is not self destructive operation */
  if (rp != xp) for (i = 1; i < xsize; i++) rp[i] = xp[i];

  if ((t >> WORD_BITS) == 0) return 0;
  for (i = 1; i < xsize; i++) {
    if (++rp[i] != 0) return 0;
  }
  return 1;
#else
  long c = 0;
  ulong i;
  
  UADD(rp[0], c, xp[0], y);
  for (i = 1; i < xsize; i++) {
    UADD(rp[i], c, xp[i], 0);
  }
  return c;
#endif  
}
static int mp_add_ul(ulong *rp, ulong rsize, 
		     ulong *xp, ulong xsize,
		     ulong y)
{
  int c = mp_add_one(rp, rsize, xp, xsize, y);
  if (c) rp[rsize-1] = c;
  return c;
}

/* Subtraction */

/* 
   xp - yp -> rp
   assume both num1 and num2 have the same length and rp has 
   at least 'len' length
   returns carry

   NB: to make better performance, this part should be written in
       assembly. but for now it's ok
 */
static long mp_sub_n(ulong *rp, ulong *xp, ulong *yp, long len)
{
  long i;
#ifdef USE_DLONG
  udlong t = (udlong)xp[0] - yp[0];
  rp[0] = (ulong)t;
  for (i = 1; i < len; i++) {
    t = (udlong)xp[i] - (udlong)yp[i] - (ulong)-(t >> WORD_BITS);
    rp[i] = (ulong)t;
  }
  return (long)(t >> WORD_BITS);
#else
  int c = 0;
  for (i = 0; i < len; i++) {
    USUB(rp[i], c, xp[i], yp[i]);
  }
  return c;
#endif
}

/* xp - yp -> rp (xsize >= ysize)
   returns carry (0 or 1)
*/
static int mp_sub(ulong *rp, ulong rsize, 
		  ulong *xp, ulong xsize,
		  ulong *yp, ulong ysize)
{
  long i;
  long c = mp_sub_n(rp, xp, yp, ysize);
  /* do the rest */
  for (i = ysize; i < xsize && c; i++) {
    rp[i] = xp[i] - 1;
    c = (rp[i] == (ulong)-1L) ? -1 : 0;
  }
  for (; i < xsize; i++) {
    rp[i] = xp[i];
  }
  /* padding carry */
  for (; i < rsize; i++) {
    rp[i] = (ulong)c;
  }
  return (int)c;
}

/* xp - y -> rp (xzise > 1)
   returns carry (0 or 1)
*/   
static int mp_sub_ul(ulong *rp, ulong rsize, 
		     ulong *xp, ulong xsize,
		     ulong y)
{
  int c = 0;
  ulong i;

  USUB(rp[0], c, xp[0], y);
  for (i = 1; i < xsize; i++) {
    USUB(rp[i], c, xp[i], 0);
  }
  rp[rsize-1] = c;
  return c;
}

/* shift operations */
static void mp_lshift(ulong *rp, ulong rsize,
		      ulong *xp, ulong xsize,
		      long amount)
{
  long nwords, nbits;
  long i;

  /* simple check */
  if (xsize == 0) return;	/* 0<<n is always 0 */
  
  nwords = amount / WORD_BITS;
  nbits = amount % WORD_BITS;
  if (nbits == 0) {
    for (i = xsize - 1; i >= 0; i--) {
      if (rsize > i + nwords)
	rp[i + nwords] = xp[i];
    }
    for (i = nwords - 1; i >= 0; i--) rp[i] = 0;
  } else {
    if (rsize > xsize + nwords) {
      rp[xsize+nwords] = xp[xsize-1]>>(WORD_BITS-nbits);
    }
    for (i = xsize - 1; i > 0; i--) {
      ulong x = (xp[i]<<nbits)|(xp[i-1]>>(WORD_BITS-nbits));
      if (rsize > i+nwords) rp[i+nwords] = x;
    }
    rp[nwords] = xp[0] << nbits;
    for (i = nwords - 1; i >= 0; i--) rp[i] = 0;
  }
}

/* returns effect size of the result 
   x >> 1
   +---+---+---+---+
   | 4 | 3 | 2 | 1 |
   +---+---+---+---+
        /   /   /
       /   /   /
   r  /   /   /
   +---+---+---+---+
   | 3 | 2 | 1 | 0 |
   +---+---+---+---+
              i-nwords
   For performance, we don't fill 0. it is callers responsibility
   to make sure the size and content if needed.
 */
static ulong mp_rshift(ulong *rp, ulong rsize,
		       ulong *xp, ulong xsize,
		       long amount)
{
  ulong nwords = amount / WORD_BITS;
  ulong nbits = amount % WORD_BITS;
  ulong i;
  if (xsize <= nwords) {
    /* TODO should we do this? */
    /* for (i = i; i < rsize; i++) rp[i] = 0; */
    rp[0] = 0;
    return 0;
  } else if (nbits == 0) {
    for (i = nwords; i < xsize; i++) {
      rp[i - nwords] = xp[i];
    }
    /* for (i -= nwords; i < rsize; i++) rp[i] = 0; */
    return xsize - nwords;
  } else {
    for (i = nwords; i < xsize - 1; i++) {
      ulong x = (xp[i+1] << (WORD_BITS - nbits))|(xp[i] >> nbits);
      rp[i - nwords] = x;
    }
    rp[i - nwords] = xp[i] >> nbits;
    /* for (i -= nwords; i < rsize; i++) rp[i] = 0; */
    return xsize - nwords;
  }
}

/* Multiplication */
/* xp * y -> rp (xsize > 1)
   assumes rp has sufficient size.
 */
static void mp_mul_ul(ulong *rp, ulong rsize,
		      ulong *xp, ulong xsize,
		      ulong y)
{
  ulong i;

#ifdef USE_DLONG
  register udlong p;

  p = (udlong)xp[0] * y;
  rp[0] = (ulong)p;
  for (i = 1; i < xsize; i++) {
    p = (udlong)xp[i] * y + (ulong)(p >> WORD_BITS);
    rp[i] = (ulong)p;
  }
  rp[i] = (ulong)(p >> WORD_BITS);

#else

  for (i = 0; i < xsize; i++) {
    ulong j;
    ulong hi, lo, r1;
    ulong x = xp[i];
    ulong c = 0;
    ulong r0 = rp[i];

    UMUL(hi, lo, x, y);
    UADD(r1, c, r0, lo);
    rp[i] = r1;

    r0 = rp[i+1];
    UADD(r1, c, r0, hi);
    rp[i+1] = r1;

    for (j = i + 2; c && j < rsize; j++) {
      r0 = rp[j];
      UADD(r1, c, r0, 0);
      rp[j] = r1;
    }
  }
#endif
}

/* forward declaration */
static void mp_mul_rec(ulong *rp, ulong rsize,
		       ulong *xp, ulong xsize,
		       ulong *yp, ulong ysize,
		       intptr_t stack_size);

static void mp_square_rec(ulong *rp, ulong rsize,
			  ulong *xp, ulong xsize,
			  intptr_t stack_size);

/*
  TODO better stack overflow detection.

  Whatever the algorithm used here uses recursive multiplication.
  Karatsuba reduces number of multiplications to 3 times so it
  makes its total order to O(N^lg(3)) or O(N^1.584) (N = number of
  elements). However this may consume C stack area as close as its
  boundary if the given N is huge.
  This introduces the same issue as Sg_BignumToString which is 
  basically fight against GC time because of too deeply used stack
  area. (memory allocation of huge array would also be an issue.)

  If there's a better way such as iterative operation of karatsuba,
  we should definitely use it.
 */
#define ALLOC_BUFFER_REC(r, type, size, stack)			\
  do {								\
    ulong s__ = (size);						\
    if ((stack) > 0 && (s__)*sizeof(type) <= (stack)) {		\
      ALLOC_TEMP_BUFFER_REC(r, type, s__);			\
      (stack) -= (s__)*sizeof(type);				\
    } else {							\
      (r) = SG_NEW_ATOMIC2(type*, sizeof(type) * s__);		\
    }								\
  } while (0)

#define ALLOC_BUFFER(r, type, size, stack)		\
  do {							\
    ALLOC_BUFFER_REC(r, type, size, stack);		\
    clear_buffer(r, size);				\
  } while (0)

#define USE_KARATSUBA
#ifdef USE_KARATSUBA
/* if the bignum length is less than this then we do 
   usual computation. */
/* according to Wikipedia karatsuba is faster when the numbers are
   bigger than 320-640 bits. 
   even though it says 320-640 bits however according to the benchmark
   it showed the boundary is around 1600 bits
*/
#define KARATSUBA_LOW_LIMIT ((1600>>3)/SIZEOF_LONG)

static long can_karatsuba(ulong xlen, ulong ylen)
{
  if (xlen < KARATSUBA_LOW_LIMIT || ylen < KARATSUBA_LOW_LIMIT) {
    return FALSE;
  } else {
    long n = max(xlen, ylen)/2;
    return n < xlen && n < ylen;
  }
}


/*
  basic karatsuba algorithm
  compute x*y 

  From Wikipedia
  xy = (b^2 + b)x1y1 - b(x1 - x0)(y1 - y0) + (b + 1)x0y0
  
  x = a*B^(n/2) + b
  y = c*B^(n/2) + d
  B = 16 (hex)
  n = max(xlen, ylen), if diff is small enough then we can always split :)
  x*y = (a*B^(n/2) + b) * (c*B^(n/2) + d)
      = B^n*ac + B^(n/2)*(ad + bc) + bd
      = 16^n*ac + 16^(n/2)*(ad + bc) + bd
 
  recursive computation
  1 ac
  2 bd
  3 (a + b)(c + d) = ac + ad + bc + bd
  now 3 - 1 - 2 = ad + bc

  Image in Scheme
  (define x #x123456789)
  (define y #x908765432)
  (define B 16)
  (define n 9)
  (define a #x12345)
  (define b #x6789)
  (define c #x90876)
  (define d #x5432)
  (let ((ac (* a c))
        (bd (* b d))
        (xx (+ (* a d) (* b c))))
    ;; 16^n * ac where n = length of b and d 
    (+ (* (expt B (* (div n 2) 2)) ac) 
       (* (expt B (div n 2)) xx)
       bd))
 */
static void karatsuba(ulong *rp, ulong rsize,
		      ulong *xp, ulong xsize,
		      ulong *yp, ulong ysize,
		      intptr_t stack_size)
{
  ulong n = (max(xsize, ysize)+1)/2, apblen, cpdlen, adbclen;
  ulong n2 = n<<1;
  ulong *a, *b, *c, *d;
  ulong *bd, *apb, *cpd, *adbc, *ac;
  ulong alen, clen, aclen;

  /* split given argument into 2 parts
     it's little endian so the last is the most significant */
  alen = xsize-n;
  clen = ysize-n;
  a = xp + n;
  b = xp;			/* until n words */
  c = yp + n;
  d = yp;			/* until n words */

  apblen = mp_safe_size_for_add(a, alen, b, n);
  cpdlen = mp_safe_size_for_add(c, clen, d, n);

  adbclen = apblen + cpdlen;
  aclen = alen+clen;

  /* initialise termpoary buffer */
  /* allocate adbc with extra space (n) so that we don't have to do
     the part of (* (expt B ...) ..) part. */
  ALLOC_BUFFER(adbc, ulong, adbclen+n, stack_size);
  ALLOC_BUFFER(ac, ulong, aclen+n2, stack_size);
  ALLOC_BUFFER(bd, ulong, n2, stack_size);

  /* these can be intact during allocation. */
  ALLOC_BUFFER_REC(apb, ulong, apblen, stack_size); /* a+b */
  ALLOC_BUFFER_REC(cpd, ulong, cpdlen, stack_size); /* c+d */
  /* we just need to clear most significant byte for addition buffer 
     nb. that's the part for carry. */
  apb[apblen-1] = 0L;
  cpd[cpdlen-1] = 0L;
  
  /* prepare for 3 (a + b) and  (c + d) */
  /* dump_xarray(a, alen); */
  /* dump_xarray(b, n); */
  /* dump_xarray(apb, apblen); */
  mp_add(apb, apblen, a, alen, b, n);
  /* dump_xarray(apb, apblen); */
  mp_add(cpd, cpdlen, c, clen, d, n);
  /* dump_xarray(cpd, cpdlen); */

  /* dump_xarray(cpd, cpdlen); */
  mp_mul_rec(ac+n2, aclen, a, alen, c, clen, stack_size); /* recursive 1 */
  /* dump_xarray(ac+n2, aclen); */
  mp_mul_rec(bd, n2, b, n, d, n, stack_size); /* recursive 2 */
  /* dump_xarray(bd, n2); */
  
  /* recursive 3 */
  mp_mul_rec(adbc+n, adbclen, apb, apblen, cpd, cpdlen, stack_size);
  /* dump_xarray(adbc+n, adbclen); */
  
  /* 3 - 1 - 2 */
  /* abdc > ac so carry won't happen */
  /* dump_xarray(ac+n2, aclen); */
  mp_sub(adbc+n, adbclen, adbc+n, adbclen, ac+n2, aclen);
  /* dump_xarray(adbc+n, adbclen); */
  mp_sub(adbc+n, adbclen, adbc+n, adbclen, bd, n2);
  /* dump_xarray(adbc+n, adbclen); */

  /* combine 16^n*ac + 16^(n/2)*(ad + bc) + bd */
  /* now br must have sufficient size to put */
  /* (should always be) aclen+n2 > adbclen+n */
  mp_add(rp, rsize, ac, aclen+n2, adbc, adbclen+n);
  mp_add(rp, rsize, rp, rsize, bd, n2);
  /* dump_xarray(rp, rsize); */
}

/* the value is found experimentally. */
#define KARATSUBA_SQUARE_LOW_LIMIT ((2880>>3)/SIZEOF_LONG)

static int can_karatsuba_square(ulong len)
{
  return len >= KARATSUBA_SQUARE_LOW_LIMIT;
}

/*
  karatsuba square
  compute x*x

  x = a*B^(n/2) + b
  n = xsize
  B = WORD_BITS
  x*x = (a*B^(n/2) + b) * (a*B^(n/2) + b)
      = B^n*a^2 + B^(n/2)*2ab + b^2

  2ab = (a+b)^2 - (a^2 + b^2)  
  x*x = B^n*a^2 + B^(n/2)*((a+b)^ - (a^2 + b^2)) + b^2
      = B^(n/2)(B^(n/2)*a^2 + ((a+b)^ - (a^2 + b^2))) + b^2
*/
static void karatsuba_square(ulong *rp, ulong rsize,
			     ulong *xp, ulong xsize,
			     intptr_t stack_size)
{
  long n = xsize/2;
  ulong *a, *b;
  ulong *b2, *apb, *a2pb2, *apb2;
  ulong alen, a2len, apblen, a2pb2len, apb2len;
  ulong a2_ext, apb2_ext, rlen;
  long n2 = n<<1;
 
  alen = xsize - n;
  a = xp + n;
  b = xp;
  /* space for shift */
  a2_ext = ((n*WORD_BITS)+WORD_BITS-1)/WORD_BITS; /* shift size of *1 */
  apb2_ext = ((n*WORD_BITS)+WORD_BITS-1)/WORD_BITS; /* shift size of *2 */
  a2len = alen<<1;
  apblen = mp_safe_size_for_add(a, alen, b, n);
  apb2len = apblen<<1;
  
  /* we can use rp for a^2 storage */
  ALLOC_BUFFER_REC(b2, ulong, n2, stack_size);
  ALLOC_BUFFER_REC(apb, ulong, apblen, stack_size);
  ALLOC_BUFFER_REC(apb2, ulong, apb2len, stack_size); /* (a+b)^2 */

  /* clear possible carry byte on stack */
  apb[apblen-1] = 0;

  /* a^2 */
  mp_square_rec(rp, a2len, a, alen, stack_size);
  /* b^2 */
  mp_square_rec(b2, n2, b, n, stack_size);

  /* allocate a2pb2 */
  a2pb2len = mp_safe_size_for_add(rp, a2len, b2, n2);
  ALLOC_TEMP_BUFFER_REC(a2pb2, ulong, a2pb2len);
  /* clear possible carry byte on stack */
  a2pb2[a2pb2len-1] = 0;
  
  mp_add(apb, apblen, a, alen, b, n);	 /* a+b */
  mp_square_rec(apb2, apb2len, apb, apblen, stack_size); /* (a+b)^2 */
  mp_add(a2pb2, a2pb2len, rp, a2len, b2, n2); /* (a^2 + b^2) */

  /* (a+b)^2 > (a^2 + b^2) */
  ASSERT(apb2len >= a2pb2len);
  mp_sub(apb2, apb2len, apb2, apb2len, a2pb2, a2pb2len);

  /* B^(n/2)*a^2 */
  mp_lshift(rp, a2len+a2_ext, rp, a2len, n*WORD_BITS);
  /* B^(n/2)*a^2 + 2ab */
  mp_add(rp, rsize, rp, a2len+a2_ext, apb2, apb2len);
  /* +1 in case of carry, don't worry won't bite */
  rlen = a2len+a2_ext;
  rlen = (rsize == rlen) ? rlen : rlen+1;
  mp_lshift(rp, rsize, rp, rlen, n*WORD_BITS);
  mp_add(rp, rsize, rp, rlen+apb2_ext, b2, n2);
}

#endif	/* USE_KARATSUBA */

/*
  Memo for future:
  There are couple of more algorithms to be implemented.

  - Schönhage–Strassen algorithm (from 1720 to 7808 64 bit words)
    https://en.wikipedia.org/wiki/Sch%C3%B6nhage%E2%80%93Strassen_algorithm
  - Fürer's algorithm (not practical)
    https://en.wikipedia.org/wiki/F%C3%BCrer's_algorithm

  Toom-3 would also be a alternative but basic idea is the same as
  Karatsuba. Thus it may consume the same or more amount of stack area.

  The following papre describes Schoenehage-Strassen very well and
  their optimisation.
  - A GMP-based Implementation of Schönhage-Strassen’s Large Integer 
    Multiplication Algorithm
    http://www.loria.fr/~gaudry/publis/issac07.pdf
 */
#undef USE_SCHOENEHAGE_STRASSEN
#ifdef USE_SCHOENEHAGE_STRASSEN
static void schoenhage_strassen(ulong *rp, ulong rsize,
				ulong *xp, ulong xsize,
				ulong *yp, ulong ysize)
{
  /* TBD */
}

#endif	/*  USE_SCHOENEHAGE_STRASSEN */


/* 
   out += in * k
 */
static inline ulong mp_mul_add(ulong *out, ulong *in, long len, ulong k)
{
  long i;
#ifdef USE_DLONG
  udlong p = (udlong)in[0] * k + out[0];
  out[0] = (ulong)p;

  for (i = 1; i < len; i++) {
    p = (udlong)in[i] * k + (ulong)(p >> WORD_BITS) + out[i];
    out[i] = (ulong)p;
  }
  return (ulong)(p >> WORD_BITS);
#else
  /* FIXME this doesn't work properly yet. */
  ulong hi, lo, c = 0;
  UMUL(hi, lo, in[0], k); 	/* hi,lo = in[0] * k  ; in[0]*k (1)*/
  UADD(lo, c, out[0], lo);	/* r1(c) = out[0] + lo; (1)+out[0] */
  UADD(hi, c, hi, 0);		/* add carry to hi */
  out[0] = lo;			/* r1 = (ulong)p */

  for (i = 1; i < len; i++) {
    ulong t = hi, lo2, c2 = 0;
    c = 0;
    UMUL(hi, lo, in[i], k);	/* in[i]*k (1) */
    UADD(lo2, c, lo, t);	/* (1) + p>>WORD_BITS (2) */
    UADD(lo, c2, lo2, out[i]);	/* (2) + out[i] */
    UADD(hi, c, hi, c2);	/* add carry */
    out[i] = lo;
  }
  return hi;
#endif
}
/* grade-school  */
static ulong* long_mul(ulong *rp, ulong rsize,
		       ulong *xp, ulong xsize,
		       ulong *yp, ulong ysize)
{
  long i;
  /* multiply first word */
  mp_mul_ul(rp, rsize, xp, xsize, yp[0]);
  /* add in subsequent words, storing the most significant word which is new
     each time */
  for (i = 1; i < ysize; i++) {
    rp[xsize + i] = mp_mul_add((rp+i), xp, xsize, yp[i]);
  }
  return rp;
}

static void mp_mul_rec(ulong *rp, ulong rsize,
		       ulong *xp, ulong xsize,
		       ulong *yp, ulong ysize,
		       intptr_t stack_size)
{
#ifdef USE_KARATSUBA
  if (can_karatsuba(xsize, ysize)) {
    karatsuba(rp, rsize, xp, xsize, yp, ysize, stack_size);
    return;
  }
#endif
  long_mul(rp, rsize, xp, xsize, yp, ysize);
}

static void mp_mul(ulong *rp, ulong rsize,
		   ulong *xp, ulong xsize,
		   ulong *yp, ulong ysize)
{
  volatile char t = 'a';	/* don't let compiler erase this */
  intptr_t stack_size = Sg_AvailableStackSize((volatile uintptr_t)&t);
  mp_mul_rec(rp, rsize, xp, xsize, yp, ysize, stack_size);
}

/* xp^2 -> rp */
/* static int add_one(ulong *num, int len, ulong carry); */
static void mp_base_square(ulong *rp, ulong rsize,
			   ulong *xp, ulong xsize)
{
  /* TODO version for not using udlong */
#ifdef USE_DLONG
  long i, j, xlen;
  ulong last = 0;

  /* store the squares, right shifted one bit */
  for (i = xsize - 1, j = xsize<<1; i >= 0; i--) {
    udlong t = xp[i];
    udlong p = t * t;
    rp[--j] = (last<<(WORD_BITS-1))|(ulong)(p>>(WORD_BITS+1));
    rp[--j] = (ulong)(p>>1);
    last = p&1;
  }
  /* then add in the off diagonal sums */
  for (i = 0, j = 1, xlen = xsize - 1; xlen; i++, j+= 2, xlen--) {
    ulong t = xp[i];
    ulong ts = xlen+j;
    t = mp_mul_add(rp+j, xp+i+1, xlen, t);
    /* add_one(rp + xlen + j, xlen + 1, t); */
    mp_add_ul(rp+ts, rsize-ts, rp+ts, xlen+1, t);
  }
  /* primitive_left_shift(rp, 2*xsize, 1); */
  mp_lshift(rp, rsize, rp, xsize<<1, 1);
  rp[0] |= xp[0] & 1;
#else
#endif
}

static void mp_square_rec(ulong *rp, ulong rsize,
			  ulong *xp, ulong xsize,
			  intptr_t stack_size)
{
  /* handle special case */
  if (!xsize) return;
#ifdef USE_KARATSUBA
  if (can_karatsuba_square(xsize)) {
    karatsuba_square(rp, rsize, xp, xsize, stack_size);
    return;
  }
#endif
  /* normal */
  mp_base_square(rp, rsize, xp, xsize);
}

static void mp_square(ulong *rp, ulong rsize,
		      ulong *xp, ulong xsize)
{
  volatile char t = 'a';	/* don't let compiler erase this */
  intptr_t stack_size = Sg_AvailableStackSize((volatile uintptr_t)&t);
  mp_square_rec(rp, rsize, xp, xsize, stack_size);
}

/* divide */

static inline int div_normalization_factor(ulong w)
{
  ulong b = (1L << (WORD_BITS - 1));
  int c = 0;
  for (; b > 0; b >>= 1, c++) {
    if (w & b) return c;
  }
  Sg_Panic("bignum.c: div_normalization_factor: can't be here. %lu", w);
  return 0;
}

/*  
    xp / yp -> dp
    xp % yp -> rem
    return size of remainder
 */
static ulong mp_div_rem_kunuth(ulong *dp,  ulong qlen,
			       ulong *rem, ulong rlen,
			       ulong *xp,  ulong xsize,
			       ulong *yp,  ulong ysize)
{
  ulong *u, *v;
  ulong de_size = xsize;
  ulong ds_size = ysize;
  int d = div_normalization_factor(yp[ds_size - 1]);
  long j, k, n, m;
  ulong vn_1, vn_2, vv, uj, uj2, cy;

#define DIGIT(num, n)							\
  (((n)%2)? HI((num)[(n)/2]) : LO((num)[(n)/2]))
#define DIGIT2(num, n)							\
  (((n)%2)?								\
   ((LO((num)[(n)/2+1])<<HALF_BITS)|HI((num)[(n)/2])):			\
   (num)[(n)/2])
#define SETDIGIT(num, n, v)						\
  (((n)%2)?								\
   (num[(n)/2]=(num[(n)/2] & LOMASK)|((v) << HALF_BITS)):		\
   (num[(n)/2]=(num[(n)/2] & HIMASK)|((v) & LOMASK)))
#define SETDIGIT2(num, n, v)						\
  (((n)%2)?								\
   ((num[(n)/2] = LO(num[(n)/2])|((v)<<HALF_BITS)),	\
    (num[(n)/2+1] = (num[(n)/2+1] & HIMASK)|HI(v))) : \
   (num[(n)/2] = (v)))
  
  if (rem) {
    u = rem;
  } else {
    ALLOC_TEMP_BUFFER(u, ulong, de_size+1);
  }
  ALLOC_TEMP_BUFFER(v, ulong, ds_size);
  if (d >= HALF_BITS) {
    d -= HALF_BITS;
    n = ds_size * 2 - 1;
    m = de_size * 2 - n;
  } else {
    n = ds_size * 2;
    m = de_size * 2 - n;
  }
  mp_lshift(u, de_size+1, xp, xsize, d);
  mp_lshift(v, ds_size, yp, ysize, d);
  vn_1 = DIGIT(v, n - 1);
  vn_2 = DIGIT(v, n - 2);

  for (j = m; j >= 0; j--) {
    ulong uu = (DIGIT(u, j+n) << HALF_BITS) + DIGIT(u, j+n-1);
    ulong qq = uu / vn_1;
    ulong rr = uu % vn_1;

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
    if (dp) 
      SETDIGIT(dp, j, qq);
  }
  return mp_rshift(u, de_size+1, u, de_size+1, d);
#undef DIGIT
#undef DIGIT2
#undef SETDIGIT
#undef SETDIGIT2
}

/* for now disable it */
#undef USE_BURNIKEL_ZIEGLER
#ifdef USE_BURNIKEL_ZIEGLER
/* 
   Reference: http://cr.yp.to/bib/1998/burnikel.ps
 */
/* the same as karatsuba since we need to multiply with karatsuba according
   to the paper. */
#define BURNIKEL_ZIEGLER_BITS      1600
#define BURNIKEL_ZIEGLER_THRESHOLD (BURNIKEL_ZIEGLER_BITS/WORD_BITS)

#define BURNIKEL_ZIEGLER_OFFSET    40

/* this doesn't consider signess */
static int mp_compare_shifted(ulong *lhs, ulong lhs_count,
			      ulong *rhs, ulong rhs_count,
			      ulong n)
{
  long i, j;
  long shifted_length = lhs_count - n;
  /* compare size */
  if (shifted_length < rhs_count) return -1;
  if (shifted_length > rhs_count) return 1;
  /* elements */
  for (i = lhs_count - 1, j = rhs_count - 1; i >= n; i--, j--) {
    if (lhs[i] < rhs[j]) return -1;
    if (lhs[i] > rhs[j]) return 1;
  }
  return 0;
}

static int mp_compare(ulong *lhs, ulong lhs_count,
		      ulong *rhs, ulong rhs_count)
{
  return mp_compare_shifted(lhs, lhs_count, rhs, rhs_count, 0);
}

/* getting a block of conceptually split blocks
   assume, bsize = t*n
 */
static ulong *burnikel_block(ulong *b, int index, int n)
{
  return b + index*n;
}

static int mp_burnikel_ziegler_d2n1n(ulong *q, ulong qn,
				     ulong *r, ulong rn,
				     ulong *a, ulong an,
				     ulong *b, ulong bn,
				     intptr_t stack_size);
/*
   From Section 2, Algorithm 2. (D 3n/2n)
   Let A and B be non negative integers, and let A < β^n*B and 
   β^(n/2) <= B < β^(2n). Algorithm D3n/2n computes the quotient
   Q = ⌊A/B⌋ with remainder R = A-Q*B
   1. Split A into three parts A = [A1,A2,A3] with Ai < β^n
   2. Split B into two parts = B = [B1,B2] with Bi < β^n
   3. Distinguish the cases A1 < B1 or A1 >= B1.
      (a) If A1 < B1, compute Ô = ⌊[A1,A2]/B1⌋ with remainder R1 using 
          algorithm D2n/1n
      (b) If A1 >= B1, set Ô = β^n - 1 and set R1 = [A1,A2] - [B1,0] + [0,B1]
          (= [A1,A2] - ÔB1)
   4. Compute D = Ô * B2 using Karatsuba multiplication.
   5. Compute R̂ = R1β^n + A4 - D
   6. As long as R̂ < 0, repeat
      (a) R̂ = R̂ + B
      (b) Ô = Ô -1
   7. Return R = R̂, Q = Ô

   NB: the procedure returns size of quotient
*/
static int mp_burnikel_ziegler_d3n2n(ulong *q, ulong qn,
				     ulong *r, ulong rn,
				     ulong *a, ulong an,
				     ulong *b, ulong bn,
				     intptr_t stack_size)
{
  long n = bn/2;
  long n2 = n<<1;
  ulong *a12, *a3;
  ulong *b1, *b2;
  ulong *rh, *d;
  long rhs, rh_size, qs, i, ds, tmp;
  
  /* step 1 */
  a12 = a; /* for step3(a), make a12 = [A1,A2] */
  a3 = a+n2;
  /* step 2 */
  b1 = b;
  b2 = b+n;

  /* TODO size of R1 (at least this is safe, though) */
  rh_size = SG_LEFT_SHIFT_SPACE(n, WORD_BITS*n);
  rh_size = SG_LEFT_SHIFT_SPACE(rh_size, WORD_BITS*n);
  tmp = an-n2+1;
  fputs("=====\n", stderr);
  ALLOC_BUFFER(rh, ulong, max(rh_size, tmp), stack_size);

  /* step 3 */
  if (mp_compare_shifted(a, an, b, bn, n) < 0) {
    fputs("d2n1n\n", stderr);
    /* we don't need remainder here */
    qs = mp_burnikel_ziegler_d2n1n(q, qn, rh, rh_size, a12, n2, b1, n, stack_size);
    rhs = rh_size;
    while (rh[rhs-1] == 0) rhs--;

    /* step4 */
    ds = n+qs;
    ALLOC_BUFFER(d, ulong, ds, stack_size);
    mp_mul(d, ds, q, qs, b2, n);
  } else {
    long i, b10s;
    ulong *b10;

    b10s = SG_LEFT_SHIFT_SPACE(n, WORD_BITS*n);
    fprintf(stderr, "shifting: %d > %d\n", b10s, n2);
    ALLOC_BUFFER(b10, ulong, b10s, stack_size);
    for (i = 0; i < n ; i++) {
      q[i] = (ulong)-1L;
    }
    qs = n;
    
    mp_add(rh, rh_size, a12, n2, b1, n);
    mp_lshift(b10, b10s, b1, n, WORD_BITS*n);

    if (b10s > n2) {
      mp_sub(rh, rh_size, b10, b10s, rh, n2);
      rhs = b10s;
    } else {
      mp_sub(rh, rh_size, rh, n2, b10, b10s);
      rhs = n2;
    }
    while (rh[rhs-1] == 0) rhs--;
    /* step4 */
    ds = SG_LEFT_SHIFT_SPACE(bn-n, WORD_BITS*n);
    ALLOC_BUFFER(d, ulong, ds, stack_size);
    mp_lshift(d, ds, b2, bn-n, WORD_BITS*n);
    mp_sub(d, ds, d, ds, b2, bn-n);
  }

  /* step5 */
  mp_lshift(rh, rh_size, rh, rhs, WORD_BITS*n);
  rhs = rh_size;
  while (rh[rhs-1] == 0) rhs--;
  /* there is no A4, use A3 */
  if (mp_add(rh, rh_size, rh, rhs, a3, an-n2)) rhs++;
  
  /* step6 */
  fprintf(stderr, "ds: %d\n", ds);
  dump_xarray(rh, rhs);
  dump_xarray(d, ds);
  
  
  while (mp_compare(rh, rhs, d, ds) < 0) {
    int c = mp_add(rh, rhs, rh, rhs, b, bn);
    if (c) {
      rh[rhs++] = 1L;
    }
    mp_sub_ul(q, qs, q, qs, 1);
  }
  mp_sub(rh, rh_size, rh, rhs, d, ds);
  /* return */
  for (i = 0; i < rhs; i++) r[i] = rh[i];

  dump_xarray(q, qs);

  return qs;
}
/* 
   From Section 2, Algorithm 1. (D 2n/1n)
   Let A and B be non negative integers, and let A < β^n*B and 
   β^(n/2) <= B < β^n. Algorithm D 2n/1n computes the quotient
   Q = ⌊A/B⌋ with remainder R = A-Q*B
   1. If n is odd or smallar than some convenient constant, compute Q
      and R by school division and return.
   2. Split A into four parts A = [A1, ..., A4] with Ai < β^(n/2). Split
      B into two parts B = [B1,B2] with Bi < β^(n/2).
   3. Compute the high part Q1 of ⌊A/B⌋ as Q1 = ⌊[A1,A2,A3]/[B1,B2]⌋ with
      remainder R1 = [R1.1,R1.2] using algorithm D3n/2n
   4. Compute the low part Q2 of ⌊A/B⌋ as Q2 = ⌊[R1.1,R1.2,A4]/[B1,B2]⌋ with
      remainder R using algorithm D3n/2n
   5. Return Q = [Q1,Q2]

   this procedure return size of quotient
*/
static int mp_burnikel_ziegler_d2n1n(ulong *q, ulong qn,
				     ulong *r, ulong rn,
				     ulong *a, ulong an,
				     ulong *b, ulong bn,
				     intptr_t stack_size)
{
  long n = bn, i, k;
  ulong *a123, *a4, *r12a4, *rt;
  ulong a123_len, q1, q2, rt_size;

  /* step 1 */
  if (n & 1 || n < BURNIKEL_ZIEGLER_THRESHOLD) {
    mp_div_rem_kunuth(q, qn, r, rn, a, an, b, bn);
    while (q[qn-1] == 0 && qn > 0) qn--;
    return qn;
  }
  /* step 2 */
  a123_len = an - n/2;
  a123 = a;
  a4 = a + a123_len;

  /* step 3 */
  rt_size = max(a123_len, n*3);
  ALLOC_BUFFER(rt, ulong, rt_size, stack_size);
  q1 = mp_burnikel_ziegler_d3n2n(q, qn, rt, rt_size, a123, a123_len, b, bn, stack_size);
  ALLOC_BUFFER_REC(r12a4, ulong, n*3, stack_size);
  for (i = 0; i < rt_size && rt[i] != 0; i++) r12a4[i] = rt[i];
  for (k = 0; k < an - a123_len; k++) r12a4[i++] = a4[k];
  /* step 4 */
  clear_buffer(rt, rt_size);
  q2 = mp_burnikel_ziegler_d3n2n(q+q1, qn-q1, rt, rt_size, r12a4, n*3, b, bn, stack_size);
  for (i = 0; i < rn && rt[i] != 0; i++) r[i] = rt[i];
  return q1 + q2;
}

/*
  Returns xp/yp, qp = quotient, rp = remainder

  From Section 2, Algorithm 3. (D r/s)
  LET A and B be non negative integer such that A < β^r, B < β^s.
  the quotient Q = ⌊A/B⌋ with remainder R = A - Q*B
  1. Set m = min{2^k|(2^k) * DIV_LIMIT) > s}
  2. Set j = ⌈s/m⌉ and n = j*m
  3. Set σ = max{r|2^r * B < β^n}
  4. (a) Set B = B*2^σ to normalize B
  (b) Set A = A*2^σ to shift A by the same amount as B
  5. Set t = min{l >= 2|A < (β^l-n) / 2}
  6. Split A conceptually into t blocks [At-1, ..., A0]β^n = A. Note that
  leftmost bit of At-1 is 0 by our choice of t.
  7. Set Zt-2 = [At-1,At-2]
  8. For i from t-2 downto 0 do
     (a) Using algorithm D2n/1n compute Qi, Ri such that Zi = B*Qi+Ri
     (b) If i > 0, set Zi-1 = [Ri, Ai-i]
  9. Return Q=[Qt-2, ..., Q0] and R = R0*2^-σ
  
  k = WORD_BITS
  DIV_LIMIT = threshold
  
  β/2 
  (this amounts to say that it has a leading 1 in its binary representation;
  if this is not the case we can shift B and A to the left)
    
 */
static ulong mp_div_rem_burnikel_zeigler(ulong *qp, ulong qlen,
					 ulong *rp, ulong rlen,
					 ulong *xp, ulong xsize,
					 ulong *yp, ulong ysize)
{
  /* this function is the root, so just check stack here */
  volatile char mark = 'a';	/* don't let compiler erase this */
  intptr_t stack_size = Sg_AvailableStackSize((volatile uintptr_t)&mark);
  
  ulong *a = xp, *b = yp;
  ulong asize = xsize /* = r */, bsize = ysize /* = s */;

  long bbits = mp_bit_size(b, bsize);
  /* step 1 */
  ulong m = 1 << (WORD_BITS - nlz(bsize/BURNIKEL_ZIEGLER_THRESHOLD));
  /* step 2 */
  ulong j = (bsize+m-1)/m;
  ulong n = j*m;
  udlong nb = WORD_BITS * n;	/* n block in bits */
  /* step 3 */
  int sigma = max(0, nb - bbits), t;
  /* step 4 */
  ulong *B, *A;
  uint B_size = SG_LEFT_SHIFT_SPACE(asize, sigma),
    A_size = SG_LEFT_SHIFT_SPACE(bsize, sigma);
  /* others */
  ulong *zi, *ri, *ai, *qi;
  int i, k, qilen, qn;
  /* shift amount is sigma */
  ALLOC_BUFFER_REC(B, ulong, B_size, stack_size);
  ALLOC_BUFFER_REC(A, ulong, A_size, stack_size);
  mp_lshift(B, B_size, b, bsize, sigma);
  mp_lshift(A, A_size, a, asize, sigma);

  dump_xarray(xp, xsize);
  dump_xarray(yp, ysize);

  if (B[B_size-1] == 0) B_size--;
  if (A[A_size-1] == 0) A_size--;
  /* step 5 
                     A
     +-----------+-----------+-----------+
     |     |     |           |  |  |  |  |
     +-----------+-----------+-----------+
                             <--   n   -->
           <--             r           -->
     <--              t*n              -->

              B
     +----+-----------+
     |    |  |  |  |  |
     +----+-----------+
          <--   s   -->
     <--  n = m*j   -->
     NB: t*n needs to cover extra space so make t a bit bigger.
   */
  t = (mp_bit_size(A, A_size) + nb)/nb;
  if (t < 2) t = 2;		/* l >= 2 */
  
  /* step 6 
     Split A conceptually into t blocks [At-1, ..., A0]β^n = A. Note that
     leftmost bit of At-1 is 0 by our choice of t.
   */
  /* step 7 zt_2 = [At-1, At-2] */
  ALLOC_TEMP_BUFFER_REC(zi, ulong, n<<1);
  /* zt_2 = [At-1, At-2] */
  ai = burnikel_block(A, t-1, n);
  for (i = 0; i < n<<1; i++) zi[i] = ai[i];

  /* step 8 */
  /* max Ri size is n */
  ALLOC_BUFFER_REC(ri, ulong, n, stack_size);
  qilen = (n<<1) - B_size + 1;
  ALLOC_BUFFER_REC(qi, ulong, qilen, stack_size);
  
  for (i = t-2; i > 0; i--) {
    clear_buffer(ri, n);
    clear_buffer(qi, qilen);
    /* step 8(a) d2n1n */
    qn = mp_burnikel_ziegler_d2n1n(qi, qilen, ri, n, zi, n<<1, B, B_size, stack_size);
    fprintf(stderr, "qn: %d\n", qn);
    /* step 8(b) set Zi-1 = [Ri, Ai-i] */
    /* Zi-1 = [Ri, Ai-i] */
    for (j = 0; j < n; j++) zi[j] = ri[j];
    ai = burnikel_block(A, i, n);
    for (k = 0; k < n; k++) zi[j++] = ai[k];
    /* Q=[Qt-2, ..., Q0] */
    for (j = 0; j < qn; j++) qp[i*j] = qi[j];
  }
  /* last iteration of step 8 */
  clear_buffer(ri, n);
  clear_buffer(qi, qilen);
  qn = mp_burnikel_ziegler_d2n1n(qi, qilen, ri, n, zi, n<<1, B, B_size, stack_size);
  for (j = 0; j < qn; j++) qp[i*j] = qi[j];
  return mp_rshift(rp, rlen, ri, n, sigma);
}
#endif

/* 
    xp / yp -> dp
    xp % yp -> rem
    return size of remainder
*/
static ulong mp_div_rem(ulong *qp, ulong qlen,
			ulong *rp, ulong rlen,
			ulong *xp, ulong xsize,
			ulong *yp, ulong ysize)
{
#ifdef USE_BURNIKEL_ZIEGLER
  fprintf(stderr, "div: %ld > %d, %ld > %d\n",
	  ysize, BURNIKEL_ZIEGLER_THRESHOLD,
	  (xsize - ysize), BURNIKEL_ZIEGLER_OFFSET);
  if (ysize > BURNIKEL_ZIEGLER_THRESHOLD &&
      (xsize - ysize) > BURNIKEL_ZIEGLER_OFFSET) {
    return mp_div_rem_burnikel_zeigler(qp, qlen, rp, rlen, 
				       xp, xsize, yp, ysize);
  }
#endif
  return mp_div_rem_kunuth(qp, qlen, rp, rlen, xp, xsize, yp, ysize);
}

/* others */
/* this is not used */
#if 0
static void mul_n1(ulong *out, ulong *in, int nlen, ulong k)
{
  udlong p = (udlong)*in++ * k;
  *out++ = (ulong)p;
  while (--nlen) {
    p = (udlong)*in++ * k + (ulong)(p >> WORD_BITS);
    *out++ = (ulong)p;
  }
  *out = (ulong)(p >> WORD_BITS);
}
#endif

/* compare 2 array whose length are the same. */
static int ulong_array_cmp_to_len(ulong *arg1, ulong *arg2, long len)
{
  long i;
  for (i = len-1; i >= 0; i--) {
    if (arg1[i] < arg2[i]) return -1;
    if (arg1[i] > arg2[i]) return 1;
  }
  return 0;
}

static ulong* mp_mont_reduce(ulong *n,   ulong nlen,
			     ulong *mod, long mlen,
			     ulong inv)
{
  ulong c = 0;
  long len = mlen;
  
  do {
    ulong carry = mp_mul_add(n, mod, mlen, inv * n[0]);
    c += mp_add_one(n+mlen, len, n+mlen, len, carry);
    ++n;
  } while (--len);
  while (c) {
    c += mp_sub_n(n, n, mod, mlen);
  }
  while (ulong_array_cmp_to_len(n, mod, mlen) >= 0) {
    mp_sub_n(n, n, mod, mlen);
  }
  return n;
}
