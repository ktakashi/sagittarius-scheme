/* bignum.inc                                      -*- mode:c; coding:utf-8; -*-
 *
 *   Copyright (c) 2010-2015  Takashi Kato <ktakashi@ymail.com>
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
typedef unsigned int uint;
typedef unsigned long ulong;

#define USE_DLONG

#if SIZEOF_LONG == 8
# ifdef __GNUC__
   typedef unsigned int dlong __attribute__((__mode__(TI)));
# else
/* try not to use dlong */
#  undef USE_DLONG
/* error for now */
#  error "sizeof(long) == 8 but not GCC (not supported yet)"
# endif
#  define SHIFT_MAGIC 6
#else
  typedef uint64_t dlong;
# define SHIFT_MAGIC 5
#endif

static inline ulong* mp_2scmpl(ulong *p, uint s)
{
  int i, c;
  for (i = 0, c = 1; i < s; i++) {
    ulong x = ~p[i];
    UADD(p[i], c, x, 0);
  }
  return p;
}

/* Addition */

/* helper */
static int mp_safe_size_for_add(ulong *xp, uint xsize, 
				ulong *yp, uint ysize)
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

/* xp + yp -> rp (xsize >= ysize)
   returns carry (0 or 1)
*/
static int mp_add(ulong *rp, uint rsize, 
		  ulong *xp, uint xsize,
		  ulong *yp, uint ysize)
{
  int c;
#ifdef USE_DLONG
  dlong sum = 0L;
  int xindex, yindex;
  for (xindex = 0, yindex = 0; yindex < ysize; xindex++, yindex++) {
    c = (int)(sum >> WORD_BITS);
    sum = (dlong)xp[xindex] + yp[yindex] + c;
    rp[xindex] = (ulong)sum;
  }
  /* copy remainder of longer number while carry propagation is required */
  c = (int)(sum >> WORD_BITS);
  for (; xindex < xsize && c; xindex++) {
    rp[xindex] = xp[xindex] + 1;
    c = (rp[xindex] == 0);
  }
  /* copy remainder of longer number */
  for (; xindex < xsize; xindex++) {
    rp[xindex] = xp[xindex];
  }
  if (c) {
    rp[rsize-1] = 1L;
  }
#else
  int i;
  unsigned long x, y;
  for (i = 0, c = 0; i < rsize; i++, xsize--, ysize--) {
    if (xsize <= 0) {
      if (ysize <= 0) {
	UADD(rp[i], c, 0, 0);
	continue;
      }
      y = yp[i];
      UADD(rp[i], c, 0, y);
      continue;
    }
    if (ysize <= 0) {
      x = xp[i];
      UADD(rp[i], c, x, 0);
      continue;
    }
    x = xp[i];
    y = yp[i];
    UADD(rp[i], c, x, y);
  }
#endif
  return c;
}

/* xp + y -> rp (xzise > 1)
   returns carry (0 or 1)
*/   
static int mp_add_ul(ulong *rp, uint rsize, 
		     ulong *xp, uint xsize,
		     ulong y)
{
  long c = 0;
  uint i;

  UADD(rp[0], c, xp[0], y);
  for (i = 1; i < xsize; i++) {
    UADD(rp[i], c, xp[i], 0);
  }
  rp[rsize - 1] = c;
  return c;
}

/* Subtraction */

/* xp - yp -> rp (xsize >= ysize)
   returns carry (0 or 1)
*/
static int mp_sub(ulong *rp, uint rsize, 
		  ulong *xp, uint xsize,
		  ulong *yp, uint ysize)
{
  int c;
#ifdef USE_DLONG
  dlong diff = 0L;
  int xindex, yindex;
  for (yindex = 0, xindex = 0; yindex < ysize; xindex++, yindex++) {
    c = (int)(diff >> WORD_BITS);
    diff = (dlong)xp[xindex] - yp[yindex] + c;
    rp[xindex] = (ulong)diff;
  }
  c = (int)(diff >> WORD_BITS);
  for (;xindex < xsize && c; xindex++) {
    rp[xindex] = xp[xindex] - 1;
    c = (rp[xindex] == (ulong)-1L) ? -1 : 0;
  }
  for (;xindex < xsize; xindex++) {
    rp[xindex] = xp[xindex];
  }
  /* padding carry */
  for (;xindex < rsize; xindex++) {
    rp[xindex] = (ulong)c;
  }
#else
  int i;
  unsigned long x, y;
  for (i = 0, c = 0; i < rsize; i++, xsize--, ysize--) {
    if (xsize <= 0) {
      if (ysize <= 0) {
	USUB(rp[i], c, 0, 0);
	continue;
      }
      y = yp[i];
      USUB(rp[i], c, 0, y);
      continue;
    }
    if (ysize <= 0) {
      x = xp[i];
      USUB(rp[i], c, x, 0);
      continue;
    }
    x = xp[i];
    y = yp[i];
    USUB(rp[i], c, x, y);
  }
#endif
  return c;
}

/* xp - y -> rp (xzise > 1)
   returns carry (0 or 1)
*/   
static int mp_sub_ul(ulong *rp, uint rsize, 
		     ulong *xp, uint xsize,
		     ulong y)
{
  long c = 0;
  uint i;

  USUB(rp[0], c, xp[0], y);
  for (i = 1; i < xsize; i++) {
    USUB(rp[i], c, xp[i], 0);
  }
  rp[rsize - 1] = c;
  return c;
}

/* Multiplication */
/* xp * y -> rp (xsize > 1)
   assumes rp has sufficient size.
 */
static void mp_mul_ul(ulong *rp, uint rsize,
		      ulong *xp, uint xsize,
		      ulong y)
{
  uint i;

#ifdef USE_DLONG
  register dlong p;

  p = (dlong)xp[0] * y;
  rp[0] = (ulong)p;
  for (i = 1; i < xsize; i++) {
    p = (dlong)xp[i] * y + (ulong)(p >> WORD_BITS);
    rp[i] = (ulong)p;
  }
  rp[i] = (ulong)(p >> WORD_BITS);

#else

  for (i = 0; i < xsize; i++) {
    uint j;
    ulong hi, lo, r1;
    ulong x = xp[i];
    ulong c = 0;
    ulong r0 = rp[i+off];

    UMUL(hi, lo, x, y);
    UADD(r1, c, r0, lo);
    rp[i+off] = r1;

    r0 = rp[i+off+1];
    UADD(r1, c, r0, hi);
    rp[i+off+1] = r1;

    for (j = i + off + 2; c && j < rsize; j++) {
      r0 = rp[j];
      UADD(r1, c, r0, 0);
      rp[j] = r1;
    }
  }
  return br;
#endif

}

/* forward declaration */
static void mp_mul(ulong *rp, uint rsize,
		   ulong *xp, uint xsize,
		   ulong *yp, uint ysize);

#define USE_KARATSUBA
#ifdef USE_KARATSUBA
/* if the bignum length is less than this then we do 
   usual computation. */
/* according to Wikipedia karatsuba is faster when the numbers are
   bigger than 320-640 bits. */
#define KARATSUBA_LOW_LIMIT ((640>>3)/SIZEOF_LONG)

static int can_karatsuba(uint xlen, uint ylen)
{
  if (xlen < KARATSUBA_LOW_LIMIT || ylen < KARATSUBA_LOW_LIMIT) {
    return FALSE;
  } else {
    int n = max(xlen, ylen)/2;
    return n < xlen && n < ylen;
  }
}


/*
  basic karatsuba algorithm
  compute x*y 
  
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
static void karatsuba(ulong *rp, uint rsize,
		      ulong *xp, uint xsize,
		      ulong *yp, uint ysize)
{
  uint n = max(xsize, ysize)/2, apblen, cpdlen, adbclen;
  uint n2 = n<<1;
  ulong *a, *b, *c, *d;
  ulong *ac, *bd, *apb, *cpd, *adbc;
  uint alen, clen, aclen;

  /* split given argument into 2 parts
     it's little endian so the last is the most significant */
  a = xp + n;
  b = xp;			/* until n words */
  c = yp + n;
  d = yp;			/* until n words */

  alen = xsize-n;
  clen = ysize-n;
  apblen = mp_safe_size_for_add(a, alen, b, n);
  cpdlen = mp_safe_size_for_add(c, clen, d, n);

  adbclen = apblen + cpdlen;
  aclen = alen+clen;

  /* initialise termpoary buffer */
  /* allocate adbc with extra space (n) so that combination can
     use this without allocating extra memory. */
  ALLOC_TEMP_BUFFER(adbc, ulong, adbclen+n);
  /* allocate ac with extra space (n2) so that combination can
     use this without allocating extra memory. */
  ALLOC_TEMP_BUFFER(ac, ulong, aclen+n2);
  ALLOC_TEMP_BUFFER(bd, ulong, n2);

  /* these can be intact during allocation. */
  ALLOC_TEMP_BUFFER_REC(apb, ulong, apblen);
  ALLOC_TEMP_BUFFER_REC(cpd, ulong, cpdlen);
  /* we just need to clear most significant byte for addition buffer 
     nb. that's the part for carry. */
  apb[apblen-1] = 0L;
  cpd[cpdlen-1] = 0L;

  mp_add(apb, apblen, a, alen, b, n); /* prepare for 3 (a + b) */
  mp_add(cpd, cpdlen, c, clen, d, n); /* prepare for 3 (c + d) */

  mp_mul(ac+n2, aclen, a, alen, c, clen);	     /* recursive 1 */
  mp_mul(bd, n2, b, n, d, n);			     /* recursive 2 */
  mp_mul(adbc+n, adbclen, apb, apblen, cpd, cpdlen); /* recursive 3 */
  /* 3 - 1 - 2 */
  /* abdc > ac so carry won't happen */
  mp_sub(adbc+n, adbclen, adbc+n, adbclen, ac+n2, aclen);
  mp_sub(adbc+n, adbclen, adbc+n, adbclen, bd, n2);

  /* combine 16^n*ac + 16^(n/2)*(ad + bc) + bd */
  /* now br must have sufficient size to put */
  mp_add(rp, rsize, ac, aclen+n2, adbc, adbclen+n);
  mp_add(rp, rsize, rp, rsize, bd, n2);
}
#endif	/* USE_KARATSUBA */

static ulong mp_mul_add(ulong *out, ulong *in, int len, ulong k)
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
/* grade-school  */
static ulong* long_mul(ulong *rp, uint rsize,
		       ulong *xp, uint xsize,
		       ulong *yp, uint ysize)
{
  /* multiply first word */
  /* mul_n1(z, x, xlen, *y++); */
  int i;
  ulong k = yp[0];
  dlong p = (dlong)xp[0] * k;
  rp[0] = (ulong)p;

  for (i = 1; i < xsize; i++) {
    p = (dlong)xp[i] * k + (ulong)(p >> WORD_BITS);
    rp[i] = (ulong)p;
  }
  rp[i] = (ulong)(p >> WORD_BITS);

  /* add in subsequent words, storing the most significant word which is new
     each time */
  for (i = 1; i < ysize; i++) {
    rp[xsize + i] = mp_mul_add((rp+i), xp, xsize, yp[i]);
  }
  return rp; 
}

static void mp_mul(ulong *rp, uint rsize,
		   ulong *xp, uint xsize,
		   ulong *yp, uint ysize)
{
#ifdef USE_KARATSUBA
  if (can_karatsuba(xsize, ysize)) {
    karatsuba(rp, rsize, xp, xsize, yp, ysize);
    return;
  }
#endif
  long_mul(rp, rsize, xp, xsize, yp, ysize);
}
