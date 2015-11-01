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

/* in case of platform whose long size if 8 but no 128 bit storage.
   we put this. however, it's not tested at all. so most likely doesn't
   work at all.
 */
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

/* 
   xp + yp -> rp
   assume both num1 and num2 have the same length and rp has 
   at least 'len' length
   returns carry

   NB: to make better performance, this part should be written in
       assembly. but for now it's ok
 */
static long mp_add_n(ulong *rp, ulong *xp, ulong *yp, int len)
{
  int i;
#ifdef USE_DLONG
  dlong t = (dlong)xp[0] + yp[0];
  rp[0] = (ulong)t;
  for (i = 1; i < len; i++) {
    t = (dlong)xp[i] + (dlong)yp[i] + (ulong)(t >> WORD_BITS);
    rp[i] = (ulong)t;
  }
  return (long)(t >> WORD_BITS);
#else
  int c = 0;
  for (i = 0; i < len; i++) {
    UADD(rp[i], c, xp[i], yp[i]);
  }
  return c;
#endif
}

/* xp + yp -> rp (xsize >= ysize)
   returns carry (0 or 1)
*/
static int mp_add(ulong *rp, uint rsize, 
		  ulong *xp, uint xsize,
		  ulong *yp, uint ysize)
{
  int i;
  int c = mp_add_n(rp, xp, yp, ysize);
  /* do the rest */
  for (i = ysize; i < xsize && c; i++) {
    rp[i] = xp[i] + 1;
    c = (rp[i] == 0);
  }
  /* copy remainder */
  for (; i < xsize; i++) {
    rp[i] = xp[i];
  }
  if (c) rp[rsize-1] = 1L;
  return c;
}

/*
  xp + y -> rp (xzise > 1)
  returns carry (0 or 1)
*/
/* mp_add_one doesn't add carry to the end */
static int mp_add_one(ulong *rp, uint rsize, 
		      ulong *xp, uint xsize,
		      ulong y)
{
#ifdef USE_DLONG
  dlong t = (dlong)xp[0] + y;
  int i;
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
  uint i;

  UADD(rp[0], c, xp[0], y);
  for (i = 1; i < xsize; i++) {
    UADD(rp[i], c, xp[i], 0);
  }
  return c;
#endif  
}
static int mp_add_ul(ulong *rp, uint rsize, 
		     ulong *xp, uint xsize,
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
static long mp_sub_n(ulong *rp, ulong *xp, ulong *yp, int len)
{
  int i;
#ifdef USE_DLONG
  dlong t = (dlong)xp[0] - yp[0];
  rp[0] = (ulong)t;
  for (i = 1; i < len; i++) {
    t = (dlong)xp[i] - (dlong)yp[i] - (ulong)-(t >> WORD_BITS);
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
static int mp_sub(ulong *rp, uint rsize, 
		  ulong *xp, uint xsize,
		  ulong *yp, uint ysize)
{
  int i;
  int c = mp_sub_n(rp, xp, yp, ysize);
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
  rp[rsize-1] = c;
  return c;
}

/* shift operations */
static void mp_lshift(ulong *rp, uint rsize,
		      ulong *xp, uint xsize,
		      long amount)
{
  long nwords, nbits;
  int i;

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
static uint mp_rshift(ulong *rp, uint rsize,
		      ulong *xp, uint xsize,
		      long amount)
{
  ulong nwords = amount / WORD_BITS;
  ulong nbits = amount % WORD_BITS;
  int i;
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
static void mp_mul(ulong *rp, uint rsize,
		   ulong *xp, uint xsize,
		   ulong *yp, uint ysize);

static void mp_square(ulong *rp, uint rsize,
		      ulong *xp, uint xsize);
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
  
  /* prepare for 3 (a + b) and  (c + d) */
  /* these check are necessary. there's a condition both a and c are
     greater than n. e.g.) xsize = 33, ysize = 33 */
  if (alen >= n) {
    mp_add(apb, apblen, a, alen, b, n);
  } else {
    mp_add(apb, apblen, b, n, a, alen);
  }
  if (clen >= n) {
    mp_add(cpd, cpdlen, c, clen, d, n);
  } else {
    mp_add(cpd, cpdlen, d, n, c, clen);
  }
  /* dump_xarray(cpd, cpdlen); */
  mp_mul(ac+n2, aclen, a, alen, c, clen);	     /* recursive 1 */
  /* dump_xarray(ac+n2, aclen); */
  mp_mul(bd, n2, b, n, d, n);			     /* recursive 2 */
  /* dump_xarray(bd, n2); */
  mp_mul(adbc+n, adbclen, apb, apblen, cpd, cpdlen); /* recursive 3 */
  /* dump_xarray(adbc+n, adbclen); */
  /* 3 - 1 - 2 */
  /* abdc > ac so carry won't happen */
  mp_sub(adbc+n, adbclen, adbc+n, adbclen, ac+n2, aclen);
  mp_sub(adbc+n, adbclen, adbc+n, adbclen, bd, n2);

  /* combine 16^n*ac + 16^(n/2)*(ad + bc) + bd */
  /* now br must have sufficient size to put */
  /* (should always be) aclen+n2 > adbclen+n */
  mp_add(rp, rsize, ac, aclen+n2, adbc, adbclen+n);
  mp_add(rp, rsize, rp, rsize, bd, n2);
}

/* the value is found experimentally. */
#define KARATSUBA_SQUARE_LOW_LIMIT ((2880>>3)/SIZEOF_LONG)

static int can_karatsuba_square(uint len)
{
  return len >= KARATSUBA_SQUARE_LOW_LIMIT;
}

/*
  karatsuba square
  compute x*x

  x = a*B^(n/2) + b
  n = xsize
  B = WORD_BITS
  x*y = (a*B^(n/2) + b) * (a*B^(n/2) + b)
      = B^n*a^2 + B^(n/2)*2ab + b^2

  2ab = (a+b)^2 - (a^2 + b^2)  
  x*y = B^n*a^2 + B^(n/2)*((a+b)^ - (a^2 + b^2)) + b^2
      = B^(n/2)(B^(n/2)*a^2 + ((a+b)^ - (a^2 + b^2))) + b^2
*/
static void karatsuba_square(ulong *rp, uint rsize,
			     ulong *xp, uint xsize)
{
  int n = xsize/2;
  ulong *a, *b;
  ulong *b2, *apb, *a2pb2, *apb2;
  uint alen, a2len, apblen, a2pb2len, apb2len;
  uint a2_ext, apb2_ext, rlen;
  int n2 = n<<1;
 
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
  ALLOC_TEMP_BUFFER_REC(b2, ulong, n2);
  ALLOC_TEMP_BUFFER_REC(apb, ulong, apblen);
  ALLOC_TEMP_BUFFER_REC(apb2, ulong, apb2len); /* (a+b)^2 */

  /* clear possible carry byte on stack */
  apb[apblen-1] = 0;

  /* a^2 */
  mp_square(rp, a2len, a, alen);
  /* b^2 */
  mp_square(b2, n2, b, n);

  /* allocate a2pb2 */
  a2pb2len = mp_safe_size_for_add(rp, a2len, b2, n2);
  ALLOC_TEMP_BUFFER_REC(a2pb2, ulong, a2pb2len);
  /* clear possible carry byte on stack */
  a2pb2[a2pb2len-1] = 0;
  
  mp_add(apb, apblen, a, alen, b, n);	 /* a+b */
  mp_square(apb2, apb2len, apb, apblen); /* (a+b)^2 */
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
   out += in * k
 */
static inline ulong mp_mul_add(ulong *out, ulong *in, int len, ulong k)
{
  int i;
#ifdef USE_DLONG
  dlong p = (dlong)in[0] * k + out[0];
  out[0] = (ulong)p;

  for (i = 1; i < len; i++) {
    p = (dlong)in[i] * k + (ulong)(p >> WORD_BITS) + out[i];
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
static ulong* long_mul(ulong *rp, uint rsize,
		       ulong *xp, uint xsize,
		       ulong *yp, uint ysize)
{
  int i;
  /* multiply first word */
  mp_mul_ul(rp, rsize, xp, xsize, yp[0]);
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

/* xp^2 -> rp */
/* static int add_one(ulong *num, int len, ulong carry); */
static void mp_base_square(ulong *rp, uint rsize,
			   ulong *xp, uint xsize)
{
  /* TODO version for not using dlong */
#ifdef USE_DLONG
  int i, j, xlen;
  ulong last = 0;

  /* store the squares, right shifted one bit */
  for (i = xsize - 1, j = xsize<<1; i >= 0; i--) {
    dlong t = xp[i];
    dlong p = t * t;
    rp[--j] = (last<<(WORD_BITS-1))|(ulong)(p>>(WORD_BITS+1));
    rp[--j] = (ulong)(p>>1);
    last = p&1;
  }
  /* then add in the off diagonal sums */
  for (i = 0, j = 1, xlen = xsize - 1; xlen; i++, j+= 2, xlen--) {
    ulong t = xp[i];
    uint ts = xlen+j;
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

static void mp_square(ulong *rp, uint rsize,
		      ulong *xp, uint xsize)
{
  /* handle special case */
  if (!xsize) return;
#ifdef USE_KARATSUBA
  if (can_karatsuba_square(xsize)) {
    karatsuba_square(rp, rsize, xp, xsize);
    return;
  }
#endif
  /* normal */
  mp_base_square(rp, rsize, xp, xsize);
}

/* divide */

static inline int div_normalization_factor(unsigned long w)
{
  ulong b = (1L << (WORD_BITS - 1)), c = 0;
  for (; b > 0; b >>= 1, c++) {
    if (w & b) return c;
  }
  FATAL("bignum.c: div_normalization_factor: can't be here");
  return 0;
}

/*  
    xp / yp -> dp
    xp % yp -> rem
    return size of remainder
 */
static uint mp_div_rem(ulong *dp, ulong *rem,
		       ulong *xp, uint xsize,
		       ulong *yp, uint ysize)
{
  ulong *u, *v;
  uint de_size = xsize;
  uint ds_size = ysize;
  int d = div_normalization_factor(yp[ds_size - 1]);
  int j, k, n, m;
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

/* others */
/* this is not used */
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

/* compare 2 array whose length are the same. */
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

static ulong* mp_mont_reduce(ulong *n, uint nlen,
			     ulong *mod, int mlen,
			     ulong inv)
{
  ulong c = 0;
  int len = mlen;
  
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
