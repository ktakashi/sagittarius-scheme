/* -*- C -*- */
/*
 * bits.h
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
#ifndef SAGITTARIUS_BITS_H_
#define SAGITTARIUS_BITS_H_

#include "sagittariusdefs.h"

#define SG_BITS_MASK(s, e)				\
  (((e) ? (1UL<<(e)) - 1 : -1) & ~((1UL<<(s)) - 1))


/* should these be macro? */
static inline int nlz32(uint32_t x)
{
  uint32_t t;
  int n = 32;
  t = x >> 16; if (t) { n -= 16 ; x = t; }
  t = x >>  8; if (t) { n -=  8 ; x = t; }
  t = x >>  4; if (t) { n -=  4 ; x = t; }
  t = x >>  2; if (t) { n -=  2 ; x = t; }
  t = x >>  1; if (t) { return n - 2; }
  return n - x;
}

static inline int nlz64(uint64_t x)
{
  uint64_t t;
  int n = 64;
  t = x >> 32; if (t) { n -= 32 ; x = t; }
  t = x >> 16; if (t) { n -= 16 ; x = t; }
  t = x >>  8; if (t) { n -=  8 ; x = t; }
  t = x >>  4; if (t) { n -=  4 ; x = t; }
  t = x >>  2; if (t) { n -=  2 ; x = t; }
  t = x >>  1; if (t) { return n - 2; }
  return n - x;
}

static inline int nlz(long x) {
  if (sizeof(long) == sizeof(uint32_t)) return nlz32((uint32_t)x);
  return nlz64((uint64_t)x);
}


static inline int nbits32(uint32_t x)
{
  uint32_t t;
  x = x - ((x >> 1) & 0x55555555);
  t = ((x >> 2) & 0x33333333);
  x = (x & 0x33333333) + t;
  x = (x + (x >> 4)) & 0x0F0F0F0F;
  x = x * 0x01010101;
  return x >> 24;
}

static inline int nbits64(uint64_t x)
{
  const uint64_t c1 = 0x5555555555555555LL;
  const uint64_t c2 = 0x3333333333333333LL;
  const uint64_t c3 = 0x0F0F0F0F0F0F0F0FLL;
  const uint64_t c4 = 0x0101010101010101LL;
  uint64_t t;
  x = x - ((x >> 1) & c1);
  t = ((x >> 2) & c2);
  x = (x & c2) + t;
  x = (x + (x >> 4)) & c3;
  x = x * c4;
  return x >> 56;
}

static inline int nbits(long x)
{
  if (sizeof(long) == sizeof(uint32_t)) return nbits32((uint32_t)x);
  return nbits64((uint64_t)x);
}


static inline int ntz32(uint32_t x)
{
    return nbits32(~x & (x - 1));
}

static inline int ntz64(uint64_t x)
{
    return nbits64(~x & (x - 1));
}

static inline int ntz(intptr_t x) {
  if (sizeof(intptr_t) == sizeof(uint32_t)) return ntz32((uint32_t)x);
  return ntz64((uint64_t)x);
}

SG_CDECL_BEGIN

SG_EXTERN int Sg_BitsCount0(const unsigned long *bits, int start, int end);
SG_EXTERN int Sg_BitsCount1(const unsigned long *bits, int start, int end);

SG_CDECL_END

#endif /* SAGITTARIUS_BITS_H_ */
