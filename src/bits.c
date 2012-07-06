/* -*- C -*- */
/*
 * bits.c
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
#define LIBSAGITTARIUS_BODY
#include "sagittarius/bits.h"
#include "sagittarius/arith.h"

int Sg_BitsCount0(const unsigned long *bits, int start, int end)
{
  int sw = start  / WORD_BITS;
  int ew = (end-1)/ WORD_BITS;
  int sb = start  % WORD_BITS;
  int eb = end    % WORD_BITS;
  unsigned long num;

  if (start == end) return 0;
  if (sw == ew) return nbits(~bits[sw] & SG_BITS_MASK(sb, eb));

  num = nbits(~bits[sw] & SG_BITS_MASK(sb, 0));
  for (sw++; sw < ew; sw++) num += nbits(~bits[sw]);
  return num + (nbits(~bits[ew] & SG_BITS_MASK(0, eb)));
}

int Sg_BitsCount1(const unsigned long *bits, int start, int end)
{
  int sw = start  / WORD_BITS;
  int ew = (end-1)/ WORD_BITS;
  int sb = start  % WORD_BITS;
  int eb = end    % WORD_BITS;
  unsigned long num;

  if (start == end) return 0;
  if (sw == ew) return nbits(bits[sw] & SG_BITS_MASK(sb, eb));

  num = nbits(bits[sw] & SG_BITS_MASK(sb, 0));
  for (sw++; sw < ew; sw++) num += nbits(bits[sw]);
  return num + (nbits((bits[ew]) & SG_BITS_MASK(0, eb)));
}
