/* sagittarius-salsa-core.h                       -*- mode: c; coding: utf-8; -*-
 *
 *   Copyright (c) 2024  Takashi Kato <ktakashi@ymail.com>
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

#ifndef SAGITTARIUS_SALSA_CORE_H_
#define SAGITTARIUS_SALSA_CORE_H_

#include <stdint.h>

#define SG_SALSA_QUARTERROUND(a, b, c, d)       \
  do {                                           \
    x[b] ^= ROL((x[a] + x[d]), 7);              \
    x[c] ^= ROL((x[b] + x[a]), 9);              \
    x[d] ^= ROL((x[c] + x[b]), 13);             \
    x[a] ^= ROL((x[d] + x[c]), 18);             \
  } while (0)

static SG_INLINE void sagittarius_salsa_core(uint8_t *out, const uint8_t *in,
					     int rounds)
{
  uint32_t x[16];
  uint32_t in_word;
  int i;

  for (i = 0; i < 16; i++) {
    LOAD32L(x[i], in + 4 * i);
  }

  for (i = 0; i < rounds; i += 2) {
    SG_SALSA_QUARTERROUND(0, 4, 8, 12);
    SG_SALSA_QUARTERROUND(5, 9, 13, 1);
    SG_SALSA_QUARTERROUND(10, 14, 2, 6);
    SG_SALSA_QUARTERROUND(15, 3, 7, 11);
    SG_SALSA_QUARTERROUND(0, 1, 2, 3);
    SG_SALSA_QUARTERROUND(5, 6, 7, 4);
    SG_SALSA_QUARTERROUND(10, 11, 8, 9);
    SG_SALSA_QUARTERROUND(15, 12, 13, 14);
  }

  for (i = 0; i < 16; i++) {
    LOAD32L(in_word, in + 4 * i);
    x[i] += in_word;
    STORE32L(x[i], out + 4 * i);
  }
}

#undef SG_SALSA_QUARTERROUND

#endif	/* SAGITTARIUS_SALSA_CORE_H_ */
