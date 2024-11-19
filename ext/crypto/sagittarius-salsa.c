/* sagittarius-salsa.c                            -*- mode: c; coding: utf-8; -*-
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

#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "sagittarius-salsa.h"

#ifdef noreturn
# define noreturn_save noreturn
# undef noreturn
#endif
#include <tomcrypt.h>
#ifdef noreturn_save
# define noreturn noreturn_save
#endif

#define QUARTERROUND(a,b,c,d)	      \
  do {				      \
    x[b] ^= (ROL((x[a] + x[d]),  7)); \
    x[c] ^= (ROL((x[b] + x[a]),  9)); \
    x[d] ^= (ROL((x[c] + x[b]), 13)); \
    x[a] ^= (ROL((x[d] + x[c]), 18)); \
  } while (0)

static void salsa_core(uint8_t *out, uint8_t *in, int rounds)
{
  uint32_t x[16];
  int i;

  memcpy(x, in, sizeof(x));
  
  for (i = 0; i < rounds; i += 2) {
    QUARTERROUND( 0, 4, 8,12);
    QUARTERROUND( 5, 9,13, 1);
    QUARTERROUND(10,14, 2, 6);
    QUARTERROUND(15, 3, 7,11);
    QUARTERROUND( 0, 1, 2, 3);
    QUARTERROUND( 5, 6, 7, 4);
    QUARTERROUND(10,11, 8, 9);
    QUARTERROUND(15,12,13,14);
  }

  for (i = 0; i < 16; i++) {
    x[i] += ((uint32_t *)in)[i];
    STORE32L(x[i], out + 4 * i);
  }
}


SgObject Sg_SalsaCore(SgObject in, int rounds)
{
  SgObject bv = Sg_ByteVectorCopy(SG_BVECTOR(in), 0, SG_BVECTOR_SIZE(in));
  return Sg_SalsaCoreX(bv, rounds);

}

SgObject Sg_SalsaCoreX(SgObject in, int rounds)
{
  long i;
  if (SG_BVECTOR_SIZE(in) % 64 != 0) {
    Sg_AssertionViolation(SG_INTERN("salsa-core!"),
      SG_MAKE_STRING("input of salsa-core! must be multiple of 64"),
      in);
  }
  for (i = 0; i < SG_BVECTOR_SIZE(in); i += 64) {
    salsa_core(SG_BVECTOR_ELEMENTS(in) + i, SG_BVECTOR_ELEMENTS(in) + i, rounds);
  }
  return in;
}

extern void Sg__Init_salsa(SgLibrary *lib);

SG_EXTENSION_ENTRY void CDECL Sg_Init_sagittarius__salsa()
{
  SgLibrary *lib;
  SG_INIT_EXTENSION(sagittarius__salsa);
  lib = SG_LIBRARY(Sg_FindLibrary(SG_INTERN("(sagittarius crypto logic salsa)"),
				  FALSE));
  Sg__Init_salsa(lib);
}
