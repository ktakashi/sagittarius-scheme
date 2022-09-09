/* sagittarius-random.h                          -*- mode: c; coding: utf-8; -*-
 *
 *   Copyright (c) 2010-2022  Takashi Kato <ktakashi@ymail.com>
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

#ifndef SAGITTARIUS_RANDOM_H_
#define SAGITTARIUS_RANDOM_H_

/* 
   New interface for cryptographic library
   The basic idea is from the same as Springkussen.

   In C, we only bind tomcrypt's functions to Scheme and
   all APIs will be implemented in Scheme
 */
#ifdef noreturn
# define noreturn_save noreturn
# undef noreturn
#endif
#include <sagittarius.h>
#include <tomcrypt.h>

#ifdef noreturn_save
# define noreturn noreturn_save
#endif

typedef struct {
  SG_HEADER;
  int prng;
  prng_state state;
} SgPrngState;

SG_CLASS_DECL(Sg_PrngStateClass)
#define SG_CLASS_PRNG_STATE (&Sg_PrngStateClass)

#define SG_PRNG_STATE(obj) ((SgPrngState *)obj)
#define SG_PRNG_STATE_P(obj) SG_XTYPEP(obj, SG_CLASS_PRNG_STATE)
#define SG_PRNG_STATE_PRNG(obj) SG_PRNG_STATE(obj)->prng
#define SG_PRNG_STATE_STATE(obj) SG_PRNG_STATE(obj)->state

#define PRNG_DESCRIPTORP(obj) \
  (SG_INTP(obj) && (prng_is_valid(SG_INT_VALUE(obj)) == CRYPT_OK))
#define PRNG_DESCRIPTOR(prng) prng_descriptor[prng]
#define PRNG_DESCRIPTOR_NAME(prng) PRNG_DESCRIPTOR(prng).name

#define PRNG_DESCRIPTOR_START(prng, state) PRNG_DESCRIPTOR(prng).start(state)
#define PRNG_DESCRIPTOR_ADD_ENTROPY(prng, state, in, len) PRNG_DESCRIPTOR(prng).add_entropy(in, len, state)
#define PRNG_DESCRIPTOR_READY(prng, state) PRNG_DESCRIPTOR(prng).ready(state)
#define PRNG_DESCRIPTOR_READ(prng, state, out, len) PRNG_DESCRIPTOR(prng).read(out, len, state)
#define PRNG_DESCRIPTOR_DONE(prng, state) PRNG_DESCRIPTOR(prng).done(state)

SgObject Sg_MakePrngState(int prng);
void Sg_InitRandom(SgLibrary *lib);

#endif /* SAGITTARIUS_RANDOM_H_ */
