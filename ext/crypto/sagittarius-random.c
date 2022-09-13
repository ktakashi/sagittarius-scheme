/* sagittarius-digest.c                          -*- mode: c; coding: utf-8; -*-
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
#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "sagittarius-random.h"

static void prng_state_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  const char *cname = PRNG_DESCRIPTOR_NAME(SG_PRNG_STATE_PRNG(self));
  SgObject name = Sg_MakeStringC(cname);
  Sg_Printf(port, UC("#<%A-prng-state %d>"), name,
	    SG_PRNG_STATE_PRNG(self));
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_PrngStateClass, prng_state_printer);

SgObject Sg_MakePrngState(int prng)
{
  SgPrngState *state = SG_NEW(SgPrngState);
  SG_SET_CLASS(state, SG_CLASS_PRNG_STATE);
  state->prng = prng;
  return SG_OBJ(state);
}

extern void Sg__Init_random(SgLibrary *lib);

void Sg_InitRandom(SgLibrary *lib)
{
  Sg__Init_random(lib);
  
#define REGISTER_PRNG(prng)						\
  if (register_prng(prng) == -1) {					\
    Sg_Warn(UC("Unable to register %S prng algorithm "),		\
	    Sg_MakeStringC((prng)->name));				\
  }

  REGISTER_PRNG(&yarrow_desc);
  REGISTER_PRNG(&fortuna_desc);
  REGISTER_PRNG(&rc4_desc);
  REGISTER_PRNG(&sober128_desc);
  REGISTER_PRNG(&sprng_desc);
  REGISTER_PRNG(&chacha20_prng_desc);

  Sg_InitStaticClass(SG_CLASS_PRNG_STATE, UC("<pring-state>"), lib, NULL, 0);
}
