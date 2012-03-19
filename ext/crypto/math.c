/* -*- C -*- */
/*
 * math.c
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
#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "math.h"

extern void Sg__Init_sagittarius_math_impl();

SG_EXTENSION_ENTRY void CDECL Sg_Init_sagittarius__math()
{
  SgLibrary *lib;
  Sg__Init_sagittarius_math_impl();

  lib = SG_LIBRARY(Sg_FindLibrary(SG_INTERN("(sagittarius math impl)"), FALSE));
  Sg_InitStaticClassWithMeta(SG_CLASS_PRNG, UC("<prng>"), lib, NULL,
			     SG_FALSE, NULL, 0);
  Sg_InitStaticClassWithMeta(SG_CLASS_HASH, UC("<hash-algorithm>"), lib, NULL,
			     SG_FALSE, NULL, 0);

#define REGISTER_PRNG(prng)						\
  if (register_prng(prng) == -1) {					\
    Sg_Warn(UC("Unable to register %S pseudo random number generator "), \
	    Sg_MakeStringC((prng)->name));				\
  }

  REGISTER_PRNG(&yarrow_desc);
  REGISTER_PRNG(&fortuna_desc);
  REGISTER_PRNG(&rc4_desc);
  REGISTER_PRNG(&sober128_desc);
  REGISTER_PRNG(&sprng_desc);

#define REGISTER_HASH(hash)						\
  if (register_hash(hash) == -1) {					\
    Sg_Warn(UC("Unable to register %S hash algorithm "),		\
	    Sg_MakeStringC((hash)->name));				\
  }
  
  REGISTER_HASH(&whirlpool_desc);
  REGISTER_HASH(&sha512_desc);
  REGISTER_HASH(&sha384_desc);
  REGISTER_HASH(&rmd160_desc);
  REGISTER_HASH(&sha256_desc);
  REGISTER_HASH(&rmd320_desc);
  REGISTER_HASH(&sha224_desc);
  REGISTER_HASH(&tiger_desc);
  REGISTER_HASH(&sha1_desc);
  REGISTER_HASH(&rmd256_desc);
  REGISTER_HASH(&rmd128_desc);
  REGISTER_HASH(&md5_desc);
  REGISTER_HASH(&md4_desc);
  REGISTER_HASH(&md2_desc);
}

