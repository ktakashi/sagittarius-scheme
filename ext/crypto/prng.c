/* -*- C -*- */
/*
 * prng.c: Pseudo Random Number Generator
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
#include "crypto.h"

SgObject Sg_MakeSecureRandom(SgString *name, int bits)
{
  const char *cname = Sg_Utf32sToUtf8s(name);
  int wprng, err;
  SgCrypto *prng = Sg_MakeCrypto(CRYPTO_PRNG);
  SG_SET_META_OBJ(prng, SG_META_CRYPTO);
  wprng = find_prng(cname);
  if (wprng == -1) {
    Sg_Error(UC("%A is not supported"), name);
    return SG_UNDEF;
  }
  err = rng_make_prng(bits, wprng, &prng->impl.prng, NULL);
  if (err != CRYPT_OK) {
    Sg_Error(UC("Failed to create prng: %A"), Sg_MakeStringC(error_to_string(err)));
    return SG_UNDEF;    
  }
  prng->impl.prng.wprng = wprng;
  return SG_OBJ(prng);
}

