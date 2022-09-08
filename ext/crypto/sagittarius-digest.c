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
#include "sagittarius-digest.h"

static void digest_state_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  const char *cname = DIGEST_DESCRIPTOR_NAME(SG_DIGEST_STATE(self)->md);
  SgObject name = Sg_MakeStringC(cname);
  Sg_Printf(port, UC("#<%A-digest-state>"), name);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_DigestStateClass, digest_state_printer);

SgObject Sg_DigestDescriptorOID(int md)
{
  unsigned long *oid = DIGEST_DESCRIPTOR_OID(md);
  unsigned long len = DIGEST_DESCRIPTOR_OIDLEN(md);
  SgStringPort buffer;
  int i;
  /* I don't think there's a more than 10 digits element, 
     so should be more than enough */
  if (len == 0) {
    return SG_FALSE;
  }
  
  Sg_InitStringOutputPort(&buffer, len * 10);
  for (i = 0; i < len; i++) {
    unsigned long e = oid[i];
    SgObject s = Sg_NumberToString(SG_MAKE_INT(e), 10, FALSE);
    if (i != 0) {
      Sg_PutcUnsafe(SG_PORT(&buffer), '.');
    }
    Sg_PutsUnsafe(SG_PORT(&buffer), SG_STRING(s));
  }
  return Sg_GetStringFromStringPort(&buffer);
}

SgObject Sg_MakeDigestState(int md)
{
  SgDigestState *state = SG_NEW(SgDigestState);
  SG_SET_CLASS(state, SG_CLASS_DIGEST_STATE);
  state->md = md;
  return SG_OBJ(state);
}

extern void Sg__Init_digest(SgLibrary *lib);

void Sg_InitDigest(SgLibrary *lib)
{
  Sg__Init_digest(lib);
  
#define REGISTER_HASH(hash)						\
  if (register_hash(hash) == -1) {					\
    Sg_Warn(UC("Unable to register %S hash algorithm "),		\
	    Sg_MakeStringC((hash)->name));				\
  }

  REGISTER_HASH(&whirlpool_desc);
  REGISTER_HASH(&tiger_desc);
  /* SHA1 */
  REGISTER_HASH(&sha1_desc);
  /* SHA2 */
  REGISTER_HASH(&sha224_desc);
  REGISTER_HASH(&sha256_desc);
  REGISTER_HASH(&sha384_desc);
  REGISTER_HASH(&sha512_desc);
  REGISTER_HASH(&sha512_224_desc);
  REGISTER_HASH(&sha512_256_desc);
  /* SHA3 */
  REGISTER_HASH(&sha3_224_desc);
  REGISTER_HASH(&sha3_256_desc);
  REGISTER_HASH(&sha3_384_desc);
  REGISTER_HASH(&sha3_512_desc);
  /* RMD */
  REGISTER_HASH(&rmd128_desc);
  REGISTER_HASH(&rmd160_desc);
  REGISTER_HASH(&rmd256_desc);
  REGISTER_HASH(&rmd320_desc);
  /* MD n */
  REGISTER_HASH(&md5_desc);
  REGISTER_HASH(&md4_desc);
  REGISTER_HASH(&md2_desc);
  /* BLAKE2b */
  REGISTER_HASH(&blake2b_160_desc);
  REGISTER_HASH(&blake2b_256_desc);
  REGISTER_HASH(&blake2b_384_desc);
  REGISTER_HASH(&blake2b_512_desc);
  /* BLAKE2s */
  REGISTER_HASH(&blake2s_128_desc);
  REGISTER_HASH(&blake2s_160_desc);
  REGISTER_HASH(&blake2s_224_desc);
  REGISTER_HASH(&blake2s_256_desc);

  Sg_InitStaticClass(SG_CLASS_DIGEST_STATE, UC("<digest-state>"), lib, NULL, 0);
}
