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

/* FIXME absolute copy&paste... */
#define SHA3_KECCAK_SPONGE_WORDS 25 /* 1600 bits > 200 bytes > 25 x ulong64 */
#define SHA3_KECCAK_ROUNDS 24

static const ulong64 s_keccakf_rndc[24] = {
   CONST64(0x0000000000000001), CONST64(0x0000000000008082),
   CONST64(0x800000000000808a), CONST64(0x8000000080008000),
   CONST64(0x000000000000808b), CONST64(0x0000000080000001),
   CONST64(0x8000000080008081), CONST64(0x8000000000008009),
   CONST64(0x000000000000008a), CONST64(0x0000000000000088),
   CONST64(0x0000000080008009), CONST64(0x000000008000000a),
   CONST64(0x000000008000808b), CONST64(0x800000000000008b),
   CONST64(0x8000000000008089), CONST64(0x8000000000008003),
   CONST64(0x8000000000008002), CONST64(0x8000000000000080),
   CONST64(0x000000000000800a), CONST64(0x800000008000000a),
   CONST64(0x8000000080008081), CONST64(0x8000000000008080),
   CONST64(0x0000000080000001), CONST64(0x8000000080008008)
};

static const unsigned s_keccakf_rotc[24] = {
  1, 3, 6, 10, 15, 21, 28, 36, 45, 55, 2, 14, 27, 41,
  56, 8, 25, 43, 62, 18, 39, 61, 20, 44
};

static const unsigned s_keccakf_piln[24] = {
  10, 7, 11, 17, 18, 3, 5, 16, 8, 21, 24, 4, 15, 23, 19,
  13, 12, 2, 20, 14, 22, 9, 6, 1
};


static void s_keccakf(ulong64 s[25])
{
   int i, j, round;
   ulong64 t, bc[5];

   for(round = 0; round < SHA3_KECCAK_ROUNDS; round++) {
      /* Theta */
      for(i = 0; i < 5; i++) {
         bc[i] = s[i] ^ s[i + 5] ^ s[i + 10] ^ s[i + 15] ^ s[i + 20];
      }
      for(i = 0; i < 5; i++) {
         t = bc[(i + 4) % 5] ^ ROL64(bc[(i + 1) % 5], 1);
         for(j = 0; j < 25; j += 5) {
            s[j + i] ^= t;
         }
      }
      /* Rho Pi */
      t = s[1];
      for(i = 0; i < 24; i++) {
         j = s_keccakf_piln[i];
         bc[0] = s[j];
         s[j] = ROL64(t, s_keccakf_rotc[i]);
         t = bc[0];
      }
      /* Chi */
      for(j = 0; j < 25; j += 5) {
         for(i = 0; i < 5; i++) {
            bc[i] = s[j + i];
         }
         for(i = 0; i < 5; i++) {
            s[j + i] ^= (~bc[(i + 1) % 5]) & bc[(i + 2) % 5];
         }
      }
      /* Iota */
      s[0] ^= s_keccakf_rndc[round];
   }
}

int keccak_init(hash_state *md, int capacity)
{
  /* We don't support weird capacity */
  LTC_ARGCHK(md != NULL);
  if (capacity != 256 && capacity != 448 &&
      capacity != 512 && capacity != 768 &&
      capacity != 1024) {
    return CRYPT_INVALID_ARG;
  }
  XMEMSET(&md->sha3, 0, sizeof(md->sha3));
  md->sha3.capacity_words = (unsigned short)(capacity / (8 * sizeof(ulong64)));
  return CRYPT_OK;
  
}
/* basicallly copy&paste from sha3.c of libtomcrypt... */
int keccak_done4(hash_state *md, ulong64 pad,
		 unsigned char *out, unsigned long outlen)
{
  /* IMPORTANT NOTE: sha3_shake_done can be called many times */
  unsigned long idx;
  unsigned i;
  
  if (outlen == 0) return CRYPT_OK; /* nothing to do */
  LTC_ARGCHK(md  != NULL);
  LTC_ARGCHK(out != NULL);

  if (!md->sha3.xof_flag) {
    /* shake_xof operation must be done only once */
    md->sha3.s[md->sha3.word_index] ^= (md->sha3.saved ^ (pad << (md->sha3.byte_index * 8)));
    md->sha3.s[SHA3_KECCAK_SPONGE_WORDS - md->sha3.capacity_words - 1] ^= CONST64(0x8000000000000000);
    s_keccakf(md->sha3.s);
    /* store sha3.s[] as little-endian bytes into sha3.sb */
    for(i = 0; i < SHA3_KECCAK_SPONGE_WORDS; i++) {
      STORE64L(md->sha3.s[i], md->sha3.sb + i * 8);
    }
    md->sha3.byte_index = 0;
    md->sha3.xof_flag = 1;
  }
   
  for (idx = 0; idx < outlen; idx++) {
    if(md->sha3.byte_index >= (SHA3_KECCAK_SPONGE_WORDS - md->sha3.capacity_words) * 8) {
      s_keccakf(md->sha3.s);
      /* store sha3.s[] as little-endian bytes into sha3.sb */
      for(i = 0; i < SHA3_KECCAK_SPONGE_WORDS; i++) {
	STORE64L(md->sha3.s[i], md->sha3.sb + i * 8);
      }
      md->sha3.byte_index = 0;
    }
    out[idx] = md->sha3.sb[md->sha3.byte_index++];
  }
  return CRYPT_OK;
}
/* Extension for libtomcrypt end */


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
  unsigned int i;
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
  /* KECCAK */
  REGISTER_HASH(&keccak_224_desc);
  REGISTER_HASH(&keccak_256_desc);
  REGISTER_HASH(&keccak_384_desc);
  REGISTER_HASH(&keccak_512_desc);
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
