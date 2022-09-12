/* sagittarius-cipher.h                          -*- mode: c; coding: utf-8; -*-
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

#ifndef SAGITTARIUS_CIPHER_H_
#define SAGITTARIUS_CIPHER_H_

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
/* 
   We don't support XTS for now, I've never heard of it and
   it has different signature of encryption and decryption 
   so doesn't match with our purpose
*/
typedef enum {
  MODE_ECB,
  MODE_CBC,
  MODE_CFB,
  MODE_OFB,
  MODE_CTR,
  MODE_LRW,
  MODE_F8 ,
  /* MODE_XTS */
} SgCipherMode;

typedef struct {
  SG_HEADER;
  SgCipherMode mode;
  union {
    symmetric_ECB ecb;
    symmetric_CBC cbc;
    symmetric_CFB cfb;
    symmetric_OFB ofb;
    symmetric_CTR ctr;
    symmetric_LRW lrw;
    symmetric_F8  f8;
    /* symmetric_xts xts; */
  };
} SgModeKey;

SG_CLASS_DECL(Sg_ModeKeyClass);
#define SG_CLASS_MODE_KEY (&Sg_ModeKeyClass)

#define SG_MODE_KEY(obj) ((SgModeKey *)obj)
#define SG_MODE_KEY_P(obj) SG_XTYPEP(obj, SG_CLASS_MODE_KEY)
#define SG_MODE_KEY_MODE(modeKey) SG_MODE_KEY(modeKey)->mode
#define SG_MODE_KEY_ECB(modeKey) SG_MODE_KEY(modeKey)->ecb
#define SG_MODE_KEY_CBC(modeKey) SG_MODE_KEY(modeKey)->cbc
#define SG_MODE_KEY_CFB(modeKey) SG_MODE_KEY(modeKey)->cfb
#define SG_MODE_KEY_OFB(modeKey) SG_MODE_KEY(modeKey)->ofb
#define SG_MODE_KEY_CTR(modeKey) SG_MODE_KEY(modeKey)->ctr
#define SG_MODE_KEY_LRW(modeKey) SG_MODE_KEY(modeKey)->lrw
#define SG_MODE_KEY_F8(modeKey)  SG_MODE_KEY(modeKey)->f8

typedef enum {
  MODE_EAX,
  MODE_OCB,
  MODE_OCB3,
  MODE_CCM,
  MODE_GCM
} SgCipherEncAuthMode;

typedef struct {
  SG_HEADER;
  SgCipherEncAuthMode mode;
  union {
    eax_state eax;
    ocb_state ocb;
    ocb3_state ocb3;
    ccm_state ccm;
    gcm_state gcm;
  };
} SgEncAuthState;

SG_CLASS_DECL(Sg_EncAuthStateClass)
#define SG_CLASS_ENC_AUTH_STATE (&Sg_EncAuthStateClass)

#define SG_ENC_AUTH_STATE(obj) ((SgEncAuthState *)obj)
#define SG_ENC_AUTH_STATE_P(obj) SG_XTYPEP(obj, SG_CLASS_ENC_AUTH_STATE)
#define SG_ENC_AUTH_STATE_MODE(obj) SG_ENC_AUTH_STATE(obj)->mode
#define SG_ENC_AUTH_STATE_EAX(obj)  SG_ENC_AUTH_STATE(obj)->eax
#define SG_ENC_AUTH_STATE_OCB(obj)  SG_ENC_AUTH_STATE(obj)->ocb
#define SG_ENC_AUTH_STATE_OCB3(obj) SG_ENC_AUTH_STATE(obj)->ocb3
#define SG_ENC_AUTH_STATE_CCM(obj)  SG_ENC_AUTH_STATE(obj)->ccm
#define SG_ENC_AUTH_STATE_GCM(obj)  SG_ENC_AUTH_STATE(obj)->gcm 

/* for stub */
#define SG_CIPHERP(cipher) \
  (SG_INTP(cipher) && (cipher_is_valid(SG_INT_VALUE(cipher)) == CRYPT_OK))

#define CIPHER_DESCRIPTOR(cipher) cipher_descriptor[cipher]
#define CIPHER_DESCRIPTOR_NAME(cipher)   CIPHER_DESCRIPTOR(cipher).name
#define CIPHER_DESCRIPTOR_BLOCK_LENGTH(cipher)   CIPHER_DESCRIPTOR(cipher).block_length
#define CIPHER_DESCRIPTOR_MIN_KEY_LENGTH(cipher) CIPHER_DESCRIPTOR(cipher).min_key_length
#define CIPHER_DESCRIPTOR_MAX_KEY_LENGTH(cipher) CIPHER_DESCRIPTOR(cipher).max_key_length
#define CIPHER_DESCRIPTOR_DEFAULT_ROUNDS(cipher) CIPHER_DESCRIPTOR(cipher).default_rounds
#define CIPHER_DESCRIPTOR_KEYSIZE(cipher, ks) CIPHER_DESCRIPTOR(cipher).keysize(ks)

SgObject Sg_MakeModeKey(SgCipherMode mode);
SgObject Sg_MakeEncAuthState(SgCipherEncAuthMode mode);

void Sg_InitCipher(SgLibrary *lib);

#endif /* SAGITTARIUS_CIPHER_H_ */
