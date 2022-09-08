/* sagittarius-cipher.c                          -*- mode: c; coding: utf-8; -*-
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
#include "sagittarius-cipher.h"

static void mode_key_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  const SgChar *mode;
  switch (SG_MODE_KEY_MODE(self)) {
  case MODE_ECB: mode = UC("ecb"); break;
  case MODE_CBC: mode = UC("cbc"); break;
  case MODE_CFB: mode = UC("cfb"); break;
  case MODE_OFB: mode = UC("ofb"); break;
  case MODE_CTR: mode = UC("ctr"); break;
  case MODE_LRW: mode = UC("lrw"); break;
  case MODE_F8:  mode = UC("f8"); break;
  /* case MODE_XTS: mode = UC("xts"); break; */
  default: mode = UC("unknown"); break;
  }
  Sg_Printf(port, UC("#<%s-mode-key>"), mode);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_ModeKeyClass, mode_key_printer);

SgObject Sg_MakeModeKey(SgCipherMode mode)
{
  SgModeKey *key = SG_NEW(SgModeKey);
  SG_SET_CLASS(key, SG_CLASS_MODE_KEY);
  key->mode = mode;
  return SG_OBJ(key);
}

static void enc_auth_state_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  const SgChar *mode;
  switch (SG_ENC_AUTH_STATE_MODE(self)) {
  case MODE_EAX: mode = UC("eax"); break;
  case MODE_OCB: mode = UC("ocb"); break;
  case MODE_OCB3: mode = UC("ocb3"); break;
  case MODE_CCM: mode = UC("ccm"); break;
  case MODE_GCM: mode = UC("gcm"); break;
  default: mode = UC("unknown"); break;
  }
  Sg_Printf(port, UC("#<%s-mode-state>"), mode);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_EncAuthStateClass, enc_auth_state_printer);
SgObject Sg_MakeEncAuthState(SgCipherEncAuthMode mode)
{
  SgEncAuthState *state = SG_NEW(SgEncAuthState);
  SG_SET_CLASS(state, SG_CLASS_ENC_AUTH_STATE);
  state->mode = mode;
  return SG_OBJ(state);
}

const struct ltc_cipher_descriptor aes128_desc =
{
    "aes128",
    100,
    16, 16, 16, 10,
    aes_setup, aes_ecb_encrypt, aes_ecb_decrypt, aes_test, aes_done, aes_keysize,
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
};

const struct ltc_cipher_descriptor aes192_desc =
{
    "aes192",
    101,
    24, 24, 16, 10,
    aes_setup, aes_ecb_encrypt, aes_ecb_decrypt, aes_test, aes_done, aes_keysize,
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
};

const struct ltc_cipher_descriptor aes256_desc =
{
    "aes256",
    102,
    32, 32, 16, 10,
    aes_setup, aes_ecb_encrypt, aes_ecb_decrypt, aes_test, aes_done, aes_keysize,
    NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL
};

extern void Sg__Init_cipher(SgLibrary *lib);

void Sg_InitCipher(SgLibrary *lib)
{
    Sg__Init_cipher(lib);

  /* Sg_InitMutex(&lock, FALSE); */
  /* initialize libtomcrypt */
#define REGISTER_CIPHER(cipher)						\
  if (register_cipher(cipher) == -1) {					\
    Sg_Warn(UC("Unable to register %S cipher"),				\
	    Sg_MakeStringC((cipher)->name));				\
  }

  REGISTER_CIPHER(&blowfish_desc);
  REGISTER_CIPHER(&xtea_desc);
  REGISTER_CIPHER(&rc2_desc);
  REGISTER_CIPHER(&rc5_desc);
  REGISTER_CIPHER(&rc6_desc);
  REGISTER_CIPHER(&safer_k64_desc);
  REGISTER_CIPHER(&safer_sk64_desc);
  REGISTER_CIPHER(&safer_k128_desc);
  REGISTER_CIPHER(&safer_sk128_desc);
  REGISTER_CIPHER(&saferp_desc);
  REGISTER_CIPHER(&aes_desc);
  REGISTER_CIPHER(&aes128_desc);
  REGISTER_CIPHER(&aes192_desc);
  REGISTER_CIPHER(&aes256_desc);
  REGISTER_CIPHER(&twofish_desc);
  REGISTER_CIPHER(&des_desc);
  REGISTER_CIPHER(&des3_desc);
  REGISTER_CIPHER(&cast5_desc);
  REGISTER_CIPHER(&noekeon_desc);
  REGISTER_CIPHER(&skipjack_desc);
  REGISTER_CIPHER(&anubis_desc);
  REGISTER_CIPHER(&khazad_desc);
  REGISTER_CIPHER(&kseed_desc);
  REGISTER_CIPHER(&kasumi_desc);
  REGISTER_CIPHER(&camellia_desc);

  /* put mode */
#define MODE_CONST(name, value)						\
  Sg_MakeBinding(lib, SG_INTERN(#name), SG_MAKE_INT(value), TRUE)

  MODE_CONST(*mode:ecb*, MODE_ECB);
  MODE_CONST(*mode:cbc*, MODE_CBC);
  MODE_CONST(*mode:cfb*, MODE_CFB);
  MODE_CONST(*mode:ofb*, MODE_OFB);
  MODE_CONST(*mode:ctr*, MODE_CTR);
  MODE_CONST(*mode:lrw*, MODE_LRW);
  MODE_CONST(*mode:f8*,  MODE_F8 );
  /* MODE_CONST(*mode:xts*, MODE_XTS); */


  Sg_MakeBinding(lib, SG_INTERN("*ctr-mode:little-endian*"), SG_MAKE_INT(CTR_COUNTER_LITTLE_ENDIAN), TRUE);
  Sg_MakeBinding(lib, SG_INTERN("*ctr-mode:big-endian*"), SG_MAKE_INT(CTR_COUNTER_BIG_ENDIAN), TRUE);
  Sg_MakeBinding(lib, SG_INTERN("*ctr-mode:rfc3686*"), SG_MAKE_INT(LTC_CTR_RFC3686), TRUE);
  
  Sg_InitStaticClass(SG_CLASS_MODE_KEY, UC("<mode-key>"), lib, NULL, 0);
}
