/* -*- mode: c; coding: utf-8; -*- */
/*
 * crypto.h
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
#include <sagittarius/extend.h>
#include "crypto.h"

static void crypto_printer(SgPort *port, SgObject self, SgWriteContext *ctx)
{
  switch (SG_CRYPTO(self)->type) {
  case CRYPTO_SYM_CIPHER: {
    struct ltc_cipher_descriptor *desc 
      = &cipher_descriptor[SG_SCIPHER(self)->cipher];
    Sg_Printf(port, UC("#<cipher %S block-size %d, key-length %d>"),
	      Sg_MakeStringC(desc->name), desc->block_length,
	      desc->min_key_length);
    break;
  }
  case CRYPTO_PUB_CIPHER:
    Sg_Printf(port, UC("#<cipher %A>"), SG_PCIPHER(self)->name);
    break;
  case CRYPTO_KEY:
    Sg_Printf(port, UC("#<%A-key>"), SG_KEY(self)->name);
    break;
  }
}

SG_INIT_META_OBJ(Sg_CryptoMeta, &crypto_printer, NULL);

static void finalize_cipher(SgObject obj, void *data)
{
  SG_SCIPHER(obj)->done(&SG_SCIPHER(obj)->skey);
}

static SgCrypto *make_crypto(SgCryptoType type)
{
  SgCrypto *c = SG_NEW(SgCrypto);
  SG_SET_META_OBJ(c, SG_META_CRYPTO);
  c->type = type;
  return c;
}

SgObject Sg_MakeCrypto(SgCryptoType type)
{
  return SG_OBJ(make_crypto(type));
}


SgObject Sg_MakeSymmetricCipher(SgString *name, SgCryptoMode mode, SgCrypto *ckey,
				SgObject iv, int rounds, SgObject padder, int ctr_mode)
{
  const char *cname = Sg_Utf32sToUtf8s(name);
  SgCrypto *crypto = make_crypto(CRYPTO_SYM_CIPHER);
  int cipher = find_cipher(cname), err;
  SgByteVector *key;
  ASSERT(SG_CRYPTO(ckey)->type == CRYPTO_KEY);
  key = SG_SECRET_KEY(SG_KEY(ckey));

  SG_SCIPHER(crypto)->cipher = cipher;
  SG_SCIPHER(crypto)->key = SG_KEY(ckey);
  SG_SCIPHER(crypto)->iv = iv;
  SG_SCIPHER(crypto)->mode = mode;
  SG_SCIPHER(crypto)->rounds = rounds;
  SG_SCIPHER(crypto)->padder = padder;
  if (cipher == -1) {
    Sg_Error(UC("%S is not supported"), name);
    return SG_UNDEF;
  }
  /* set up mode */
  switch (mode) {
  case MODE_ECB:
    err = ecb_start(cipher, SG_BVECTOR_ELEMENTS(key), SG_BVECTOR_SIZE(key),
		    rounds, &SG_SCIPHER(crypto)->skey.ecb_key);
    SG_INIT_CIPHER(SG_SCIPHER(crypto),
		   ecb_encrypt, ecb_decrypt, NULL, NULL, ecb_done);
    break;
  case MODE_CBC:
    if (!SG_BVECTOR(iv)) {
      Sg_Error(UC("iv required on CBC mode"));
      return SG_UNDEF;
    }
    err = cbc_start(cipher, SG_BVECTOR_ELEMENTS(iv),
		    SG_BVECTOR_ELEMENTS(key), SG_BVECTOR_SIZE(key),
		    rounds, &SG_SCIPHER(crypto)->skey.cbc_key);
    SG_INIT_CIPHER(SG_SCIPHER(crypto),
		   cbc_encrypt, cbc_decrypt, cbc_getiv, cbc_setiv, cbc_done);
    break;
  case MODE_CFB:
    if (!SG_BVECTOR(iv)) {
      Sg_Error(UC("iv required on CFB mode"));
      return SG_UNDEF;
    }
    err = cfb_start(cipher, SG_BVECTOR_ELEMENTS(iv),
		    SG_BVECTOR_ELEMENTS(key), SG_BVECTOR_SIZE(key),
		    rounds, &SG_SCIPHER(crypto)->skey.cfb_key);
    SG_INIT_CIPHER(SG_SCIPHER(crypto),
		   cfb_encrypt, cfb_decrypt, cfb_getiv, cfb_setiv, cfb_done);
    break;
  case MODE_OFB:
    if (!SG_BVECTOR(iv)) {
      Sg_Error(UC("iv required on OFB mode"));
      return SG_UNDEF;
    }
    err =ofb_start(cipher, SG_BVECTOR_ELEMENTS(iv),
		   SG_BVECTOR_ELEMENTS(key), SG_BVECTOR_SIZE(key),
		   rounds, &SG_SCIPHER(crypto)->skey.ofb_key);
    SG_INIT_CIPHER(SG_SCIPHER(crypto),
		   ofb_encrypt, ofb_decrypt, ofb_getiv, ofb_setiv, ofb_done);
    break;
  case MODE_CTR:
    if (!SG_BVECTOR(iv)) {
      Sg_Error(UC("iv required on CTR mode"));
      return SG_UNDEF;
    }
    err = ctr_start(cipher, SG_BVECTOR_ELEMENTS(iv),
		    SG_BVECTOR_ELEMENTS(key), SG_BVECTOR_SIZE(key),
		    rounds,
		    /* counter size is 0 by default.
		       (using a full block length. see libtomcrypto manual.)
		    */
		    ctr_mode,
		    &SG_SCIPHER(crypto)->skey.ctr_key);
    SG_INIT_CIPHER(SG_SCIPHER(crypto),
		   ctr_encrypt, ctr_decrypt, NULL, NULL, ctr_done);
    break;
  default:
    Sg_Error(UC("invalid mode %d"), mode);
    return SG_UNDEF;
  }
  if (err != CRYPT_OK) {
    Sg_Error(UC("%S initialization failed: %A"),
	     name, Sg_MakeStringC(error_to_string(err)));
    return SG_UNDEF;
  }
  Sg_RegisterFinalizer(crypto, finalize_cipher, NULL);
  return SG_OBJ(crypto);
}

SgObject Sg_MakePublicKeyCipher(SgObject name, SgObject key, SgObject encrypter,
				SgObject decrypter, SgObject padder, SgObject signer,
				SgObject verifier)
{
  SgCrypto *crypto = make_crypto(CRYPTO_PUB_CIPHER); 
  SG_PCIPHER(crypto)->name = name;
  SG_PCIPHER(crypto)->key = key;
  SG_PCIPHER(crypto)->encrypter = encrypter;
  SG_PCIPHER(crypto)->decrypter = decrypter;
  SG_PCIPHER(crypto)->padder = padder;
  SG_PCIPHER(crypto)->signer = signer;
  SG_PCIPHER(crypto)->verifier = verifier;
  return SG_OBJ(crypto);
}

int Sg_SuggestKeysize(SgString *name, int keysize)
{
  int cipher = find_cipher(Sg_Utf32sToUtf8s(name));
  int err;
  struct ltc_cipher_descriptor *desc;
  if (cipher == -1) {
    Sg_Error(UC("%A is not supported"), name);
    return -1;
  }
  desc= &cipher_descriptor[cipher];
  if ((err = desc->keysize(&keysize)) != CRYPT_OK) {
    Sg_Error(UC("Failed to get key size: %A"), Sg_MakeStringC(error_to_string(err)));
    return -1;
  }
  return keysize;
}

static SgObject symmetric_encrypt(SgCrypto *crypto, SgByteVector *data)
{
  int len, err;
  SgByteVector *ct;
  ASSERT(SG_CRYPTO(crypto)->type == CRYPTO_SYM_CIPHER);

  if (!SG_FALSEP(SG_SCIPHER(crypto)->padder)) {
    struct ltc_cipher_descriptor *desc
      = &cipher_descriptor[SG_SCIPHER(crypto)->cipher];
    int block_size = desc->block_length;
    data = Sg_Apply3(SG_SCIPHER(crypto)->padder,
		     data, SG_MAKE_INT(block_size), SG_TRUE);
  }
  len = SG_BVECTOR_SIZE(data);
  ct = Sg_MakeByteVector(len, 0);
  err = SG_SCIPHER(crypto)->encrypt(SG_BVECTOR_ELEMENTS(data),
				    SG_BVECTOR_ELEMENTS(ct),
				    len,
				    &SG_SCIPHER(crypto)->skey);
  if (err != CRYPT_OK) {
    Sg_Error(UC("%A"), error_to_string(err));
    return SG_UNDEF;
  }
  return SG_OBJ(ct);
}

static SgObject public_key_encrypt(SgCrypto *crypto, SgByteVector *data)
{
  if (!SG_FALSEP(SG_PCIPHER(crypto)->padder)) {
    data = Sg_Apply2(SG_PCIPHER(crypto)->padder, data, SG_TRUE);
  }
  return Sg_Apply2(SG_PCIPHER(crypto)->encrypter, data, SG_PCIPHER(crypto)->key);
}

SgObject Sg_Encrypt(SgCrypto *crypto, SgByteVector *data)
{
  switch (crypto->type) {
  case CRYPTO_SYM_CIPHER:
    return symmetric_encrypt(crypto, data);
  case CRYPTO_PUB_CIPHER:
    return public_key_encrypt(crypto, data);
  default:
    Sg_Error(UC("encrypt requires cipher, but got %S"), crypto);
  }
  return SG_UNDEF;		/* dummy */
}

static SgObject symmetric_decrypt(SgCrypto *crypto, SgByteVector *data)
{
  uint8_t *key = SG_BVECTOR_ELEMENTS(SG_SECRET_KEY(SG_SCIPHER(crypto)->key));
  int keylen = SG_BVECTOR_SIZE(SG_SECRET_KEY(SG_SCIPHER(crypto)->key));
  int rounds = SG_SCIPHER(crypto)->rounds;
  SgByteVector *pt;
  int len = SG_BVECTOR_SIZE(data), err, block_size = -1;
  ASSERT(SG_CRYPTO(crypto)->type == CRYPTO_SYM_CIPHER);
  switch (SG_SCIPHER(crypto)->mode) {
  case MODE_ECB:
    ecb_start(SG_SCIPHER(crypto)->cipher, key, keylen, rounds,
	      &SG_SCIPHER(crypto)->skey.ecb_key);
    goto entry;
  case MODE_CBC:
    cbc_start(SG_SCIPHER(crypto)->cipher,
	      SG_BVECTOR_ELEMENTS(SG_SCIPHER(crypto)->iv),
	      key, keylen, rounds,
	      &SG_SCIPHER(crypto)->skey.cbc_key);
  entry: {
      struct ltc_cipher_descriptor *desc
	= &cipher_descriptor[SG_SCIPHER(crypto)->cipher];
      block_size = desc->block_length;
    }
    break;
  default: break;
  }
  pt = Sg_MakeByteVector(len, 0);
  err = SG_SCIPHER(crypto)->decrypt(SG_BVECTOR_ELEMENTS(data),
				    SG_BVECTOR_ELEMENTS(pt),
				    len,
				    &SG_SCIPHER(crypto)->skey);
  if (!SG_FALSEP(SG_SCIPHER(crypto)->padder)) {
    /* drop padding */
    pt = Sg_Apply3(SG_SCIPHER(crypto)->padder,
		   pt, SG_MAKE_INT(block_size), SG_FALSE);
  }
  if (err != CRYPT_OK) {
    Sg_Error(UC("%A"), error_to_string(err));
    return SG_UNDEF;
  }
  return SG_OBJ(pt);
}

static SgObject public_key_decrypt(SgCrypto *crypto, SgByteVector *data)
{
  data = Sg_Apply2(SG_PCIPHER(crypto)->decrypter, data, SG_PCIPHER(crypto)->key);
  if (!SG_FALSEP(SG_PCIPHER(crypto)->padder)) {
    data = Sg_Apply2(SG_PCIPHER(crypto)->padder, data, SG_FALSE);
  }
  return SG_OBJ(data);
}


SgObject Sg_Decrypt(SgCrypto *crypto, SgByteVector *data)
{
  switch (crypto->type) {
  case CRYPTO_SYM_CIPHER:
    return symmetric_decrypt(crypto, data);
  case CRYPTO_PUB_CIPHER:
    return public_key_decrypt(crypto, data);
  default:
    Sg_Error(UC("decrypt requires cipher, but got %S"), crypto);
  }
  return SG_UNDEF;		/* dummy */
}

static SgObject apply_with_option(SgObject proc, SgObject data, SgObject key,
				  SgObject opt)
{

}

SgObject Sg_Signature(SgCrypto *crypto, SgByteVector *data, SgObject opt)
{
  switch (crypto->type) {
  case CRYPTO_SYM_CIPHER:
    Sg_Error(UC("symmetric cipher does not support signing, %S"), crypto);
    break;
  case CRYPTO_PUB_CIPHER: {
    SgObject h = SG_NIL, t = SG_NIL;
    SG_APPEND1(h, t, data);
    SG_APPEND1(h, t, SG_PCIPHER(crypto)->key);
    SG_APPEND(h, t, opt);
    return Sg_Apply(SG_PCIPHER(crypto)->signer, h);
  }
  default:
    Sg_Error(UC("decrypt requires cipher, but got %S"), crypto);
  }
  return SG_UNDEF;		/* dummy */
}

SgObject Sg_Verify(SgCrypto *crypto, SgByteVector *M, SgByteVector *S, SgObject opt)
{
  switch (crypto->type) {
  case CRYPTO_SYM_CIPHER:
    Sg_Error(UC("symmetric cipher does not support verify, %S"), crypto);
    break;
  case CRYPTO_PUB_CIPHER: {
    SgObject h = SG_NIL, t = SG_NIL;
    SG_APPEND1(h, t, M);
    SG_APPEND1(h, t, S);
    SG_APPEND1(h, t, SG_PCIPHER(crypto)->key);
    SG_APPEND(h, t, opt);
    return Sg_Apply(SG_PCIPHER(crypto)->verifier, h);
  }
  default:
    Sg_Error(UC("decrypt requires cipher, but got %S"), crypto);
  }
  return SG_UNDEF;		/* dummy */
}

extern void Sg__Init_sagittarius_crypto_impl();
extern void Sg__InitKey(SgObject lib);

SG_EXTENSION_ENTRY void Sg_Init_sagittarius__crypto()
{
  SgLibrary *lib;
  SG_INIT_EXTENSION(sagittarius__crypto);

  Sg__Init_sagittarius_crypto_impl();
  lib = Sg_FindLibrary(SG_INTERN("(sagittarius crypto impl)"), FALSE);
  Sg__InitKey(lib);
  /* initialize libtomcrypt */
#define REGISTER_CIPHER(cipher)						\
  if (register_cipher(cipher) == -1) {					\
    Sg_Warn(UC("Unable to register %S cipher"), Sg_MakeStringC((cipher)->name)); \
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

  /* put mode */
#define MODE_CONST(name) Sg_InsertBinding(lib, SG_INTERN(#name), SG_MAKE_INT(name))
  MODE_CONST(MODE_ECB);
  MODE_CONST(MODE_CBC);
  MODE_CONST(MODE_CFB);
  MODE_CONST(MODE_OFB);
  MODE_CONST(MODE_CTR);
  MODE_CONST(CTR_COUNTER_LITTLE_ENDIAN);
  MODE_CONST(CTR_COUNTER_BIG_ENDIAN);
  MODE_CONST(LTC_CTR_RFC3686);
}
