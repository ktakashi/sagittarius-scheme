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
#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "crypto.h"

static void cipher_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<cipher %S>"), SG_CIPHER(self)->spi);
}

SgClass *Sg__CipherCPL[] = {
  SG_CLASS_CRYPTO,
  SG_CLASS_TOP,
  NULL,
};

SgClass *Sg__CipherSpiCPL[] = {
  SG_CLASS_CIPHER_SPI,
  SG_CLASS_CRYPTO,
  SG_CLASS_TOP,
  NULL,
};

SG_DEFINE_ABSTRACT_CLASS(Sg_CryptoClass, NULL);
SG_DEFINE_ABSTRACT_CLASS(Sg_CipherSpiClass, Sg__CipherSpiCPL+1);

SG_DEFINE_BUILTIN_CLASS(Sg_CipherClass, cipher_printer,
			NULL, NULL, NULL, Sg__CipherCPL);


static void builtin_cipher_spi_print(SgObject self, SgPort *port,
				     SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<spi %A>"), SG_BUILTIN_CIPHER_SPI(self)->name);
}
SG_DEFINE_BUILTIN_CLASS(Sg_BuiltinCipherSpiClass, builtin_cipher_spi_print,
			NULL, NULL, NULL, Sg__CipherSpiCPL);

static void finalize_cipher_spi(SgObject obj, void *data)
{
  SG_BUILTIN_CIPHER_SPI(obj)->done(&SG_BUILTIN_CIPHER_SPI(obj)->skey);
}

static SgBuiltinCipherSpi *make_builtin_cipher_spi()
{
  SgBuiltinCipherSpi *c = SG_NEW(SgBuiltinCipherSpi);
  SG_SET_CLASS(c, SG_CLASS_BUILTIN_CIPHER_SPI);
  return c;
}

SgObject Sg_MakeBuiltinCipherSpi(SgString *name, SgCryptoMode mode,
				 SgObject ckey, SgObject iv, int rounds,
				 SgObject padder, int ctr_mode)
{
  const char *cname = Sg_Utf32sToUtf8s(name);
  SgBuiltinCipherSpi *spi = make_builtin_cipher_spi();
  int cipher = find_cipher(cname), err;
  SgByteVector *key;
  keysize_proc keysize;

  ASSERT(SG_BUILTIN_SYMMETRIC_KEY_P(ckey));

  key = SG_BUILTIN_SYMMETRIC_KEY(ckey)->secretKey;
  spi->name = name;
  spi->cipher = cipher;
  spi->key = SG_BUILTIN_SYMMETRIC_KEY(ckey);
  spi->iv = iv;
  spi->mode = mode;
  spi->rounds = rounds;
  spi->padder = padder;

  if (cipher == -1) {
    Sg_Error(UC("%S is not supported"), name);
    return SG_UNDEF;
  }
  keysize = cipher_descriptor[cipher].keysize;

  /* set up mode */
  switch (mode) {
  case MODE_ECB:
    err = ecb_start(cipher, SG_BVECTOR_ELEMENTS(key), SG_BVECTOR_SIZE(key),
		    rounds, &spi->skey.ecb_key);
    SG_INIT_CIPHER(spi, ecb_encrypt, ecb_decrypt,
		   NULL, NULL, ecb_done, keysize);
    break;
  case MODE_CBC:
    if (!SG_BVECTOR(iv)) {
      Sg_Error(UC("iv required on CBC mode"));
      return SG_UNDEF;
    }
    err = cbc_start(cipher, SG_BVECTOR_ELEMENTS(iv),
		    SG_BVECTOR_ELEMENTS(key), SG_BVECTOR_SIZE(key),
		    rounds, &spi->skey.cbc_key);
    SG_INIT_CIPHER(spi,
		   cbc_encrypt, cbc_decrypt, cbc_getiv, cbc_setiv,
		   cbc_done, keysize);
    break;
  case MODE_CFB:
    if (!SG_BVECTOR(iv)) {
      Sg_Error(UC("iv required on CFB mode"));
      return SG_UNDEF;
    }
    err = cfb_start(cipher, SG_BVECTOR_ELEMENTS(iv),
		    SG_BVECTOR_ELEMENTS(key), SG_BVECTOR_SIZE(key),
		    rounds, &spi->skey.cfb_key);
    SG_INIT_CIPHER(spi,
		   cfb_encrypt, cfb_decrypt, cfb_getiv, cfb_setiv,
		   cfb_done, keysize);
    break;
  case MODE_OFB:
    if (!SG_BVECTOR(iv)) {
      Sg_Error(UC("iv required on OFB mode"));
      return SG_UNDEF;
    }
    err =ofb_start(cipher, SG_BVECTOR_ELEMENTS(iv),
		   SG_BVECTOR_ELEMENTS(key), SG_BVECTOR_SIZE(key),
		   rounds, &spi->skey.ofb_key);
    SG_INIT_CIPHER(spi,
		   ofb_encrypt, ofb_decrypt, ofb_getiv, ofb_setiv,
		   ofb_done, keysize);
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
		    &spi->skey.ctr_key);
    SG_INIT_CIPHER(spi,
		   ctr_encrypt, ctr_decrypt, NULL, NULL, ctr_done, keysize);
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
  Sg_RegisterFinalizer(spi, finalize_cipher_spi, NULL);
  return SG_OBJ(spi);
}

SgObject Sg_MakeCipher(SgObject spi)
{
  SgCipher *c = SG_NEW(SgCipher);
  SG_SET_CLASS(c, SG_CLASS_CIPHER);
  c->spi = spi;
  return SG_OBJ(c);
}

int Sg_SuggestKeysize(SgCipher *cipher, int keysize)
{
  SgObject spi = cipher->spi;

  if (SG_BUILTIN_CIPHER_SPI_P(spi)) {
    int err;
    if ((err = SG_BUILTIN_CIPHER_SPI(spi)->keysize(&keysize)) != CRYPT_OK) {
      Sg_Error(UC("Failed to get key size: %A"),
	       Sg_MakeStringC(error_to_string(err)));
      return -1;
    }
    return keysize;
  } else {
    /* must be others */
    SgObject r = Sg_Apply1(SG_CIPHER_SPI(spi)->keysize, SG_MAKE_INT(keysize));
    if (SG_INTP(r)) return SG_INT_VALUE(r);
    else {
      Sg_Error(UC("Failed to get key size: %A"), r);
      return -1;
    }
  }
}

static SgObject symmetric_encrypt(SgCipher *crypto, SgByteVector *d)
{
  int len, err;
  SgObject data = d, ct;	/* cipher text */
  SgBuiltinCipherSpi *spi = SG_BUILTIN_CIPHER_SPI(crypto->spi);

  ASSERT(SG_BUILTIN_CIPHER_SPI_P(spi));

  if (!SG_FALSEP(spi->padder)) {
    struct ltc_cipher_descriptor *desc
      = &cipher_descriptor[spi->cipher];
    int block_size = desc->block_length;
    data = Sg_Apply3(spi->padder, data, SG_MAKE_INT(block_size), SG_TRUE);
  }
  len = SG_BVECTOR_SIZE(data);
  ct = Sg_MakeByteVector(len, 0);
  err = spi->encrypt(SG_BVECTOR_ELEMENTS(data), SG_BVECTOR_ELEMENTS(ct),
		     len, &spi->skey);
  if (err != CRYPT_OK) {
    Sg_Error(UC("%A"), error_to_string(err));
    return SG_UNDEF;
  }
  return SG_OBJ(ct);
}

static SgObject public_key_encrypt(SgCipher *crypto, SgByteVector *d)
{
  SgObject data = d;
  if (!SG_FALSEP(SG_CIPHER_SPI(crypto->spi)->padder)) {
    data = Sg_Apply2(SG_CIPHER_SPI(crypto->spi)->padder, data, SG_TRUE);
  }
  return Sg_Apply2(SG_CIPHER_SPI(crypto->spi)->encrypter, data, 
		   SG_CIPHER_SPI(crypto->spi)->key);
}

SgObject Sg_Encrypt(SgCipher *crypto, SgByteVector *data)
{
  if (SG_BUILTIN_CIPHER_SPI_P(crypto->spi)) {
    return symmetric_encrypt(crypto, data);
  } else {
    return public_key_encrypt(crypto, data);
  }
}

static SgObject symmetric_decrypt(SgCipher *crypto, SgByteVector *data)
{
  SgBuiltinCipherSpi *spi = SG_BUILTIN_CIPHER_SPI(crypto->spi);
  uint8_t *key = SG_BVECTOR_ELEMENTS(SG_SECRET_KEY(spi->key));
  int keylen = SG_BVECTOR_SIZE(SG_SECRET_KEY(spi->key));
  int rounds = spi->rounds;
  SgObject pt;			/* plain text */
  int len = SG_BVECTOR_SIZE(data), err, block_size = -1;

  switch (spi->mode) {
  case MODE_ECB:
    ecb_start(spi->cipher, key, keylen, rounds,
	      &spi->skey.ecb_key);
    goto entry;
  case MODE_CBC:
    cbc_start(spi->cipher,
	      SG_BVECTOR_ELEMENTS(spi->iv),
	      key, keylen, rounds,
	      &spi->skey.cbc_key);
  entry: {
      struct ltc_cipher_descriptor *desc
	= &cipher_descriptor[spi->cipher];
      block_size = desc->block_length;
    }
    break;
  default: break;
  }
  pt = Sg_MakeByteVector(len, 0);
  err = spi->decrypt(SG_BVECTOR_ELEMENTS(data), SG_BVECTOR_ELEMENTS(pt),
		     len, &spi->skey);
  if (err != CRYPT_OK) {
    Sg_Error(UC("%A"), error_to_string(err));
    return SG_UNDEF;
  }

  if (!SG_FALSEP(spi->padder)) {
    /* drop padding */
    pt = Sg_Apply3(spi->padder, pt, SG_MAKE_INT(block_size), SG_FALSE);
  }
  return pt;
}

static SgObject public_key_decrypt(SgCipher *crypto, SgByteVector *data)
{
  SgObject d = SG_OBJ(data);
  d = Sg_Apply2(SG_CIPHER_SPI(crypto->spi)->decrypter, d,
		   SG_CIPHER_SPI(crypto->spi)->key);
  if (!SG_FALSEP(SG_CIPHER_SPI(crypto->spi)->padder)) {
    d = Sg_Apply2(SG_CIPHER_SPI(crypto->spi)->padder, d, SG_FALSE);
  }
  return d;
}


SgObject Sg_Decrypt(SgCipher *crypto, SgByteVector *data)
{
  if (SG_BUILTIN_CIPHER_SPI_P(crypto->spi)) {
    return symmetric_decrypt(crypto, data);
  } else {
    return public_key_decrypt(crypto, data);
  }
}

SgObject Sg_Signature(SgCipher *crypto, SgByteVector *data, SgObject opt)
{
  if (SG_BUILTIN_CIPHER_SPI_P(crypto->spi)) {
    Sg_Error(UC("builtin cipher does not support signing, %S"), crypto);
    return SG_UNDEF;		/* dummy */
  } else {
    SgObject h = SG_NIL, t = SG_NIL;
    ASSERT(SG_CIPHER_SPI(crypto->spi)->signer);
    SG_APPEND1(h, t, data);
    SG_APPEND1(h, t, SG_CIPHER_SPI(crypto->spi)->key);
    SG_APPEND(h, t, opt);
    return Sg_Apply(SG_CIPHER_SPI(crypto->spi)->signer, h);
  }
}

SgObject Sg_Verify(SgCipher *crypto, SgByteVector *M, SgByteVector *S,
		   SgObject opt)
{
  if (SG_BUILTIN_CIPHER_SPI_P(crypto->spi)) {
    Sg_Error(UC("builtin cipher does not support verify, %S"), crypto);
    return SG_UNDEF;		/* dummy */
  } else {
    SgObject h = SG_NIL, t = SG_NIL;
    ASSERT(SG_CIPHER_SPI(crypto->spi)->verifier);
    SG_APPEND1(h, t, M);
    SG_APPEND1(h, t, S);
    SG_APPEND1(h, t, SG_CIPHER_SPI(crypto->spi)->key);
    SG_APPEND(h, t, opt);
    return Sg_Apply(SG_CIPHER_SPI(crypto->spi)->verifier, h);
  }
}

struct table_entry_t
{
  SgObject name;
  SgObject spi;
  struct table_entry_t *next;
};

static struct
{
  int dummy;
  struct table_entry_t *entries;
} table = { 1, NULL };

static SgInternalMutex lock;

int Sg_RegisterSpi(SgObject name, SgObject spiClass)
{
  struct table_entry_t *e;
  SgObject r = Sg_LoookupSpi(name);
  /* already there, we won't overwrite.
     TODO, should we overwrite this?
   */
  if (!SG_FALSEP(r)) return FALSE;

  Sg_LockMutex(&lock);
  e = SG_NEW(struct table_entry_t);
  e->name = name;
  e->spi = spiClass;
  e->next = table.entries;
  table.entries = e;
  Sg_UnlockMutex(&lock);
  return TRUE;
}

SgObject Sg_LoookupSpi(SgObject name)
{
  struct table_entry_t *all;
  Sg_LockMutex(&lock);
  for (all = table.entries; all; all = all->next) {
    if (Sg_EqualP(name, all->name)) {
      Sg_UnlockMutex(&lock);
      return all->spi;
    }
  }
  Sg_UnlockMutex(&lock);
  /* now we need to check builtin */
  if (SG_STRINGP(name)) {
    const char *cname = Sg_Utf32sToUtf8s(SG_STRING(name));
    if(find_cipher(cname) != -1) return SG_TRUE;
  }
  return SG_FALSE;
}



static SgObject ci_name(SgCipherSpi *spi)
{
  return spi->name;
}

static SgObject ci_key(SgCipherSpi *spi)
{
  return spi->key;
}

static SgObject ci_encrypt(SgCipherSpi *spi)
{
  return spi->encrypter;
}

static SgObject ci_decrypt(SgCipherSpi *spi)
{
  return spi->decrypter;
}

static SgObject ci_padder(SgCipherSpi *spi)
{
  return spi->padder;
}

static SgObject ci_signer(SgCipherSpi *spi)
{
  return spi->signer;
}

static SgObject ci_verifier(SgCipherSpi *spi)
{
  return spi->verifier;
}

static SgObject ci_keysize(SgCipherSpi *spi)
{
  return spi->keysize;
}

static SgObject ci_data(SgCipherSpi *spi)
{
  return spi->data;
}

static void ci_name_set(SgCipherSpi *spi, SgObject value)
{
  spi->name = value;
}
static void ci_key_set(SgCipherSpi *spi, SgObject value)
{
  spi->key = value;
}
static void ci_encrypt_set(SgCipherSpi *spi, SgObject value)
{
  spi->encrypter = value;
}
static void ci_decrypt_set(SgCipherSpi *spi, SgObject value)
{
  spi->decrypter = value;
}
static void ci_padder_set(SgCipherSpi *spi, SgObject value)
{
  spi->padder = value;
}
static void ci_signer_set(SgCipherSpi *spi, SgObject value)
{
  spi->signer = value;
}
static void ci_verifier_set(SgCipherSpi *spi, SgObject value)
{
  spi->verifier = value;
}
static void ci_keysize_set(SgCipherSpi *spi, SgObject value)
{
  spi->keysize = value;
}
static void ci_data_set(SgCipherSpi *spi, SgObject value)
{
  spi->data = value;
}

/* slots for cipher-spi */
static SgSlotAccessor cipher_spi_slots[] = {
  SG_CLASS_SLOT_SPEC("name",     0, ci_name,    ci_name_set),
  SG_CLASS_SLOT_SPEC("key",      1, ci_key,     ci_key_set),
  SG_CLASS_SLOT_SPEC("encrypt",  2, ci_encrypt, ci_encrypt_set),
  SG_CLASS_SLOT_SPEC("decrypt",  3, ci_decrypt, ci_decrypt_set),
  SG_CLASS_SLOT_SPEC("padder",   4, ci_padder,  ci_padder_set),
  SG_CLASS_SLOT_SPEC("signer",   5, ci_signer,  ci_signer_set),
  SG_CLASS_SLOT_SPEC("verifier", 6, ci_verifier,ci_verifier_set),
  SG_CLASS_SLOT_SPEC("keysize",  7, ci_keysize, ci_keysize_set),
  SG_CLASS_SLOT_SPEC("data",     8, ci_data,    ci_data_set),
  { { NULL } }
};


static SgObject bci_name(SgBuiltinCipherSpi *spi)
{
  return spi->name;
}

static SgObject invalid_ref(SgBuiltinCipherSpi *spi)
{
  Sg_Error(UC("can not refer builtin spi slots"));
  return SG_UNDEF;		/* dummy */
}

static void invalid_set(SgBuiltinCipherSpi *spi, SgObject value)
{
  Sg_Error(UC("can not set builtin spi slots"));
}

static SgSlotAccessor builtin_cipher_spi_slots[] = {
  SG_CLASS_SLOT_SPEC("name",     0, bci_name,    invalid_set),
  SG_CLASS_SLOT_SPEC("key",      1, invalid_ref, invalid_set),
  SG_CLASS_SLOT_SPEC("encrypt",  2, invalid_ref, invalid_set),
  SG_CLASS_SLOT_SPEC("decrypt",  3, invalid_ref, invalid_set),
  SG_CLASS_SLOT_SPEC("padder",   4, invalid_ref, invalid_set),
  SG_CLASS_SLOT_SPEC("signer",   5, invalid_ref, invalid_set),
  SG_CLASS_SLOT_SPEC("verifier", 6, invalid_ref, invalid_set),
  SG_CLASS_SLOT_SPEC("keysize",  7, invalid_ref, invalid_set),
  SG_CLASS_SLOT_SPEC("data",     8, invalid_ref, invalid_set),
  { { NULL } }
};



extern void Sg__Init_sagittarius_crypto_impl();
SG_CDECL_BEGIN
extern void Sg__InitKey(SgObject lib);
SG_CDECL_END

SG_EXTENSION_ENTRY void CDECL Sg_Init_sagittarius__crypto()
{
  SgLibrary *lib;
  SG_INIT_EXTENSION(sagittarius__crypto);

  Sg__Init_sagittarius_crypto_impl();
  lib = SG_LIBRARY(Sg_FindLibrary(SG_INTERN("(sagittarius crypto impl)"),
				  FALSE));
  Sg__InitKey(lib);

  Sg_InitMutex(&lock, FALSE);
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
#define MODE_CONST(name)					\
  Sg_MakeBinding(lib, SG_INTERN(#name), SG_MAKE_INT(name), TRUE)

  MODE_CONST(MODE_ECB);
  MODE_CONST(MODE_CBC);
  MODE_CONST(MODE_CFB);
  MODE_CONST(MODE_OFB);
  MODE_CONST(MODE_CTR);
  MODE_CONST(CTR_COUNTER_LITTLE_ENDIAN);
  MODE_CONST(CTR_COUNTER_BIG_ENDIAN);
  MODE_CONST(LTC_CTR_RFC3686);

  Sg_InitStaticClass(SG_CLASS_CRYPTO, UC("<crypto>"), lib, NULL, 0);
  Sg_InitStaticClass(SG_CLASS_CIPHER, UC("<cipher>"), lib, NULL, 0);
  /* TODO add slots */
  Sg_InitStaticClass(SG_CLASS_CIPHER_SPI, UC("<cipher-spi>"), lib,
		     cipher_spi_slots, 0);
  /* TODO add dummy slots to raise error */
  Sg_InitStaticClass(SG_CLASS_BUILTIN_CIPHER_SPI,
		     UC("<builtin-cipher-spi>"), lib,
		     builtin_cipher_spi_slots, 0);

  Sg_InitStaticClass(SG_CLASS_KEY, UC("<key>"), lib, NULL, 0);
  Sg_InitStaticClass(SG_CLASS_SYMMETRIC_KEY,
		     UC("<symmetric-key>"), lib, NULL, 0);
  Sg_InitStaticClass(SG_CLASS_BUILTIN_SYMMETRIC_KEY,
		     UC("<bultin-symmetric-key>"), lib, NULL, 0);
  Sg_InitStaticClass(SG_CLASS_ASYMMETRIC_KEY, UC("<asymmetric-key>"),
		     lib, NULL, 0);
}
