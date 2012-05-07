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
#ifndef SAGITTARIUS_CRYPTO_H_
#define SAGITTARIUS_CRYPTO_H_

#include <sagittarius.h>
#include <tomcrypt.h>

typedef enum {
  MODE_ECB,
  MODE_CBC,
  MODE_CFB,
  MODE_OFB,
  MODE_CTR
} SgCryptoMode;


/* key */
typedef enum {
  SG_PUBLIC,
  SG_PRIVATE,
  SG_SECRET,
} SgKeyType;

/*
  27-3-2012 Re-writing
  won't change Scheme APIs.

  Use CLOS class for cipher and keys.
  the hierarchy is like this;

     +------- <crypto> ------+
     |           |           |
  <cipher>  <cipher-spi>   <key>
                 |
        <builtin-cipher-spi>


          +------ <key> ------+
          |                   |
    <symmetric-key>    <asymmetric-key>
          |
  <buitlin-symmetric-key>

  The whole purpose of this hierarchy is to make adding a new algorithm easier.
  The name spi is actually Service Provider Interface but in this case we do 
  not have any other providers, so actually just for algorithm.
 */
/* class declaration */
SG_CLASS_DECL(Sg_CryptoClass);
SG_CLASS_DECL(Sg_CipherClass);
SG_CLASS_DECL(Sg_CipherSpiClass);
SG_CLASS_DECL(Sg_BuiltinCipherSpiClass);
SG_CLASS_DECL(Sg_KeyClass);
SG_CLASS_DECL(Sg_SymmetricKeyClass);
SG_CLASS_DECL(Sg_BuiltinSymmetricKeyClass);
SG_CLASS_DECL(Sg_AsymmetricKeyClass);

#define SG_CLASS_CRYPTO                (&Sg_CryptoClass)
#define SG_CLASS_CIPHER                (&Sg_CipherClass)
#define SG_CLASS_CIPHER_SPI            (&Sg_CipherSpiClass)
#define SG_CLASS_BUILTIN_CIPHER_SPI    (&Sg_BuiltinCipherSpiClass)
#define SG_CLASS_KEY                   (&Sg_KeyClass)
#define SG_CLASS_SYMMETRIC_KEY         (&Sg_SymmetricKeyClass)
#define SG_CLASS_BUILTIN_SYMMETRIC_KEY (&Sg_BuiltinSymmetricKeyClass)
#define SG_CLASS_ASYMMETRIC_KEY        (&Sg_AsymmetricKeyClass)

/* <crypto>, <key> and <asymmetric-key> are abstract */
#define SG_CRYPTOP(obj) SG_XTYPEP(obj, SG_CLASS_CRYPTO)

#define SG_CIPHER(obj)  ((SgCipher*)obj)
#define SG_CIPHERP(obj) SG_XTYPEP(obj, SG_CLASS_CIPHER)

#define SG_CIPHER_SPI(obj)  ((SgCipherSpi*)obj)
#define SG_CIPHER_SPI_P(obj) SG_XTYPEP(obj, SG_CLASS_CIPHER_SPI)

#define SG_BUILTIN_CIPHER_SPI(obj)  ((SgBuiltinCipherSpi*)obj)
#define SG_BUILTIN_CIPHER_SPI_P(obj) SG_XTYPEP(obj, SG_CLASS_BUILTIN_CIPHER_SPI)

#define SG_KEYP(obj) SG_XTYPEP(obj, SG_CLASS_KEY)

#define SG_SYMMETRIC_KEY_P(obj) SG_XTYPEP(obj, SG_CLASS_SYMMETRIC_KEY)

#define SG_BUILTIN_SYMMETRIC_KEY(obj)   ((SgBuiltinSymmetricKey*)obj)
#define SG_BUILTIN_SYMMETRIC_KEY_P(obj)			\
  SG_XTYPEP(obj, SG_CLASS_BUILTIN_SYMMETRIC_KEY)

#define SG_ASYMMETRIC_KEY_P(obj) SG_XTYPEP(obj, SG_CLASS_ASYMMETRIC_KEY)


typedef struct
{
  SG_HEADER;
  SgObject name;
  SgByteVector *secretKey;
} SgBuiltinSymmetricKey;

#define SG_SECRET_KEY(k) (k)->secretKey

/* symmetric key cryptosystem */
typedef int (*encrypt_proc)(const unsigned char *pt,
			    unsigned char *ct,
			    unsigned long len,
			    /* for future convenience */
			    void *skey);
typedef int (*decrypt_proc)(const unsigned char *ct,
			    unsigned char *pt,
			    unsigned long len,
			    /* for future convenience */
			    void *skey);
typedef int (*iv_proc)(unsigned char *IV, unsigned long *len, void *skey);
typedef int (*done_proc)(void *skey);
typedef int (*keysize_proc)(int *keysize);

typedef struct symmetric_cipher_rec_t
{
  SG_HEADER;
  SgObject       name;
  SgCryptoMode   mode;
  SgObject       iv;	    /* bytevector or #f, cf) ECB does not need iv */
  int            cipher;    /* the index into the cipher_descriptor */
  int            rounds;    /* # of round */
  SgObject       padder;    /* to padding */
  SgBuiltinSymmetricKey *key;	/* raw key */
  union {
    symmetric_CBC cbc_key;
    symmetric_CTR ctr_key;
    symmetric_OFB ofb_key;
    symmetric_CFB cfb_key;
    symmetric_ECB ecb_key;
  } skey;
  /* These properties are wrapper for libtomcrypt
     we do not put start function because on CTR and ECB mode it's not the same
     signature.
   */
  /* XXX_encrypt will be in here */
  encrypt_proc encrypt;
  decrypt_proc decrypt;
  /* on ECB mode these will be NULL */
  iv_proc getiv;
  iv_proc setiv;
  /* clean up */
  done_proc done;
  keysize_proc keysize;
} SgBuiltinCipherSpi;

/* hopefully we have enough */
typedef struct public_key_cipher_ret_t
{
  SG_HEADER;
  SgObject name;
  SgObject key;
  SgObject encrypter;
  SgObject decrypter;
  SgObject padder;
  SgObject signer;
  SgObject verifier;
  SgObject keysize;
  SgObject data;
  SgObject blocksize;
} SgCipherSpi;


typedef struct SgCipherRec
{
  SG_INSTANCE_HEADER;
  SgObject spi;
} SgCipher;

#define SG_INIT_CIPHER(cipher, enc, dec, giv, siv, end, keysiz)	\
  do {								\
    (cipher)->encrypt = (encrypt_proc)(enc);			\
    (cipher)->decrypt = (decrypt_proc)(dec);			\
    (cipher)->getiv = (iv_proc)(giv);				\
    (cipher)->setiv = (iv_proc)(siv);				\
    (cipher)->done = (done_proc)(end);				\
    (cipher)->keysize = (keysize_proc)(keysiz);			\
  } while (0);

SgObject Sg_MakeBuiltinCipherSpi(SgString *name, SgCryptoMode mode,
				 SgObject key, SgObject iv, int rounds,
				 SgObject padder, int ctr_mode);
SgObject Sg_MakeCipher(SgObject spi);
int      Sg_SuggestKeysize(SgCipher *cipher, int keysize);
int      Sg_CipherBlockSize(SgCipher *cipher);

SgObject Sg_Encrypt(SgCipher *crypto, SgByteVector *data);
SgObject Sg_Decrypt(SgCipher *crypto, SgByteVector *data);

SgObject Sg_Signature(SgCipher *crypto, SgByteVector *data, SgObject opt);
SgObject Sg_Verify(SgCipher *crypto, SgByteVector *M, SgByteVector *S,
		   SgObject opt);

/* keys */
SgObject Sg_GenerateSecretKey(SgString *name, SgByteVector *key);

/* plugin */
int      Sg_RegisterSpi(SgObject name, SgObject spi);
SgObject Sg_LookupSpi(SgObject name);

#endif /* SAGITTARIUS_CRYPTO_H_ */
