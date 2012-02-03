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
/*

 */
typedef enum {
  CRYPTO_SYM_CIPHER,
  CRYPTO_PUB_CIPHER,
  CRYPTO_KEY,
} SgCryptoType;

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

typedef struct SgKeyRec
{
  SgSymbol *name;
  SgKeyType type;
  union {
#if 0
    union {
      dsa_key dsa;
      rsa_key rsa;
    } privateKey, publicKey;	/* both are the same key */
#endif
    /* raw key value */
    SgByteVector *secretKey;
  } impl;
} SgKey;

/* key record type for procedure key? */
extern SgObject key_rtd;

/* for convenience */
#define SG_PRIVATE_KEY(obj) (&((obj)->impl.privateKey))
#define SG_PUBLIC_KEY(obj)  (&((obj)->impl.publicKey))
#define SG_SECRET_KEY(obj)  ((obj)->impl.secretKey)


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

typedef struct symmetric_cipher_rec_t
{
  SgCryptoMode  mode;
  SgObject      iv;   /* bytevector or #f, cf) ECB does not need iv */
  int           cipher;	    /* the index into the cipher_descriptor */
  int           rounds;	    /* # of round */
  SgObject      padder;	    /* to padding */
  SgKey        *key;	    /* raw key */
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
} symmetric_cipher_t;

typedef struct public_key_cipher_ret_t
{
  SgObject name;
  SgObject key;			/* public/private key */
  SgObject encrypter;		/* if key was public */
  SgObject decrypter;		/* if key was private */
  SgObject padder;
  SgObject signer;
  SgObject verifier;
} public_key_cipher_t;


typedef struct SgCryptoRec
{
  SG_HEADER;
  SgCryptoType type;
  union {
    symmetric_cipher_t  scipher;
    public_key_cipher_t pcipher;
    SgKey               key;
  } impl;
} SgCrypto;

SG_CLASS_DECL(Sg_CryptoClass);
#define SG_CLASS_CRYPTO   (&Sg_CryptoClass)
#define SG_CRYPTO(obj)   ((SgCrypto *)obj)
#define SG_CRYPTOP(obj) SG_XTYPEP(obj, SG_CLASS_CRYPTO)
/* accessor */
#define SG_SCIPHER(obj) (&(SG_CRYPTO(obj)->impl.scipher))
#define SG_PCIPHER(obj) (&(SG_CRYPTO(obj)->impl.pcipher))

#define SG_INIT_CIPHER(cipher, enc, dec, giv, siv, end)	\
  do {							\
    (cipher)->encrypt = (encrypt_proc)(enc);		\
    (cipher)->decrypt = (decrypt_proc)(dec);		\
    (cipher)->getiv = (iv_proc)(giv);			\
    (cipher)->setiv = (iv_proc)(siv);			\
    (cipher)->done = (done_proc)(end);			\
  } while (0);


#define SG_KEYP(obj) (SG_CRYPTO(obj)->type == CRYPTO_KEY)
#define SG_KEY(obj)   (&(SG_CRYPTO(obj)->impl.key))

#define argumentAsCrypto(index, tmp_, var_)				\
  castArgumentType(index, tmp_, var_, crypto, SG_CRYPTO_P, SG_CRYPTO)

SgObject Sg_MakeCrypto(SgCryptoType type);

SgObject Sg_MakeSymmetricCipher(SgString *name, SgCryptoMode mode, SgCrypto *key,
				SgObject iv, int rounds, SgObject padder, int ctr_mode);
SgObject Sg_MakePublicKeyCipher(SgObject name, SgObject key, SgObject encrypter,
				SgObject decrypter, SgObject padder, SgObject signer,
				SgObject verifier);
int      Sg_SuggestKeysize(SgString *name, int keysize);

SgObject Sg_Encrypt(SgCrypto *crypto, SgByteVector *data);
SgObject Sg_Decrypt(SgCrypto *crypto, SgByteVector *data);

SgObject Sg_Signature(SgCrypto *crypto, SgByteVector *data, SgObject opt);
SgObject Sg_Verify(SgCrypto *crypto, SgByteVector *M, SgByteVector *S, SgObject opt);

/* keys */
SgObject Sg_GenerateSecretKey(SgString *type, SgByteVector *key);


#endif /* SAGITTARIUS_CRYPTO_H_ */
