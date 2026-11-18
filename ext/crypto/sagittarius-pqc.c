/* sagittarius-pqc.c - Experimental PQC adapter.
 * SPDX-License-Identifier: BSD-2-Clause
 */
#include <sagittarius.h>
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include <sagittarius/private.h>
#include "sagittarius-pqc.h"
#include "pqc/libtomcrypt/tomcrypt_private.h"

#define BYTES(o) SG_BVECTOR_ELEMENTS(o)
#define LENGTH(o) ((unsigned long)SG_BVECTOR_SIZE(o))

typedef union {
  mlkem_key kem;
  mldsa_key ml;
  slhdsa_key slh;
} pqc_key;

typedef struct {
  unsigned long public_key, private_key, output, seed, randomness;
} pqc_sizes;

static void check_error(const char *who, int error)
{
  if (error != CRYPT_OK) {
    /* Do not include private input, backend codes or intermediate values. */
    Sg_AssertionViolation(Sg_Intern(Sg_MakeStringC(who)),
      SG_MAKE_STRING("Invalid PQC input or native operation failure"), SG_NIL);
  }
}

static int parameter_id(SgWord family, SgWord parameter)
{
  static const int kem[] = { LTC_MLKEM_512, LTC_MLKEM_768, LTC_MLKEM_1024 };
  static const int ml[] = { LTC_MLDSA_44, LTC_MLDSA_65, LTC_MLDSA_87 };
  static const int slh[] = {
    LTC_SLHDSA_SHA2_128S, LTC_SLHDSA_SHA2_128F,
    LTC_SLHDSA_SHA2_192S, LTC_SLHDSA_SHA2_192F,
    LTC_SLHDSA_SHA2_256S, LTC_SLHDSA_SHA2_256F,
    LTC_SLHDSA_SHAKE_128S, LTC_SLHDSA_SHAKE_128F,
    LTC_SLHDSA_SHAKE_192S, LTC_SLHDSA_SHAKE_192F,
    LTC_SLHDSA_SHAKE_256S, LTC_SLHDSA_SHAKE_256F,
    LTC_SLHDSA_HASH_SHA2_128S_WITH_SHA256,
    LTC_SLHDSA_HASH_SHA2_128F_WITH_SHA256,
    LTC_SLHDSA_HASH_SHA2_192S_WITH_SHA512,
    LTC_SLHDSA_HASH_SHA2_192F_WITH_SHA512,
    LTC_SLHDSA_HASH_SHA2_256S_WITH_SHA512,
    LTC_SLHDSA_HASH_SHA2_256F_WITH_SHA512,
    LTC_SLHDSA_HASH_SHAKE_128S_WITH_SHAKE128,
    LTC_SLHDSA_HASH_SHAKE_128F_WITH_SHAKE128,
    LTC_SLHDSA_HASH_SHAKE_192S_WITH_SHAKE256,
    LTC_SLHDSA_HASH_SHAKE_192F_WITH_SHAKE256,
    LTC_SLHDSA_HASH_SHAKE_256S_WITH_SHAKE256,
    LTC_SLHDSA_HASH_SHAKE_256F_WITH_SHAKE256
  };
  if (parameter >= 0) {
    if (family == 0 && parameter < 3) return kem[parameter];
    if (family == 1 && parameter < 3) return ml[parameter];
    if (family == 2 && parameter < 24) return slh[parameter];
  }
  check_error("pqc-parameter", CRYPT_INVALID_ARG);
  return -1;
}

static pqc_sizes get_sizes(SgWord family, SgWord parameter)
{
  pqc_sizes sizes = { 0 };
  int id = parameter_id(family, parameter), error;
  if (family == 0) {
    error = mlkem_get_sizes(id, &sizes.public_key, &sizes.private_key,
                            &sizes.output, NULL, &sizes.seed, &sizes.randomness);
  } else if (family == 1) {
    error = mldsa_get_sizes(id, &sizes.public_key, &sizes.private_key,
                            &sizes.output, &sizes.randomness, NULL, &sizes.seed);
  } else {
    error = slhdsa_get_sizes(id, &sizes.public_key, &sizes.private_key,
                             &sizes.output, &sizes.randomness, &sizes.seed);
  }
  check_error("pqc-sizes", error);
  return sizes;
}

static void clear_secret(void *object, void *data)
{
  (void)data;
  zeromem(BYTES(object), LENGTH(object));
}

SgObject Sg_PQCProtect(SgObject bytes)
{
  Sg_RegisterFinalizer(bytes, clear_secret, NULL);
  return bytes;
}

SgObject Sg_PQCClear(SgObject bytes)
{
  clear_secret(bytes, NULL);
  return SG_UNDEF;
}

static void release_key(int family, pqc_key *key)
{
  if (family == 0) mlkem_free(&key->kem);
  else if (family == 1) mldsa_free(&key->ml);
  else slhdsa_free(&key->slh);
}

static int import_key(int family, int id, SgObject bytes, int privateP,
                      pqc_key *key)
{
  int type = privateP ? PK_PRIVATE : PK_PUBLIC;
  memset(key, 0, sizeof(*key));
  if (family == 0)
    return mlkem_import_raw(BYTES(bytes), LENGTH(bytes), type, id, &key->kem);
  if (family == 1)
    return mldsa_import_raw(BYTES(bytes), LENGTH(bytes), type, id, &key->ml);
  return slhdsa_import_raw(BYTES(bytes), LENGTH(bytes), type, id, &key->slh);
}

static unsigned char *public_bytes(int family, pqc_key *key)
{
  if (family == 0) return key->kem.pk;
  if (family == 1) return key->ml.pk;
  return key->slh.pk;
}

static unsigned char *private_bytes(int family, pqc_key *key)
{
  if (family == 0) return key->kem.sk;
  if (family == 1) return key->ml.sk;
  return key->slh.sk;
}

SgObject Sg_PQCSizes(SgWord family, SgWord parameter)
{
  pqc_sizes s = get_sizes(family, parameter);
  return Sg_Values5(SG_MAKE_INT(s.public_key), SG_MAKE_INT(s.private_key),
                    SG_MAKE_INT(s.output), SG_MAKE_INT(s.seed),
                    SG_MAKE_INT(s.randomness));
}

SgObject Sg_PQCExpand(SgWord family, SgWord parameter, SgObject seed)
{
  pqc_sizes s = get_sizes(family, parameter);
  int id = parameter_id(family, parameter), error;
  pqc_key key = { 0 };
  SgObject public_key = Sg_MakeByteVector(s.public_key, 0);
  SgObject private_key = Sg_PQCProtect(Sg_MakeByteVector(s.private_key, 0));
  if (family == 0)
    error = mlkem_make_key_from_seed(id, BYTES(seed), LENGTH(seed), &key.kem);
  else if (family == 1)
    error = mldsa_make_key_from_seed(id, BYTES(seed), LENGTH(seed), &key.ml);
  else
    error = slhdsa_make_key_from_seed(id, BYTES(seed), LENGTH(seed), &key.slh);
  if (error == CRYPT_OK) {
    memcpy(BYTES(public_key), public_bytes(family, &key), s.public_key);
    memcpy(BYTES(private_key), private_bytes(family, &key), s.private_key);
  }
  release_key(family, &key);
  check_error("pqc-expand", error);
  return Sg_Values2(public_key, private_key);
}

SgObject Sg_PQCImport(SgWord family, SgWord parameter, SgObject bytes, int privateP)
{
  pqc_sizes s = get_sizes(family, parameter);
  int id = parameter_id(family, parameter), error;
  pqc_key key = { 0 };
  SgObject public_key = Sg_MakeByteVector(s.public_key, 0);
  error = import_key(family, id, bytes, privateP, &key);
  if (error == CRYPT_OK)
    memcpy(BYTES(public_key), public_bytes(family, &key), s.public_key);
  release_key(family, &key);
  check_error("pqc-import", error);
  return public_key;
}

SgObject Sg_PQCEncapsulate(SgWord parameter, SgObject bytes, SgObject randomness)
{
  pqc_sizes s = get_sizes(0, parameter);
  int id = parameter_id(0, parameter), error;
  unsigned long ctlen = s.output, sslen = 32;
  pqc_key key = { 0 };
  SgObject ciphertext = Sg_MakeByteVector(ctlen, 0);
  SgObject secret = Sg_PQCProtect(Sg_MakeByteVector(sslen, 0));
  error = import_key(0, id, bytes, FALSE, &key);
  if (error == CRYPT_OK)
    error = mlkem_encaps_ex(BYTES(ciphertext), &ctlen, BYTES(secret), &sslen,
                            BYTES(randomness), LENGTH(randomness), &key.kem);
  release_key(0, &key);
  if (error != CRYPT_OK) Sg_PQCClear(secret);
  check_error("pqc-encapsulate", error);
  return Sg_Values2(ciphertext, secret);
}

SgObject Sg_PQCDecapsulate(SgWord parameter, SgObject bytes, SgObject ciphertext)
{
  int id = parameter_id(0, parameter), error;
  unsigned long sslen = 32;
  pqc_key key = { 0 };
  SgObject secret = Sg_PQCProtect(Sg_MakeByteVector(sslen, 0));
  error = import_key(0, id, bytes, TRUE, &key);
  if (error == CRYPT_OK)
    error = mlkem_decaps(BYTES(secret), &sslen, BYTES(ciphertext),
                         LENGTH(ciphertext), &key.kem);
  release_key(0, &key);
  if (error != CRYPT_OK) Sg_PQCClear(secret);
  check_error("pqc-decapsulate", error);
  return secret;
}

SgObject Sg_PQCSign(SgWord family, SgWord parameter, SgObject bytes, SgObject message,
                    SgObject context, SgObject randomness, int representativeP)
{
  pqc_sizes s = get_sizes(family, parameter);
  int id = parameter_id(family, parameter), error;
  unsigned long siglen = s.output;
  pqc_key key = { 0 };
  SgObject signature = Sg_MakeByteVector(siglen, 0);
  if (family == 0 || LENGTH(context) > 255 ||
      (representativeP && (family != 1 || LENGTH(context) != 0)))
    check_error("pqc-sign", CRYPT_INVALID_ARG);
  error = import_key(family, id, bytes, TRUE, &key);
  if (error == CRYPT_OK && family == 1) {
    if (representativeP)
      error = mldsa_sign_ex_mu(BYTES(message), LENGTH(message), BYTES(signature),
                               &siglen, BYTES(randomness), LENGTH(randomness),
                               &key.ml);
    else
      error = mldsa_sign_ex(BYTES(message), LENGTH(message), BYTES(signature),
                            &siglen, BYTES(context), LENGTH(context),
                            BYTES(randomness), LENGTH(randomness), &key.ml);
  } else if (error == CRYPT_OK) {
    error = slhdsa_sign_ex(BYTES(message), LENGTH(message), BYTES(signature),
                           &siglen, BYTES(context), LENGTH(context),
                           BYTES(randomness), LENGTH(randomness), &key.slh);
  }
  release_key(family, &key);
  check_error("pqc-sign", error);
  return signature;
}

SgObject Sg_PQCVerify(SgWord family, SgWord parameter, SgObject bytes, SgObject message,
                      SgObject context, SgObject signature, int representativeP)
{
  pqc_sizes s = get_sizes(family, parameter);
  int id = parameter_id(family, parameter), error, valid = 0;
  pqc_key key = { 0 };
  if (family == 0 || LENGTH(context) > 255 ||
      (representativeP && (family != 1 || LENGTH(context) != 0)))
    check_error("pqc-verify", CRYPT_INVALID_ARG);
  /* A malformed signature is a verification failure, not an input oracle. */
  if (LENGTH(signature) != s.output) return SG_FALSE;
  error = import_key(family, id, bytes, FALSE, &key);
  if (error == CRYPT_OK && family == 1) {
    if (representativeP)
      error = mldsa_verify_mu(BYTES(signature), LENGTH(signature),
                              BYTES(message), LENGTH(message), &valid, &key.ml);
    else
      error = mldsa_verify(BYTES(signature), LENGTH(signature),
                           BYTES(message), LENGTH(message), BYTES(context),
                           LENGTH(context), &valid, &key.ml);
  } else if (error == CRYPT_OK) {
    error = slhdsa_verify(BYTES(signature), LENGTH(signature),
                          BYTES(message), LENGTH(message), BYTES(context),
                          LENGTH(context), &valid, &key.slh);
  }
  release_key(family, &key);
  check_error("pqc-verify", error);
  return SG_MAKE_BOOL(valid);
}

extern void Sg__Init_pqc(SgLibrary *lib);

SG_EXTENSION_ENTRY void CDECL Sg_Init_sagittarius__pqc()
{
  SgLibrary *lib;
  SG_INIT_EXTENSION(sagittarius__pqc);
  lib = SG_LIBRARY(Sg_FindLibrary(SG_INTERN("(sagittarius crypto logic pqc)"),
                                  FALSE));
  Sg__Init_pqc(lib);
}
