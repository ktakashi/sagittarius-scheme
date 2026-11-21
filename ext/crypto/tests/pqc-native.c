/* Standalone sanitizer harness for the vendored raw PQC operations.
 * SPDX-License-Identifier: BSD-2-Clause
 */
#include "../pqc/libtomcrypt/tomcrypt_private.h"
#include <stdio.h>

#define CHECK(expr) do { \
  if (!(expr)) { \
    fprintf(stderr, "PQC native check failed at line %d\n", __LINE__); \
    return 1; \
  } \
} while (0)

int main(void)
{
  unsigned char seed[96] = { 0 }, randomness[32] = { 0 };
  unsigned char signature[49856], ciphertext[1568], secret[32], recovered[32];
  unsigned long length, slen;
  int parameter, valid, i;

  for (parameter = 0; parameter < 3; parameter++) {
    mlkem_key key = { 0 }, imported = { 0 };
    CHECK(mlkem_make_key_from_seed(parameter, seed, 64, &key) == CRYPT_OK);
    CHECK(mlkem_import_raw(key.sk, key.sklen, PK_PRIVATE, parameter, &imported) == CRYPT_OK);
    mlkem_free(&imported);
    length = sizeof(ciphertext);
    slen = sizeof(secret);
    CHECK(mlkem_encaps_ex(ciphertext, &length, secret, &slen,
                          randomness, 32, &key) == CRYPT_OK);
    CHECK(mlkem_decaps(recovered, &slen, ciphertext, length, &key) == CRYPT_OK);
    CHECK(memcmp(secret, recovered, 32) == 0);
    ciphertext[0] ^= 1;
    CHECK(mlkem_decaps(recovered, &slen, ciphertext, length, &key) == CRYPT_OK);
    CHECK(memcmp(secret, recovered, 32) != 0);
    for (i = 0; i < 32; i++) {
      CHECK(mlkem_import_raw(key.sk, i, PK_PRIVATE, parameter, &imported) != CRYPT_OK);
      mlkem_free(&imported);
    }
    mlkem_free(&key);
  }

  for (parameter = 0; parameter < 6; parameter++) {
    mldsa_key key = { 0 }, imported = { 0 };
    CHECK(mldsa_make_key_from_seed(parameter, seed, 32, &key) == CRYPT_OK);
    CHECK(mldsa_import_raw(key.sk, key.sklen, PK_PRIVATE, parameter, &imported) == CRYPT_OK);
    mldsa_free(&imported);
    length = sizeof(signature);
    CHECK(mldsa_sign_ex(seed, 1, signature, &length, seed, 0,
                        randomness, 32, &key) == CRYPT_OK);
    CHECK(mldsa_verify(signature, length, seed, 1, seed, 0, &valid, &key) == CRYPT_OK);
    CHECK(valid);
    for (i = 0; i < 32; i++) {
      signature[i] ^= 1;
      CHECK(mldsa_verify(signature, length, seed, 1, seed, 0, &valid, &key) == CRYPT_OK);
      CHECK(!valid);
      signature[i] ^= 1;
      CHECK(mldsa_import_raw(key.sk, i, PK_PRIVATE, parameter, &imported) != CRYPT_OK);
      mldsa_free(&imported);
    }
    mldsa_free(&key);
  }

  for (parameter = 0; parameter < 24; parameter++) {
    slhdsa_key key = { 0 }, imported = { 0 };
    unsigned long seedlen, rndlen;
    CHECK(slhdsa_get_sizes(parameter, NULL, NULL, NULL, &rndlen, &seedlen) == CRYPT_OK);
    CHECK(slhdsa_make_key_from_seed(parameter, seed, seedlen, &key) == CRYPT_OK);
    CHECK(slhdsa_import_raw(key.sk, key.sklen, PK_PRIVATE, parameter, &imported) == CRYPT_OK);
    slhdsa_free(&imported);
    length = sizeof(signature);
    CHECK(slhdsa_sign_ex(seed, 1, signature, &length, seed, 0,
                         randomness, rndlen, &key) == CRYPT_OK);
    CHECK(slhdsa_verify(signature, length, seed, 1, seed, 0, &valid, &key) == CRYPT_OK);
    CHECK(valid);
    for (i = 0; i < 4; i++) {
      signature[i] ^= 1;
      CHECK(slhdsa_verify(signature, length, seed, 1, seed, 0, &valid, &key) == CRYPT_OK);
      CHECK(!valid);
      signature[i] ^= 1;
      CHECK(slhdsa_import_raw(key.sk, i, PK_PRIVATE, parameter, &imported) != CRYPT_OK);
      slhdsa_free(&imported);
    }
    slhdsa_free(&key);
  }
  puts("PQC native sanitizer checks passed");
  return 0;
}
