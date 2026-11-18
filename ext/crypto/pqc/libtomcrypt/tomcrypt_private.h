/* Compatibility boundary for the pinned, public-domain PQC sources. */
#ifndef SAGITTARIUS_PQC_TOMCRYPT_PRIVATE_H_
#define SAGITTARIUS_PQC_TOMCRYPT_PRIVATE_H_

#include "../../libtomcrypt/src/headers/tomcrypt_private.h"

#define LTC_MLKEM
#define LTC_MLDSA
#define LTC_SLHDSA
#include "tomcrypt_pqc.h"

#define LTC_ARRAY_SIZE(a) (sizeof(a) / sizeof((a)[0]))

enum ltc_pqc_prehash {
  LTC_PQC_PH_SHA256 = 1,
  LTC_PQC_PH_SHA512 = 2,
  LTC_PQC_PH_SHAKE128 = 3,
  LTC_PQC_PH_SHAKE256 = 4
};
int pqc_prehash_oid_der(int, const unsigned char **, unsigned long *);
int pqc_prehash_size(int, unsigned long *);
int pqc_prehash(int, const unsigned char *, unsigned long,
                unsigned char *, unsigned long *);
int pqc_prehash_init(int, hash_state *);
int pqc_prehash_process(int, hash_state *, const unsigned char *, unsigned long);
int pqc_prehash_done(int, hash_state *, unsigned char *, unsigned long *);
int ltc_algname_match(const char *, const char *);

#endif
