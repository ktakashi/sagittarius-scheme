/* LibTomCrypt, modular cryptographic library -- Tom St Denis */
/* SPDX-License-Identifier: Unlicense */

#include "tomcrypt_private.h"

/**
  @file error_to_string.c
  Convert error codes to ASCII strings, Tom St Denis
*/

static const char * const err_2_str[CRYPT_ERR_NUM] =
{
   "CRYPT_OK",
   "CRYPT_ERROR",
   "Non-fatal 'no-operation' requested.",

   "Invalid key size.",
   "Invalid number of rounds for block cipher.",
   "Algorithm failed test vectors.",

   "Buffer overflow.",
   "Invalid input packet.",

   "Invalid number of bits for a PRNG.",
   "Error reading the PRNG.",

   "Invalid cipher specified.",
   "Invalid hash specified.",
   "Invalid PRNG specified.",

   "Out of memory.",

   "Invalid PK key or key type specified for function.",
   "A private PK key is required.",

   "Invalid argument provided.",
   "File Not Found",

   "Invalid PK type.",

   "An overflow of a value was detected/prevented.",

   "An ASN.1 decoding error occurred.",

   "The input was longer than expected.",

   "Invalid size input for PK parameters.",

   "Invalid size for prime.",
   "Invalid padding.",

   "Hash applied to too many bits.",
   "Password context to decrypt key file is missing.",
   "The PEM header was not recognized",
};

LTC_STATIC_ASSERT(correct_err_2_str_size, (sizeof(err_2_str)/sizeof(err_2_str[0])) == CRYPT_ERR_NUM)

/**
   Convert an LTC error code to ASCII
   @param err    The error code
   @return A pointer to the ASCII NUL terminated string for the error or "Invalid error code." if the err code was not valid.
*/
const char *error_to_string(int err)
{
   if (err < 0 || err >= CRYPT_ERR_NUM) {
      return "Invalid error code.";
   }
   return err_2_str[err];
}

