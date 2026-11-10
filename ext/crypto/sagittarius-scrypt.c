/* sagittarius-scrypt.c                           -*- mode: c; coding: utf-8; -*-
 *
 *   Copyright (c) 2026  Takashi Kato <ktakashi@ymail.com>
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
#include "sagittarius-scrypt.h"

#include <stddef.h>
#include <stdlib.h>
#include <string.h>

#ifdef noreturn
# define noreturn_save noreturn
# undef noreturn
#endif
#include <tomcrypt.h>
#ifdef noreturn_save
# define noreturn noreturn_save
#endif

#include "sagittarius-salsa-core.h"

#define SCRYPT_WORD_BYTES 64
#define SCRYPT_BLOCK_BYTES 128

static void secure_bzero(void *data, size_t size)
{
  volatile uint8_t *p = (volatile uint8_t *)data;
  while (size-- != 0) {
    *p++ = 0;
  }
}

static void secure_free(uint8_t *data, size_t size)
{
  if (data != NULL) {
    secure_bzero(data, size);
    free(data);
  }
}

static int power_of_two_p(uint64_t n)
{
  return n > 0 && (n & (n - 1)) == 0;
}

static uint64_t parameter_to_u64(SgObject who, SgObject name, SgObject value)
{
  int oor = FALSE;
  uint64_t r = Sg_GetIntegerU64Clamp(value, SG_CLAMP_NONE, &oor);
  if (oor) {
    Sg_AssertionViolation(who,
			  SG_MAKE_STRING("parameter must be non-negative and "
					 "must fit uint64"),
			  SG_LIST2(name, value));
    return 0;
  }
  return r;
}

static int block_size_for_r(uint64_t r, size_t *block_size)
{
  if (r > (uint64_t)(SIZE_MAX / SCRYPT_BLOCK_BYTES)) {
    return FALSE;
  }
  *block_size = (size_t)r * SCRYPT_BLOCK_BYTES;
  return TRUE;
}

static uint64_t integerify(const uint8_t *x, size_t r)
{
  uint64_t v;
  size_t index = ((r << 1) - 1) * SCRYPT_WORD_BYTES;
  LOAD64L(v, x + index);
  return v;
}

static void xor_block(uint8_t *dst, const uint8_t *src, size_t size)
{
  size_t i;
  for (i = 0; i < size; i++) {
    dst[i] ^= src[i];
  }
}

static void scrypt_block_mix_core(const uint8_t *in, uint8_t *out, size_t r)
{
  uint8_t x[SCRYPT_WORD_BYTES];
  size_t i, j;
  size_t total_blocks = r << 1;

  memcpy(x, in + (total_blocks - 1) * SCRYPT_WORD_BYTES, SCRYPT_WORD_BYTES);
  for (i = 0; i < total_blocks; i++) {
    size_t in_offset = i * SCRYPT_WORD_BYTES;
    size_t out_offset = ((i >> 1) + ((i & 1) ? r : 0)) * SCRYPT_WORD_BYTES;
    for (j = 0; j < SCRYPT_WORD_BYTES; j++) {
      x[j] ^= in[in_offset + j];
    }
    sagittarius_salsa_core(x, x, 8);
    memcpy(out + out_offset, x, SCRYPT_WORD_BYTES);
  }
  secure_bzero(x, sizeof(x));
}

SgObject Sg_ScryptBlockMixX(SgObject bv, SgObject r)
{
  SgObject who = SG_INTERN("scrypt-block-mix!");
  uint64_t r_u64 = parameter_to_u64(who, SG_MAKE_STRING("r"), r);
  size_t block_size;
  size_t actual_size = (size_t)SG_BVECTOR_SIZE(bv);
  uint8_t *workspace;

  if (r_u64 == 0) {
    Sg_AssertionViolation(who,
			  SG_MAKE_STRING("r must be greater than 0"),
			  SG_LIST1(r));
    return SG_UNDEF;
  }
  if (!block_size_for_r(r_u64, &block_size)) {
    Sg_AssertionViolation(who,
			  SG_MAKE_STRING("r is too large for the platform"),
			  SG_LIST1(r));
    return SG_UNDEF;
  }
  if (actual_size != block_size) {
    Sg_AssertionViolation(who,
			  SG_MAKE_STRING("input bytevector size must be 128 * r"),
			  SG_LIST3(bv, Sg_MakeIntegerFromU64(actual_size),
				   Sg_MakeIntegerFromU64(block_size)));
    return SG_UNDEF;
  }

  workspace = (uint8_t *)malloc(block_size);
  if (workspace == NULL) {
    Sg_ImplementationRestrictionViolation(
      who,
      SG_MAKE_STRING("failed to allocate BlockMix working buffer"),
      SG_LIST1(Sg_MakeIntegerFromU64(block_size)));
    return SG_UNDEF;
  }

  scrypt_block_mix_core(SG_BVECTOR_ELEMENTS(bv), workspace, (size_t)r_u64);
  memcpy(SG_BVECTOR_ELEMENTS(bv), workspace, block_size);
  secure_free(workspace, block_size);
  return bv;
}

SgObject Sg_ScryptRomixX(SgObject bv, SgObject n, SgObject r)
{
  SgObject who = SG_INTERN("scrypt-romix!");
  uint64_t n_u64 = parameter_to_u64(who, SG_MAKE_STRING("n"), n);
  uint64_t r_u64 = parameter_to_u64(who, SG_MAKE_STRING("r"), r);
  size_t n_size_t = (size_t)n_u64;
  size_t block_size;
  size_t v_size;
  size_t actual_size = (size_t)SG_BVECTOR_SIZE(bv);
  size_t i;
  uint8_t *v = NULL;
  uint8_t *x = NULL;
  uint8_t *y = NULL;
  uint8_t *tmp;
  const char *error_message = NULL;
  SgObject error_irritants = SG_NIL;

  if (r_u64 == 0) {
    Sg_AssertionViolation(who,
			  SG_MAKE_STRING("r must be greater than 0"),
			  SG_LIST1(r));
    return SG_UNDEF;
  }
  if (n_u64 <= 1 || !power_of_two_p(n_u64)) {
    Sg_AssertionViolation(who,
			  SG_MAKE_STRING("N must be greater than 1 and power of 2"),
			  SG_LIST1(n));
    return SG_UNDEF;
  }
  if (!block_size_for_r(r_u64, &block_size)) {
    Sg_AssertionViolation(who,
			  SG_MAKE_STRING("r is too large for the platform"),
			  SG_LIST1(r));
    return SG_UNDEF;
  }
  if (actual_size != block_size) {
    Sg_AssertionViolation(who,
			  SG_MAKE_STRING("input bytevector size must be 128 * r"),
			  SG_LIST3(bv, Sg_MakeIntegerFromU64(actual_size),
				   Sg_MakeIntegerFromU64(block_size)));
    return SG_UNDEF;
  }
  if (n_u64 > (uint64_t)(SIZE_MAX / block_size)) {
    Sg_AssertionViolation(
      who,
      SG_MAKE_STRING("N * (128 * r) exceeds platform allocation limits"),
      SG_LIST2(n, r));
    return SG_UNDEF;
  }
  v_size = n_size_t * block_size;

  v = (uint8_t *)malloc(v_size);
  if (v == NULL) {
    error_message = "failed to allocate ROMix V buffer";
    error_irritants = SG_LIST2(Sg_MakeIntegerFromU64(v_size), n);
    goto cleanup;
  }
  x = (uint8_t *)malloc(block_size);
  if (x == NULL) {
    error_message = "failed to allocate ROMix X buffer";
    error_irritants = SG_LIST1(Sg_MakeIntegerFromU64(block_size));
    goto cleanup;
  }
  y = (uint8_t *)malloc(block_size);
  if (y == NULL) {
    error_message = "failed to allocate ROMix Y buffer";
    error_irritants = SG_LIST1(Sg_MakeIntegerFromU64(block_size));
    goto cleanup;
  }

  memcpy(x, SG_BVECTOR_ELEMENTS(bv), block_size);
  for (i = 0; i < n_size_t; i++) {
    memcpy(v + i * block_size, x, block_size);
    scrypt_block_mix_core(x, y, (size_t)r_u64);
    tmp = x;
    x = y;
    y = tmp;
  }

  for (i = 0; i < n_size_t; i++) {
    size_t j = (size_t)(integerify(x, (size_t)r_u64) & (n_u64 - 1));
    xor_block(x, v + j * block_size, block_size);
    scrypt_block_mix_core(x, y, (size_t)r_u64);
    tmp = x;
    x = y;
    y = tmp;
  }

  memcpy(SG_BVECTOR_ELEMENTS(bv), x, block_size);

 cleanup:
  secure_free(v, v_size);
  secure_free(x, block_size);
  secure_free(y, block_size);
  if (error_message != NULL) {
    Sg_ImplementationRestrictionViolation(
      who, Sg_MakeStringC(error_message), error_irritants);
    return SG_UNDEF;
  }
  return bv;
}

extern void Sg__Init_scrypt(SgLibrary *lib);

SG_EXTENSION_ENTRY void CDECL Sg_Init_sagittarius__scrypt()
{
  SgLibrary *lib;
  SG_INIT_EXTENSION(sagittarius__scrypt);
  lib = SG_LIBRARY(Sg_FindLibrary(SG_INTERN("(sagittarius crypto logic scrypt)"),
				  FALSE));
  Sg__Init_scrypt(lib);
}
