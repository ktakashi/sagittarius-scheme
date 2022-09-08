/* sagittarius-digest.h                          -*- mode: c; coding: utf-8; -*-
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

#ifndef SAGITTARIUS_DIGEST_H_
#define SAGITTARIUS_DIGEST_H_

/* 
   New interface for cryptographic library
   The basic idea is from the same as Springkussen.

   In C, we only bind tomcrypt's functions to Scheme and
   all APIs will be implemented in Scheme
 */
#ifdef noreturn
# define noreturn_save noreturn
# undef noreturn
#endif
#include <sagittarius.h>
#include <tomcrypt.h>

#ifdef noreturn_save
# define noreturn noreturn_save
#endif

typedef struct {
  SG_HEADER;
  int md;
  hash_state state;
} SgDigestState;

SG_CLASS_DECL(Sg_DigestStateClass)
#define SG_CLASS_DIGEST_STATE (&Sg_DigestStateClass)

#define SG_DIGEST_STATE(obj) ((SgDigestState *)obj)
#define SG_DIGEST_STATE_P(obj) SG_XTYPEP(obj, SG_CLASS_DIGEST_STATE)
#define SG_DIGEST_STATE_MD(obj) SG_DIGEST_STATE(obj)->md
#define SG_DIGEST_STATE_STATE(obj) SG_DIGEST_STATE(obj)->state

#define DIGEST_DESCRIPTORP(obj) \
  (SG_INTP(obj) && (hash_is_valid(SG_INT_VALUE(obj)) == CRYPT_OK))
#define DIGEST_DESCRIPTOR(md) hash_descriptor[md]
#define DIGEST_DESCRIPTOR_NAME(md) DIGEST_DESCRIPTOR(md).name
#define DIGEST_DESCRIPTOR_DIGEST_SIZE(md) DIGEST_DESCRIPTOR(md).hashsize
#define DIGEST_DESCRIPTOR_BLOCK_SIZE(md) DIGEST_DESCRIPTOR(md).blocksize
#define DIGEST_DESCRIPTOR_OID(md) DIGEST_DESCRIPTOR(md).OID
#define DIGEST_DESCRIPTOR_OIDLEN(md) DIGEST_DESCRIPTOR(md).OIDlen

#define DIGEST_DESCRIPTOR_INIT(md, state) DIGEST_DESCRIPTOR(md).init(state)
#define DIGEST_DESCRIPTOR_PROCESS(md, state, in, len) DIGEST_DESCRIPTOR(md).process(state, in, len)
#define DIGEST_DESCRIPTOR_DONE(md, state, out) DIGEST_DESCRIPTOR(md).done(state, out)

SgObject Sg_DigestDescriptorOID(int md);
SgObject Sg_MakeDigestState(int md);
void Sg_InitDigest(SgLibrary *lib);

#endif /* SAGITTARIUS_DIGEST_H_ */
