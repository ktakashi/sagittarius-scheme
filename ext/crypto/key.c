/* -*- C -*- */
/*
 * key.c: Key generation.
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
#include "crypto.h"

SgObject Sg_GenerateSecretKey(SgString *type, SgByteVector *key)
{
  int len = SG_BVECTOR_SIZE(key);
  SgCrypto *crypto;
  /* check key length */
  len = Sg_SuggestKeysize(type, len);
  /* check length */
  if (len != SG_BVECTOR_SIZE(key)) {
    /* shorten */
    SgByteVector *tmp = Sg_MakeByteVector(len, 0);
    Sg_ByteVectorCopyX(key, 0, tmp, 0, len);
    key = tmp;
    tmp = NULL;
  }
  crypto = Sg_MakeCrypto(CRYPTO_KEY);
  SG_SECRET_KEY(SG_KEY(crypto)) = key;
  SG_KEY(crypto)->name = SG_SYMBOL(Sg_Intern(type));
  return SG_OBJ(crypto);
}

static SgRecordType KEY_TYPE;
SgObject key_rtd = SG_UNDEF;

void Sg__InitKey(SgObject lib)
{
  SgObject rtd, rcd, nullvec = Sg_MakeVector(0, SG_UNDEF);
  SgObject key = SG_INTERN("key");
  rtd = Sg_MakeRecordTypeDescriptor(key, SG_FALSE, SG_FALSE,
				    FALSE, FALSE, nullvec);
  rcd = Sg_MakeRecordConstructorDescriptor(rtd, SG_FALSE, SG_FALSE);
  SG_INIT_RECORD_TYPE(&KEY_TYPE, key, rtd, rcd);
  Sg_InsertBinding(lib, key, &KEY_TYPE);
  key_rtd = rtd;
}
