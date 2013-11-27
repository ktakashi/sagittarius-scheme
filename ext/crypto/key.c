/* key.c                                           -*- mode:c; coding:utf-8; -*-
 *
 *  Key generation.
 *
 *   Copyright (c) 2010-2013  Takashi Kato <ktakashi@ymail.com>
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
#include <sagittarius/builtin-keywords.h>
#include "crypto.h"

SgClass *Sg__KeyCPL[] = {
  SG_CLASS_SYMMETRIC_KEY,
  SG_CLASS_KEY,
  SG_CLASS_CRYPTO,
  SG_CLASS_TOP,
  NULL,
};
SG_DEFINE_ABSTRACT_CLASS(Sg_KeyClass, Sg__KeyCPL+2);
SG_DEFINE_ABSTRACT_CLASS(Sg_AsymmetricKeyClass, Sg__KeyCPL+1);
SG_DEFINE_ABSTRACT_CLASS(Sg_SymmetricKeyClass, Sg__KeyCPL+1);

static void symmetric_key_print(SgObject self, SgPort *port, 
				SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<%A-key>"), SG_BUILTIN_SYMMETRIC_KEY(self)->name);
}

SG_DEFINE_BUILTIN_CLASS(Sg_BuiltinSymmetricKeyClass, symmetric_key_print,
			NULL, NULL, NULL, Sg__KeyCPL);


static SgBuiltinSymmetricKey *make_skey()
{
  SgBuiltinSymmetricKey *z = SG_NEW(SgBuiltinSymmetricKey);
  SG_SET_CLASS(z, SG_CLASS_BUILTIN_SYMMETRIC_KEY);
  return z;
}

/* for backward compatibility */
SgObject Sg_GenerateSecretKey(SgString *name, SgByteVector *key)
{
  int len = SG_BVECTOR_SIZE(key);
  SgBuiltinSymmetricKey *skey;
  const char *cname = Sg_Utf32sToUtf8s(name);
  int cipher = find_cipher(cname), err;

  /* check key length */
  if ((err = cipher_descriptor[cipher].keysize(&len)) != CRYPT_OK) {
    Sg_Error(UC("Failed to get key size: %A"),
	     Sg_MakeStringC(error_to_string(err)));
    return SG_FALSE;
  }

  /* check length */
  if (len != SG_BVECTOR_SIZE(key)) {
    /* shorten */
    SgByteVector *tmp = SG_BVECTOR(Sg_MakeByteVector(len, 0));
    Sg_ByteVectorCopyX(key, 0, tmp, 0, len);
    key = tmp;
    tmp = NULL;
  }
  skey = make_skey();
  skey->name = name;
  skey->secretKey = key;

  return SG_OBJ(skey);
}

SG_DEFINE_GENERIC(Sg_GenericGenerateSecretKey, Sg_NoNextMethod, NULL);

static SgObject gen_secret_impl(SgObject *args, int argc, void *data)
{
  /* type must be check by here */
  return Sg_GenerateSecretKey(SG_KEYWORD_NAME(args[0]), SG_BVECTOR(args[1]));
}
SG_DEFINE_SUBR(gen_secret, 2, 0, gen_secret_impl, SG_FALSE, NULL);
static SgClass *generate_secret_key_SPEC[] = {
  SG_CLASS_KEYWORD, SG_CLASS_BVECTOR
};
static SG_DEFINE_METHOD(generate_secret_key_rec,
			&Sg_GenericGenerateSecretKey,
			2, 0, generate_secret_key_SPEC, &gen_secret);

SG_CDECL_BEGIN
void Sg__InitKey(SgLibrary *lib)
{
  Sg_InitBuiltinGeneric(&Sg_GenericGenerateSecretKey,
			UC("generate-secret-key"), SG_LIBRARY(lib));
  Sg_InitBuiltinMethod(&generate_secret_key_rec);


  Sg_InitStaticClass(SG_CLASS_KEY, UC("<key>"), lib, NULL, 0);
  Sg_InitStaticClass(SG_CLASS_SYMMETRIC_KEY,
		     UC("<symmetric-key>"), lib, NULL, 0);
  Sg_InitStaticClass(SG_CLASS_BUILTIN_SYMMETRIC_KEY,
		     UC("<builtin-symmetric-key>"), lib, NULL, 0);
  Sg_InitStaticClass(SG_CLASS_ASYMMETRIC_KEY, UC("<asymmetric-key>"),
		     lib, NULL, 0);
}
SG_CDECL_END
