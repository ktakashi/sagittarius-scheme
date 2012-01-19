/* -*- C -*- */
/*
 * hash.c
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
#define LIBSAGITTARIUS_BODY
#include "math.h"

static void hash_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<%A>"), SG_HASH(self)->name);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_HashAlgoClass, hash_printer);

static SgHashAlgo* make_hash(int type, SgString *name)
{
  SgHashAlgo *h = SG_NEW(SgHashAlgo);
  SG_SET_CLASS(h, SG_CLASS_HASH);
  h->type = type;
  h->name = name;
  return h;
}

SgObject Sg_MakeHash(SgString *name, SgObject process)
{
  SgHashAlgo *hash;

  if (SG_FALSEP(process)) {
    const char *cname = Sg_Utf32sToUtf8s(name);
    int index = find_hash(cname);
    hash = make_hash(SG_BUILTIN_HASH, name);
    hash->impl.builtin.index = index;
    hash_descriptor[index].init(&hash->impl.builtin.state);
  } else {
    hash = make_hash(SG_CUSTOM_HASH, name);
    hash->impl.custom.state = Sg_Apply4(process, SG_FALSE, SG_FALSE, SG_FALSE,
					SG_INTERN("init"));
    hash->impl.custom.process = process;
  }
  hash->initialized = TRUE;
  return SG_OBJ(hash);
}

int Sg_HashInit(SgHashAlgo *algo)
{
  if (algo->initialized) {
    /* if it's initialized, we do not initialize it twice.
       I'm not sure if it's a good behaviour.
     */
    return FALSE;
  }
  if (algo->type == SG_BUILTIN_HASH) {
    int index = algo->impl.builtin.index;
    hash_descriptor[index].init(&algo->impl.builtin.state);
  } else {
    SgObject process = algo->impl.custom.process;
    algo->impl.custom.state = Sg_Apply4(process, SG_FALSE, SG_FALSE, SG_FALSE,
					SG_INTERN("init"));
  }
  algo->initialized = TRUE;
  return TRUE;
}

void Sg_HashProcess(SgHashAlgo *algo, SgByteVector *in)
{
  if (!algo->initialized) {
    Sg_Error(UC("%A is not initialized"), algo);
  } else {
    if (algo->type == SG_BUILTIN_HASH) {
      int index = algo->impl.builtin.index;
      int err = hash_descriptor[index].process(&algo->impl.builtin.state,
					       SG_BVECTOR_ELEMENTS(in),
					       SG_BVECTOR_SIZE(in));
      if (err != CRYPT_OK) {
	Sg_Error(UC("%A"), Sg_MakeStringC(error_to_string(err)));
      }
    } else {
      SgObject process = algo->impl.custom.process;
      Sg_Apply4(process, algo->impl.custom.state, in, SG_FALSE,
		SG_INTERN("process"));
    } 
  }
}

void Sg_HashDone(SgHashAlgo *algo, SgByteVector *out)
{
  if (!algo->initialized) {
    Sg_Error(UC("%A is not initialized"), algo);
  } else {
    if (algo->type == SG_BUILTIN_HASH) {
      int index = algo->impl.builtin.index;
      int err = hash_descriptor[index].done(&algo->impl.builtin.state,
					    SG_BVECTOR_ELEMENTS(out));
      if (err != CRYPT_OK) {
	Sg_Error(UC("%A"), Sg_MakeStringC(error_to_string(err)));
      }
    } else {
      SgObject process = algo->impl.custom.process;
      Sg_Apply4(process, algo->impl.custom.state, SG_FALSE, out,
		SG_INTERN("done"));
    } 
  }
  algo->initialized = FALSE;
}

SgObject Sg_HashSize(SgHashAlgo *algo)
{
  if (algo->type == SG_BUILTIN_HASH) {
    int index = algo->impl.builtin.index;
    unsigned long size = hash_descriptor[index].hashsize;
    return Sg_MakeIntegerU(size);
  } else {
    SgObject process = algo->impl.custom.process;
    return Sg_Apply4(process, algo->impl.custom.state, SG_FALSE, SG_FALSE,
		     SG_INTERN("size"));
  }
}

SgObject Sg_HashOid(SgHashAlgo *algo)
{
  if (algo->type == SG_BUILTIN_HASH) {
    unsigned int index = algo->impl.builtin.index, i;
    unsigned long *OID = hash_descriptor[index].OID;
    unsigned long len = hash_descriptor[index].OIDlen;
    /* construct oid string */
    SgObject h = SG_NIL, t = SG_NIL;
    SgObject s = SG_UNDEF, cp;	/* return string */
    const SgObject dot = Sg_MakeString(UC("."), SG_LITERAL_STRING);

    if (len == 0) return SG_FALSE; /* given hash algorithm does not have OID */
    for (i = 0; i < len; i++) {
      SG_APPEND1(h, t, Sg_Sprintf(UC("%A"), Sg_MakeIntegerU(OID[i])));
    }
    s = SG_CAR(h);
    SG_FOR_EACH(cp, SG_CDR(h)) {
      s = Sg_StringAppend2(s, dot);
      s = Sg_StringAppend2(s, SG_CAR(cp));
    }
    return s;
  } else {
    SgObject process = algo->impl.custom.process;
    return Sg_Apply4(process, algo->impl.custom.state, SG_FALSE, SG_FALSE,
		     SG_INTERN("oid"));
  }  
}
