/* -*- C -*- */
/*
 * hash.c
 *
 *   Copyright (c) 2010-2015  Takashi Kato <ktakashi@ymail.com>
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
#include "sagittarius-math.h"

static SgObject hash_allocate(SgClass *klass, SgObject initargs);

static void hash_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<%A>"), SG_BUILTIN_HASH(self)->name);
}

SG_DEFINE_ABSTRACT_CLASS(Sg_HashAlgoClass, NULL);

SgClass *Sg__HashCPL[] = {
  SG_CLASS_HASH,
  SG_CLASS_TOP,
  NULL,
};

SG_DEFINE_BUILTIN_CLASS(Sg_BuiltinHashAlgoClass, hash_printer,
			NULL, NULL, NULL, Sg__HashCPL);
SG_DEFINE_BASE_CLASS(Sg_UserHashAlgoClass, SgUserHashAlgo,
		     NULL, NULL, NULL, hash_allocate, Sg__HashCPL);

static SgObject hash_allocate(SgClass *klass, SgObject initargs)
{
  SgUserHashAlgo *algo = SG_ALLOCATE(SgUserHashAlgo, klass);
  SG_SET_CLASS(algo, klass);
  return SG_OBJ(algo);
}

static SgBuiltinHashAlgo* make_hash(SgString *name)
{
  SgBuiltinHashAlgo *h = SG_NEW(SgBuiltinHashAlgo);
  SG_SET_CLASS(h, SG_CLASS_BUILTIN_HASH);
  h->name = name;
  return h;
}

SgObject Sg_MakeHash(SgString *name)
{
  SgBuiltinHashAlgo *hash;

  const char *cname = Sg_Utf32sToUtf8s(name);
  int index = find_hash(cname);
  if (index < 0) {
    Sg_Error(UC("non supported hash name %A"), name);
  }
  hash = make_hash(name);
  hash->index = index;
  hash_descriptor[index].init(&hash->state);
  hash->initialized = TRUE;
  return SG_OBJ(hash);
}

SgObject Sg_VMHashInit(SgObject algo)
{
  if (SG_BUILTIN_HASH_P(algo)) {
    int index;
    if (SG_BUILTIN_HASH(algo)->initialized) {
	/* if it's initialized, we do not initialize it twice.
	   I'm not sure if it's a good behaviour.
      */
      return SG_FALSE;
    }
    index = SG_BUILTIN_HASH(algo)->index;
    hash_descriptor[index].init(&SG_BUILTIN_HASH(algo)->state);
    SG_BUILTIN_HASH(algo)->initialized = TRUE;
    return SG_TRUE;
  } else {
    /* should check by i'm lazy... */
    return Sg_VMApply1(SG_USER_HASH(algo)->init, algo);
  }
}

SgObject Sg_VMHashProcess(SgObject algo, SgByteVector *in, int start, int end)
{
  int len = SG_BVECTOR_SIZE(in);
  SG_CHECK_START_END(start, end, len);
  if (SG_BUILTIN_HASH_P(algo)) {
    if (!SG_BUILTIN_HASH(algo)->initialized) {
      Sg_Error(UC("%A is not initialized"), algo);
    } else {
      int index = SG_BUILTIN_HASH(algo)->index;
      int err = hash_descriptor[index].process(&SG_BUILTIN_HASH(algo)->state,
					       SG_BVECTOR_ELEMENTS(in)+start,
					       end-start);
      if (err != CRYPT_OK) {
	Sg_Error(UC("%A"), Sg_MakeStringC(error_to_string(err)));
      }
    }
    return SG_UNDEF;
  } else {
    SgObject proc = SG_USER_HASH(algo)->process;
    /* return value should be checked but i'm lazy... */
    if (SG_PROCEDURE_REQUIRED(proc) == 2) {
      /* copy in if needed */
      if (start && end != len) {
	in = SG_BVECTOR(Sg_ByteVectorCopy(SG_BVECTOR(in), start, end));
      }
      return Sg_VMApply2(proc, algo, in);
    } else {
      /* new interface */
      return Sg_VMApply4(proc, algo, in, 
			 SG_MAKE_INT(start), SG_MAKE_INT(end));
    }
  }
}

static SgObject copy_out(SgObject result, void **data)
{
  SgByteVector *oin = SG_BVECTOR(data[0]);
  int start = (int)data[1], end = (int)data[2];
  Sg_ByteVectorCopyX(SG_BVECTOR(result), 0,
		     oin, start, end-start);
  return SG_OBJ(oin);
}

SgObject Sg_VMHashDone(SgObject algo, SgByteVector *out, int start, int end)
{
  int len = SG_BVECTOR_SIZE(out);
  SG_CHECK_START_END(start, end, len);
  if (SG_BUILTIN_HASH_P(algo)) {
    if (!SG_BUILTIN_HASH(algo)->initialized) {
      Sg_Error(UC("%A is not initialized"), algo);
    } else {
      int index = SG_BUILTIN_HASH(algo)->index;
      int err, size;
      size = hash_descriptor[index].hashsize;
      if (end-start < size) {
	Sg_Error(UC("hash-done!: Out of range. (%d - %d)"), start, end);
      }
      err = hash_descriptor[index].done(&SG_BUILTIN_HASH(algo)->state,
					SG_BVECTOR_ELEMENTS(out)+start);
      if (err != CRYPT_OK) {
	Sg_Error(UC("%A"), Sg_MakeStringC(error_to_string(err)));
      }
    }
    SG_BUILTIN_HASH(algo)->initialized = FALSE;
    return SG_OBJ(out);
  } else {
    SgObject proc = SG_USER_HASH(algo)->done;
    /* why this was there? */
    /*
    void *d[1];
    d[0] = SG_OBJ(out);
    Sg_VMPushCC(return_out, d, 1);
    */
    /* return value should be checked but i'm lazy... */
    if (SG_PROCEDURE_REQUIRED(proc) == 2) {
      /* copy in if needed 
	 This is only for backward compatibility
       */
      if (start && end != len) {
	void *d[3];
	d[0] = out;		/* save original */
	d[1] = SG_OBJ(start);
	d[2] = SG_OBJ(end);
	Sg_VMPushCC(copy_out, d, 3);
	out = SG_BVECTOR(Sg_ByteVectorCopy(out, start, end));
      }
      return Sg_VMApply2(proc, algo, out);
    } else {
      /* new interface */
      return Sg_VMApply4(proc, algo, out, 
			 SG_MAKE_INT(start), SG_MAKE_INT(end));
    }
  }
}

SgObject Sg_HashBlockSize(SgObject algo)
{
  if (SG_BUILTIN_HASH_P(algo)) {
    int index = SG_BUILTIN_HASH(algo)->index;
    unsigned long size = hash_descriptor[index].blocksize;
    return Sg_MakeIntegerU(size);
  } else {
    return SG_USER_HASH(algo)->blockSize;
  }
}

SgObject Sg_HashSize(SgObject algo)
{
  if (SG_BUILTIN_HASH_P(algo)) {
    int index = SG_BUILTIN_HASH(algo)->index;
    unsigned long size = hash_descriptor[index].hashsize;
    return Sg_MakeIntegerU(size);
  } else {
    return SG_USER_HASH(algo)->hashSize;
  }
}

SgObject Sg_HashOid(SgObject algo)
{

  if (SG_BUILTIN_HASH_P(algo)) {
    unsigned int index = SG_BUILTIN_HASH(algo)->index, i;
    unsigned long *OID = hash_descriptor[index].OID;
    unsigned long len = hash_descriptor[index].OIDlen;
    /* construct oid string */
    SgObject h = SG_NIL, t = SG_NIL;
    SgObject s = SG_UNDEF, cp;	/* return string */
    SgString *dot = SG_MAKE_STRING(".");

    if (len == 0) return SG_FALSE; /* given hash algorithm does not have OID */
    for (i = 0; i < len; i++) {
      SG_APPEND1(h, t, Sg_Sprintf(UC("%A"), Sg_MakeIntegerU(OID[i])));
    }
    s = SG_CAR(h);
    SG_FOR_EACH(cp, SG_CDR(h)) {
      s = Sg_StringAppend2(SG_STRING(s), dot);
      s = Sg_StringAppend2(SG_STRING(s), SG_STRING(SG_CAR(cp)));
    }
    return s;
  } else {
    return SG_USER_HASH(algo)->oid;
  }  
}

struct table_entry_t
{
  SgObject name;
  SgObject hash;
  struct table_entry_t *next;
};

static struct
{
  int dummy;
  struct table_entry_t *entries;
} table = { 1, NULL };

/* WATCOM has somehow the same name already */
#define lock lock_
static SgInternalMutex lock;

int Sg_RegisterHash(SgObject name, SgObject algo)
{
  struct table_entry_t *e;
  SgObject r = Sg_LookupHash(name);
  if (!SG_FALSEP(r)) return FALSE;

  Sg_LockMutex(&lock);
  e = SG_NEW(struct table_entry_t);
  e->name = name;
  e->hash = algo;
  e->next = table.entries;
  table.entries = e;
  Sg_UnlockMutex(&lock);
  return TRUE;
}

SgObject Sg_LookupHash(SgObject name)
{
  struct table_entry_t *all;
  Sg_LockMutex(&lock);
  for (all = table.entries; all; all = all->next) {
    if (Sg_EqualP(name, all->name)) {
      Sg_UnlockMutex(&lock);
      return all->hash;
    }
  }
  Sg_UnlockMutex(&lock);
  /* now we need to check builtin */
  if (SG_STRINGP(name)) {
    const char *cname = Sg_Utf32sToUtf8s(SG_STRING(name));
    if(find_hash(cname) != -1) return SG_TRUE;
  }
  return SG_FALSE;
}

/* slots */
static SgObject uh_init(SgUserHashAlgo *algo)
{
  return algo->init;
}
static SgObject uh_process(SgUserHashAlgo *algo)
{
  return algo->process;
}
static SgObject uh_done(SgUserHashAlgo *algo)
{
  return algo->done;
}
static SgObject uh_block_size(SgUserHashAlgo *algo)
{
  return algo->blockSize;
}
static SgObject uh_hash_size(SgUserHashAlgo *algo)
{
  return algo->hashSize;
}
static SgObject uh_oid(SgUserHashAlgo *algo)
{
  return algo->oid;
}
static SgObject uh_state(SgUserHashAlgo *algo)
{
  return algo->state;
}

static void uh_init_set(SgUserHashAlgo *algo, SgObject value)
{
  if (!SG_PROCEDUREP(value)) {
    Sg_Error(UC("procdure required, but got %S"), value);
  }
  algo->init = value;
}
static void uh_process_set(SgUserHashAlgo *algo, SgObject value)
{
  if (!SG_PROCEDUREP(value)) {
    Sg_Error(UC("procdure required, but got %S"), value);
  }
  algo->process = value;
}
static void uh_done_set(SgUserHashAlgo *algo, SgObject value)
{
  if (!SG_PROCEDUREP(value)) {
    Sg_Error(UC("procdure required, but got %S"), value);
  }
  algo->done = value;
}
static void uh_block_size_set(SgUserHashAlgo *algo, SgObject value)
{
  if (!SG_EXACT_INTP(value)) {
    Sg_Error(UC("exact non negative integer required, but got %S"), value);
  }
  if (Sg_NegativeP(value)) {
    Sg_Error(UC("exact non negative integer required, but got %S"), value);
  }
  algo->blockSize = value;
}
static void uh_hash_size_set(SgUserHashAlgo *algo, SgObject value)
{
  if (!SG_EXACT_INTP(value)) {
    Sg_Error(UC("exact non negative integer required, but got %S"), value);
  }
  if (Sg_NegativeP(value)) {
    Sg_Error(UC("exact non negative integer required, but got %S"), value);
  }
  algo->hashSize = value;
}

static void uh_oid_set(SgUserHashAlgo *algo, SgObject value)
{
  if (!SG_STRINGP(value) && !SG_FALSEP(value)) {
    Sg_Error(UC("string or #f required, but got %S"), value);
  }
  algo->oid = value;
}
static void uh_state_set(SgUserHashAlgo *algo, SgObject value)
{
  algo->state = value;
}

static SgSlotAccessor user_hash_slots[] = {
  SG_CLASS_SLOT_SPEC("init",       0, uh_init,       uh_init_set),
  SG_CLASS_SLOT_SPEC("process",    1, uh_process,    uh_process_set),
  SG_CLASS_SLOT_SPEC("done",       2, uh_done,       uh_done_set),
  SG_CLASS_SLOT_SPEC("hash-size",  3, uh_hash_size,  uh_hash_size_set),
  SG_CLASS_SLOT_SPEC("block-size", 4, uh_block_size, uh_block_size_set),
  SG_CLASS_SLOT_SPEC("oid",        5, uh_oid,        uh_oid_set),
  SG_CLASS_SLOT_SPEC("state",      6, uh_state,      uh_state_set),
  { { NULL } }
};


void Sg__InitHash(SgLibrary *lib)
{
  Sg_InitMutex(&lock, FALSE);

#define REGISTER_HASH(hash)						\
  if (register_hash(hash) == -1) {					\
    Sg_Warn(UC("Unable to register %S hash algorithm "),		\
	    Sg_MakeStringC((hash)->name));				\
  }
  
  REGISTER_HASH(&whirlpool_desc);
  REGISTER_HASH(&sha512_desc);
  REGISTER_HASH(&sha384_desc);
  REGISTER_HASH(&rmd160_desc);
  REGISTER_HASH(&sha256_desc);
  REGISTER_HASH(&rmd320_desc);
  REGISTER_HASH(&sha224_desc);
  REGISTER_HASH(&tiger_desc);
  REGISTER_HASH(&sha1_desc);
  REGISTER_HASH(&rmd256_desc);
  REGISTER_HASH(&rmd128_desc);
  REGISTER_HASH(&md5_desc);
  REGISTER_HASH(&md4_desc);
  REGISTER_HASH(&md2_desc);

  Sg_InitStaticClass(SG_CLASS_HASH, UC("<hash-algorithm>"), lib, NULL, 0);
  Sg_InitStaticClass(SG_CLASS_BUILTIN_HASH, UC("<builtin-hash-algorithm>"),
		     lib, NULL, 0);
  Sg_InitStaticClass(SG_CLASS_USER_HASH, UC("<user-hash-algorithm>"), lib,
		     user_hash_slots, 0);

}

