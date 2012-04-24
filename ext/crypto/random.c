/* -*- C -*- */
/*
 * random.c
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
#include <sagittarius.h>
#define LIBSAGITTARIUS_BODY
#include <sagittarius/extend.h>
#include "math.h"

static SgObject prng_allocate(SgClass *klass, SgObject initargs);

static void prng_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  Sg_Printf(port, UC("#<prng %A>"), SG_BUILTIN_PRNG(self)->name);
}

SG_DEFINE_ABSTRACT_CLASS(Sg_PrngClass, NULL);

static SgClass *Sg__PrngCPL[] = {
  SG_CLASS_SECURE_RANDOM,
  SG_CLASS_PRNG,
  SG_CLASS_TOP,
  NULL,
};
SG_DEFINE_ABSTRACT_CLASS(Sg_SecureRandomClass, NULL);
SG_DEFINE_BUILTIN_CLASS(Sg_BuiltinPrngClass, prng_printer,
			NULL, NULL, NULL, Sg__PrngCPL);
SG_DEFINE_BASE_CLASS(Sg_UserPrngClass, SgUserPrng,
		     NULL, NULL, NULL, prng_allocate, Sg__PrngCPL+1);

static SgObject prng_allocate(SgClass *klass, SgObject initargs)
{
  SgUserPrng *prng = SG_ALLOCATE(SgUserPrng, klass);
  SG_SET_CLASS(prng, klass);
  return SG_OBJ(prng);
}

static SgBuiltinPrng* make_prng(SgString *name)
{
  SgBuiltinPrng *prng = SG_NEW(SgBuiltinPrng);
  SG_SET_CLASS(prng, SG_CLASS_BUILTIN_PRNG);
  prng->name = name;
  return prng;
}

static void finalize_prng(SgObject prng, void *data)
{
  prng_descriptor[SG_BUILTIN_PRNG(prng)->wprng]
    .done(&SG_BUILTIN_PRNG(prng)->prng);
}

SgObject Sg_MakePseudoRandom(SgString *name, SgObject seed)
{
  const char *cname = Sg_Utf32sToUtf8s(name);
  int wprng = find_prng(cname), err;
  SgBuiltinPrng *prng;
  
  if (wprng == -1) {
    Sg_Error(UC("%A is not supported"), name);
    return SG_UNDEF;
  }

  prng = make_prng(name);
  SG_BUILTIN_PRNG(prng)->wprng = wprng;
  err = prng_descriptor[wprng].start(&prng->prng);
  if (err != CRYPT_OK) goto err;

  err = prng_descriptor[wprng].ready(&prng->prng);
  if (err != CRYPT_OK) goto err;

  if (!SG_FALSEP(seed)) {
    if (SG_BVECTORP(seed)) {
      err = prng_descriptor[wprng]
	.add_entropy(SG_BVECTOR_ELEMENTS(seed), 
		     SG_BVECTOR_SIZE(seed), &prng->prng);
      if (err != CRYPT_OK) goto err;
    } else { goto err; }
  }

  Sg_RegisterFinalizer(prng, finalize_prng, NULL);
  return SG_OBJ(prng);
 err:
  Sg_Error(UC("Failed to initialize pseudo random: %A"),
	   Sg_MakeStringC(error_to_string(err)));
  return SG_UNDEF;
}

void Sg_SetSeed(SgObject prng, SgByteVector *seed)
{
  if (SG_BUILTIN_PRNG_P(prng)) {
    int err;
    err = prng_descriptor[SG_BUILTIN_PRNG(prng)->wprng]
      .add_entropy(SG_BVECTOR_ELEMENTS(seed),
		   SG_BVECTOR_SIZE(seed),
		   &SG_BUILTIN_PRNG(prng)->prng);
    if (err != CRYPT_OK) {
      Sg_Error(UC("Failed to set seed. %A"), seed);
    }
  } else {
    Sg_Apply2(SG_USER_PRNG(prng)->setSeed, prng, seed);
  }
}

SgObject Sg_MakeSecureRandom(SgString *name, int bits)
{
  const char *cname = Sg_Utf32sToUtf8s(name);
  int wprng = find_prng(cname), err;
  SgBuiltinPrng *prng;
  
  if (wprng == -1) {
    Sg_Error(UC("%A is not supported"), name);
    return SG_UNDEF;
  }

  prng = make_prng(name);
  prng->wprng = wprng;

  err = rng_make_prng(bits, wprng, &prng->prng, NULL);
  if (err != CRYPT_OK) {
    Sg_Error(UC("Failed to initialize secure random: %A"),
	     Sg_MakeStringC(error_to_string(err)));
    return SG_UNDEF;
  }
  Sg_RegisterFinalizer(prng, finalize_prng, NULL);
  return SG_OBJ(prng);
}

SgObject Sg_ReadSysRandom(int bits)
{
  SgObject buf;
  bits = ((bits/8)+((bits&7)!=0?1:0)) * 2;
  buf = Sg_MakeByteVector(bits, 0);
  if (rng_get_bytes(SG_BVECTOR_ELEMENTS(buf), (unsigned long)bits, NULL) != 
      (unsigned long)bits) {
    Sg_Error(UC("failed to read system prng"));
  }
  return buf;
}

SgObject Sg_ReadRandomBytes(SgObject prng, int size)
{
  SgObject buf;
  if (SG_BUILTIN_PRNG_P(prng)) {
    buf = Sg_MakeByteVector(size, 0);
    if (prng_descriptor[SG_BUILTIN_PRNG(prng)->wprng]
	.read(SG_BVECTOR_ELEMENTS(buf), size,
	      &SG_BUILTIN_PRNG(prng)->prng) != (unsigned long)size) {
      Sg_Error(UC("read random error"));
      return SG_UNDEF;
    }
  } else {
    buf = Sg_Apply2(SG_USER_PRNG(prng)->readRandom, prng, Sg_MakeInteger(size));
    /* sanity check */
    if (SG_BVECTOR_SIZE(buf) != size) {
      Sg_Error(UC("read-random procedure returned invalid size %d (%S)"),
	       size, prng);
    }
  }
  return buf;
}

/* FIXME this registration code is the same as the one in hash.c.
   must be refactored. */
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

int Sg_RegisterPrng(SgObject name, SgObject algo)
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

SgObject Sg_LookupPrng(SgObject name)
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
    if(find_prng(cname) != -1) return SG_TRUE;
  }
  return SG_FALSE;
}


static SgObject up_seed(SgUserPrng *prng)
{
  return prng->setSeed;
}

static SgObject up_read(SgUserPrng *prng)
{
  return prng->readRandom;
}

static void up_seed_set(SgUserPrng *prng, SgObject value)
{
  if (!SG_PROCEDUREP(value)) {
    Sg_Error(UC("procedure required, but got %S"), value);
  }
  prng->setSeed = value;
}
static void up_read_set(SgUserPrng *prng, SgObject value)
{
  if (!SG_PROCEDUREP(value)) {
    Sg_Error(UC("procedure required, but got %S"), value);
  }
  prng->readRandom = value;
}

static SgSlotAccessor user_prng_slots[] = {
  SG_CLASS_SLOT_SPEC("set-seed!",   0, up_seed, up_seed_set),
  SG_CLASS_SLOT_SPEC("read-random", 1, up_read, up_read_set),
  { { NULL } }
};

void Sg__InitPrng(SgLibrary *lib)
{
  Sg_InitMutex(&lock, FALSE);

#define REGISTER_PRNG(prng)						\
  if (register_prng(prng) == -1) {					\
    Sg_Warn(UC("Unable to register %S pseudo random number generator "), \
	    Sg_MakeStringC((prng)->name));				\
  }

  REGISTER_PRNG(&yarrow_desc);
  REGISTER_PRNG(&fortuna_desc);
  REGISTER_PRNG(&rc4_desc);
  REGISTER_PRNG(&sober128_desc);
  REGISTER_PRNG(&sprng_desc);

  Sg_InitStaticClass(SG_CLASS_PRNG, UC("<prng>"), lib, NULL, 0);
  Sg_InitStaticClass(SG_CLASS_BUILTIN_PRNG, UC("<builtin-prng>"), lib, NULL, 0);
  Sg_InitStaticClass(SG_CLASS_USER_PRNG, UC("<user-prng>"), lib, 
		     user_prng_slots, 0);
  Sg_InitStaticClass(SG_CLASS_PRNG, UC("<secure-random>"), lib, NULL, 0);
}
