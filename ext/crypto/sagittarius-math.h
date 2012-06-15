/* -*- C -*- */
/*
 * random.h: math random library
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
#ifndef SAGITTARIUS_MATH_H_
#define SAGITTARIUS_MATH_H_

/* 
   The reason why random library is in crypto extension is we are using
   tomcrypto pseudo random. However random is just math not cryptographic.
   So we put it here and separate library.
 */
#include <sagittarius.h>
#include <tomcrypt.h>


/* pseudo random number generator */
/* PRNG
              <prng>
           /          \
  <builtin-prng>   <user-prng>

  extra interface <secure-random>

  <builtin-prng> is always <secure-random> 
 */

typedef struct SgBuiltinPrngRec
{
  SG_HEADER;
  SgString  *name;
  int        wprng;
  prng_state prng;
} SgBuiltinPrng;

typedef struct SgUserPrngRec
{
  SG_INSTANCE_HEADER;
  SgObject setSeed;
  SgObject readRandom;
} SgUserPrng;

SG_CLASS_DECL(Sg_PrngClass);
SG_CLASS_DECL(Sg_BuiltinPrngClass);
SG_CLASS_DECL(Sg_UserPrngClass);
SG_CLASS_DECL(Sg_SecureRandomClass);
#define SG_CLASS_PRNG (&Sg_PrngClass)
#define SG_PRNGP(obj) Sg_TypeP(obj, SG_CLASS_PRNG)
#define SG_PRNG       SG_OBJ

#define SG_CLASS_BUILTIN_PRNG (&Sg_BuiltinPrngClass)
#define SG_BUILTIN_PRNG(obj)   ((SgBuiltinPrng *)obj)
#define SG_BUILTIN_PRNG_P(obj) SG_XTYPEP(obj, SG_CLASS_BUILTIN_PRNG)

#define SG_CLASS_USER_PRNG  (&Sg_UserPrngClass)
#define SG_USER_PRNG(obj)   ((SgUserPrng *)obj)
#define SG_USER_PRNG_P(obj) SG_XTYPEP(obj, SG_CLASS_USER_PRNG)

#define SG_CLASS_SECURE_RANDOM  (&Sg_SecureRandomClass)
#define SG_SECURE_RANDOM_P(obj) Sg_TypeP(obj, SG_CLASS_SECURE_RANDOM)

/* hash algorithm
             <hash-algorithm>
             /              \
   <user-hash-algorith> <builtin-hash-algorith>
 */
typedef struct SgBuiltinHashAlgoRec
{
  SG_HEADER;
  SgString  *name;
  int        initialized;
  int        index;
  hash_state state;
} SgBuiltinHashAlgo;

typedef struct SgUserHashAlgoRec
{
  SG_INSTANCE_HEADER;
  SgObject init;
  SgObject process;
  SgObject done;
  SgObject hashSize;
  SgObject blockSize;
  SgObject oid;
  SgObject state;
} SgUserHashAlgo;

SG_CLASS_DECL(Sg_HashAlgoClass);
SG_CLASS_DECL(Sg_BuiltinHashAlgoClass);
SG_CLASS_DECL(Sg_UserHashAlgoClass);
#define SG_CLASS_HASH   (&Sg_HashAlgoClass)
#define SG_HASH_P(obj) Sg_TypeP(obj, SG_CLASS_HASH)

#define SG_CLASS_BUILTIN_HASH  (&Sg_BuiltinHashAlgoClass)
#define SG_BUILTIN_HASH(obj)   ((SgBuiltinHashAlgo *)obj)
#define SG_BUILTIN_HASH_P(obj) SG_XTYPEP(obj, SG_CLASS_BUILTIN_HASH)

#define SG_CLASS_USER_HASH  (&Sg_UserHashAlgoClass)
#define SG_USER_HASH(obj)   ((SgUserHashAlgo *)obj)
#define SG_USER_HASH_P(obj) SG_XTYPEP(obj, SG_CLASS_USER_HASH)

#define SG_HASH_ALGORITHM   SG_OBJ
#define SG_HASH_ALGORITHM_P SG_HASH_P

/* random */
SgObject Sg_MakePseudoRandom(SgString *name, SgObject seed);
SgObject Sg_MakeSecureRandom(SgString *name, int bits);
void     Sg_SetSeed(SgObject prng, SgByteVector *seed);
SgObject Sg_ReadRandomBytes(SgObject prng, int size);
/* for Scheme secure random creation */
SgObject Sg_ReadSysRandom(int bits);

SgObject Sg_MakeHash(SgString *name);
int      Sg_HashInit(SgObject algo);
void     Sg_HashProcess(SgObject algo, SgByteVector *in);
void     Sg_HashDone(SgObject algo, SgByteVector *out);
SgObject Sg_HashSize(SgObject algo);
SgObject Sg_HashBlockSize(SgObject algo);
SgObject Sg_HashOid(SgObject algo);

/* registers */
int      Sg_RegisterHash(SgObject name, SgObject algo);
SgObject Sg_LookupHash(SgObject name);

int      Sg_RegisterPrng(SgObject name, SgObject prng);
SgObject Sg_LookupPrng(SgObject name);

#endif /* SAGITTARIUS_MATH_H_ */
