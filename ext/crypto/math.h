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

enum {
  SG_BUILTIN_PRNG,
  SG_SECURE_PRNG,
  SG_CUSTOM_PRNG,
};
/* pseudo random number generator */
typedef struct SgPrngRec
{
  SG_HEADER;
  SgString  *name;
  int        type;
  union {
    struct {
      int        wprng;
      prng_state prng;
    } builtin;
    /* we might need state or so, but for now */
    SgObject     readRandom;
  } impl;
} SgPrng;

SG_CLASS_DECL(Sg_PrngClass);
#define SG_CLASS_PRNG   (&Sg_PrngClass)
#define SG_PRNG(obj)   ((SgPrng *)obj)
#define SG_PRNGP(obj) SG_XTYPEP(obj, SG_CLASS_PRNG)

/* hash algorithm */
enum {
  SG_BUILTIN_HASH,
  SG_CUSTOM_HASH,
};
typedef struct SgHashAlgoRec
{
  SG_HEADER;
  SgString *name;
  int       type;
  int       initialized;

  union {
    struct {
      int        index;
      hash_state state;
    } builtin;
    struct {
      SgObject   state;
      /* 
	 process will be (lambda (state in out type) ...)
	 state = hash state
	 in    = if type = 'process' bytevector, otherwise #f
	 out   = if type = 'done bytevector, otherwise #f
	 type  = must be one of them, 'init 'process 'done or 'size
	 on 'init, process must return a state.
       */
      SgObject   process;
    } custom;
  } impl;
} SgHashAlgo;

SG_CLASS_DECL(Sg_HashAlgoClass);
#define SG_CLASS_HASH   (&Sg_HashAlgoClass)
#define SG_HASH(obj)   ((SgHashAlgo *)obj)
#define SG_HASH_P(obj) SG_XTYPEP(obj, SG_CLASS_HASH)

#define SG_HASH_ALGO   SG_HASH
#define SG_HASH_ALGO_P SG_HASH_P

/* random */
SgObject Sg_MakePseudoRandom(SgString *name, SgObject seed);
SgObject Sg_MakeSecureRandom(SgString *name, int bits);
void     Sg_SetSeed(SgPrng *prng, SgByteVector *seed);
SgObject Sg_ReadRandomBytes(SgPrng *prng, int size);
SgObject Sg_MakeCustomPrng(SgString *name, SgObject readRandom);

SgObject Sg_MakeHash(SgString *name, SgObject process);
int      Sg_HashInit(SgHashAlgo *algo);
void     Sg_HashProcess(SgHashAlgo *algo, SgByteVector *in);
void     Sg_HashDone(SgHashAlgo *algo, SgByteVector *out);
SgObject Sg_HashSize(SgHashAlgo *algo);
SgObject Sg_HashOid(SgHashAlgo *algo);

#endif /* SAGITTARIUS_MATH_H_ */
