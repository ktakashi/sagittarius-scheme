/* -*- C -*- */
/*
 * cache.h: compiled cache.
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
#ifndef SAGITTARIUS_CACHE_H_
#define SAGITTARIUS_CACHE_H_

#include "sagittariusdefs.h"

/* read cache state */
enum {
  CACHE_READ,
  RE_CACHE_NEEDED,
  INVALID_CACHE,
};

/* we need to show this for write cache */
struct cache_ctx_rec
{
  SgHashTable *sharedObjects;
  int          uid;
  /* for pass1 */
  jmp_buf      escape;
  int          index;		/* code builder index */
};
typedef struct cache_ctx_rec SgWriteCacheCtx;

/* for reading cache */

struct read_ctx_rec
{
  SgHashTable *sharedObjects;
  SgHashTable *seen;
  int isLinkNeeded;
  int insnP;			/* for temporary flag */
  SgString    *file;
  jmp_buf      escape;
};
typedef struct read_ctx_rec SgReadCacheCtx;

SG_CDECL_BEGIN

SG_EXTERN int  Sg_WriteCache(SgObject name, SgString *id, SgObject cache);
SG_EXTERN int  Sg_ReadCache(SgString *id);
SG_EXTERN void Sg_CleanCache(SgObject target);

/* cache helper */
SG_EXTERN SgObject Sg_WriteCacheScanRec(SgObject obj, SgObject cbs,
					SgWriteCacheCtx *ctx);
SG_EXTERN void  Sg_WriteObjectCache(SgObject o, SgPort *out,
				    SgWriteCacheCtx *ctx);
SG_EXTERN SgObject Sg_ReadCacheObject(SgPort *p, SgReadCacheCtx *ctx);

SG_CDECL_END

#endif /* SAGITTARIUS_CACHE_H_ */
