/* sagittarius-zlib.h                             -*- mode: c; coding:utf-8; -*-
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
#ifndef SAGITTARIUS_ZLIB_H_
#define SAGITTARIUS_ZLIB_H_

#include <sagittarius.h>
#include <zlib.h>

SG_CLASS_DECL(Sg_ZStreamClass);
#define SG_CLASS_ZSTREAM (&Sg_ZStreamClass)

typedef struct SgZStreamRec
{
  SG_HEADER;
  z_streamp strm;
} SgZStream;

#define SG_ZSTREAM(obj)   ((SgZStream*)obj)
#define SG_ZSTREAM_P(obj) SG_XTYPEP(obj, SG_CLASS_ZSTREAM)

SG_CDECL_BEGIN

/* we only wrap zlib very thin. other Scheme staff must be in Scheme file. */
SgObject Sg_DeflateInit(int level, int windowBits, int memLevel, int strategy);
int      Sg_DeflateReset(SgZStream *strm);
int      Sg_DeflateSetDictionary(SgZStream *strm, SgByteVector *dict);
int      Sg_Deflate(SgZStream *strm, SgByteVector *data, SgByteVector *dest,
		    int flush);
int      Sg_DeflateEnd(SgZStream *strm);

SgObject Sg_InflateInit(int windowBits);
int      Sg_InflateReset(SgZStream *strm, int windowBits);
int      Sg_InflateSetDictionary(SgZStream *strm, SgByteVector *dict);
int      Sg_InflateSync(SgZStream *strm);
int      Sg_Inflate(SgZStream *strm, SgByteVector *data, SgByteVector *dest, 
		    int flush);
int      Sg_InflateEnd(SgZStream *strm, int flush);

/* misc */
SgObject Sg_ZlibVersion();

SG_CDECL_END

#endif /* SAGITTARIUS_ZLIB_H_ */
