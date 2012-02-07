/* -*- mode: c; coding: utf-8; -*- */
/*
 * zlib.h
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
#define LIBSAGITTARIUS_EXT_BODY
#include <sagittarius/extend.h>
#include "zlib.h"

static void zstream_printer(SgObject self, SgPort *port, SgWriteContext *ctx)
{
  SgZStream *z = SG_ZSTREAM(self);
  Sg_Printf(port, UC("#<z-stream %p>"), z->strm);
}

SG_DEFINE_BUILTIN_CLASS_SIMPLE(Sg_ZStreamClass, zstream_printer);

static SgZStream * make_zstream()
{
  z_streamp strm = SG_NEW_ATOMIC2(z_streamp, sizeof(z_stream));
  SgZStream *z = SG_NEW(SgZStream);
  SG_SET_CLASS(z, SG_CLASS_ZSTREAM);
  z->strm = strm;
  return z;
}

SgObject Sg_DeflateInit(int level, int windowBits, int memLevel, int strategy)
{
  SgZStream *zs = make_zstream();
  int r;
  zs->strm->zalloc = NULL;
  zs->strm->zfree = NULL;
  zs->strm->opaque = NULL;
  zs->strm->next_in = NULL;
  zs->strm->avail_in = 0;
  r = deflateInit2(zs->strm, level, Z_DEFLATED, windowBits, memLevel, strategy);

  if (r != Z_OK) {
    /* TODO make zlib error condition */
    Sg_Error(UC("deflateInit2 error: %S"), Sg_MakeStringC(zs->strm->msg));
  }
  return zs;
}

int Sg_DeflateReset(SgZStream *strm)
{
  return deflateReset(strm->strm);
}

int Sg_DeflateSetDictionary(SgZStream *strm, SgByteVector *dict)
{
  return deflateSetDictionary(strm->strm,
			      SG_BVECTOR_ELEMENTS(dict),
			      SG_BVECTOR_SIZE(dict));
}

int Sg_Deflate(SgZStream *strm, SgByteVector *data, SgByteVector *dest,
	       int flush)
{
  int ret;
  strm->strm->next_in = SG_BVECTOR_ELEMENTS(data);
  strm->strm->avail_in = SG_BVECTOR_SIZE(data);
  strm->strm->next_out = SG_BVECTOR_ELEMENTS(dest);
  strm->strm->avail_out = SG_BVECTOR_SIZE(dest);
  ret = deflate(strm->strm, flush);

  return ret;
}

int Sg_DeflateEnd(SgZStream *strm)
{
  return deflateEnd(strm->strm);
}


SgObject Sg_InflateInit(int windowBits)
{
  SgZStream *zs = make_zstream();
  int r;
  zs->strm->zalloc = NULL;
  zs->strm->zfree = NULL;
  zs->strm->opaque = NULL;
  zs->strm->next_in = NULL;
  zs->strm->avail_in = 0;
  r = inflateInit2(zs->strm, windowBits);
  if (r != Z_OK) {
    Sg_Error(UC("inflateInit2 error: %S"), Sg_MakeStringC(zs->strm->msg));
  }
  return zs;
}

int Sg_InflateReset(SgZStream *strm, int windowBits)
{
  int r;
  if (windowBits < 0) {
    r = inflateReset(strm->strm);
  } else {
    r = inflateReset2(strm->strm, windowBits);
  }
  return r;
}

int Sg_InflateSetDictionary(SgZStream *strm, SgByteVector *dict)
{
  return inflateSetDictionary(strm->strm,
			      SG_BVECTOR_ELEMENTS(dict),
			      SG_BVECTOR_SIZE(dict));
}

int Sg_InflateSync(SgZStream *strm)
{
  return inflateSync(strm->strm);
}

int Sg_Inflate(SgZStream *strm, SgByteVector *data, SgByteVector *dest, 
	       int flush)
{
  strm->strm->next_in = SG_BVECTOR_ELEMENTS(data);
  strm->strm->avail_in = SG_BVECTOR_SIZE(data);
  strm->strm->next_out = SG_BVECTOR_ELEMENTS(dest);
  strm->strm->avail_out = SG_BVECTOR_SIZE(dest);
  return inflate(strm->strm, flush);  
}

int Sg_InflateEnd(SgZStream *strm, int flush)
{
  return inflateEnd(strm->strm);
}

static SgObject z_version = SG_UNDEF;

SgObject Sg_ZlibVersion()
{
  return z_version;
}

extern void Sg__Init_sagittarius_zlib();

SG_EXTENSION_ENTRY void Sg_Init_sagittarius__zlib()
{
  SgLibrary *lib;
  SG_INIT_EXTENSION(sagittarius__zlib);

  Sg__Init_sagittarius_zlib();
  lib = SG_LIBRARY(Sg_FindLibrary(SG_SYMBOL(SG_INTERN("(sagittarius zlib)")),
				  FALSE));
#define insert_binding(v)				\
  Sg_MakeBinding(lib, SG_SYMBOL(SG_INTERN(#v)), SG_MAKE_INT(v), TRUE)

insert_binding(Z_NO_FLUSH     );
insert_binding(Z_PARTIAL_FLUSH);
insert_binding(Z_SYNC_FLUSH   );
insert_binding(Z_FULL_FLUSH   );
insert_binding(Z_FINISH       );
insert_binding(Z_BLOCK        );
insert_binding(Z_TREES        );
insert_binding(Z_OK           );
insert_binding(Z_STREAM_END   );
insert_binding(Z_NEED_DICT    );
insert_binding(Z_ERRNO        );
insert_binding(Z_STREAM_ERROR );
insert_binding(Z_DATA_ERROR   );
insert_binding(Z_MEM_ERROR    );
insert_binding(Z_BUF_ERROR    );
insert_binding(Z_VERSION_ERROR);
insert_binding(Z_NO_COMPRESSION     );
insert_binding(Z_BEST_SPEED         );
insert_binding(Z_BEST_COMPRESSION   );
insert_binding(Z_DEFAULT_COMPRESSION);
insert_binding(Z_FILTERED        );
insert_binding(Z_HUFFMAN_ONLY    );
insert_binding(Z_RLE             );
insert_binding(Z_FIXED           );
insert_binding(Z_DEFAULT_STRATEGY);
insert_binding(Z_BINARY );
insert_binding(Z_TEXT   );
insert_binding(Z_ASCII  );
insert_binding(Z_UNKNOWN);

#undef insert_binding
 z_version = SG_MAKE_STRING(ZLIB_VERSION);
 /* well this class should not be used, but for consistancy */
 Sg_InitStaticClassWithMeta(SG_CLASS_ZSTREAM, UC("<z-stream>"), lib, NULL,
			    SG_FALSE, NULL, 0);
}
