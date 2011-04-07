/* -*- C -*- */
/*
 * codec.h
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
#ifndef SAGITTARIUS_CODEC_H_
#define SAGITTARIUS_CODEC_H_

#include "sagittariusdefs.h"

typedef enum {
  UTF_16BE,
  UTF_16LE,
  UTF_16CHECK_BOM,
  UTF_32BE,
  UTF_32LE,
  UTF_32USE_NATIVE_ENDIAN,
  NO_BOM,
} Endianness;

struct SgCodecRec
{
  SG_HEADER;
  int    (*putChar)(SgObject, SgPort*, SgChar, ErrorHandlingMode);
  SgChar (*getChar)(SgObject, SgPort*, ErrorHandlingMode, int);
  SgString  *name;
  Endianness endian;
};

#define SG_CODECP(obj) (SG_PTRP(obj) && IS_TYPE(obj, TC_CODEC))
#define SG_CODEC(obj)  ((SgCodec*)obj)
/* accessor */
#define SG_CODEC_NAME(obj)   (SG_CODEC(obj)->name)
#define SG_CODEC_ENDIAN(obj) (SG_CODEC(obj)->endian)

SG_CDECL_BEGIN

SG_EXTERN SgObject Sg_MakeUtf8Codec();
SG_EXTERN SgObject Sg_MakeUtf16Codec(Endianness endian);
SG_EXTERN SgObject Sg_MakeUtf32Codec(Endianness endian);
SG_EXTERN SgObject Sg_MakeLatin1Codec();

/* check BOM */
SG_EXTERN Endianness Sg_Utf16CheckBOM(SgByteVector *bv);
SG_EXTERN Endianness Sg_Utf32CheckBOM(SgByteVector *bv);

SG_CDECL_END

#endif /* SAGITTARIUS_CODEC_H_ */

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
