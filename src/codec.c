// -*- C -*-
/*
 * codec.c
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
#include "sagittarius/codec.h"
#include "sagittarius/bytevector.h"
#include "sagittarius/port.h"
#include "sagittarius/error.h"
#include "sagittarius/string.h"
#include "sagittarius/unicode.h"

static int putUtf8Char(SgObject self, SgPort *port, SgChar c, ErrorHandlingMode mode)
{
  uint8_t buf[4];
  int64_t size = Sg_ConvertUcs4ToUtf8(c, buf, mode);

  return (int)(SG_BINARY_PORT(port)->putU8Array(port, buf, size));
}

static SgChar getUtf8Char(SgObject self, SgPort *port, ErrorHandlingMode mode, int checkBOM)
{
  return Sg_ConvertUtf8ToUcs4(port, mode);
}

SgObject Sg_MakeUtf8Codec()
{
  SgCodec* z = SG_NEW(SgCodec);
  SG_SET_HEADER(z, TC_CODEC);
  z->putChar = putUtf8Char;
  z->getChar = getUtf8Char;
  z->name = Sg_MakeString(UC("utf8-codec"), SG_LITERAL_STRING);
  z->endian = NO_BOM;
  return SG_OBJ(z);
}

static int putUtf16Char(SgObject self, SgPort *port, SgChar c, ErrorHandlingMode mode)
{
  uint8_t buf[4];
  int64_t size = Sg_ConvertUcs4ToUtf16(c, buf, mode, SG_CODEC(self)->endian == UTF_16LE);

  return (int)(SG_BINARY_PORT(port)->putU8Array(port, buf, size));
}

static SgChar getUtf16Char(SgObject self, SgPort *port, ErrorHandlingMode mode, int checkBOM)
{
  return Sg_ConvertUtf16ToUcs4(port, mode, SG_CODEC(self), checkBOM);
}

SgObject Sg_MakeUtf16Codec(Endianness endian)
{
  SgCodec* z;
  ASSERT(endian == UTF_16BE || endian == UTF_16LE || endian == UTF_16CHECK_BOM);
  z = SG_NEW(SgCodec);
  SG_SET_HEADER(z, TC_CODEC);
  z->putChar = putUtf16Char;
  z->getChar = getUtf16Char;
  z->name = Sg_MakeString(UC("utf16-codec"), SG_LITERAL_STRING);
  z->endian = endian;
  return SG_OBJ(z);
}

SgObject Sg_MakeUtf32Codec(Endianness endian)
{
  SgCodec* z = SG_NEW(SgCodec);
  SG_SET_HEADER(z, TC_CODEC);
  /* later
  z->putChar = putUtf32Char;
  z->getChar = getUtf32Char;
  */
  z->name = Sg_MakeString(UC("utf32-codec"), SG_LITERAL_STRING);
  z->endian = endian;
  return SG_OBJ(z);
}

SgObject Sg_MakeLatin1Codec()
{
  SgCodec* z = SG_NEW(SgCodec);
  SG_SET_HEADER(z, TC_CODEC);
  /* later
  z->putChar = putLatin1Char;
  z->getChar = getLatin1Char;
  */
  z->name = Sg_MakeString(UC("latin1-codec"), SG_LITERAL_STRING);
  return SG_OBJ(z);
}


Endianness Sg_Utf16CheckBOM(SgByteVector *bv)
{
  if (SG_BVECTOR_SIZE(bv) >= 2) {
    if (SG_BVECTOR_ELEMENT(bv, 0) == 0xFE
	&& SG_BVECTOR_ELEMENT(bv, 1) == 0xFF) {
      return UTF_16BE;
    } else if (SG_BVECTOR_ELEMENT(bv, 0) == 0xFF
	       && SG_BVECTOR_ELEMENT(bv, 1) == 0xFE) {
      return UTF_16LE;
    } else {
      return NO_BOM;
    }
  } else {
    return NO_BOM;
  }
}

Endianness Sg_Utf32CheckBOM(SgByteVector *bv)
{
  if (SG_BVECTOR_SIZE(bv) >= 4) {
    if (SG_BVECTOR_ELEMENT(bv, 0) == 0x00
	&& SG_BVECTOR_ELEMENT(bv, 1) == 0x00
	&& SG_BVECTOR_ELEMENT(bv, 2) == 0xFE
	&& SG_BVECTOR_ELEMENT(bv, 3) == 0xFF) {
      return UTF_32BE;
    } else if (SG_BVECTOR_ELEMENT(bv, 0) == 0xFF
	       && SG_BVECTOR_ELEMENT(bv, 1) == 0xFE
	       && SG_BVECTOR_ELEMENT(bv, 2) == 0x00
	       && SG_BVECTOR_ELEMENT(bv, 3) == 0x00) {
      return UTF_32LE;
    } else {
      return NO_BOM;
    }
  } else {
    return NO_BOM;
  }
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
