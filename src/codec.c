/* -*- C -*- */
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
#include "sagittarius/core.h"
#include "sagittarius/bytevector.h"
#include "sagittarius/port.h"
#include "sagittarius/error.h"
#include "sagittarius/string.h"
#include "sagittarius/symbol.h"
#include "sagittarius/unicode.h"
#include "sagittarius/writer.h"

static SgClass *codec_cpl[] = {
  SG_CLASS_CODEC,
  NULL
};

static void codec_print(SgObject obj, SgPort *port, SgWriteContext *ctx)
{
  Sg_Putuz(port, UC("#<codec "));
  Sg_Puts(port, SG_CODEC_NAME(obj));
  Sg_PutcUnsafe(port, '>');
}

SG_DEFINE_BUILTIN_CLASS(Sg_CodecClass, codec_print, NULL, NULL, NULL,
			codec_cpl);

static SgCodec* make_codec()
{
  SgCodec* z = SG_NEW(SgCodec);
  SG_SET_CLASS(z, SG_CLASS_CODEC);
  z->type = SG_BUILTIN_CODEC;
  return z;
}

/* To handle custom binary port */
/* TODO this macro is also in port.c */
#define SG_CUSTOM_BINARY_PORT(obj)  (SG_CUSTOM_PORT(obj)->impl.bport)
#define put_binary_array(port, buf, size)				\
  do {									\
    if (SG_BINARY_PORTP(port)) {					\
      return (int)(SG_BINARY_PORT(port)->putU8Array(port, buf, size));	\
    } else if (SG_CUSTOM_PORTP(port)) {					\
      ASSERT(SG_CUSTOM_PORT(port)->type == SG_BINARY_CUSTOM_PORT_TYPE);	\
      return (int)(SG_CUSTOM_BINARY_PORT(port)->putU8Array(port, buf, size)); \
    } else {								\
      Sg_Panic("[internal] codec got textual port");			\
      return -1;		/* dummy */				\
    }									\
  } while (0)

static int put_utf8_char(SgObject self, SgPort *port, SgChar c,
			 ErrorHandlingMode mode)
{
  uint8_t buf[4];
  int64_t size = Sg_ConvertUcs4ToUtf8(c, buf, mode);
  put_binary_array(port, buf, size);
}

static SgChar get_utf8_char(SgObject self, SgPort *port,
			    ErrorHandlingMode mode, int checkBOM)
{
  return Sg_ConvertUtf8ToUcs4(port, mode);
}

static int64_t read_utf8(SgObject self, SgPort *port, SgChar *buf, int64_t size,
			 ErrorHandlingMode mode, int checkBOM)
{
  /* for now super naive implementation */
  int64_t i;
  for (i = 0; i < size; i++) {
    buf[i] = get_utf8_char(self, port, mode, checkBOM);
  }
  return i;
}

static int64_t write_utf8(SgObject self, SgPort* port, const SgChar* buf,
			  int64_t size, ErrorHandlingMode mode)
{
  /* for now super naive implementation */
  int64_t i;
  for (i = 0; i < size; i++) {
    put_utf8_char(self, port, buf[i], mode);
  }
  return i;
}

SgObject Sg_MakeUtf8Codec()
{
  SgCodec *z = make_codec();
  SG_CODEC_BUILTIN(z)->putc = put_utf8_char;
  SG_CODEC_BUILTIN(z)->getc = get_utf8_char;
  SG_CODEC_BUILTIN(z)->readc = read_utf8;
  SG_CODEC_BUILTIN(z)->writec = write_utf8;
  SG_CODEC_NAME(z) = Sg_MakeString(UC("utf8-codec"), SG_LITERAL_STRING);
  SG_CODEC_ENDIAN(z) = NO_BOM;
  return SG_OBJ(z);
}

static int put_utf16_char(SgObject self, SgPort *port, SgChar c,
			  ErrorHandlingMode mode)
{
  uint8_t buf[4];
  int littlep = SG_CODEC(self)->impl.builtin.endian == UTF_16LE;
  int64_t size = Sg_ConvertUcs4ToUtf16(c, buf, mode, littlep);
  put_binary_array(port, buf, size);
}

static SgChar get_utf16_char(SgObject self, SgPort *port,
			     ErrorHandlingMode mode, int checkBOM)
{
  return Sg_ConvertUtf16ToUcs4(port, mode, SG_CODEC(self), checkBOM);
}

static int64_t read_utf16(SgObject self, SgPort *port, SgChar *buf,
			  int64_t size, ErrorHandlingMode mode, int checkBOM)
{
  /* for now super naive implementation */
  int64_t i;
  for (i = 0; i < size; i++) {
    buf[i] = get_utf16_char(self, port, mode, checkBOM);
  }
  return i;
}

static int64_t write_utf16(SgObject self, SgPort* port, const SgChar* buf,
			   int64_t size, ErrorHandlingMode mode)
{
  /* for now super naive implementation */
  int64_t i;
  for (i = 0; i < size; i++) {
    put_utf16_char(self, port, buf[i], mode);
  }
  return i;
}


SgObject Sg_MakeUtf16Codec(Endianness endian)
{
  SgCodec* z;
  ASSERT(endian == UTF_16BE || endian == UTF_16LE || endian == UTF_16CHECK_BOM);
  z = make_codec();
  SG_CODEC_BUILTIN(z)->putc = put_utf16_char;
  SG_CODEC_BUILTIN(z)->getc = get_utf16_char;
  SG_CODEC_BUILTIN(z)->readc = read_utf16;
  SG_CODEC_BUILTIN(z)->writec = write_utf16;
  SG_CODEC_NAME(z) = Sg_MakeString(UC("utf16-codec"), SG_LITERAL_STRING);
  SG_CODEC_ENDIAN(z) = endian;
  return SG_OBJ(z);
}


#define decodeError(who)						\
  if (mode == SG_RAISE_ERROR) {						\
    Sg_IOError(SG_IO_DECODE_ERROR, who,					\
	       Sg_Sprintf(UC("invalid encode. %S, %s:%x"),		\
			  self, UC(__FILE__), __LINE__),		\
	       SG_UNDEF, port);						\
  } else if (mode == SG_REPLACE_ERROR) {				\
    return 0xFFFD;							\
  } else {								\
    ASSERT(mode == SG_IGNORE_ERROR);					\
    goto retry;								\
  }


static int put_utf32_char(SgObject self, SgPort *port, SgChar u,
			  ErrorHandlingMode mode)
{
  uint8_t buf[4];
  if (SG_CODEC_ENDIAN(self) == UTF_32LE) {
    buf[0] = u;
    buf[1] = u >> 8;
    buf[2] = u >> 16;
    buf[3] = u >> 24;
  } else {
    buf[0] = u >> 24;
    buf[1] = u >> 16;
    buf[2] = u >> 8;
    buf[3] = u;
  }
  put_binary_array(port, buf, 4);
}

static SgChar get_utf32_char(SgObject self, SgPort *port, 
			     ErrorHandlingMode mode, int checkBOM)
{
  int a, b, c, d;
 retry:
  a = Sg_GetbUnsafe(port);
  if (a == EOF) return EOF;
  b = Sg_GetbUnsafe(port);
  if (b == EOF) {
    decodeError(SG_INTERN("utf32-codec"));
  }
  c = Sg_GetbUnsafe(port);
  if (c == EOF) {
    decodeError(SG_INTERN("utf32-codec"));
  }
  d = Sg_GetbUnsafe(port);
  if (d == EOF) {
    decodeError(SG_INTERN("utf32-codec"));
  }
  if (SG_CODEC_ENDIAN(self) == UTF_32LE) {
    return
      ((uint8_t)a)       |
      ((uint8_t)b) << 8  |
      ((uint8_t)c) << 16 |
      ((uint8_t)d) << 24;
  } else {
    return
      ((uint8_t)d)       |
      ((uint8_t)c) << 8  |
      ((uint8_t)b) << 16 |
      ((uint8_t)a) << 24;
  }
}


static int64_t read_utf32(SgObject self, SgPort *port, SgChar *buf,
			  int64_t size, ErrorHandlingMode mode, int checkBOM)
{
  /* for now super naive implementation */
  int64_t i;
  for (i = 0; i < size; i++) {
    buf[i] = get_utf32_char(self, port, mode, checkBOM);
  }
  return i;
}

static int64_t write_utf32(SgObject self, SgPort* port, const SgChar* buf,
			   int64_t size, ErrorHandlingMode mode)
{
  /* for now super naive implementation */
  int64_t i;
  for (i = 0; i < size; i++) {
    put_utf32_char(self, port, buf[i], mode);
  }
  return i;
}


SgObject Sg_MakeUtf32Codec(Endianness endian)
{
  SgCodec* z = make_codec();
  if (endian == UTF_32USE_NATIVE_ENDIAN) {
#if WORDS_BIGENDIAN
    SG_CODEC_ENDIAN(z) = UTF_32BE;
#else
    SG_CODEC_ENDIAN(z) = UTF_32LE;
#endif
    SG_CODEC_NAME(z) = SG_MAKE_STRING("utf32-codec");
  } else {
    ASSERT(endian == UTF_32LE || endian == UTF_32BE);
#if WORDS_BIGENDIAN
    if (endian == UTF_32BE) {
      SG_CODEC_NAME(z) = SG_MAKE_STRING("utf32-codec");
    } else {
      SG_CODEC_NAME(z) = SG_MAKE_STRING("utf32-codec(little)");
    }
#else
    if (endian == UTF_32BE) {
      SG_CODEC_NAME(z) = SG_MAKE_STRING("utf32-codec(big)");
    } else {
      SG_CODEC_NAME(z) = SG_MAKE_STRING("utf32-codec");
    }
#endif
    SG_CODEC_ENDIAN(z) = endian;
  }
  SG_CODEC_BUILTIN(z)->putc = put_utf32_char;
  SG_CODEC_BUILTIN(z)->getc = get_utf32_char;
  SG_CODEC_BUILTIN(z)->readc = read_utf32;
  SG_CODEC_BUILTIN(z)->writec = write_utf32;
  return SG_OBJ(z);
}

static int put_latin1_char(SgObject self, SgPort *port, SgChar c,
			   ErrorHandlingMode mode)
{
  uint8_t buf[1];
  int size = 0;
  if (c <= 0xFF) {
    buf[0] = c;
    size = 1;
  } else {
    if (mode == SG_RAISE_ERROR) {
      Sg_IOError(SG_IO_ENCODE_ERROR, SG_INTERN("latin-1-codec"),
		 Sg_Sprintf(UC("Invalid encode for latin-1-codec %s:%x\n"), 
			    UC(__FILE__), __LINE__),
		 SG_UNDEF, port);
      return 0;
    } else if (mode == SG_REPLACE_ERROR) {
      buf[0] = '?';
      size = 1;
    } else {
      ASSERT(mode == SG_IGNORE_ERROR);
      size = 0;
    }
  }
  put_binary_array(port, buf, size);
}

static SgChar get_latin1_char(SgObject self, SgPort *port,
			      ErrorHandlingMode mode, int checkBOM)
{
  int f;
 retry:
  f = Sg_GetbUnsafe(port);
  if (f == EOF) return EOF;
  if (f <= 0xFF) {
    return (SgChar)f;
  } else {
    decodeError(SG_INTERN("latin-1-codec"));
  }
  return ' ';
}


static int64_t read_latin1(SgObject self, SgPort *port, SgChar *buf,
			   int64_t size, ErrorHandlingMode mode, int checkBOM)
{
  /* for now super naive implementation */
  int64_t i;
  for (i = 0; i < size; i++) {
    buf[i] = get_latin1_char(self, port, mode, checkBOM);
  }
  return i;
}

static int64_t write_latin1(SgObject self, SgPort* port, const SgChar* buf,
			    int64_t size, ErrorHandlingMode mode)
{
  /* for now super naive implementation */
  int64_t i;
  for (i = 0; i < size; i++) {
    put_latin1_char(self, port, buf[i], mode);
  }
  return i;
}


SgObject Sg_MakeLatin1Codec()
{
  SgCodec* z = make_codec();

  SG_CODEC_BUILTIN(z)->putc = put_latin1_char;
  SG_CODEC_BUILTIN(z)->getc = get_latin1_char;
  SG_CODEC_BUILTIN(z)->readc = read_latin1;
  SG_CODEC_BUILTIN(z)->writec = write_latin1;
  SG_CODEC_NAME(z) = Sg_MakeString(UC("latin1-codec"), SG_LITERAL_STRING);
  return SG_OBJ(z);
}


Endianness Sg_Utf16CheckBOM(SgByteVector *bv)
{
  if (SG_BVECTOR_SIZE(bv) >= 2) {
    if (SG_BVECTOR_ELEMENT(bv, 0) == 0xFE &&
	SG_BVECTOR_ELEMENT(bv, 1) == 0xFF) {
      return UTF_16BE;
    } else if (SG_BVECTOR_ELEMENT(bv, 0) == 0xFF &&
	       SG_BVECTOR_ELEMENT(bv, 1) == 0xFE) {
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

SgObject Sg_MakeCustomCodecSimple(SgObject name, SgObject getc,
				  SgObject putc, SgObject data)
{
  SgCodec *z = make_codec();
  z->type = SG_CUSTOM_CODEC;
  z->name = name;
  SG_CODEC_CUSTOM(z)->putc = putc;
  SG_CODEC_CUSTOM(z)->getc = getc;
  SG_CODEC_CUSTOM(z)->data = data;
  /* TODO default read and write */
  SG_CODEC_CUSTOM(z)->readc  = SG_UNDEF;
  SG_CODEC_CUSTOM(z)->writec = SG_UNDEF;
  return SG_OBJ(z);
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
