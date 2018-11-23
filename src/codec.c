/* codec.c                                         -*- mode:c; coding:utf-8; -*-
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
#define LIBSAGITTARIUS_BODY
#include "sagittarius/codec.h"
#include "sagittarius/core.h"
#include "sagittarius/bytevector.h"
#include "sagittarius/port.h"
#include "sagittarius/error.h"
#include "sagittarius/string.h"
#include "sagittarius/symbol.h"
#include "sagittarius/subr.h"
#include "sagittarius/unicode.h"
#include "sagittarius/writer.h"
#include "sagittarius/vm.h"

static SgClass *codec_cpl[] = {
  SG_CLASS_CODEC,
  SG_CLASS_TOP,
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
#define put_binary_array(port, buf, size)	\
  do {						\
    Sg_WritebUnsafe(port, buf, 0, size);	\
    return (size);				\
  } while (0);

static int put_utf8_char(SgObject self, SgPort *port, SgChar c,
			 SgErrorHandlingMode mode)
{
  uint8_t buf[4];
  int size = Sg_ConvertUcs4ToUtf8(c, buf, mode);
  put_binary_array(port, buf, size);
}

static SgChar get_utf8_char(SgObject self, SgPort *port,
			    SgErrorHandlingMode mode, int checkBOM)
{
  return Sg_ConvertUtf8ToUcs4(port, mode);
}

#define TMP_BUF_SIZE 1024
/* According to Wikipedia, UTF-8 can have max 6 bytes. */
#define MAX_UTF8_SIZE 6

#define DEFINE_READER(cname, convertor, calc)				\
  static int64_t cname(SgObject self, SgPort *port, SgChar *buf,	\
		       int64_t size, SgErrorHandlingMode mode,		\
		       int checkBOM)					\
  {									\
    uint8_t read_buf[TMP_BUF_SIZE] = {0};				\
    int64_t i=0, reading_size, read_size, rest = size, conved;		\
    reading_size = (calc);						\
    if (reading_size > TMP_BUF_SIZE) {					\
      reading_size = TMP_BUF_SIZE;					\
    }									\
    while (1) {								\
      read_size = Sg_ReadbUnsafe(port, read_buf, reading_size);		\
      if (read_size == 0) break;					\
      conved = convertor(SG_CODEC(self), read_buf, read_size,		\
			 buf+i, rest, port, mode, checkBOM);		\
      i += conved;							\
      if (read_size < reading_size) break;				\
      if (size == i) break;						\
      rest -= conved;							\
      if (rest < reading_size) reading_size = rest;			\
    }									\
    return i;								\
  }

/* calculate read size. for UTF-8 we read size, because we don't know
   how many bytes one character may contain. */
DEFINE_READER(read_utf8, Sg_ConvertUtf8BufferToUcs4, size);

static int64_t write_utf8(SgObject self, SgPort* port, SgChar *str,
			  int64_t count, SgErrorHandlingMode mode)
{
  /* we at lease need 'size' size buffer. */
  uint8_t tmp[TMP_BUF_SIZE];
  int converted_size = 0;
  int64_t i;

  /* we can not know the real size until we convert. */
  for (i = 0; i < count; i++) {
    /* put_utf8_char(self, port, buf[i], mode); */
    SgChar c = str[i];
    converted_size += Sg_ConvertUcs4ToUtf8(c, tmp + converted_size, mode);
    if (converted_size >= TMP_BUF_SIZE - MAX_UTF8_SIZE) {
      /* flush */
      Sg_WritebUnsafe(port, tmp, 0, converted_size);
      converted_size = 0;
    }
  }
  if (converted_size != 0) {
    /* flush the rest */
    Sg_WritebUnsafe(port, tmp, 0, converted_size);
  }

  return i;
}

SgObject Sg_MakeUtf8Codec()
{
  /* utf8 codec can be static */
  static SgCodec *z = NULL;
  if (z == NULL) {
    z = make_codec();
    SG_CODEC_BUILTIN(z)->putc = put_utf8_char;
    SG_CODEC_BUILTIN(z)->getc = get_utf8_char;
    SG_CODEC_BUILTIN(z)->readc = read_utf8;
    SG_CODEC_BUILTIN(z)->writec = write_utf8;
    SG_CODEC_NAME(z) = SG_MAKE_STRING("utf8-codec");
    SG_CODEC_ENDIAN(z) = NO_BOM;
  }
  return SG_OBJ(z);
}

#define PUT_BOM(port, endian, littlep)					\
  do {									\
    int64_t mark;							\
    if (SG_BINARY_PORTP(port)) {					\
      mark = SG_PORT(port)->position;					\
    } else {								\
      Sg_Panic("[internal error] codec got textual port");		\
      mark = -1;		/* dummy */				\
    }									\
    if (mark == 0 && endian == UTF_16CHECK_BOM) {			\
      if (littlep) {							\
	uint8_t bom[2] = {0xff, 0xfe};					\
	Sg_WritebUnsafe(port, bom, 0, 2);				\
      } else {								\
	uint8_t bom[2] = {0xfe, 0xff};					\
	Sg_WritebUnsafe(port, bom, 0, 2);				\
      }									\
    }									\
  } while (0);


static int put_utf16_char(SgObject self, SgPort *port, SgChar c,
			  SgErrorHandlingMode mode)
{
  uint8_t buf[4];
  int littlep = SG_CODEC_BUILTIN(self)->littlep;
  int size = Sg_ConvertUcs4ToUtf16(c, buf, mode, littlep);
  /* put BOM if the it's starting position */
  PUT_BOM(port, SG_CODEC_ENDIAN(self), littlep);
  put_binary_array(port, buf, size);
}

static SgChar get_utf16_char(SgObject self, SgPort *port,
			     SgErrorHandlingMode mode, int checkBOM)
{
  return Sg_ConvertUtf16ToUcs4(port, mode, SG_CODEC(self), checkBOM);
}

/* we know utf16 definitely have 2 bytes character. */
DEFINE_READER(read_utf16, Sg_ConvertUtf16BufferToUcs4, size*2);

static int64_t write_utf16(SgObject self, SgPort* port, SgChar *str,
			   int64_t count, SgErrorHandlingMode mode)
{
  /* we at lease need 'size' size buffer. */
  uint8_t tmp[TMP_BUF_SIZE];
  int converted_size = 0, littlep = SG_CODEC_BUILTIN(self)->littlep;
  int64_t i;
  PUT_BOM(port, SG_CODEC_ENDIAN(self), littlep);
  /* we can not know the real size until we convert. */
  for (i = 0; i < count; i++) {
    SgChar c = str[i];
    converted_size += Sg_ConvertUcs4ToUtf16(c, tmp + converted_size,
					    mode, littlep);
    if (converted_size >= TMP_BUF_SIZE) {
      /* flush */
      Sg_WritebUnsafe(port, tmp, 0, converted_size);
      converted_size = 0;
    }
  }
  if (converted_size != 0) {
    /* flush the rest */
    Sg_WritebUnsafe(port, tmp, 0, converted_size);
  }
  return i;
}


SgObject Sg_MakeUtf16Codec(SgEndianness endian)
{
  SgCodec* z;
  ASSERT(endian == UTF_16BE || endian == UTF_16LE || endian == UTF_16CHECK_BOM);
  z = make_codec();
  SG_CODEC_BUILTIN(z)->littlep = (endian == UTF_16LE);
  SG_CODEC_BUILTIN(z)->putc = put_utf16_char;
  SG_CODEC_BUILTIN(z)->getc = get_utf16_char;
  SG_CODEC_BUILTIN(z)->readc = read_utf16;
  SG_CODEC_BUILTIN(z)->writec = write_utf16;
  SG_CODEC_NAME(z) = SG_MAKE_STRING("utf16-codec");
  SG_CODEC_ENDIAN(z) = endian;
  return SG_OBJ(z);
}


#define decodeError(who)						\
  switch (mode) {							\
  case SG_RAISE_ERROR:							\
    Sg_IOError(SG_IO_DECODE_ERROR, who,					\
	       Sg_Sprintf(UC("invalid encode. %S, %s:%d"),		\
			  self, UC(__FILE__), __LINE__),		\
	       SG_UNDEF, port);						\
    return -1;			/* dummy */				\
  case SG_REPLACE_ERROR:						\
    return 0xFFFD;							\
  case SG_IGNORE_ERROR:							\
    goto retry;								\
 default:								\
   Sg_Panic("[internal error] unknown error mode.");			\
   return -1;			/* dummy */				\
  }

static void char_to_utf8_array(SgObject self, SgChar u, uint8_t *buf)
{
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
}

static int put_utf32_char(SgObject self, SgPort *port, SgChar u,
			  SgErrorHandlingMode mode)
{
  uint8_t buf[4];
  /* for now we don't do for UTF-32 */
  /* PUT_BOM(port, SG_CODEC_ENDIAN(self)); */
  char_to_utf8_array(self, u, buf);
  put_binary_array(port, buf, 4);
}

static SgChar get_utf32(int (*u8_reader)(void *), SgObject self, 
			SgPort *port,
			SgErrorHandlingMode mode, void *data)
{
  int a, b, c, d;
  SgChar sv;
 retry:
  a = u8_reader(data);
  if (a == EOF) return EOF;
  b = u8_reader(data);
  if (b == EOF) {
    decodeError(SG_INTERN("utf32-codec"));
  }
  c = u8_reader(data);
  if (c == EOF) {
    decodeError(SG_INTERN("utf32-codec"));
  }
  d = u8_reader(data);
  if (d == EOF) {
    decodeError(SG_INTERN("utf32-codec"));
  }
  if (SG_CODEC_ENDIAN(self) == UTF_32LE) {
    sv = (((uint8_t)a)       |
	 ((uint8_t)b) << 8  |
	 ((uint8_t)c) << 16 |
	 ((uint8_t)d) << 24);
  } else {
    sv = (((uint8_t)d)       |
	 ((uint8_t)c) << 8  |
	 ((uint8_t)b) << 16 |
	 ((uint8_t)a) << 24);
  }

  if (sv > 0x10FFFF || (0xD800 <= sv && sv <= 0xDFFF)) {
    decodeError(SG_INTERN("utf32-codec"));
  }

  return sv;
}

static int port_u8_reader(void *data)
{
  return Sg_GetbUnsafe(SG_PORT(data));
}

static SgChar get_utf32_char(SgObject self, SgPort *port, 
			     SgErrorHandlingMode mode, int checkBOM)
{
  return get_utf32(port_u8_reader, self, port, mode, port);
}

typedef struct
{
  int64_t  pos;
  uint8_t *buf;
  int64_t  buf_size;
  SgPort  *port;
} u8_reader_ctx;
static int buffer_u8_reader(void *data)
{
  u8_reader_ctx *ctx = (u8_reader_ctx *)data;
  if (ctx->pos < ctx->buf_size) {
    return ctx->buf[ctx->pos++];
  } else {
    return Sg_GetbUnsafe(ctx->port);
  }
}
static int64_t convert_utf32_buffer_ucs32(SgCodec *codec,
					  uint8_t *u8buf, int64_t u8size,
					  SgChar *buf, int64_t size,
					  SgPort *port,
					  SgErrorHandlingMode mode,
					  int checkBOM)
{
  int64_t i;
  u8_reader_ctx ctx;
  ctx.pos = 0;
  ctx.buf = u8buf;
  ctx.buf_size = u8size;
  ctx.port = port;
  for (i = 0; i < size; i++) {
    SgChar c = get_utf32(buffer_u8_reader, codec, port, mode, &ctx);
    if (c == EOF) return i;
    buf[i] = c;
  }
  return i;
}

/* UTF32 is always 4 byte */
DEFINE_READER(read_utf32, convert_utf32_buffer_ucs32, size*4);

static int64_t write_utf32(SgObject self, SgPort* port, SgChar *s,
			   int64_t count, SgErrorHandlingMode mode)
{
  uint8_t tmp[TMP_BUF_SIZE];
  int converted = 0;
  int64_t i;
  /* PUT_BOM(port, SG_CODEC_ENDIAN(self)); */
  for (i = 0; i < count; i++) {
    char_to_utf8_array(self, s[i], tmp + converted);
    converted += 4;
    if (converted >= TMP_BUF_SIZE) {
      Sg_WritebUnsafe(port, tmp, 0, converted);
      converted = 0;
    }
  }
  if (converted != 0) {
    Sg_WritebUnsafe(port, tmp, 0, converted);
    converted = 0;
  }
  return i;
}


SgObject Sg_MakeUtf32Codec(SgEndianness endian)
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
  SG_CODEC_BUILTIN(z)->littlep = (SG_CODEC_ENDIAN(z) == UTF_32LE);
  SG_CODEC_BUILTIN(z)->putc = put_utf32_char;
  SG_CODEC_BUILTIN(z)->getc = get_utf32_char;
  SG_CODEC_BUILTIN(z)->readc = read_utf32;
  SG_CODEC_BUILTIN(z)->writec = write_utf32;
  return SG_OBJ(z);
}

static int convert_latin1(SgPort *port, SgChar c,
			  uint8_t *buf, SgErrorHandlingMode mode)
{
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
  return size;
}

static int put_latin1_char(SgObject self, SgPort *port, SgChar c,
			   SgErrorHandlingMode mode)
{
  uint8_t buf[1];
  int size = convert_latin1(port, c, buf, mode);
  put_binary_array(port, buf, size);
}

static SgChar get_latin1_char(SgObject self, SgPort *port,
			      SgErrorHandlingMode mode, int checkBOM)
{
  int f;
  f = Sg_GetbUnsafe(port);
  if (f == EOF) return EOF;
  return (SgChar)f;
}

static int get_latin1_from_ctx(u8_reader_ctx *ctx)
{
  int f;
  f = buffer_u8_reader(ctx);
  if (f == EOF) return EOF;
  return (SgChar)f;
}

static int64_t convert_latin1_buffer_ucs32(SgCodec *codec,
					   uint8_t *u8buf, int64_t u8size,
					   SgChar *buf, int64_t size,
					   SgPort *port,
					   SgErrorHandlingMode mode,
					   int checkBOM)
{
  int64_t i;
  u8_reader_ctx ctx;
  ctx.pos = 0;
  ctx.buf = u8buf;
  ctx.buf_size = u8size;
  ctx.port = port;
  for (i = 0; i < size; i++) {
    SgChar c = get_latin1_from_ctx(&ctx);
    if (c == EOF) return i;
    buf[i] = c;
  }
  return i;
}

/* latin1 is always one byte */
DEFINE_READER(read_latin1, convert_latin1_buffer_ucs32, size);

static int64_t write_latin1(SgObject self, SgPort* port, SgChar *s,
			    int64_t count, SgErrorHandlingMode mode)
{
  /* actually, we can just dump it, but for checking... */
  uint8_t tmp[TMP_BUF_SIZE];
  int converted = 0;
  int64_t i;
  for (i = 0; i < count; i++) {
    converted += convert_latin1(port, s[i], tmp + converted, mode);
    if (converted >= TMP_BUF_SIZE) {
      Sg_WritebUnsafe(port, tmp, 0, converted);
      converted = 0;
    }
  }
  if (converted != 0) {
    Sg_WritebUnsafe(port, tmp, 0, converted);
    converted = 0;
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
  SG_CODEC_NAME(z) = SG_MAKE_STRING("latin1-codec");
  return SG_OBJ(z);
}


SgEndianness Sg_Utf16CheckBOM(SgByteVector *bv)
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

SgEndianness Sg_Utf32CheckBOM(SgByteVector *bv)
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

static SgObject readc_proc(SgObject *args, int argc, void *data)
{
  SgObject codec, port, size, mode, sdata;
  SgStringPort out;
  int count, i;
  codec = SG_OBJ(data);
  if (argc != 4) {
    Sg_WrongNumberOfArgumentsViolation(SG_INTERN("default-codec-readc"),
				       4, argc, codec);
  }
  port = args[0];
  size = args[1];
  mode = args[2];
  sdata = args[3];
  count = SG_INT_VALUE(size);
  Sg_InitStringOutputPort(&out, -1);
  /* transcoder must handle the first character */
  for (i = 0; i < count; i++) {
    SgObject c = Sg_Apply4(SG_CODEC_CUSTOM(codec)->getc, port, mode, SG_FALSE,
			   sdata);
    if (SG_CHARP(c)) {
      Sg_PutcUnsafe(SG_PORT(&out), SG_CHAR_VALUE(c));
    } else if (SG_EOFP(c)) {
      break;
    } else {
      Sg_AssertionViolation(SG_INTERN("default-codec-readc"),
			    SG_MAKE_STRING("getc procedure returned non character object"), c);
    }
  }
  return Sg_GetStringFromStringPort(&out);
}

static SgObject writec_proc(SgObject *args, int argc, void *data)
{
  SgObject codec, port, str, mode, sdata;
  int i;
  codec = SG_OBJ(data);
  if (argc != 4) {
    Sg_WrongNumberOfArgumentsViolation(SG_INTERN("default-codec-writec"),
				       4, argc, codec);
  }
  port = args[0];
  str = args[1];
  mode = args[2];
  sdata = args[3];
  /* transcoder should handle the first character */
  for (i = 0; i < SG_STRING_SIZE(str); i++) {
    Sg_Apply4(SG_CODEC_CUSTOM(codec)->putc, port,
	      SG_MAKE_CHAR(SG_STRING_VALUE_AT(str, i)), mode,
	      sdata);
  }
  return SG_MAKE_INT(i);
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
  SG_CODEC_CUSTOM(z)->readc  = Sg_MakeSubr(readc_proc, z, 4, 0, SG_FALSE);
  SG_CODEC_CUSTOM(z)->writec = Sg_MakeSubr(writec_proc, z, 4, 0, SG_FALSE);
  return SG_OBJ(z);
}

/*
  end of file
  Local Variables:
  coding: utf-8-unix
  End:
*/
